/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* zero-window alpha-beta search */

#include <stdlib.h>
#include <stdio.h>

#include <ab_info.h>
#include <search.h>
#include <node.h>
#include <prune.h>
#include <debug.h>
#include <tourn.h>
#include <prune.h>
#include <exact.h>
#include <check.h>
#include <stat.h>

move_t find_best ();
move_t next_move ();
void abmake_move ();
void absetup_move ();
eval_t ab (), ab2 ();
int guess_min (), elapsed_time ();
void print_all ();
void find_legals ();
void put_in ();

extern stat_info_t Stat_Info[2][MAX_MOVES];
extern general_info_t General_Info[2];
extern ab_info_t AB_Info[2];
extern struct killer_table Killer_Table[2][MAX_PIECES+1];
extern Node    Game_Record[MAX_MOVES];
extern double   Time_Fraction[2][MAX_MOVES / 2];
extern int      Print_Search,
  		Save_Leaf,
                Steal_Time,
                Corner[5],
                C1[5],
                C2[5],
                X_Square[MAX_PIECES],
                C_Square[MAX_PIECES],
                C_Corner[9],
                C_B[9],
                C_A[9],
                Right_16,
                Time_Left[2],
                Type1_Time[MAX_PIECES],
                Type2_Time[MAX_PIECES],
                Color_Turn_Num[2],
                Turn_Num,
                Discs,
		Wake_Up,
                Accept_Interrupt,
                wake_up (),
                Node_Count, Leaf_Nodes;

char           *print_eval (), *strcat (),
    	       *print_discs (), *print_depth (),
  	       *print_finished_depth (), *print_type ();
static void insert_best_move (legal_t legals, move_t move);
static void print_return (move_t move, ANode *next_rec, 
		     eval_t window_min, eval_t window_max);

/* converts the external node representation to what the alpha-beta,
   zero-window routines need.
   It puts node into abstack[0], and adds a few things */

ANode ABstack[30];		/* max depth is 30 */

move_t deepen_front_end (node, stealing)
     Node *node;		/* contains board, color, and move made from here (none yet) */
     int stealing;		/* thinking on opponent's time */
{
  move_t stealfe(), deepen();

  copy_board (node->board, ABstack[0].board);
  ABstack[0].color = node->color;
  ABstack[0].key = compute_key (node->board);
  compute_discs ((Node *) ABstack);
  return stealing ? stealfe  (ABstack, ABstack+1,ABstack+2,
			      /* don't want to pass junk move */
			      node == Game_Record ? NO_MOVE : node[-1].move) :
    		    deepen (ABstack, ABstack+1,
			    /* don't want to pass junk move */
			    node == Game_Record ? NO_MOVE : node[-1].move);
}

move_t stealfe (current_rec, next_rec, final_rec, prev_move)
     ANode *current_rec, *next_rec, *final_rec;
     move_t prev_move;
{
  int depth = AB_Info[current_rec->color].small_search_depth;
  move_t best;

  /* first have to guess move */
  if ((best = find_best(current_rec)) == NO_MOVE) /* nothing in hash table */
    {
      absetup_move (current_rec, NO_MOVE);
      ab (current_rec, depth, -INFINITY, INFINITY,
	  current_rec->color, General_Info[current_rec->color].eval_stuff.eval);
      if (Wake_Up)		/* couldn't even do small search */
	return NO_MOVE;
      best = current_rec->best;
    }
  if (!is_legal (current_rec, best))
#ifdef DEBUG
    internal_error ("Claiming illegal move to be legal.");
#else
  {
    legal_list legals;

    fprintf (stderr, "Claiming illegal move to be legal.  Patching...");
    find_legals (current_rec, NO_MOVE, best);
    if (legals.max == 0)
      best = NO_MOVE;
    else
      best = legals.data[0];
  }
#endif
  
  /* now make guessed move */
  current_rec->move = best;
  abmake_move (current_rec, next_rec, TRUE);
  steal (next_rec, final_rec, best);
  return best;
}

int is_legal (node, move)
  Node *node;
  move_t move;
{
  legal_list legals;
  int i;

  find_legals (node, NO_MOVE, &legals);
  if (!LEGAL(move))		/* if NO_MOVE */
    return legals.max == 0;	/* has to be null legal list */
  for (i=0;i<legals.max;i++)
    if (legals.data[i] == move)
      return 1;
  return 0;
}
    
/* deepen performs iterative deepening.  It calls alpha-beta with the
 * starting stack element whenever it determines there is enough time to do
 * another iteration.  It returns the best move found so far.  color = whose
 * move it is.  All other data is found in the global array Game_Record. */

move_t deepen (current_rec, next_rec, prev_move)
    ANode          *current_rec,
                   *next_rec;
    move_t prev_move;
{
    double          finished_depth[2];	/* how many plies were finished.  Used
					   to decide how deep to start the search. */
    move_t          best;		/* temporary storage of best move */
    move_t          move1,     
                    move2;
    float           time_left;
    eval_t          prev_alpha,		/* alpha one level above - used to
					 * avg for window */
                    eval_alpha;		/* the alpha from eval (not pieces) -
					 * for guessing */
    int             finished_nodes,	/* how many top level nodes are done */
                    i,			/* for looping through depth */
                    index;
    int             agree;		/* how many of the last iterations
					 * agreed */
    eval_t          window_center,
                    window_min,
                    window_max,
		    diff;
    int             found_better,	/* whether a better top-level was
					 * found */
                    time_used = 0,	/* how much time has been used */
    	  	    first_depth,
                    allocated_time,
                    upper_time_limit,
                    lower_time_limit;
    legal_list legals;		/* pointer to legals */
    static char    *exact_name[3] = {"    ", "  ? ", " ?? "};
    color_t color, opp_color;
    square_t *board;
  int num_empty;

    Wake_Up = FALSE;
    color = current_rec->color;
    board = current_rec->board;
    opp_color = Opposite(color);
    if (!LEGAL(prev_move))
      prev_move = MAX_PIECES;

    find_legals ((Node *)current_rec, prev_move, &legals);
    allocated_time = Time_Fraction[color][Turn_Num / 2] * General_Info[color].time_left;
    /* subtract stealing time */
    allocated_time -= Stat_Info[color][Turn_Num-1].time_used;
    if (allocated_time < 0)
      {
	allocated_time = lower_time_limit = 0;
	upper_time_limit = 1000; /* need 1 sec for alarm call */
      }
    else
      {
	upper_time_limit = allocated_time * 1.4;
	lower_time_limit = allocated_time * 0.4;
      }

    finished_nodes = 0;
    Node_Count = 0;
    Leaf_Nodes = 0;

    /* In case we do TYPE 1 immediately ... */
    current_rec->alpha = AB_Info[color].last_score;
    num_empty = MAX_PIECES - num_discs(current_rec);

    /* if used up time allocation or comleted Type 2 search to end while opponent was thinking: */
    if (Steal_Time == SOLVED || num_empty > 12 && allocated_time == 0)
    {
      /* look up guy in hash table */
	if (!LEGAL(best = find_best (current_rec)))
	    curse ("Unexpected illegal move in top level.");
	else
	{
	    found_better = FALSE;
	    if (AB_Info[color].exact != INCOMPLETE)
	      finished_nodes = legals.max;
	    else
	      finished_nodes =
		(int) (0.5 +
		       (AB_Info[color].exact - (int)AB_Info[color].exact)
		       * legals.max);
/*	    i = finished_depth[color]; */
	    i = AB_Info[color].depth;
	    current_rec->alpha = AB_Info[color].last_score;  /* didn't do a search */
	    current_rec->best = best;
	    lower_time_limit = upper_time_limit = 0;
	    goto wake;
	}
    }
    if (upper_time_limit >= General_Info[color].time_left || General_Info[color].time_left < 60000)
	upper_time_limit = allocated_time;
    if (lower_time_limit < 0)
	lower_time_limit = 0;

    first_depth = Max(2, AB_Info[color].depth-2);

    /* Unless we've searched to bottom, always repeat the search from last
     * time (unless a best move can be found in the hash table, which means
     * we've searched this subtree before). */
    if (!AB_Info[color].bottom_searched)
    {
	current_rec->alpha = AB_Info[color].last_score;
	best = find_best (current_rec);
	current_rec->best = current_rec->move = best != NO_MOVE ? best : legals.data[0];
	time_used = elapsed_time (FALSE);
	if (Print_Search)
	    print_all ("Deepen", print_depth (first_depth),
		       current_rec->best, NO_MOVE,
		       strcat (print_eval (current_rec->alpha), "    "),
		       time_used, lower_time_limit, upper_time_limit, color);
    }
    alarm ((unsigned) upper_time_limit / 1000);
    agree = 1;

#ifdef PRUNE_XC
    if (!AB_Info[color].bottom_searched)
    {
	have_non_x_move = FALSE;
	for (i = 0; i < legals.max; i++)
	    if (!X_Square[legals.data[i]] && !C_Square[legals.data[i]])
	    {
		have_non_x_move = TRUE;
		break;
	    }
    }
#endif

    /* Do deepening one level at a time.  Since top level will be handled
     * here in deepen, i is really one less than the actual deepened depth.
     * Have a move ready, so allow for interrupt. */
    Leaf_Nodes = 0;
    Accept_Interrupt = TRUE;

    /* there's a funny bug I can't find, where somehow (I think) steal
       searches to a huge depth (bogusly) because the legal moves aren't
       set right, so then deepen tries to pick up from there.  So a
       patch: */
    if (first_depth >= 20)	/* horrid patch */
    {
      fprintf (stderr, "Warning: had to lower first_depth in deepen\n");
      first_depth = 5;
    }

    for (i = first_depth; i <= MAX_DEPTH; i++)
    {
	if (AB_Info[color].bottom_searched == AB2_2 ||
	    AB_Info[color].bottom_searched == AB2_1 &&
	    General_Info[color].time_left - time_used > Type2_Time[num_empty])
	{
    type2:
	    /* have already searched to the end - now searching to the end */
	    /* set up a move */
	    current_rec->next_legal_try = Killer_Table[color][prev_move].order;

	    if (!LEGAL(best = find_best (current_rec)))
	    {
		best = next_move (current_rec, NO_MOVE);
		put_in (current_rec->key, num_discs(current_rec),
			current_rec->color, best);
	    }
	    current_rec->best = current_rec->already_tried = current_rec->move = best;
	    finished_nodes = 0;
	    insert_best_move (&legals, best);
	    abmake_move (current_rec, next_rec, TRUE);

	    window_max = (window_min = AB_Info[color].last_score - 1) +
	      (AB_Info[color].bottom_searched == AB2_1 ? 4 : 2);

	    /* reset alarm to more time */
	    alarm ((unsigned) 0);
	    time_left = General_Info[color].time_left - time_used;
	    /* make sure we save enough time to do the next search */
	    upper_time_limit = Min (time_left * 0.7,
				    time_left - Type2_Time[num_empty - 2]);
	    alarm ((unsigned) upper_time_limit / 1000);

	    /* alpha-beta for first branch */
	    ab2 (next_rec, window_min, window_max, opp_color);
	    print_return (current_rec->best, next_rec, window_min, window_max);
	    if (Wake_Up)
		goto wake;
	    current_rec->alpha = -next_rec->alpha;

	    /* failing high */
	    if (current_rec->alpha >= window_max)
	    {
		while (current_rec->alpha >= window_max)
		{
		    window_max = (window_min = window_max + 5) + 5;
		    current_rec->already_tried = current_rec->move = best;
		    absetup_move (next_rec, current_rec->move);
		    ab2 (next_rec, window_min, window_max, opp_color);
		    print_return (current_rec->move, next_rec, 
				  window_min, window_max);
		    if (Wake_Up)
			goto wake;
		    current_rec->alpha = -next_rec->alpha;
		}
		if (current_rec->alpha <= window_min)
		{
		    window_min = (window_max = window_min) - 5;
		    absetup_move (next_rec, current_rec->move);
		    ab2 (next_rec, window_min, window_max, opp_color);
		    print_return (current_rec->move, next_rec, 
				  window_min, window_max);
		    if (Wake_Up)
			goto wake;
		    current_rec->alpha = -next_rec->alpha;
		}
	    } else
	    if (current_rec->alpha <= window_min && AB_Info[color].bottom_searched == AB2_1 &&
		AB_Info[color].last_score != 0)
		/* Failing low - only care when last search == AB2_1 and
		 * result is not 0 because with TYPE2 searches and AB2_1
		 * searches with result 0, we CANNOT fail low. */
	    {
		while (current_rec->alpha <= window_min)
		{
		    window_min = (window_max = window_min - 5) - 5;
		    current_rec->already_tried = current_rec->move = best;
		    absetup_move (next_rec, current_rec->move);
		    ab2 (next_rec, window_min, window_max, opp_color);
		    print_return (current_rec->move, next_rec, 
				  window_min, window_max);
		    if (Wake_Up)
			goto wake;
		    current_rec->alpha = -next_rec->alpha;
		}
		if (current_rec->alpha >= window_max)
		{
		    window_max = (window_min = window_max) + 5;
		    absetup_move (next_rec, current_rec->move);
		    ab2 (next_rec, window_min, window_max, opp_color);
		    print_return (current_rec->move, next_rec, 
				  window_min, window_max);
		    if (Wake_Up)
			goto wake;
		    current_rec->alpha = -next_rec->alpha;
		}
	    }
	    /* go through the remaining branches */
	    AB_Info[color].bottom_searched = AB2_2;
	    window_min = current_rec->alpha;
	    found_better = FALSE;
	    finished_nodes++;
	    for (index = 0; index < legals.max; index++)
	    {
		if (legals.data[index] == current_rec->already_tried)
		    continue;

		finished_nodes++;
		current_rec->move = legals.data[index];
		abmake_move (current_rec, next_rec, TRUE);
		window_max = window_min + 1;
		ab2 (next_rec, window_min, window_max, opp_color);
		print_return (current_rec->move, next_rec, 
			      window_min, window_max);
		if (Wake_Up)
		    goto wake;
		if (-next_rec->alpha < window_max)
		    continue;

		/* If it's the first one, we're OK. */
		if (!found_better)
		{
		    found_better = TRUE;
		    current_rec->alpha = ++window_min;
		    current_rec->best = current_rec->move;
		    insert_best_move (&legals, current_rec->best);
		    put_in (current_rec->key, num_discs(current_rec),
			    current_rec->color, current_rec->move);
		    /* printf ("Found 1st better -> %s\n", uncvt (current_rec
		     * -> best)); */
		    continue;
		}
		/* If already have a better one, must search both.  May want
		 * to change this later - if we're losing, and two better
		 * moves are found or if there is plenty of time, should find
		 * out which is better */
		move1 = current_rec->move;
		move2 = current_rec->best;
		insert_best_move (&legals, move1);
		absetup_move (next_rec, move1);
		current_rec->move = move1;
		ab2 (next_rec, window_max, (eval_t) 64, opp_color);
		printf ("Ab2-2 (%s) returns %3d (%d - %d) %s", 
			uncvt (move1), -next_rec->alpha, 
			window_min, window_max, Wake_Up ? "Alarm!\n" : "\n");
#ifdef DEBUG		
		fflush (stdout);
#endif
		if (Wake_Up)
		    goto wake;
		current_rec->alpha = window_min = -next_rec->alpha;
		current_rec->move = move2;
		abmake_move (current_rec, next_rec, TRUE);
		ab2 (next_rec, window_min, (eval_t) 64, opp_color);
		printf ("Ab2-2 (%s) returns %3d (%d - %d) %s", 
			uncvt (move2), -next_rec->alpha, 
			window_min, window_max, Wake_Up ? "Alarm!\n" : "\n");
#ifdef DEBUG
		fflush (stdout);
#endif
		if (Wake_Up)
		    goto wake;
		if (-next_rec->alpha <= window_min)
		{
		    current_rec->best = move1;
		    put_in (current_rec->key, num_discs(current_rec),
			    current_rec->color, current_rec->best);
		} else
		{
		    insert_best_move (&legals, move2);
		    current_rec->alpha = window_min = -next_rec->alpha;
		}
		found_better = FALSE;
	    }
	    break;
	}
	/* Searching to end for the first time - use of zero/one windows
	 * around 0 */
	else
	if (AB_Info[color].bottom_searched || num_empty - i <= 9 &&
	    (i >= 6 || num_empty <= 12) && General_Info[color].time_left - time_used > Type1_Time[num_empty])
	{
	    eval_alpha = current_rec->alpha;
	    current_rec->next_legal_try = Killer_Table[color][prev_move].order;
	    if (!LEGAL(best = find_best (current_rec)))
	    {
		best = next_move (current_rec, NO_MOVE);
		put_in (current_rec->key, num_discs(current_rec),
			current_rec->color, best);
	    }
	    current_rec->best = current_rec->already_tried = current_rec->move = best;

	    /* Clear old alarm and set a longer one - done after best is set
	     * so that even if we're out of time, we will have some move. */
	    alarm ((unsigned) 0);
	    time_left = General_Info[color].time_left - time_used;
	    upper_time_limit = Min (time_left * 0.5, time_left - Type2_Time[num_empty - 2]);
	    alarm ((unsigned) upper_time_limit / 1000);

	    finished_nodes = 0;
	    /* We're always unsure of the value, so set found_better TRUE. */
	    found_better = TRUE;
	    abmake_move (current_rec, next_rec, TRUE);

	    /* Use a one-window for win/loss/tie. */
	    window_min = -1;
	    window_max = 1;
	    ab2 (next_rec, window_min, window_max, opp_color);
	    if (Wake_Up)
		goto wake;
	    finished_nodes++;
	    if (AB_Info[color].bottom_searched != AB2_2)
		AB_Info[color].bottom_searched = AB2_1;

	    window_min = current_rec->alpha = -next_rec->alpha;
	    if (current_rec->alpha > 0)
	    {
	      put_in (current_rec->key, num_discs(current_rec),
		      current_rec->color, current_rec->move);
	      insert_best_move (&legals, current_rec->move);
	      goto done;
	    }
	    /* go through each of the remaining moves */
	    for (index = 0; index < legals.max; index++)
	    {
		if (legals.data[index] == current_rec->already_tried)
		    continue;

		finished_nodes++;
		current_rec->move = legals.data[index];
		abmake_move (current_rec, next_rec, TRUE);
		window_max = window_min + 1;
		ab2 (next_rec, window_min, window_max, opp_color);

		if (Wake_Up)
		    goto wake;
		if (-next_rec->alpha < window_max)
		    continue;

		current_rec->alpha = -next_rec->alpha;
		current_rec->best = current_rec->move;
		put_in (current_rec->key, num_discs(current_rec),
			current_rec->color, current_rec->move);
		insert_best_move (&legals, current_rec->best);
		window_min = window_max;
		if (current_rec->alpha > 0)
		    break;
	    }

    done:
	    printf ("Type 1 AB2 finished - found %s (%d) guess = ", 
		     uncvt (current_rec->best), current_rec->alpha);
#ifdef DEBUG
	    fflush (stdout);
#endif
	    if (current_rec->alpha > 0)
		/* We will win, but guess how much for window next time; if
		 * evaluation function <= 0, don't want to guess negative
		 * though. */
	    {
		register eval_t temp;

		if ((temp = guess_min (eval_alpha)) > 0)
		    current_rec->alpha = temp;
	    }
	    else if (current_rec->alpha == 0)
		/* We will at least tie, and this IS the best move! */
	    {
		printf ("%d (elapsed time = %6.2f)\n",
			current_rec->alpha, elapsed_time (FALSE) / 1000.0);
		printf ("Know best move; no Type 2 search necessary.\n");
#ifdef DEBUG		
		fflush (stdout);
#endif		
		finished_nodes = legals.max;
		goto wake;
	    }
	    else			/* current_rec->alpha < 0 */
	    {
		register eval_t temp;

		if ((temp = guess_min (eval_alpha)) < 0)
		    current_rec->alpha = temp;
	    }
	    printf ("%d (elapsed time = %6.2f)\n",
		    current_rec->alpha, elapsed_time (FALSE) / 1000.0);
#ifdef DEBUG
	    fflush (stdout);
#endif
	    /* If we've used less than a quarter of the time, find better
	     * move. */
	    time_used = elapsed_time (FALSE);
	    if ((General_Info[color].time_left - time_used) > Type2_Time[num_empty])
	    {
		AB_Info[color].last_score = current_rec->alpha;
		print_all (print_type (AB_Info[color].bottom_searched),
			   print_depth (num_empty),
			   current_rec->best, NO_MOVE,
			   strcat (print_discs (current_rec->alpha),
				   AB_Info[color].bottom_searched ?
				   "    " : " ??"),
			   time_used,
			   lower_time_limit, upper_time_limit,
			   color);
		goto type2;
	    }
	    finished_nodes = legals.max;
	    goto wake;
	}
	/* not searching to the end */
	else
	{
	    finished_nodes = 0;
	    AB_Info[color].last_score = current_rec->alpha;
	    if (i > AB_Info[color].max_levels)
		break;
	    agree++;			/* assume this one will agree with
					 * last */
	    current_rec->next_legal_try = Killer_Table[color][prev_move].order;	/* set up move */

	    if (!LEGAL(best = find_best (current_rec)))
	    {
		best = next_move (current_rec, NO_MOVE);
		put_in (current_rec->key, num_discs(current_rec),
			current_rec->color, best);
	    }
	    insert_best_move (&legals, best);
	    current_rec->best =	current_rec->already_tried = current_rec->move = best;
	    abmake_move (current_rec, next_rec, TRUE);

	    /* It is more accurate to average the last two alpha's for
	     * window. */
	    window_center = i > first_depth ? (current_rec->alpha + prev_alpha) / 2 : current_rec->alpha;

	    /* This may need some tuning. */
	    diff = (Abs (current_rec->alpha) / 4) + 10000;
	    window_max = window_center + diff;
	    window_min = window_center - diff;
	    ab (next_rec, i, window_min, window_max, opp_color,	General_Info[color].eval_stuff.eval);

	    if (Wake_Up)
		goto wake;
	    current_rec->alpha = -next_rec->alpha;

	    while (current_rec->alpha >= window_max)
	    {
	      /* printf ("Failed high (%d %d)!\n", window_min, window_max);*/
		diff *= 2;
		window_min = window_max;
		window_max = window_max + diff;
		absetup_move (next_rec, current_rec->move);
		ab (next_rec, i, window_min, window_max, opp_color,General_Info[color].eval_stuff.eval);
		if (Wake_Up)
		    goto wake;
		current_rec->alpha = -next_rec->alpha;
	    }

	    while (current_rec->alpha <= window_min)
	    {
	      /* printf ("Failed low! (%d %d)\n", window_min, window_max); */
		diff *= 2;
		window_max = window_min;
		window_min = window_min - diff;
		absetup_move (next_rec, current_rec->move);
		ab (next_rec, i, window_min, window_max, opp_color,General_Info[color].eval_stuff.eval);
		if (Wake_Up)
		    goto wake;
		current_rec->alpha = -next_rec->alpha;
	    }

	    /* go through the remaining moves */
	    window_min = current_rec->alpha;
	    found_better = FALSE;
	    finished_nodes++;
	    for (index = 0; index < legals.max; index++)
	    {
		if (legals.data[index] == current_rec->already_tried)
		    continue;

		finished_nodes++;
		current_rec->move = legals.data[index];

#ifdef PRUNE_XC
		if (have_non_x_move &&
		    (((xnum = X_Square[current_rec->move]) &&
		      (Empty (board[Corner[xnum]])) &&
		      ((Discs <= TOP_ALWAYS_PRUNE_X) ||
		       ((Discs <= TOP_CONDITIONAL_PRUNE_X) &&
			(Occupied (board[C1[xnum]])) &&
			(Occupied (board[C2[xnum]]))))) ||
		     ((cnum = C_Square[current_rec->move]) &&
		      (Empty (board[C_Corner[cnum]])) &&
		      (Empty (board[C_B[cnum]])) &&
		      (board[C_A[cnum]] != opp_color) &&
		      (Discs <= TOP_PRUNE_C))))
		    continue;
#endif

		abmake_move (current_rec, next_rec, TRUE);
		window_max = window_min + 1;
		ab (next_rec, i, window_min, window_max, opp_color,General_Info[color].eval_stuff.eval);
		if (Wake_Up)
		    goto wake;
		if (-next_rec->alpha < window_max)
		    continue;

		/* If it's the first one, we're OK. */
		if (!found_better)
		{
		    agree = 1;
		    found_better = TRUE;
		    current_rec->alpha = -next_rec->alpha;
		    current_rec->best = current_rec->move;
		    put_in (current_rec->key, num_discs(current_rec),
			    current_rec->color, current_rec->move);
		    insert_best_move (&legals, current_rec->best);
		    continue;
		}
		/* If already have a better one, must search both. */
		move1 = current_rec->move;
		move2 = current_rec->best;
		insert_best_move (&legals, move1);
		absetup_move (next_rec, move1);
		current_rec->move = move1;
		ab (next_rec, i, window_max, INFINITY, opp_color,General_Info[color].eval_stuff.eval);

		if (Wake_Up)
		{
		    Wake_Up = FALSE;	/* Could be important, so spend at
					 * most 1/16 of the time remaining. */
		    alarm ((unsigned) (General_Info[color].time_left - upper_time_limit) * 64);
		    absetup_move (next_rec, move1);
		    ab (next_rec, i, window_max, INFINITY, opp_color,General_Info[color].eval_stuff.eval);
		    if (Wake_Up)
			goto wake;
		}
		current_rec->alpha = window_min = -next_rec->alpha;
		current_rec->move = move2;
		abmake_move (current_rec, next_rec, TRUE);
		ab (next_rec, i, window_min, INFINITY, opp_color,General_Info[color].eval_stuff.eval);

		if (Wake_Up)
		{
		    Wake_Up = FALSE;	/* Could be important, so spend at
					 * most 1/16 of the time remaining. */
		    alarm ((unsigned) (General_Info[color].time_left - upper_time_limit) * 64);
		    absetup_move (next_rec, move2);
		    ab (next_rec, i, window_max, INFINITY, opp_color,General_Info[color].eval_stuff.eval);
		    if (Wake_Up)
			goto wake;
		}
		if (-next_rec->alpha <= window_min)
		{
		    current_rec->best = move1;
		    put_in (current_rec->key, num_discs(current_rec),
			    current_rec->color, current_rec->best);
		    insert_best_move (&legals, current_rec->best);
		} else
		{
		    insert_best_move (&legals, move2);
		    current_rec->alpha = window_min = -next_rec->alpha;
		}
		found_better = FALSE;
	    }
	}

	prev_alpha = !found_better ? current_rec->alpha : current_rec->alpha > 0 ?
	  current_rec->alpha * 1.5 : current_rec->alpha * 0.67;

	/* Quit iterative deepening if one of the following is true: (1)
	 * exceeded allocated time; (2) exceeded lower bound and last two
	 * iterations agree; or (3) at end of game either by not enough
	 squares left, or (4) wipe-out */
	time_used = elapsed_time (FALSE);
	if (time_used > allocated_time || time_used > lower_time_limit && agree >= 2 || i > num_empty || abs(current_rec->alpha)==INFINITY)
	    break;

	if (Print_Search)
	    print_all ("Deepen",
		       print_depth (i + 1),
		       current_rec->best, NO_MOVE,
		       strcat (print_eval (current_rec->alpha),
			       found_better ? "  ? " : "    "),
		       time_used, lower_time_limit, upper_time_limit, color);
    }

wake:
    AB_Info[color].exact =
      (Wake_Up || finished_nodes < legals.max) ? INCOMPLETE : (found_better ? INEXACT : EXACT);
    if (AB_Info[color].bottom_searched)
	finished_depth[color] = num_empty - 1 + (double) finished_nodes / legals.max;
    else
    {
	finished_depth[color] = i + (double) finished_nodes / legals.max;
	AB_Info[color].depth = finished_depth[color];
    }

    {
      who_t who_temp;

      if (AB_Info[color].bottom_searched == AB2_1)
	who_temp = AB2_1;
      else if (AB_Info[color].bottom_searched == AB2_2)
	who_temp = AB2_2;
      else
	who_temp = AB;
          
      Stat_Info[color][Turn_Num].who = who_temp;
    }
    Stat_Info[color][Turn_Num].exact = AB_Info[color].exact;
    Stat_Info[color][Turn_Num].value = current_rec->alpha;
    Stat_Info[color][Turn_Num].nodes_searched = Node_Count + finished_nodes;
    Stat_Info[color][Turn_Num].leaf_searched = (int) finished_depth[color] <= first_depth && Steal_Time != SOLVED ?
      (Leaf_Nodes = Stat_Info[color][Turn_Num].leaf_searched) : Wake_Up && !AB_Info[color].bottom_searched ?
	(Leaf_Nodes = Save_Leaf) : Leaf_Nodes;
    Stat_Info[color][Turn_Num].depth_searched = finished_depth[color];

    /* just in case interrupt caused a fake bottom_search */
    /* only save the alpha if we completed >=1 branch in zero-window */
    if ((!AB_Info[color].bottom_searched ||
	abs((int) current_rec->alpha) <= MAX_PIECES)
	&& !(AB_Info[color].exact == INCOMPLETE && finished_nodes == 0))
      AB_Info[color].last_score = current_rec->alpha;

    time_used = elapsed_time (FALSE);
    Stat_Info[color][Turn_Num].time_used = time_used;
    if (Print_Search)
	print_all (print_type (AB_Info[color].bottom_searched),
		   print_finished_depth (finished_depth[color]),
		   current_rec->best, NO_MOVE,
		   strcat (AB_Info[color].bottom_searched ?
			     print_discs (current_rec->alpha) :
			     print_eval (current_rec->alpha),
			   exact_name[(int) AB_Info[color].exact]),
		   time_used, lower_time_limit, upper_time_limit, color);
    Stat_Info[color][Turn_Num].exact = AB_Info[color].exact;
    if (!Wake_Up)
      alarm (0);		/* clear alarm */
    Accept_Interrupt = FALSE;
    return current_rec->best;
}

/* guess window min given alpha - known win only */

int guess_min (alpha)
     eval_t alpha;
{
  eval_t          absvalue;
  register int    i;
  static eval_t   bound[35] =
    {
      100000, 125000, 150000, 175000, 200000,
      250000, 300000, 350000, 400000, 450000,
      500000, 550000, 600000, 650000, 700000,
      750000, 800000, 850000, 900000, 950000,
      1000000, 1050000, 1100000, 1150000, 1200000,
      1300000, 1350000, 1400000, 1450000, 1500000,
      1600000, 1700000, 1800000, 1900000, 2000000,
    };

  absvalue = alpha >= 0 ? alpha : -alpha;
  for (i = 0; i < 35; i++)
    if (bound[i] >= absvalue)
      break;
  return alpha >= 0 ? i : -i;
}

/* insert_best_move puts the newly found best move in the beginning of the legals list, and move everything down one.  This is helpful because the hash table alone can only capture the best move, but not order the moves (although this doesn't guarantee perfect ordering either).  Really should use linked list. */

static void insert_best_move (legals, move)
     register legal_t legals;
     register move_t move;
{
  register int    i;

  for (i = 0; i < legals->max; i++)
    if (legals->data[i] == move)
      break;
  if (i >= legals->max)
    return;

  for (; i > 0; --i)
    legals->data[i] = legals->data[i - 1];
  legals->data[0] = move;
}

static void print_return (move_t move, ANode *next_rec, 
		     eval_t window_min, eval_t window_max)
{
  printf ("Ab2 (%s) returns %d (%d - %d)%s\n", 
	  uncvt(move), -next_rec->alpha, window_min, window_max, 
	  Wake_Up ? " Alarm!" : "");
#ifdef DEBUG
  fflush (stdout);
#endif
}
