/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* alpha-beta stealing - thinking on opponent's time */

#include <stdio.h>

#include <stat.h>
#include <search.h>
#include <general.h>
#include <node.h>
#include <prune.h>
#include <tourn.h>
#include <exact.h>
#include <ab_info.h>

int guess_min ();
move_t find_best ();
void absetup_move ();
void print_all ();
void put_in ();
move_t next_move ();
void abmake_move ();
eval_t ab2 (), ab ();
void insert_best_move2 ();

extern stat_info_t Stat_Info[2][MAX_MOVES];
extern general_info_t General_Info[2];
extern struct killer_table Killer_Table[2][MAX_PIECES+1];
extern Node    Game_Record[MAX_MOVES];
extern double   Time_Fraction[2][MAX_MOVES / 2];
extern          Type2_Time[MAX_PIECES],
                Leaf_Nodes,
                Node_Count,
                Steal_Time,		/* how much time bill stole (thief) */
                C_Square[MAX_PIECES],
                C_Corner[9],
                C_B[9],
                C_A[9],
	        Save_Leaf,
                X_Square[MAX_PIECES],
                Corner[5],
                C1[5],
                C2[5],
                Print_Steal,
                Print_Response,
                Color_Turn_Num[2],
                Turn_Num,
  		Wake_Up,
                Discs;			/* number of new discs placed */
extern ab_info_t AB_Info[2];

char *print_eval (), *print_depth (), *print_discs (), *print_finished_depth (), *print_type ();

int Is_stealing;

/* steal performs iterative deepening while the opponent is thinking.
   Its parameters are identical to those of deepen, except the addition
   of max_level, which tells it how many levels to deepen.  normal_steal
   is TRUE when stealing; FALSE when just searching for a best move for
   opponent (we don't want to print or update globals). */

void steal (current_rec, next_rec, prev_move)
     ANode	    *current_rec, *next_rec;
     move_t prev_move;
{
  double          finished_depth[2];	/* how many plies were finished */
  int		  best,			/* temporary storage of best move */
		  temp;
  move_t	  move1,
		  move2;
  int 		  num_empty;
  eval_t	  prev_alpha,		/* alpha one level above - used to average for window */
                  eval_alpha;		/* the alpha from eval (not pieces) - for guessing */
  int		  finished_nodes,	/* how many top level nodes are done */
		  i,			/* counter */
		  index;
  eval_t	  diff,			/* window size / 2 */
		  window_center,
		  window_min,
		  window_max;
  color_t color = current_rec->color;
  legal_list 	  legals;
  color_t	  opp_color = Opposite(color); /* temp storage of opponent's color */
  int		  found_better,		/* whether a better top-level was found */
		  first_level,		/* what level to begin deepen */
  		  start_time = elapsed_time (FALSE);

  Wake_Up = FALSE;
  Is_stealing = TRUE;
  find_legals ((Node *)current_rec, prev_move, &legals);
  num_empty = MAX_PIECES - num_discs(current_rec);
  if (legals.max <= 0)
  {
    Is_stealing = FALSE;
    return;
  }

  Node_Count = 0;
  if (prev_alpha = AB_Info[opp_color].last_score) /* try value from 1 turn ago */
    prev_alpha = AB_Info[color].last_score; /* if no good, use value from 2 turns ago */

  /* Unless we've searched to bottom, always repeat the search from
     last time (unless a best move can be found in the hash table, which
     means we've searched this subtree before). */

  first_level = Max (AB_Info[color].first_steal_depth - 2, 3) - 1;
  /* In case we do TYPE 1 immediately ... */
  current_rec->alpha = AB_Info[color].last_score;

  if (!AB_Info[color].bottom_searched)
    {
      current_rec->alpha = AB_Info[color].last_score;
      best = find_best (current_rec);
      current_rec->best = current_rec->move =
	best >= 0 ? best : legals.data[0];
    }

#ifdef PRUNE_XC
  if (!AB_Info[color].bottom_searched)
    {
      have_non_x_move = FALSE;
      for (i = 0; i < legals.max; i++)
	{
	  if (!X_Square[legals.data[i]] && !C_Square[legals.data[i]])
	    {
	      have_non_x_move = TRUE;
	      break;
	    }
	}
    }
#endif

  Leaf_Nodes = 0;

  /* Deepening one level at a time.  Since top level will be handled
     here in deepen, i is really one less than the actual deepened depth. */

    for (i = first_level; i < MAX_DEPTH; i++)
    {
	Save_Leaf = Leaf_Nodes;
	if (AB_Info[color].bottom_searched)
	{
    type2:
	  /* Have already searched to the end - now searching to the end (?). */

	  /* set up a move */
	    current_rec->next_legal_try = Killer_Table[color][prev_move].order;
	    best = find_best (current_rec);
	    if (best < 0)
	    {
		best = next_move (current_rec, NO_MOVE);
		put_in (current_rec->key, num_discs(current_rec),
			current_rec->color, best);
	    }
	    current_rec->best =
		current_rec->already_tried = current_rec->move = best;
	    finished_nodes = 0;

	    insert_best_move2 (&legals, best, prev_move, color);
	    abmake_move (current_rec, next_rec, TRUE);

	    window_max = (window_min = AB_Info[color].last_score - 1) +
	      (AB_Info[color].bottom_searched == AB2_1 ? 4 : 2);

	    /* alpha-beta for first branch */
	    ab2 (next_rec, window_min, window_max, opp_color);

	    if (Wake_Up)
		goto wake;
	    current_rec->alpha = -next_rec->alpha;

	    /* failing high */
	    if (current_rec->alpha >= window_max)
	    {
		while (current_rec->alpha >= window_max)
		{
		    AB_Info[color].last_score = current_rec->alpha;
		    window_max = (window_min = window_max + 5) + 5;
		    current_rec->already_tried = current_rec->move = best;
		    absetup_move (next_rec, current_rec->move);
		    ab2 (next_rec, window_min, window_max, opp_color);
		    if (Wake_Up)
		    {
		      /* Needed so that deepen() doesn't get confused about alpha. */
			current_rec->alpha = AB_Info[color].last_score;
			goto wake;
		    }
		    current_rec->alpha = -next_rec->alpha;
		}
		if (current_rec->alpha <= window_min)
		{
		    window_min = (window_max = window_min) - 5;
		    absetup_move (next_rec, current_rec->move);
		    ab2 (next_rec, window_min, window_max, opp_color);
		    if (Wake_Up)
		    {
		      /* Needed so that deepen() doesn't get confused about alpha. */
			current_rec->alpha = AB_Info[color].last_score;
			goto wake;
		    }
		    current_rec->alpha = -next_rec->alpha;
		}
	    }
	    else if (current_rec->alpha <= window_min && AB_Info[color].bottom_searched == AB2_1)
	      /* Failing low - only care when last search == AB2_1 because for TYPE2 searches, we CANNOT fail low. */
	    {
		while (current_rec->alpha <= window_min)
		{
		    AB_Info[color].last_score = current_rec->alpha;
		    window_min = (window_max = window_min - 5) - 5;
		    current_rec->already_tried = current_rec->move = best;
		    absetup_move (next_rec, current_rec->move);
		    ab2 (next_rec, window_min, window_max, opp_color);
		    if (Wake_Up)
		    {
		      /* Needed so that deepen() doesn't get confused about alpha. */
			current_rec->alpha = AB_Info[color].last_score;
			goto wake;
		    }
		    current_rec->alpha = -next_rec->alpha;
		}
		if (current_rec->alpha >= window_max)
		{
		    window_max = (window_min = window_max) + 5;
		    absetup_move (next_rec, current_rec->move);
		    ab2 (next_rec, window_min, window_max, opp_color);
		    if (Wake_Up)
		    {
		      /* Needed so that deepen() doesn't get confused about alpha. */
			current_rec->alpha = AB_Info[color].last_score;
			goto wake;
		    }
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
		    insert_best_move2 (&legals, current_rec->best, prev_move, color);
		    put_in (current_rec->key, num_discs(current_rec),
			    current_rec->color, current_rec->move);
		    continue;
		  }
		
		/* If already have a better one, must search both.  May want to change this later - if we're losing, and two better moves are found or if there is plenty of time, should find out which is better. */
		move1 = current_rec->move;
		move2 = current_rec->best;
		insert_best_move2 (&legals, move1, prev_move, color);
		absetup_move (next_rec, move1);
		current_rec->move = move1;
		ab2 (next_rec, window_max, (eval_t)64, opp_color);
		if (Wake_Up)
		  goto wake;
		current_rec->alpha = window_min = -next_rec->alpha;
		
		current_rec->move = move2;
		abmake_move (current_rec, next_rec, TRUE);
		ab2 (next_rec, window_min, (eval_t)64, opp_color);
		if (Wake_Up)
		  goto wake;
		
		if (-next_rec->alpha <= window_min)
		  {
		    current_rec->best = move1;
		    put_in (current_rec->key, num_discs(current_rec),
			    current_rec->color, current_rec->best);
		  }
		else
		  {
		    insert_best_move2 (&legals, move2, prev_move, color);
		    current_rec->alpha = window_min = -next_rec->alpha;
		  }
		found_better = FALSE;
	      }
	    break;
	}

	/* Searching to end for the first time - use of zero/one windows for speed Last
	   criterion = must have time to solve endgame next move -
	   otherwise opponent will likely use little time, in which case the TYPE1 search won't succeed anyway. */
	else if (num_empty <= 17 && num_empty - i <= 9 &&
		 (i >= 6 || num_empty <= 12) && General_Info[color].time_left > Type2_Time[num_empty - 2])
	{
	    eval_alpha = current_rec->alpha;

	    current_rec->next_legal_try =
		Killer_Table[color][prev_move].order;
	    best = find_best (current_rec);
	    if (best < 0)
	    {
		best = next_move (current_rec, NO_MOVE);
		put_in (current_rec->key, num_discs(current_rec),
			current_rec->color, best);
	    }
	    current_rec->best = current_rec->already_tried = current_rec->move = best;
	    finished_nodes = 0;
	    abmake_move (current_rec, next_rec, TRUE);

	    /* Use a one-window for win/loss/tie. */
	    window_min = -1;
	    window_max = 1;
	    ab2 (next_rec, window_min, window_max, opp_color);
	    if (Wake_Up)
	    {
	    /* Must restore alpha so we don't mislead deepen () */
		current_rec->alpha = eval_alpha;
		goto wake;
	    }
	    finished_nodes++;

	    window_min = -next_rec->alpha;
	    if (current_rec->alpha >= 1)
	    {
		put_in (current_rec->key, num_discs(current_rec),
			current_rec->color, current_rec->move);
		insert_best_move2 (&legals, current_rec->best, prev_move, color);
		goto done;
	    }

	    /* We're always unsure of the value, so set found_better TRUE. */
	    found_better = TRUE;

	    /* go through each of the remaining moves */
	    if (current_rec->alpha <= 0)
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
		    {
		      current_rec->alpha = eval_alpha;
		      goto wake;
		    }
		  
		  if (-next_rec->alpha < window_max)
		    continue;

		  current_rec->alpha = -next_rec->alpha;
		  current_rec->best = current_rec->move;
		  put_in (current_rec->key, num_discs(current_rec),
			  current_rec->color, current_rec->move);
		  insert_best_move2 (&legals, current_rec->best, prev_move, color);
		  window_min = window_max;
		  if (current_rec->alpha >= 1)
		    goto done;
		}

	  done:
	    AB_Info[color].bottom_searched = AB2_1;

	    if (current_rec->alpha > 0)
	      /* We will win, but guess how much for window next time - if evaluation function <= 0, don't want to guess negative though. */
	    {
		temp = guess_min (eval_alpha);
		if (temp > 0)
		    current_rec->alpha = temp;
	    }
	    else
	    if (current_rec->alpha < 0)
	    {
		temp = guess_min (eval_alpha);
		if (temp < 0)
		    current_rec->alpha = temp;
	    }

	    AB_Info[color].last_score = current_rec->alpha;
	    goto type2;
	}

	/* not searching to the end */
	else
	{
	    finished_nodes = 0;
	    if (i >= AB_Info[color].max_levels)
		break;

	    /* set up move */
	    current_rec->next_legal_try = Killer_Table[color][prev_move].order;
	    best = find_best (current_rec);
	    if (best < 0)
	    {
		best = next_move (current_rec, NO_MOVE);
		put_in (current_rec->key, num_discs(current_rec),
			current_rec->color, best);
	    }
	    insert_best_move2 (&legals, best, prev_move, color);
	    current_rec->best =
		current_rec->already_tried = current_rec->move = best;

	    abmake_move (current_rec, next_rec, TRUE);

	    /* it is more accurate to average the last two alpha's for window */
	    window_center = i > first_level ? (current_rec->alpha + prev_alpha) / 2 : current_rec->alpha;

	    /* this may need some tuning */
	    diff = Abs(current_rec->alpha) / 4 + 10000;
	    window_max = window_center + diff;
	    window_min = window_center - diff;
	    ab (next_rec, i, window_min, window_max, opp_color, General_Info[color].eval_stuff.eval);
	    if (Wake_Up)
		goto wake;
	    current_rec->alpha = -next_rec->alpha;
	    while (current_rec->alpha >= window_max)
	    {
		diff *= 2;
		window_max = (window_min = window_max) + diff;
		absetup_move (next_rec, current_rec->move);
		ab (next_rec, i, window_min, window_max, opp_color, General_Info[color].eval_stuff.eval);
		if (Wake_Up)
		    goto wake;
		current_rec->alpha = -next_rec->alpha;
	    }
	    while (current_rec->alpha <= window_min)
	    {
		diff *= 2;
		window_min = (window_max = window_min) - diff;
		absetup_move (next_rec, current_rec->move);
		ab (next_rec, i, window_min, window_max, opp_color, General_Info[color].eval_stuff.eval);
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
		ab (next_rec, i, window_min, window_max, opp_color, General_Info[color].eval_stuff.eval);
		
		if (Wake_Up)
		  goto wake;
		
		if (-next_rec->alpha < window_max)
		  continue;
		
		/* If it's the first one, we're OK. */
		if (!found_better)
		  {
		    found_better = TRUE;
		    current_rec->best = current_rec->move;
		    put_in (current_rec->key, num_discs(current_rec),
			    current_rec->color, current_rec->move);
		    insert_best_move2 (&legals, current_rec->best, prev_move, color);
		    continue;
		  }
		
		/* If already have a better one, must search both. */
		move1 = current_rec->move;
		move2 = current_rec->best;
		insert_best_move2 (&legals, move1, prev_move, color);
		absetup_move (next_rec, move1);
		current_rec->move = move1;
		ab (next_rec, i, window_max, INFINITY, opp_color, General_Info[color].eval_stuff.eval);
		if (Wake_Up)
		  goto wake;
		current_rec->alpha = window_min = -next_rec->alpha;
		
		current_rec->move = move2;
		abmake_move (current_rec, next_rec, TRUE);
		ab (next_rec, i, window_max, INFINITY, opp_color, General_Info[color].eval_stuff.eval);
		if (Wake_Up)
		  goto wake;
		
		if (-next_rec->alpha <= window_min)
		  {
		    current_rec->best = move1;
		    put_in (current_rec->key, num_discs(current_rec),
			    current_rec->color, current_rec->best);
		    insert_best_move2 (&legals, current_rec->best, prev_move, color);
		  }
		else
		  {
		    insert_best_move2 (&legals, move2, prev_move, color);
		    current_rec->alpha = window_min = -next_rec->alpha;
		  }
		found_better = FALSE;
	      }
	}

	prev_alpha = !found_better ? current_rec->alpha : current_rec->alpha > 0 ? current_rec->alpha * 1.5 : current_rec->alpha * 0.67;

	if (Print_Steal)
	    print_all (print_type (AB_Info[color].bottom_searched),
		       AB_Info[color].bottom_searched ? print_finished_depth (finished_depth[color]) :
				   			print_depth (i + 1),
		       current_rec->best, NO_MOVE,
		       print_discs (-current_rec->alpha), 0, 0, 0, color);
    }

 wake:
  if (AB_Info[color].bottom_searched != AB) /* done some endagme search */
    {
      finished_depth[color] = num_empty - 1 + (double)finished_nodes / legals.max;
      AB_Info[color].exact = Wake_Up || finished_nodes < legals.max ? INCOMPLETE : found_better ? INEXACT : EXACT;
      AB_Info[color].depth = num_empty;	/* finished all the way to end */
    }
  else
    {
      finished_depth[color] = i >= AB_Info[color].max_levels ?
	AB_Info[color].max_levels : i + (double)finished_nodes / legals.max;
      AB_Info[color].exact = finished_nodes > 0 && finished_nodes < legals.max ?
	INCOMPLETE : found_better ? INEXACT : EXACT;
      AB_Info[color].depth = finished_depth[color] > i ? i + 1 : i;
    }

  /* Save_Leaf always contains the right number of leaf nodes: on
     wake, Leaf_Nodes is incomplete; on exceeding max_level, Leaf_Nodes was
     just zeroed. */
  Leaf_Nodes = Save_Leaf;
  AB_Info[color].last_score = current_rec->alpha;

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
    Stat_Info[color][Turn_Num].leaf_searched = Leaf_Nodes;
    Stat_Info[color][Turn_Num].depth_searched = finished_depth[color];

    /* just in case interrupt caused a fake bottom_search */
    if (!AB_Info[color].bottom_searched || abs((int) current_rec->alpha) <= MAX_PIECES)
      AB_Info[color].last_score = current_rec->alpha;

    Stat_Info[color][Turn_Num].time_used = elapsed_time (FALSE) - start_time;

  if (Print_Steal)
    print_all ("Steal", print_finished_depth (finished_depth[color]),
	       current_rec->best, NO_MOVE, print_discs (-current_rec->alpha), 0, 0, 0, opp_color);
  Is_stealing = FALSE;
  if (!Wake_Up)
    alarm ((unsigned)0);
}


/* insert_best_move2 is the same as insert_best_move (see zero.c), except that this version updates the killer table as well.  This way, if we have to search the same state in zero.c, the moves will be in the same order. */

void insert_best_move2 (legals, move, prev_move, color)
     register legal_t legals;
     move_t	     move, prev_move;
     color_t	     color;
{
  int             index;
  struct double_link *first, *this;
  register int	  i;

  for (i = 0; i < legals->max; i++)
    if (legals->data[i] == move)
      break;

  if (i >= legals->max)
    return;

 /* Shift all moves before this move down 1 and insert move in 1st pos. */
  if (i > 0)
    {
      index = i;
      for (i = index; i > 0; i--)
	legals->data[i] = legals->data[i - 1];
      legals->data[0] = move;
    }

  /* update the killer entry */
  first = Killer_Table[color][prev_move].order;
  this = Killer_Table[color][prev_move].resp_addr[move];
  if (first != this)
    {
      if (this->next != NULL)
	this->next->prev = this->prev;
      if (this->prev != NULL)
	this->prev->next = this->next;
      this->next = first;
      this->prev = NULL;
      first->prev = this;
      Killer_Table[color][prev_move].order = this;
    }
}
