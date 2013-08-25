/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdlib.h>
#include <stdio.h>

#include <node.h>
#include <exact.h>
#include <check.h>
#include <math.h>
#include <eval.h>
#include <ss_info.h>
#include <stat.h>

#undef MAX_DEPTH
#define MAX_DEPTH 64			/* maximum depth for selective search */
#define Print_Scoreboard TRUE
#define Print_Other TRUE

void internal_error ();

extern int expand_best ();
extern int expand_given ();
extern void recombine ();
extern char    *Dump_File;
extern int      Flag_Y, Leaf_Nodes, Nodes_Searched;
extern double Time_Fraction[2][MAX_MOVES/2];
extern char *print_interval ();

extern ss_info_t SS_Info[2];
extern general_info_t General_Info[2];
extern stat_info_t Stat_Info[2][MAX_MOVES];
extern int Wake_Up;
extern int Turn_Num;
extern int      Flag_X;
extern Node Game_Record[MAX_MOVES];
extern int Corrector;

static void print_search ();
static void free_children ();
static void print_scoreboard ();
static void print_other ();
SNode *best_child ();
static SNode *best_child_by_median ();
static int check_stop_early ();

color_t		My_Stat_Color;
eval_t          (*My_Eval) ();
int             My_Parm;
int             Dist[MAX_DEPTH];
SNode          *Free_List = NULL;
static SNode   *Cached_Root[2] = { NULL, NULL };
int		My_Endgame_Depth;
int		My_Hybrid_Depth;
int             My_Offset;


move_t          SS (game_record, stealing)
    Node           *game_record;
     int             stealing;
{
  register SNode *root;
  color_t real_color;
  /* stat_color is who we're running the search for */
  color_t stat_color;
  int             expansions = 0,
    		    old_nodes,
    	   	    start_nodes,
    		    time, start_time,
		    allocated_time;
  int search_done;
  int interval;

  start_time = time = 0;
  real_color = game_record->color;
  stat_color = stealing ? Opposite(real_color) : real_color;
  Leaf_Nodes = 0;
  if (Cached_Root[stat_color] != NULL)
    {
      root = Cached_Root[stat_color];	/* use the saved root */
      start_nodes = old_nodes = root->nodes;
    }
  else
    /* make a new root */
    {
      if ((root = (SNode *) malloc (sizeof (SNode))) == NULL)
	internal_error ("can't allocate new root in SS");
      start_nodes = old_nodes = 1;
      Cached_Root[stat_color] = root;
      memcpy(root->board, game_record->board, sizeof root->board);
      root->color = game_record->color;
      root->bl_pieces = game_record->bl_pieces;
      root->wh_pieces = game_record->wh_pieces;
      root->to_here = game_record <= Game_Record ? NO_MOVE : game_record[-1].move;
      memcpy(root->features, game_record->features, sizeof root->features);
      root->upper = MAX_SCORE;		/* dummy bounds */
      root->lower = MIN_SCORE;
      root->nodes = 1;
#if PARENT
      root->parent = NULL;
#endif
      root->child = NULL;
      root->next = NULL;
    }

  Wake_Up = FALSE;
  Nodes_Searched = 0;
  search_done = root->child != NULL && DONE (root);
  interval = SS_Info[stat_color].interval;
  My_Stat_Color = stat_color;
  My_Parm = SS_Info[stat_color].parm;
  My_Endgame_Depth = SS_Info[stat_color].endgame_depth;
  My_Hybrid_Depth = SS_Info[stat_color].hybrid_depth;
  My_Offset = SS_Info[stat_color].offset;
  My_Eval = Flag_Y ? General_Info[stat_color].eval_stuff.out : General_Info[stat_color].eval_stuff.eval;
  (*My_Eval) (root, root);		/* throw in level-above info */

  /* figure out timing */
  if (stealing)
    allocated_time = -1;
  else
    {
      float time_left;

      time_left = General_Info[stat_color].time_left;
      if (time_left <= 0)
	allocated_time = 0;
      else
	{
	  allocated_time = time_left * Time_Fraction[stat_color][Turn_Num / 2]; /* don't subtract stealing time from normal allocation */
	  if (Turn_Num >= 48 && Turn_Num < 52)
	    allocated_time *= 2;		/* compensate for screwed-up Time_Fraction tables */
	  allocated_time *= 1.5;		/* this is really max. time */
	  if (allocated_time < 1000)
	    allocated_time = 1000;
	  alarm ((unsigned) (allocated_time / 1000)); /* set alarm */
	}
    }

  /* clear some statistics */
  {
    int i;

    for (i = 0; i < MAX_DEPTH; i++)
      Dist[i] = 0;
  }

  /* make sure the root is expanded */
  if (root->child == NULL)
  {
      expansions++;
      if (expand_given(root, 0, NULL) < 0)
	  internal_error("can't expand root");
  }

  /* expand all children, make the tree two plies */
  {
    SNode *child;

    child = root->child;
    while (child != NULL)
      {
	if (child->child == NULL && !DONE(child))
	  {
	    expansions++;
	    root->nodes += expand_given(child, 1, root);
	  }
	  child = child->next;
      }
  }
  recombine(root);
  if (allocated_time == 0 || Wake_Up || DONE(root))
    goto print_final;

  /* print starting search information */
  time = elapsed_time(FALSE);
  print_search (root, 'S', stealing, time, allocated_time);
  old_nodes = root->nodes;

  /* expand the grandchildren, make the tree three plies */
  {
      SNode *child;
      SNode *grandchild;
      int new_nodes;

      child = root->child;
      while (child != NULL)
      {
	  if (DONE(child))
	      goto next_child;
	  grandchild = child->child;
	  while (grandchild != NULL)
	  {
	      if (grandchild->child == NULL && !DONE(grandchild))
	      {
		  expansions++;
		  new_nodes = expand_given(grandchild, 2, child);
		  root->nodes += new_nodes;
		  child->nodes += new_nodes;
	      }
	      grandchild = grandchild->next;
	  }
	  recombine(child);
	next_child:
	  child = child->next;
      }
  }
  recombine(root);
  if (allocated_time == 0 || Wake_Up || DONE(root))
    goto print_final;

  /* outer search loop */
  for (;;)
    {
      int old_time = time;
      register int enough_nodes;

      enough_nodes = old_nodes + interval;

      /* inner search loop */
      for (;;)
	{
	  if (root->nodes >= enough_nodes)
	     break;

	  expansions++;
	  /* pick a leaf node and expand it */
	  if (expand_best (root) >= 0)
	    {
	      search_done = Wake_Up || DONE(root);
	      if (search_done)
		break;
	    }
	  else
	    {
	      fprintf (stderr, "Out of memory!\n");
	      search_done = TRUE;
	      break;
	    }
	}

      /* recompute nodes per second */
      time = elapsed_time(FALSE);
      if (time > 1000000000)
	  abort();
      if (time > old_time + 40) /* 40 ms */
	SS_Info[stat_color].nps = (root->nodes - old_nodes) * 1000 / (time - old_time);

      /* check whether we should stop now */
      if (search_done || !stealing && (time >= allocated_time ||check_stop_early (root, time, allocated_time)))
	  break;

      /* print search info */
      print_search (root, 'R', stealing, time, allocated_time);

      old_nodes = root->nodes;
    }

 print_final:
  /* print final search info */
  print_search (root, DONE(root) ? 'X' : Wake_Up ? 'I' : time >= allocated_time ? 'T' : 'E', stealing, time, allocated_time);

  if (Print_Scoreboard)
    print_scoreboard (root);
  if (Print_Other)
    print_other (root, expansions);
  if (Dump_File != NULL)
    dump_tree (root);

  {
    /* update stat-info */
    Stat_Info[stat_color][Turn_Num].who = SELECT;
    Stat_Info[stat_color][Turn_Num].time_used = elapsed_time(FALSE) - start_time;
    Stat_Info[stat_color][Turn_Num].nodes_searched = root->nodes - start_nodes;
    Stat_Info[stat_color][Turn_Num].leaf_searched = Leaf_Nodes;
    Stat_Info[stat_color][Turn_Num].exact = EXACT;
    Stat_Info[stat_color][Turn_Num].value = root->lower;
    Stat_Info[stat_color][Turn_Num].other_value = root->upper;

    {
      register SNode *node;
      register int depth;

      for (node = root, depth = 0; (node = best_child (node)) != NULL; depth++) ; /* compute depth of main line */
      Stat_Info[stat_color][Turn_Num].depth_searched = depth;
    }
  }

  alarm ((unsigned) 0);		/* clear alarm */

  /* return best move */
  return root->child == NULL ? NO_MOVE : best_child (root)->to_here;
}


static void print_search (root, code, stealing, time, allocated_time)
    SNode *root;
    char code;
    int stealing;
    int time;
    int allocated_time;
{
    color_t real_color;
    color_t stat_color;
    char nodes[32];
    move_t move;
    move_t reply;

    real_color = root->color;
    stat_color = stealing ? Opposite(real_color) : real_color;
    sprintf (nodes, "%c%7d %5d", code, root->nodes,
	     (int) SS_Info[stat_color].nps);
    if (root->child == NULL)		/* only one node in tree? */
      reply = move = REALLY_NO_MOVE;
    else
    {
      register SNode *best = best_child (root);

      move = best->to_here;
      reply = best->child == NULL ? REALLY_NO_MOVE : best_child (best)->to_here;
    }
    print_all ("Select", nodes, move, reply,
	       print_interval (-root->lower, -root->upper),
	       time, stealing ? 0 : allocated_time, 0, real_color);
}


SNode *best_child (parent)
     register SNode *parent;
{
  register SNode *child;
  register SNode *best_child;
  register eval_t best_lower;
  register eval_t best_upper;

  child = parent->child;
  if (child == NULL)
    return NULL;
  best_child = child;
  best_lower = child->lower;
  best_upper = child->upper;
  for (child = child->next; child != NULL; child = child->next)
  {
      register eval_t lower;
      register eval_t upper;
      register eval_t diff;

      lower = child->lower;
      diff = WIN_VALUE(lower) - WIN_VALUE(best_lower);
      if (diff < 0)
	  continue;
      if (diff > 0)
	  goto better;
      upper = child->upper;
      diff = WIN_VALUE(upper) - WIN_VALUE(best_upper);
      if (diff < 0)
	  continue;
      if (diff > 0)
	  goto better;
      diff = lower - best_lower;
      if (diff < 0)
	  continue;
      if (diff > 0)
	  goto better;
      diff = upper - best_upper;
      if (diff <= 0)
	  continue;
    better:
      best_child = child;
      best_lower = lower;
      best_upper = upper;
  }

  return best_child;
}

#if 0
static SNode *best_child_by_median (parent)
     register SNode *parent;
{
  register SNode *best, *child;
  register eval_t best_sum, child_sum;

  if ((best = parent->child) == NULL)
    return NULL;

  best_sum = best->upper + best->lower;
  for (child = best->next; child != NULL; child = child->next)
    {
      if ((child_sum = child->upper + child->lower) > best_sum)
	{
	  best = child;
	  best_sum = child_sum;
	}
    }

  return best;
}
#endif

/* 
 * This function decides if we should stop early by looking at
 * how much the best move contributed to the root.
 */

static int check_stop_early (root, current_time, allocated_time)
    SNode *root;
    int current_time;
    int allocated_time;
{
    SNode *child;
    eval_t best_lower;
    eval_t lower;
    eval_t bonus;

    if (!INEXACT_UPPER (root->upper))
	return FALSE;
    child = root->child;
    best_lower = child->lower;
    for (child = child->next; child != NULL; child = child->next)
    {
	lower = child->lower;
	if (lower > best_lower)
	    best_lower = lower;
    }
    if (!INEXACT_LOWER (best_lower))
	return FALSE;
    bonus = -(best_lower + root->upper);
    CHECK (bonus >= 0, "bonus is negative");
    /* bonus = 0 -> 1/4 max. time , bonus = log(2) -> max. time */
    return current_time > ((double) bonus / My_Parm * M_LOG2E * 0.75 + 0.25) * allocated_time;
}

static void print_scoreboard (root)
     register SNode *root;
{
  register SNode *child;

  puts ("+---------scoreboard---------+");

  for (child = root->child; child != NULL; child = child->next)
    printf ("|%s (%6d) = %s |\n", uncvt (child->to_here), child->nodes,
	    print_interval (child->upper, child->lower));
  puts ("+----------------------------+");
}

static void print_other (root, expansions)
     register SNode *root;
     int expansions;
{
  register SNode *node;
  register int    depth, depth_max;

  fputs ("main line:", stdout);
  for (node = root, depth = 0; (node = best_child (node)) != NULL; depth++)
    printf (" %s", uncvt (node->to_here));

  fputs ("\ndistribution:", stdout);
  depth_max = MAX_DEPTH;
  while (--depth_max >= 0 && Dist[depth_max] <= 0)
    ;
  {
    int i;

    for (i = 0; i <= depth_max; i++)
      printf (" %d", Dist[i]);
  }
  puts ("");

#if 0
  for (node = root, depth_by_median = 0; (node = best_child_by_median (node)) != NULL; depth_by_median++)
    ;
  printf ("depth = %d, depth_by_median = %d, depth_max = %d\n", depth, depth_by_median, depth_max + 1);
#endif
}


/* called by make_move to update the cached trees */

void SS_make_move (move)
     register move_t move;
{
  color_t color;			/* which color's tree we're cleaning */

  if (Flag_X)
    move = REALLY_NO_MOVE;		/* don't match anything; i.e., flush the whole tree */

  for (color = BLACK; color <= WHITE; color++)
    {
      register SNode *root, *child, *new_root;
      
      if ((root = Cached_Root[color]) == NULL) /* no nodes in tree? */
        continue;			/* that was easy */

      if ((child = root->child) == NULL) /* one node in tree? */
	{
	  Cached_Root[color] = NULL;	/* nothing left */
	  root->next = Free_List;
	  Free_List = root;		/* put this node on free list */
	  continue;
	}

      new_root = NULL;
      if (child->to_here == move)	/* first child is move made? */
	{
	  new_root = child;		/* this was the move we made */
	  root->child = child->next;	/* unlink move made, so it's not freed */
	}
      else
	for (; child->next != NULL; child = child->next)
	  if (child->next->to_here == move)
	    {
	      new_root = child->next;	/* this is it */
	      child->next = new_root->next; /* unlink it */
	      break;
	    }

      if (new_root != NULL)
	{
	  printf ("[%s tree has %d nodes.]\n", color == BLACK ? "Black" : "White", new_root->nodes);
	  new_root->next = NULL;
	}
      if (root->child != NULL)
	free_children (root);		/* free up everything not under move made */
      Cached_Root[color] = root->child = new_root; /* relink move made */
    }
}


/* free_children frees all nodes strictly below parent.
   Requires parent != NULL and parent->child != NULL. */

static void free_children (parent)
     register SNode *parent;
{
  static int      depth = 0;
  register SNode *child;

  depth++;
  CHECK (depth < MAX_DEPTH, "cycle in free_children");

  for (child = parent->child;; child = child->next)
    {
      if (child->child != NULL)
	free_children (child);
      if (child->next == NULL)		/* don't run off the end */
	break;
    }

  child->next = Free_List;		/* child is now last of parent's children */
  Free_List = parent->child;		/* put all parent's children on free list at once */
  depth--;
}


/* called by take_back to throw away the cached trees */

void SS_clear_cache ()
{
  color_t color;

  for (color = BLACK; color <= WHITE; color++)
    {
      SNode *root;

      if ((root = Cached_Root[color]) != NULL)
	{
	  if (root->child != NULL)
	    free_children (root);
	  root->next = Free_List;
	  Free_List = root;
	  Cached_Root[color] = NULL;
	}
    }
}


/* for each top-level node (depth = 1), it saves the number of nodes underneath.
   This is so that the fix_steal procedure can adjust how much was actually done
   based upon the effort expended underneath the move the user actually picked */

static num_nodes[32];		/* not more than 32 successors, hopefully */

void save_steal ()
{
  int i;
  color_t color;

  for (i = 0; i < 32; i++)
    num_nodes[i] = 0;
  for (color = BLACK; color <= WHITE; color++)
    {
      register SNode *root = Cached_Root[color], *node;

      if (root == NULL)		/* no saved tree */
	continue;
      for (i = 0, node = root->child; i < 32 && node; node = node->next, i++)
	num_nodes[i] = node->nodes;
      if (i >= 32)
	internal_error ("too many children in save_steal");
    }
}

/* We don't care about the guess, but ab_fix_steal does.  Oh well. */

void SS_fix_steal (move, guess)
     move_t move, guess;
{
  register SNode *node;
  static stat_info_t zero;
  register int i;
#if 0
  int total_nodes = 0;
#endif

#define color BLACK		/* this is wrong, of course */
#ifdef lint
  /* make lint think we use guess */
  exit (guess);
#endif
  if (Cached_Root[color] == NULL)
    {
      Stat_Info[~Turn_Num&1][Turn_Num] = zero;
      return;
    }
#if 0
  for (i = 0; i < 32; i++)
    total_nodes += num_nodes[i];
#endif
  /* look for move */
  for (node = Cached_Root[color]->child; node; node = node->next, i++)
    if (node->to_here == move)		/* found it? */
      {
	stat_info_t *ptr = &Stat_Info[Opposite(Turn_Num&1)][Turn_Num];

	/* could be 0 because we're done with search (converged) */
	if (ptr->nodes_searched != 0)
	  {
	    double factor;

	    factor = (double) node->nodes-num_nodes[i] / ptr->nodes_searched;
	    ptr->nodes_searched = ptr->nodes_searched * factor;
	    ptr->leaf_searched = ptr->leaf_searched * factor;
	    ptr->time_used = ptr->time_used * factor;
	  }
	if (best_child (Cached_Root[color]) == node) /* on main line? */
	  ptr->depth_searched--;
	else
	  ptr->depth_searched = 0;
	break;
      }
}
