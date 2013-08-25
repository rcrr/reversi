/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdio.h>
#include <math.h>
#include <sys/types.h>

#include <node.h>
#include <search.h>
#include <prune.h>
#include <exact.h>
#include <check.h>
#include <stat.h>

#undef MAX_DEPTH
#define MAX_DEPTH 64
#define HYBRID 1
#define MAX_CHANGE 0x1000000

#define PERVERSE (My_Parm == 99000)

extern int total_leaf_nodes;
extern int Wake_Up;
extern color_t My_Stat_Color;
extern int My_Parm;
#if HYBRID
extern int My_Hybrid_Depth;	/* alpha-beta depth for non-endgame, used only for book generation */
#endif
extern int My_Endgame_Depth;	/* alpha-beta(2) depth for endgame */
extern general_info_t General_Info[2];
extern int Dist[];
extern int My_Offset;

void recombine ();


int expand_best (root)
     register SNode *root;
{
  SNode *parent;
  SNode *stack[MAX_DEPTH];
  int depth;
  int new_nodes;

  /* find the node to expand */
  parent = root;
  depth = 0;
  for (;;)
    {
      register SNode *child;
      register SNode *best_child;
      register eval_t best_upper;
      
      child = parent->child;
      if (child == NULL)
	break;
#if PARENT
      CHECK(child->parent == parent, "parent is wrong");
#endif
      stack[depth] = parent;
      depth++;
      child = parent->child;
      best_child = child;
      best_upper = child->upper;

      for (child = child->next; child != NULL; child = child->next)
	{
	  register eval_t upper;

	  upper = child->upper;
	  if (upper > best_upper)
	    {
	      best_child = child;
	      best_upper = upper;
	    }
        }

      CHECK(best_child != NULL, "no child to expand");
      CHECK(!DONE (best_child), "expanding done node in expand");
      CHECK(depth < MAX_DEPTH, "depth too large in expand");
      parent = best_child;
    }

  new_nodes = expand_given (parent, depth, stack[depth - 1]);

 update_others:
  /* update nodes above parent */
  while (--depth >= 0)
    {
      parent = stack[depth];
      parent->nodes += new_nodes;
      recombine (parent);
    }

  return new_nodes;
}


int expand_given (parent, depth, grandparent)
    SNode *parent;
    int depth;
    SNode *grandparent;
{
  int new_nodes = 0;			/* how many nodes we've added to the tree */
  int *dist;
  extern int Nodes_Searched;

  /* expand children */
  {
    color_t	    color = parent->color, /* color to move in parent */
		    opp_color = Opposite(color); /* color to move in children */
    int		    discs = parent->bl_pieces + parent->wh_pieces;
    extern int	    Leaf_Nodes, Ab2_Leaf_Nodes;
#ifdef PRUNE_XC
    int		    use_scum = FALSE;	/* using moves we previously rejected? */
#endif

    legal_list legals;
    register int move_num;			/* index into legals.data */
    register SNode *child;		/* this BETTER be in a register */
    extern SNode *Free_List;

    find_legals ((Node *)parent, parent->to_here, &legals);

    if (legals.max <= 0)		/* no legal moves? */
      {
	legals.max = 1;
	legals.data[0] = NO_MOVE;
	if (!LEGAL(parent->to_here))	/* double pass = end of game */
	  {
	    int diff;
	    eval_t value;

	    diff = opp_color == BLACK ? parent->bl_pieces - parent->wh_pieces : parent->wh_pieces - parent->bl_pieces;
	    value = SHIFT_DIFF(diff);
	    if (PERVERSE)
		value = -value;
	    parent->upper = value - CONS_VALUE;
	    parent->lower = value + CONS_VALUE;
	    return 1;
	  }
      }
    else
      {
	discs++;
      }

    if (depth != 0 && discs >= MAX_PIECES - My_Endgame_Depth)
	/* use ab2 for an exact evaluation (piece differential), don't use on root */
      {
	static ANode ab2stack[33];
	register ANode *next_rec = ab2stack + 1;
	int diff;
	eval_t value;

	/* put relevant info in ab2stack */
	bcopy ((char *)grandparent, (char *)(next_rec - 1), sizeof (SNode));
	bcopy ((char *)parent, (char *)next_rec, sizeof (SNode));
	absetup_move (next_rec, parent->to_here);

	diff = ab2 (next_rec, -MAX_PIECES, MAX_PIECES, color);
	if (Wake_Up)
	  return 0;
	value = SHIFT_DIFF(diff);
	if (PERVERSE)
	  value = -value;
	parent->upper = value - CONS_VALUE;
	parent->lower = value + CONS_VALUE;
	new_nodes += Ab2_Leaf_Nodes;
	parent->nodes = Ab2_Leaf_Nodes + 1;
	return new_nodes;
      }

    for (move_num = 0; move_num < legals.max; move_num++)
      {
	register move_t move = legals.data[move_num];

#ifdef PRUNE_XC
	if (move >= 0 && !use_scum)
	  {
	    register int xnum, cnum;
	    extern int X_Square[MAX_PIECES], C_Square[MAX_PIECES];
	    extern int Corner[5], C1[5], C2[5], C_Corner[9], C_B[9], C_A[9];

	    /* horrible pruning heuristic */
	    if ((xnum = X_Square[move]) > 0 &&
		Empty(parent->board[Corner[xnum]]) &&
		(discs <= TOP_ALWAYS_PRUNE_X ||
		 discs <= TOP_CONDITIONAL_PRUNE_X &&
		 Occupied (parent->board[C1[xnum]]) &&
		 Occupied (parent->board[C2[xnum]])) ||
		(discs <= TOP_PRUNE_C &&
		 (cnum = C_Square[move]) > 0 &&
		 Empty(parent->board[C_Corner[cnum]]) &&
		 Empty(parent->board[C_B[cnum]]) &&
		 parent->board[C_A[cnum]] != opp_color))
	      {
		if (new_nodes > 0 || move_num < legals.max - 1)
		  continue;		/* skip this */
		use_scum = TRUE;
		move = legals.data[move_num = 0]; /* go back to first move */
	      }
	  }
#endif

	if (new_nodes <= 0)
	  /* allocate first child */
	  {
	    if (Free_List != NULL)
	      /* from free list */
	      parent->child = child = Free_List;
	    else
	      /* from malloc */
	      {
		register char *temp = (char *) malloc (sizeof (Node));

		if (temp == NULL)
		  return -1;
		parent->child = child = (SNode *)temp;
		child->next = NULL;
	      }
	  }
	else
	  /* allocate non-first child */
	  {
	    if (child->next != NULL)
	      /* from free list */
	      child = child->next;
	    else
	      /* from malloc */
	      {
		register char *temp = (char *) malloc (sizeof (Node));

		if (temp == NULL)
		  {
		    Free_List = parent->child; /* back out new nodes */
		    parent->child = NULL;
		    return -1;
		  }
		child = child->next = (SNode *)temp;
		child->next = NULL;
	      }
	  }

	child->color = opp_color;
	child->to_here = move;
#if PARENT
	child->parent = parent;
#endif
	child->child = NULL;
	copy_board (parent->board, child->board);

	if (!LEGAL(move))			/* no move? */
	  {
	    child->bl_pieces = parent->bl_pieces;
	    child->wh_pieces = parent->wh_pieces;
	  }
	else
	  {
	    extern int      Next_Arr[], Next_St[];
	    register int   *j, *end;

	    simple_do_flips ((Node *) parent, (Node *) child, child->to_here);
	    end = &Next_Arr[Next_St[move + 1]]; /* where to stop at */
	    for (j = &Next_Arr[Next_St[move]]; j < end; j++)
	      if (child->board[*j] == EMPTY)
		child->board[*j] = ESSQ;
	  }

	if (child->bl_pieces == 0 || child->wh_pieces == 0)
	  {
	    int diff;
	    eval_t value;

	    diff = color == BLACK ? child->bl_pieces - child->wh_pieces : child->wh_pieces - child->bl_pieces;
	    value = SHIFT_DIFF(diff);
	    if (PERVERSE)
		value = -value;
	    child->upper = value - CONS_VALUE;
	    child->lower = value + CONS_VALUE;
	    child->nodes = 1;
	    new_nodes++;
	    continue;
	  }

	{
	  register eval_t value;

#if HYBRID
	  if (My_Hybrid_Depth <= 0)
	    /* normal evaluation */
	    {
#endif
	      extern eval_t (*My_Eval) ();

	      /* positive = good for color to move in child */
	      value = -My_Offset - (*My_Eval) (child, parent);
	      total_leaf_nodes++;
	      child->nodes = 1;
	      new_nodes++;
#if HYBRID
	    }
	  else
	    /* use ab search for evaluation */
	    {
	      ANode abstack[16];
	      register ANode *next_rec = &abstack[1];
	      extern int Turn_Num;

	      bcopy ((char *)parent, (char *)(next_rec - 1), sizeof (Node));
	      bcopy ((char *)child, (char *)next_rec, sizeof (Node));
	      absetup_move (next_rec, child->to_here);

	      {
		int leaf_nodes = Leaf_Nodes;

#define INF ((eval_t)(1 << 30))
		ab (next_rec, My_Hybrid_Depth, -INF, INF,
		    opp_color, General_Info[COLOR(Turn_Num)].eval_stuff.eval);
		value = -My_Offset - next_rec->alpha;
		new_nodes += child->nodes = Leaf_Nodes - leaf_nodes;
	      }
	    }
#endif
	  if (PERVERSE)
	      value = -value;
	  child->upper = SHIFT_UPPER(value);
	  child->lower = SHIFT_LOWER(value);
	}
      }

    CHECK(child != NULL, "no child in expand");
    Free_List = child->next;
    child->next = NULL;
  }

  /* update statistics */
  Nodes_Searched += new_nodes;
  Dist[depth] += new_nodes;

  /* update parent */
  parent->nodes = new_nodes + 1;
  {
    eval_t old_upper;
    eval_t old_lower;
    eval_t new_upper;
    eval_t new_lower;

    old_upper = parent->upper;
    old_lower = parent->lower;
    recombine (parent);
    new_upper = parent->upper;
    new_lower = parent->lower;
    if (INEXACT_UPPER(new_upper) && INEXACT_LOWER(new_lower) &&
	INEXACT_UPPER(old_upper) && INEXACT_LOWER(old_lower))
      {
	struct expansion_stats* stats;
	eval_t upper_change;
	eval_t lower_change;
	eval_t total_change;

	stats = &Expansion_Stats[My_Stat_Color][parent->wh_pieces +
				 parent->bl_pieces][parent->color];
	stats->count++;
	upper_change = new_upper - old_upper;
	CHECK (upper_change > -MAX_CHANGE && upper_change < MAX_CHANGE,
	       "upper change is too big");
	stats->upper_sum += upper_change;
	stats->upper_sum_square += (double)upper_change * upper_change;
	lower_change = new_lower - old_lower;
	CHECK (lower_change > -MAX_CHANGE && lower_change < MAX_CHANGE,
	       "lower change is too big");
	stats->lower_sum += lower_change;
	stats->lower_sum_square += (double) lower_change * lower_change;
	total_change = upper_change + lower_change;
	stats->total_sum_abs += total_change < 0 ? -total_change : total_change;
	stats->total_sum_square += (double) total_change * total_change;
      }
  }
  return new_nodes;
}

void recombine (node)
     register SNode *node;
{
  register SNode *child;
  register eval_t upper, lower;

  child = node->child;
  CHECK(child != NULL, "no children to recombine");

  upper = child->upper;
  lower = child->lower;
  for (child = child->next; child != NULL; child = child->next)
    {
      if (child->upper > upper)
	upper = child->upper;
      if (child->lower > lower)
	lower = child->lower;
    }

  /* add redundancy bonus */
  if (!INEXACT_LOWER(lower))
    {
      eval_t base;
      int best_diff;
      eval_t aux;

      best_diff = UNSHIFT_DIFF(lower);
      aux = 0;
      for (child = node->child; child != NULL; child = child->next)
	{
	  eval_t child_lower;
	  int piece_diff;

	  child_lower = child->lower;
	  piece_diff = best_diff - UNSHIFT_DIFF(child_lower);
	  CHECK(piece_diff >= 0, "diff is negative");
	  if (piece_diff < MAX_PIECE_DIFF)
	    aux += UNSHIFT_AUX(child_lower) >> piece_diff * PIECE_SHIFT;
	}
      CHECK(aux >= CONS_VALUE, "aux is too small");
      CHECK(aux < AUX_CUTOFF, "aux is too big");
      lower = EXACT_BASE(lower) + aux;
    }
  else
    {
      double parm = My_Parm;
      register double iparm = 1.0 / parm;
      register double weight = 0.0;

      for (child = node->child; child != NULL; child = child->next)
        {
	  eval_t diff;

	  diff = child->lower - lower;
	  if (diff < PROB_CUTOFF)
	    weight += exp (diff * iparm);
        }
      lower += log (weight) * parm;
    }

  CHECK(upper >= lower - AUX_CUTOFF, "crossed bounds in recombine");
  node->lower = -upper;
  node->upper = -lower;
}
