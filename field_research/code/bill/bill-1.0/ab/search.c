/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* alpha-beta search - this file contains two procedures - ab - uses normal
 * fixed-depth alpha beta search.  Part of iterative deepening. 
 *
 * ab2 - uses search to the end. both MUST BE controlled by a top-level
 * zero-window search. */

#include <stdio.h>

#include <node.h>
#include <general.h>
#include <search.h>
#include <tourn.h>
#include <eval.h>
#include <debug.h>
#include <check.h>

/* #define TREE_PRINT  */

#define num_discs(node) ((node)->bl_pieces+(node)->wh_pieces)
  /* ABCOLOR 'returns' the color to move at depth */
#define ABCOLOR(depth,color) ((depth) & 1 ? Opposite(color) : (color))

static int sign (int n);
void update_killer ();
void ab_put_in ();

extern Node	Game_Record[MAX_MOVES];
extern int      Leaf_Nodes,
                Wake_Up;
extern int Is_stealing;
extern struct killer_table Killer_Table[2][MAX_PIECES+1];

int   		Ab2_Leaf_Nodes;
int input_ready ();
int total_leaf_nodes = 0;

#define NODE_CHECK_INTERVAL 1000

/* ab returns the "best" move from the given board and computes the branching
 * factor for use by deepend. start contains the starting board, legal moves,
 * alpha value value Levels is how many levels to search. Color is whose move
 * it is.
 * Whoever calls ab must makes sure that an evaluation function is provided.
   Someone must also call put_in_features () if this is a 1 level search */

eval_t ab (abstack, levels, window_min, window_max, color, eval)
     ANode           abstack[]; /* where to put the 'tree' */
     int             levels;	/* levels to search */
     eval_t          window_min,/* for doing zero windowing */
       		     window_max; /* similarly */
     color_t         color;     /* who makes first move */
     eval_fn_t       eval;	/* the evaluation function to call */
{
  register ANode *TOP, *NEXT;
  register int	  depth = 0;		/* current search depth */

  int             prev;
  extern Nodes_Searched;
  int            wipeOut = FALSE;

#ifdef TREE_PRINT
  printf ("IN ab (%s %s %d %d %d)\n", Color_Name[color], uncvt (abstack[-1].move), levels, window_min, window_max);
#endif

  /* this removes zero-window  - need to also change windowing of 1st branch. window_max = INFINITY; */

  abstack[0].more = levels;		/* how many levels we should do */
  abstack[0].alpha = -window_max;

  TOP = &abstack[depth];
  NEXT = &abstack[depth - 1];
  abstack[0].best = NO_MOVE;
  if (levels <= 0)			/* no search, just evaluate */
    return (TOP->alpha = (*eval) (TOP, NEXT));

  while (depth > 0 || abstack[0].next_move != NO_MOVE)
  {
    CHECK (TOP->more >= 0, "negative levels remaining in ab");
    if (TOP->more <= 0)		/* at bottom? */
    {
      total_leaf_nodes++;
      /* need to check for waiting input? */
      if (Is_stealing && total_leaf_nodes % NODE_CHECK_INTERVAL == 0)
	Wake_Up = input_ready ();
      if (!wipeOut)
	TOP->alpha = (*eval) (TOP, NEXT);
      else			/* have to figure out who wins */
      {
	wipeOut = FALSE;	/* reset for next wipe out in tree */
	/* find value assuming black to move */
	TOP->alpha = sign (TOP->bl_pieces-TOP->wh_pieces)*INFINITY;
	/* in case white to move, negate value */
	if (TOP->color == WHITE)
	  TOP->alpha = -TOP->alpha;
      }
    }

    if (depth >= 1 && (TOP->more <= 0 || TOP->next_move == NO_MOVE))
    {
      if (depth > 1 && TOP->alpha <= abstack[depth - 2].alpha) /* alpha cutoff? */
      {
	/* skip to uncles of this node */
	prev = abstack[depth - 2].move;
	if (LEGAL(prev) && LEGAL(NEXT->move))
	  update_killer (ABCOLOR (depth - 1, color), prev, NEXT->move);
	depth -= 2;
	TOP = &abstack[depth];
	NEXT = &abstack[depth - 1];
#ifdef TREE_PRINT
	cutoff (depth + 2, abstack[depth + 2].alpha, abstack[depth].alpha);
#endif
      }
      else if (depth == 1 && TOP->alpha <= window_min) /* top-level cut-off? */
      {
	prev = abstack[-1].move;
	if (prev >= 0 && NEXT->move >= 0)
	  update_killer (ABCOLOR (0, color), prev, NEXT->move);
	if (-TOP->alpha > NEXT->alpha)
	  NEXT->alpha = -TOP->alpha;
#ifdef TREE_PRINT
	cutoff (depth, TOP->alpha, window_min);
#endif
	break;
      }
      else			/* beta value needs updating */
      {
	if (-TOP->alpha > NEXT->alpha)
	{
	  /* update cutoff value */
	  NEXT->alpha = -TOP->alpha;
	  ab_put_in (NEXT);
	  /* update best move */
	  NEXT->best = NEXT->move;
	  if (depth > 1)
	  {
	    prev = abstack[depth - 2].move;
	    if (prev >= 0 && NEXT->move >= 0)
	      update_killer (ABCOLOR (depth - 1, color), prev, NEXT->move);
	  }
#ifdef TREE_PRINT
	  update_alpha (depth - 1, TOP->alpha, NEXT->move);
#endif
	}

	depth--;
	TOP = NEXT;
	NEXT = &abstack[depth - 1];
      }

      if (Wake_Up)
	return abstack[0].alpha;
      TOP->next_move = next_move (TOP, TOP->already_tried);
      continue;
    }				/* IF */

    if (depth <= 0)		/* top level is a special case */
      abstack[1].alpha = window_min;
    else			/* bring down alpha value */
      abstack[depth + 1].alpha = abstack[depth - 1].alpha;
    depth++;
    NEXT = TOP;
    TOP = &abstack[depth];

    NEXT->move = NEXT->next_move == REALLY_NO_MOVE ? NO_MOVE : NEXT->next_move;
    TOP->more = NEXT->more - 1;
    abmake_move (NEXT, TOP, TOP->more > 0);
    Nodes_Searched++;
    /*	How_Many[TOP->more]++;  */

    if (NEXT->next_move == REALLY_NO_MOVE)
      if (TOP->next_move != REALLY_NO_MOVE) /* Last time = no moves, now have moves, so stop expanding last. */
	NEXT->next_move = NO_MOVE;
      else			/* Both last and this = no moves - make ->more zero to quit. */
      {
	TOP->more = 0;
	wipeOut = TRUE;
	ab_put_in (NEXT);
      }

#ifdef TREE_PRINT
    treeprint (depth, Color_Name[ABCOLOR (depth - 1, color)], uncvt (NEXT->move));
#endif
  }				/* while */
  ab_put_in (abstack);
#ifdef TREE_PRINT
    printf ("\nAB returning (%d %s)\n",
	    abstack[0].alpha, uncvt (abstack[0].best));
#endif
  return abstack[0].alpha;
}

/* ab2 returns the "best" move from the given board and computes the
 * branching factor for use by deepen. */

eval_t ab2 (abstack, window_min, window_max, color)
     ANode           abstack[];
     eval_t          window_min, window_max;
     color_t         color;
{
  register ANode *TOP, *NEXT;
  register int	  depth = 0;		/* current search depth */
  int		  at_bottom;

#ifdef TREE_PRINT
    printf ("IN ab (%s %s %d %d)\n", Color_Name[color],
	    uncvt (abstack[-1].move), window_min, window_max);
#endif

    Ab2_Leaf_Nodes = 0;
    abstack[0].alpha = -window_max;
    TOP = &abstack[depth];
    NEXT = &abstack[depth - 1];
    abstack[0].best = NO_MOVE;
    while (depth > 0 || abstack[0].next_move != NO_MOVE)
      {
	if (at_bottom = TOP->next_move == REALLY_NO_MOVE && NEXT->next_move == REALLY_NO_MOVE || TOP->bl_pieces + TOP->wh_pieces == MAX_PIECES)
	{
	  total_leaf_nodes++;
	  /* need to check for waiting input? */
	  if (Is_stealing && total_leaf_nodes % NODE_CHECK_INTERVAL == 0)
	    Wake_Up = input_ready ();

	  Ab2_Leaf_Nodes++;
	  Leaf_Nodes++;
	  TOP->alpha = ABCOLOR (depth, color) == BLACK ?
	    	       TOP->bl_pieces - TOP->wh_pieces : TOP->wh_pieces - TOP->bl_pieces;

	  /* Group everything within the window together ... if (ABCOLOR
	     * (depth, color) == color) { if (TOP -> alpha > window_min &&
	     * TOP -> alpha < window_max) TOP -> alpha = window_min + 2; }
	     * else { if (TOP -> alpha < -window_min && TOP -> alpha >
	     * -window_max) TOP -> alpha = window_min - 2; } */
#ifdef TREE_PRINT
	  printf (" %d (B : %d - W : %d)\n", TOP->alpha, TOP->bl_pieces, TOP->wh_pieces);
#endif
	}

	if (depth >= 1 && (at_bottom || TOP->next_move == NO_MOVE))
	  {
	    /* if alpha cutoff: */
	    if (depth > 1 && TOP->alpha <= abstack[depth - 2].alpha)
		/* then skip to uncles of this node: */
	    {
		depth -= 2;
		TOP = &abstack[depth];
		NEXT = &abstack[depth - 1];
#ifdef TREE_PRINT
		cutoff (depth + 2, abstack[depth + 2].alpha, abstack[depth].alpha);
#endif
	    }
	    else if (depth == 1 && TOP->alpha <= window_min)
		/* Top-level cut-off */
	    {
		if (-TOP->alpha > NEXT->alpha)
		    NEXT->alpha = -TOP->alpha;
#ifdef TREE_PRINT
		cutoff (depth, TOP->alpha, window_min);
#endif
		break;
	    }
	    else
	    {
		/* if beta value needs updating: */
		if (-TOP->alpha > NEXT->alpha)
		{
		    /* update cutoff value: */
		    NEXT->alpha = -TOP->alpha;
		    ab_put_in (NEXT);
		    /* update best move */
		    NEXT->best = NEXT->move;
#ifdef TREE_PRINT
		    update_alpha (depth - 1, TOP->alpha, NEXT->move);
#endif
		}
		depth--;
		TOP = NEXT;
		NEXT = &abstack[depth - 1];
	    }

	    if (Wake_Up)
	      return -abstack[0].alpha;
	    TOP->next_move = next_move2 (TOP, TOP->already_tried);
	    continue;
	  }				/* IF */

	if (depth == 0)			/* top level is a special case */
	  abstack[1].alpha = window_min;
	else				/* bring down alpha value */
	  abstack[depth + 1].alpha = abstack[depth - 1].alpha;

	depth++;
	NEXT = TOP;
	TOP = &abstack[depth];

	NEXT->move = NEXT->next_move != REALLY_NO_MOVE ? NEXT->next_move : NO_MOVE;
	abmake_move2 (NEXT, TOP, NEXT->move);
	if (TOP->next_move != REALLY_NO_MOVE && NEXT->next_move == REALLY_NO_MOVE)
	    /* last time = no moves, now have moves, so stop expanding last */
	  NEXT->next_move = NO_MOVE;

#ifdef TREE_PRINT
	treeprint (depth, Color_Name[ABCOLOR (depth - 1, color)],
		   uncvt (NEXT->move));
#endif

      }					/* while */
  ab_put_in (abstack);
#ifdef TREE_PRINT
    printf ("\nAB returning (%d %s)\n",
	    abstack[0].alpha, uncvt (abstack[0].best));
#endif

  return -abstack[0].alpha;
}

void ab_put_in (node)
     register ANode *node;
{
  put_in (node->key, num_discs(node), node->color, node->move);
}

/* Updates the killer table by moving up this_move up one move in the linked
 * list entry for prev_move and color */

void update_killer (color, prev_move, this_move)
    int             color,
                    prev_move,
                    this_move;
{
  register struct double_link *one, *two, *three, *four;

  three = Killer_Table[color][prev_move].resp_addr[this_move];
  if (three == NULL || (two = three->prev) == NULL)
    return;

  one = two->prev;
  if ((four = three->next) == NULL)
    if (one == NULL)
      {
	Killer_Table[color][prev_move].order = three;
	three->next = two;
	three->prev = NULL;
	two->prev = three;
	two->next = NULL;
      }
    else
      {
	one->next = three;
	three->next = two;
	two->next = NULL;
	three->prev = one;
	two->prev = three;
      }
  else
    if (one == NULL)
      {
	Killer_Table[color][prev_move].order = three;
	three->next = two;
	three->prev = NULL;
	two->next = four;
	two->prev = three;
	four->prev = two;
      }
    else
      {
	one->next = three;
	three->next = two;
	two->next = four;
	four->prev = two;
	two->prev = three;
	three->prev = one;
      }
}

/* computes hash key */

int compute_key (board)
     board_t board;
{
  register int i, key = 0;
  extern Random[2][MAX_PIECES];
  
  key = 0;
  for (i=0;i<MAX_PIECES;i++)
    if (Occupied(board[i]))
	key ^= Random[(unsigned char) board[i]][i];
  return key;
}

#ifdef TREE_PRINT
forward_prune (level, player, move)
    int             level;
    char           *player,
                   *move;
{
    register int i;

    printf ("\n");
    for (i = 0; i < level; i++)
	printf ("        ");
    printf ("%s (%c) -> X-Square -> Forward Pruned.", move, player[0]);
    fflush (stdout);
}

treeprint (level, player, move)
    int             level;
    char           *player,
                   *move;
{
    register int i;

    printf ("\n");
    for (i = 1; i < level; i++)
	printf ("        ");
    printf ("%s (%c)", move, player[0]);
    fflush (stdout);
}

cutoff (level, now, alpha)
    int             level,
                    now,
                    alpha;
{
    register        i;

    printf ("\n");
    for (i = 1; i < level; i++)
	printf ("        ");
    printf ("Cutoff! (%d <= %d)", now, alpha);
    fflush (stdout);
}

update_alpha (level, alpha, best)
    int             level,
                    alpha;
    char           *best;
{
    register int i;

    printf ("\n");
    for (i = 0; i < level; i++)
	printf ("        ");
    printf ("Updating alpha[%d] = %d (best = %s)", level, alpha, uncvt (best));
    fflush (stdout);
}
#endif

static int sign (int n)
{
  return n < 0 ? -1 : (n > 0 ? 1 : 0);
}
