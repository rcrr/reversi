/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdio.h>
#include <node.h>
#define INF ((eval_t)(1 << 28))

#undef DEBUG_EVAL			/* debugging legal moves, etc. */

extern int      Leaf_Nodes,
		Sq_Map[MAX_PIECES];

#ifdef DEBUG_EVAL

/* data for real_gen.c */

#define NUM_TABLES 32

extern struct one
{
  char           *name;			/* the human+array name for this table */
  int             map[8],		/* the squares that go with in bits
					 * 0-7 */
  size;		/* total ternary digits */
}               data[NUM_TABLES];
#endif

eval_t win_prob4();
static feature_t Save_Combined_Features[4];

/* evaluation which uses the new edge table */

eval_t evaluate_newedge (node, old_node)
     Node *node, *old_node;
{
  extern double	  Mean_Win2[MAX_PIECES + 1][4],
		  Mean_Loss2[MAX_PIECES + 1][4],
		  ICov_Win2[MAX_PIECES + 1][4][4],
		  ICov_Loss2[MAX_PIECES + 1][4][4],
		  LnDiff2[MAX_PIECES + 1];
  feature_t combined_features[4];
  extern int Flag_Y;
  register eval_t eval;
  int discs;

  Leaf_Nodes++;
  if (node->bl_pieces <= 0)
    return node->color == BLACK ? -INF : INF;
  if (node->wh_pieces <= 0)
    return node->color == BLACK ? INF : -INF;
  compute4_newedge (node->board, node->color, node->features);
  combine4 (node->features, old_node->features, combined_features);
  discs = num_discs(node);
  if (Flag_Y)
    bcopy ((char *) combined_features, (char *) Save_Combined_Features, sizeof combined_features);
  eval = win_prob4 (combined_features, Mean_Win2[discs], Mean_Loss2[discs],
		    ICov_Win2[discs], ICov_Loss2[discs], LnDiff2[discs]);
#define BIG	(1 << 24)
  if (eval < -BIG || eval > BIG)
    internal_error ("ridiculous evaluation");
  return eval;
}
