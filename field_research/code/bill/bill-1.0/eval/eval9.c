/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * Gaussian Classifier routines for NewBill 
 *
 * Kai-Fu Lee 7/4/86 Created. 
 *
 * Sanjoy Mahajan 8/10/86 rewritten for Bill 3.0 
 *
 * Joe Keane 1/16/87 moved out compute ()
 * Joe Keane 4/11/87 modified for eight-feature evaluation
 *	     9/19/87 forked from eval8
 */

#include <stdio.h>

#include <general.h>
#include <node.h>

extern int Leaf_Nodes;
#if NONLINEAR
extern double	Mean_Win9[MAX_PIECES + 1][9],
                Mean_Loss9[MAX_PIECES + 1][9],
                ICov_Win9[MAX_PIECES + 1][9][9],
		ICov_Loss9[MAX_PIECES + 1][9][9],
		LnDiff9[MAX_PIECES + 1];
#else
extern int	Coef9[65][10];
#endif


/*
 * my 9-feature evaluation function
 */

eval_t eval9 (node, features, old_features)
     Node *node;
     feature_t features[4], old_features[4];
{
  feature_t combined_features[9];
  register eval_t eval;
  int pieces = num_discs(node);
  eval_t linearn();

  Leaf_Nodes++;
  compute4 (node->board, node->color, features);
  merge9 (features, old_features, combined_features,
	  node->color == BLACK ? node->bl_pieces - node->wh_pieces : node->wh_pieces - node->bl_pieces);
#if NONLINEAR
  eval = win_probn (9, combined_features, Mean_Win9[pieces], Mean_Loss9[pieces], ICov_Win9[pieces], ICov_Loss9[pieces], LnDiff9[pieces]);
#else
  eval = linearn (9, combined_features, Coef9[pieces]);
#endif
#define BIG	(1 << 24)
  if (eval < -BIG || eval > BIG)
    internal_error ("ridiculous evaluation");
  return eval;
}

