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
 *	     6/19/87 forked from eval8
 */

#include <stdio.h>

#include <general.h>
#include <node.h>

extern int Leaf_Nodes, Coef21[65][22];


/*
 * my 21-feature evaluation function
 */

eval_t eval21 (node, features, old_features)
     Node *node;
     feature_t features[4], old_features[4];
{
  feature_t combined_features[21];
  register eval_t eval;
  eval_t linearn();

  Leaf_Nodes++;
  compute4 (node->board, node->color, features);
  combine21 (features, old_features, combined_features,
	     node->color == BLACK ? node->bl_pieces - node->wh_pieces : node->wh_pieces - node->bl_pieces);
  eval = linearn (21, combined_features, Coef21[num_discs(node)]) / 100;

#define BIG	(1 << 24)
  if (eval < -BIG || eval > BIG)
    internal_error ("ridiculous evaluation");
  return eval;
}
