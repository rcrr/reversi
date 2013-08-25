/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <node.h>
#define INF (1 << 24)

eval_t eval80 (node, features, old_features)
     Node *node;
     feature_t features[4], old_features[4];
{
  feature_t combined_features[4];
  extern Leaf_Nodes;

  Leaf_Nodes++;
  if (node->bl_pieces <= 0)
    return node->color == BLACK ? -INF : INF;
  if (node->wh_pieces <= 0)
    return node->color == BLACK ? INF : -INF;
  compute4 (node->board, node->color, features);
  combine4 (features, old_features, combined_features);
  return combined_features[0] * 438 + combined_features[1] * 375 +
         combined_features[2] * 93 + combined_features[3] * 69 + num_discs(node) * 900 + 100000;
}
