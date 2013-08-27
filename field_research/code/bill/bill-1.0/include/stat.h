/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <ab_info.h>
#include "eval.h"

typedef struct
{
  int nodes_searched,		/* total nodes in tree (including leaf nodes) */
      leaf_searched,		/* calls to eval */
      time_used;		/* in milliseconds, I think */
  eval_t value,			/* value put here by some search */
	 other_value;		/* additional field if wanted */
  exact_t exact;		/* whether value is EXACT, INEXACT, or INCOMPLETE */
  who_t who;			/* select, ab, ab2, singular */
  double depth_searched;	/* how many plies, or some similar measure */
} stat_info_t;

struct expansion_stats
{
    int count;
    double upper_sum;
    double upper_sum_square;
    double lower_sum;
    double lower_sum_square;
    double total_sum_abs;
    double total_sum_square;
};

extern struct expansion_stats Expansion_Stats[2][64][2];
