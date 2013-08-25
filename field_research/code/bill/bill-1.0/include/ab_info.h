/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#ifndef AB_INFO_H

#include <eval.h>

typedef enum { EXACT, INEXACT, INCOMPLETE } exact_t;
typedef enum {AB, SELECT, AB2_1, AB2_2 } who_t;

typedef struct
{
  int max_levels,		/* maximum number of plies to search */
      small_search_depth,	/* for stealing */
      first_steal_depth;	/* also for stealing */
  eval_t last_score;		/* previous returned score for this color */
  exact_t exact;
  double depth;			/* depth of last search for this color */
  who_t bottom_searched;		/* what kind of search ab has done for this color */
} ab_info_t;

#define AB_INFO_H
#endif
