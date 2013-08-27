/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * Combine features with some bogus algorithm.
 */

#include <node.h>

/*
 * level_above the values for player and opponent (level above) into an
 * advantage that takes into account the fact that 10:9 is not as good as 2:1
 * The values must be positive.  If they are not, hell could break loose.
 */

#define shift(x) ((x) + 15*32)
#define plevel_above(n, o) (2000 * (n - o) / (n + o + 80))
#define wlevel_above(n, o) (3000 * (n - o) / (abs (n + o) + 100))
#define mlevel_above(n, o) (3000 * (n - o) / (n + o + 700))


/*
 * combine_features takes the feature values in these_features and
 * previous_features, and combines them properly <- if you believe this...
 */

combine4 (new_features, old_features, combined_features)
     feature_t new_features[4], old_features[4], combined_features[4];
{
  register feature_t *new = new_features, *old = old_features, *comb = combined_features;
  register int n, o;			/* NOT feature_t */

  /* now compute stuff to pass to win_prob */
  comb[0] = new[0];			/* new edge total */
  n = new[1];				/* new pot-mob */
  o = old[1];				/* old pot-mob */
  comb[1] = plevel_above(n, o);		/* combined pot-mob */
  n = shift(new[2]);			/* shifted new weighted square */
  o = shift(old[2]);			/* shifted old weighted square */
  comb[2] = wlevel_above(n, o);		/* combined weighted square */
  n = new[3];				/* new mobility final */
  o = old[3];				/* old mobility final */
  comb[3] = mlevel_above(n, o);		/* combined mobility final */
}
