/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * Merge new and old feature vectors and piece difference for 17-feature evaluation.
 *
 * Joe Keane 6/13/87 separated
 *	     6/18/87 forked from merge8
 */

#include <bill-features.h>


merge17 (new_features, old_features, combined_features, piece_difference)
     feature_t new_features[8], old_features[8], combined_features[17];
     register int piece_difference;
{
  register feature_t *new = new_features, *old = old_features, *comb = combined_features;

  *comb++ = *new++;			/* new north edge */
  *comb++ = *old++;			/* old north edge */
  *comb++ = *new++;			/* new south edge */
  *comb++ = *old++;			/* old south edge */
  *comb++ = *new++;			/* new west edge */
  *comb++ = *old++;			/* old west edge */
  *comb++ = *new++;			/* new east edge */
  *comb++ = *old++;			/* old east edge */
  *comb++ = *new++;			/* new pot-mob */
  *comb++ = *old++;			/* old pot-mob */
  *comb++ = *new++;			/* new weighted square */
  *comb++ = *old++;			/* old weighted square */
  *comb++ = *new++;			/* new mobility final */
  *comb++ = *old++;			/* old mobility final */
  *comb++ = piece_difference;		/* piece difference */
}
