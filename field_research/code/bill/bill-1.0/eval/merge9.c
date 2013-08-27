/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * Merge new and old feature vectors and piece difference for 9-feature evaluation.
 *
 * Joe Keane 6/13/87 separated
 */

#include <bill-features.h>


merge9 (new_features, old_features, combined_features, piece_difference)
     feature_t new_features[4], old_features[4], combined_features[9];
     register int piece_difference;
{
  register feature_t *new = new_features, *old = old_features, *comb = combined_features;

  *comb++ = *new++;			/* new edge total */
  *comb++ = *old++;			/* old edge total */
  *comb++ = *new++;			/* new pot-mob */
  *comb++ = *old++;			/* old pot-mob */
  *comb++ = *new++;			/* new weighted square */
  *comb++ = *old++;			/* old weighted square */
  *comb++ = *new++;			/* new mobility final */
  *comb++ = *old++;			/* old mobility final */
  *comb++ = piece_difference;		/* piece difference */
}
