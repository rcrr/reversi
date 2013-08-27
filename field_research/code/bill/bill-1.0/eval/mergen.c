/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * Generic merge procedure for new evaluation.
 *
 * Joe Keane 6/13/87 separated
 *	     6/18/87 forked from merge8
 */

#include <bill-features.h>


mergen (n, new_features, old_features, combined_features, piece_difference)
     int n;
     feature_t new_features[], old_features[], combined_features[];
     register int piece_difference;
{
  register int i;
  register feature_t *new = new_features, *old = old_features, *comb = combined_features;

  for (i = 0; i < n; i++)
    {
      *comb++ = *new++;			/* new feature */
      *comb++ = *old++;			/* old feature */
    }
  *comb++ = piece_difference;		/* piece difference */
}
