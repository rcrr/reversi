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

#include <bill-features.h>


/* combine21 takes the feature values in new_features and old_features, and puts their differences and difference-sum products in combined_features. */

combine21 (new_features, old_features, combined_features, piece_difference)
     feature_t new_features[4], old_features[4], combined_features[16];
     int piece_difference;
{
  feature_t *new = new_features, *old = old_features, *comb = combined_features;
  feature_t sums[4];
  feature_t *sum, d;

  sum = sums;
  *comb++ = d = *new - *old;
  *comb++ = (*sum++ = *new++ + *old++) * d;
  *comb++ = (*sum++ = *new++ + *old++) * d;
  *comb++ = (*sum++ = *new++ + *old++) * d;
  *comb++ = (*sum++ = *new++ + *old++) * d;

  new -= 3;
  old -= 3;

  sum = sums;
  *comb++ = d = *new++ - *old++;
  *comb++ = *sum++ * d;
  *comb++ = *sum++ * d;
  *comb++ = *sum++ * d;
  *comb++ = *sum++ * d;

  sum = sums;
  *comb++ = d = *new++ - *old++;
  *comb++ = *sum++ * d;
  *comb++ = *sum++ * d;
  *comb++ = *sum++ * d;
  *comb++ = *sum++ * d;

  sum = sums;
  *comb++ = d = *new++ - *old++;
  *comb++ = *sum++ * d;
  *comb++ = *sum++ * d;
  *comb++ = *sum++ * d;
  *comb++ = *sum++ * d;

  *comb++ = piece_difference;
}
