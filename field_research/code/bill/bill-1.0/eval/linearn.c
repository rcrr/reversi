/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <bill-features.h>
#include <eval.h>

/*
 * compute any linear evaluation
 */

eval_t linearn (n, combined_features, coefficients)
     int n;
     feature_t combined_features[];
     int coefficients[];
{
  register int i;
  register feature_t *comb = combined_features;
  register int *coef = coefficients;
  register eval_t eval = 0;

  for (i = 0; i < n; i++)
    eval += *comb++ * *coef++;
  return eval + *coef;			/* don't forget constant term */
}
