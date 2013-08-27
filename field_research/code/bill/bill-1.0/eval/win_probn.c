/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * function to compute new, nonlinear evaluation function. 
 *
 * Joe Keane 1/18/87 rewritten
 * Joe Keane 4/11/87 again
 */

#include <bill-features.h>
#include <check.h>


/*
 * returns ln (p_win / p_loss) * 200000
 */

int win_probn (n, combined_features, mean_win, mean_loss, icov_win, icov_loss, lndiff)
     register int n;
     feature_t combined_features[];
     double mean_win[], mean_loss[], icov_win[], icov_loss[], lndiff;
{
  double diffs[16];
  register int i, j;
  register double p_diff = 0.0;

  for (i = 0; i < n; i++)
    diffs[i] = combined_features[i] - mean_win[i];

  for (i = 0; i < n; i++)
    {
      register double temp = 0.0;

      for (j = 0; j < n; j++)
	temp += icov_win[i * n + j] * diffs[j];
      p_diff += diffs[i] * temp;
    }

  for (i = 0; i < n; i++)
    diffs[i] = combined_features[i] - mean_loss[i];

  for (i = 0; i < n; i++)
    {
      register double temp = 0.0;

      for (j = 0; j < n; j++)
	temp += icov_loss[i * n + j] * diffs[j];
      p_diff += diffs[i] * temp;
    }

  CHECK(p_diff >= -20.0 && p_diff < 20.0, "ridiculous win_prob");
  return (p_diff + lndiff + lndiff) * -100000.0;
}
