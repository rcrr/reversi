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

#include <general.h>
#include <bill-features.h>
#include <eval.h>

void flip_icov ();

/*
 * returns 200000 * ln (p_win / p_loss)
 */

eval_t win_prob4 (combined_features, mean_win, mean_loss, icov_win, icov_loss, lndiff)
    feature_t combined_features[4];
    double mean_win[4], mean_loss[4], icov_win[4][4], icov_loss[4][4], lndiff;
{
    register feature_t *comb;
    register double *mean;
    double diff0;
    double diff1;
    double diff2;
    double diff3;
    register double (*icov)[4];
    double p_diff;

    comb = combined_features;
    mean = mean_win;
    diff0 = comb[0] - mean[0];
    diff1 = comb[1] - mean[1];
    diff2 = comb[2] - mean[2];
    diff3 = comb[3] - mean[3];
  
    icov = icov_win;
    p_diff = (diff0 * icov[0][0] + diff1 * icov[0][1] + diff2 * icov[0][2] + diff3 * icov[0][3]) * diff0 + (diff1 * icov[1][1] + diff2 * icov[1][2] + diff3 * icov[1][3]) * diff1 + (diff2 * icov[2][2] + diff3 * icov[2][3]) * diff2 + diff3 * icov[3][3] * diff3;

    mean = mean_loss;
    diff0 = comb[0] - mean[0];
    diff1 = comb[1] - mean[1];
    diff2 = comb[2] - mean[2];
    diff3 = comb[3] - mean[3];

    icov = icov_loss;
    p_diff -= (diff0 * icov[0][0] + diff1 * icov[0][1] + diff2 * icov[0][2] + diff3 * icov[0][3]) * diff0 + (diff1 * icov[1][1] + diff2 * icov[1][2] + diff3 * icov[1][3]) * diff1 + (diff2 * icov[2][2] + diff3 * icov[2][3]) * diff2 + diff3 * icov[3][3] * diff3;

    return -100000 * (p_diff + lndiff + lndiff);
}


void init_cov ()
{
  extern double	  ICov_Win4[MAX_PIECES + 1][NUM_FEATURES][NUM_FEATURES],
                  ICov_Loss4[MAX_PIECES + 1][NUM_FEATURES][NUM_FEATURES],
                  ICov_Win2[MAX_PIECES + 1][NUM_FEATURES][NUM_FEATURES],
                  ICov_Loss2[MAX_PIECES + 1][NUM_FEATURES][NUM_FEATURES];
  register int discs;

  for (discs = 4; discs <= MAX_PIECES; discs++)
    {
      flip_icov (ICov_Win4[discs]);
      flip_icov (ICov_Win2[discs]);
      flip_icov (ICov_Loss4[discs]);
      flip_icov (ICov_Loss2[discs]);
    }
}

void flip_icov (icov)
    register double icov[NUM_FEATURES][NUM_FEATURES];
{
  register int i, j;

  for (i = 0; i < NUM_FEATURES - 1; i++)
    for (j = i + 1; j < NUM_FEATURES; j++)
      icov[i][j] += icov[j][i];
}
