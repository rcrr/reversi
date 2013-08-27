/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* TIME
 *  Functions to compute real time elapsed
 *
 * Kai-Fu Lee
 * 7/31/84 Created
 * 8/24/85 Modified for Othello
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <sys/types.h>

#if HAVE_SYS_TIME_H
#include <sys/time.h>
#else
#include <time.h>		/* worth trying */
#endif

#include <node.h>

extern double	Time_Raw_Fraction[MAX_MOVES / 2];
double		Time_Fraction[2][MAX_MOVES / 2];

/* elapsed_time returns the number of milliseconds elapsed since last elapsed_time (TRUE).  If (!update), just peek and return how much time has elapsed.  If (update), actually change the from time for next call. */

elapsed_time (update)
     int	     update;
{
  struct timeval current_time;
  static struct timeval start_time;
  register  ret;

  gettimeofday (&current_time, (struct timezone*)NULL);
  ret = (current_time.tv_sec - start_time.tv_sec) * 1000 + (current_time.tv_usec - start_time.tv_usec) / 1000;
  if (update)
      start_time = current_time;
  return ret;
}

/* Allocate percentage-of-time-left for each move */

allocate_time (moves_made, color)
     int	     moves_made, color;
{
  double	  real_total;
  int		  max;
  register int	  i;

  /* Extra 0.5% for first move since need time for lower levels */
  real_total = 1.005;
  for (i = 0; i < moves_made; i++)
    real_total -= Time_Raw_Fraction[i];

  Time_Fraction[color][moves_made] = 
    (Time_Raw_Fraction[moves_made] + 0.005) / real_total;
  real_total -= Time_Raw_Fraction[moves_made] + 0.005;
  max = MAX_MOVES / 2;
  for (i = moves_made + 1; i < max; i++)
    {
      Time_Fraction[color][i] = Time_Raw_Fraction[i] / real_total;
      real_total -= Time_Raw_Fraction[i];
    }
}
