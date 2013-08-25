/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdio.h>
#include <math.h>

/* #define DEBUG */
#define COUNT 59049

extern short New_Edge_Table[], Edge_Table[];

float compute_scale (short *table)
{
    double sums;
    double sums2;
    int sumw;
    double sumws;
    double sumws2;
    int i;
    double mean;
    double sdev;

    sums = 0;
    sums2 = 0;
    sumw = 0;
    sumws = 0;
    sumws2 = 0;

    for (i = 0; i < COUNT; i++)
    {
	int score;
	int weight;
	int temp;
	int j;

	score = table[i];
	sums += score;
	sums2 += score * score;
	weight = 1;
	temp = i;

	for (j = 0; j < 10; j++)
	{
	    if (temp % 3 == 2)
		weight *= 2;
	    temp /= 3;
	}

	sumw += weight;
	sumws += weight * score;
	sumws2 += weight * score * score;
    }

    mean = sums / COUNT;
    sdev = sqrt(sums2 / COUNT - mean * mean);
#ifdef DEBUG
    fprintf(stderr, "mean=%12.6f sdev=%12.6f\n", mean, sdev);
#endif
    mean = sumws / sumw;
    sdev = sqrt(sumws2 / sumw - mean * mean);
#ifdef DEBUG
    fprintf(stderr, "weighted mean=%12.6f sdev=%12.6f\n\n", mean, sdev);
#endif
    return sdev;
}

int main()
{
  printf ("float New_Edge_Scale = %f/%f;\n",
	  compute_scale (Edge_Table),
	  compute_scale (New_Edge_Table));
  return 0;
}
