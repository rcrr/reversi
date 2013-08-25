/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdio.h>
#define BLACK 0
#define WHITE 2
#define LENGTH (3 * 3 * 3 * 3 * 3 * 3 * 3 * 3 * 3 * 3 * 17)

main ()
{
  char *sbrk ();
  char *base;
  register char *ptr;
  int xg, xb, h, g, f, e, d, c;
  register b, a;
  
  fputs ("Starting...\n", stderr);
  ptr = base = sbrk (LENGTH);

  for (xg = BLACK; xg <= WHITE; xg++)
    for (xb = BLACK; xb <= WHITE; xb++)
      for (h = BLACK; h <= WHITE; h++)
	for (g = BLACK; g <= WHITE; g++)
	  for (f = BLACK; f <= WHITE; f++)
	    for (e = BLACK; e <= WHITE; e++)
	      for (d = BLACK; d <= WHITE; d++)
		for (c = BLACK; c <= WHITE; c++)
		  for (b = BLACK; b <= WHITE; b++)
		    for (a = BLACK; a <= WHITE; a++)
		      {
			register char *name = "BW.";
			extern short Edge_Table[3][3][3][3][3][3][3][3][3][3];

			*ptr++ = name[a];
			*ptr++ = name[b];
			*ptr++ = name[c];
			*ptr++ = name[d];
			*ptr++ = name[e];
			*ptr++ = name[f];
			*ptr++ = name[g];
			*ptr++ = name[h];
			*ptr++ = '/';
			*ptr++ = name[xb];
			*ptr++ = name[xg];
			sprintf (ptr, "%5d", Edge_Table[xg][xb][h][g][f][e][d][c][b][a]);
			ptr += 5;
			*ptr++ = '\n';
		      }

  if (ptr != base + LENGTH)
    quit (-1, "Arg!\n");
  fputs ("Writing...\n", stderr);
  write (1, base, LENGTH);
  fputs ("Done.\n", stderr);
}
