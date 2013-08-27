/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* generates 'short Count_Rest[65536]', which converts 16-bits of legal moves into a mobility count.
   The weights to assign a move are given by sq_weights.  The average value is 1150. */

#define MAX_PIECES 64

extern Sq_Weights[MAX_PIECES], Sq_Map[MAX_PIECES];

main()
{
  register i;

  printf ("short Count_Rest[65536] = \n{");
  for (i=0;i<65536;i++)		/* go through each legal move bit-string */
    {
      register j, val = 0;

      for (j=0;j<16;j++)
	if ((1<<j)&i)		/* a legal move in bit position j? */
	  val += Sq_Weights[Sq_Map[32+j]];
      /* val now has worth of this move string */
      if (i%10 == 0)		/* if need to newline-and-indent */
	printf ("\n    ");
      printf(" %5d,", val);
    }
  printf ("\n};\n");
  return 0;
}

