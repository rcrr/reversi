/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* generate table to count the edge moves */

#define MAX_PIECES 64

extern int Sq_Weights[MAX_PIECES];
extern double Mscale;

main()
{
    register i,j,total;

    printf("short Count_Edge[256] = \n{\n    ");
    for (i=0;i<256;i++)
    {
	total = 0;
	for (j=0;j<8;j++)
	    if (((i>>j)&1) == 1)	/* a 1 in bit j? */
	        total += Sq_Weights[j];
	printf("%5d,",(int) (Mscale*total));
	if (i%10==9)
	    printf("\n    ");
    }
    printf("\n};\n");
    return 0;
}
