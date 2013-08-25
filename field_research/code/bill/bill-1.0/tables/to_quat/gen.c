/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * this program generates the 6561 entry table that converts a ternary edge
 * index into 4-nary.  It also generates the 8-bit table that converts 8-bit
 * legal move string (for the edge) into a 4-nary number. 
 */

#define MAX_PIECES 8

short           Edge_To_Quat[6561];
short           String_To_Quat[256];

main()
{
    int        edge[MAX_PIECES],
                    i;

/* print table to convert old edge -> new edge */
    printf("unsigned short Edge_To_Quat[6561] = \n{\n    ");
    for (i = 0; i < 6561; i++)
    {
	printf(" %5u,", to_edge(edge, i));
	if (i % 10 == 9)
	    printf("\n    ");
    }
    printf("\n};\n\n");

/*
 * print table to convert legal string to form addable to new edge 
 */
    printf("unsigned short String_To_Quat[256] = \n{\n    ");
    for (i = 0; i < 256; i++)
    {
	register        j,
	                total;

	total = 0;
	for (j = 0; j < MAX_PIECES; j++)/* compute new value */
	    if (((i >> j) & 1) == 1)	/* a 1 in bit pos. j? */
		total += 1 << (2 * j);
	printf(" %5u,", total);
	if (i % 10 == 9)
	    printf("\n    ");
    }
    printf("\n};\n");
    return 0;
}

to_edge(edge, i)
register        edge[MAX_PIECES],
                i;
{
    register        j,
                    total = 0;

    for (j = MAX_PIECES - 1; j >= 0; j--)
    {
	edge[j] = i % 3;
	i /= 3;
    }
    for (j = 0; j < MAX_PIECES; j++)
	total = 4 * total + edge[j];
    return (total);
}
