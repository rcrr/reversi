/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* take move_data.c and makes the type of Move_val stuff needed by work.c */

#include <tables.h>

#define FUDGE_FACTOR 0.5		/* moves to edge only penalized half,
					 * because other people will take care
					 * of them */

extern          Flip_Penalty[NUM_TABLES][MAX_PIECES - 1];
extern double   Flip_Weights[MAX_PIECES + 1];

main()
{
    register        i,
                    j,
                    k,
                    l,
                    total,
                    size;

    printf("#include <tables.h>\n\n");
    printf("int Move_Penalty[NUM_TABLES][MAX_PIECES-1][MAX_PIECES+1] = \n{\n");
    for (i = 0; i < NUM_TABLES; i++)
    {
	size = pieces(i);
	printf("    { /* %d */\n        {0, },\n", i);
	for (j = 1; j < size - 1; j++)	/* for each starting spot */
	{
	    printf("        {0,");	/* can't flip no discs */
	    for (k = 1; k < size - j; k++)	/* for each # flipped */
	    {
		total = 0;

		for (l = j; l < j + k; l++)	/* for each disc flipped */
		    total += Flip_Penalty[i][l];
	    /*
	     * if a move to the end of a line (i.e., an edge), must reduce the
	     * value of it by multiplying by (another) fudge factor: 
	     */
		if (j == 1)
		{
		    if (i == DIAG8)	/* move to corner? */
			total = 0;	/* will be taken care of in edge-legals */
		    else
			total = total * FUDGE_FACTOR;
		}
		printf("%5d,", (int) ((double) total * Flip_Weights[k] / (double) k));
	    }
	    printf("},\n");
	}
	printf("    },\n");
    }
    printf("};\n");
    return 0;
}
