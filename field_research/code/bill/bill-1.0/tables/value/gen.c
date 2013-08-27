/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* this program generates the big value array need by the table generator.
   The output is an array: Value[NUM_TABLES][NUM_TYPES][MAX_PIECES][MAX_PIECES+1]
   The last dimension is how long the sequence is.  The second to last is the
   starting disc of the sequence: (0-Size).
 */

#include <tables.h>

extern double  weights[NUM_TABLES][NUM_TYPES][MAX_PIECES+1];
extern int Value[NUM_TABLES][NUM_TYPES][MAX_PIECES];

main()
{
    int             size;
    register         i, j, k, l, m, total;

    printf("#include <tables.h>\n\n");
    printf("int Value[NUM_TABLES][NUM_TYPES][MAX_PIECES][MAX_PIECES+1] = \n{\n");
    for (i = 0; i < NUM_TABLES; i++)		/* do each table */
    {
	size = pieces(i);
	printf("    {\t/* table number = %d */\n",i);			/* beginning paren for this
						 * table */
	for (j = 0; j < NUM_TYPES; j++)		/* do each type */
	{
	    printf("        {\t/* type number = %d */\n",j);
	    for (k = 0; k < size; k++)		/* do each starting location
						 * (lower end) */
	    {
		printf("            { %5d,",0);
		for (l = 1; l <= size - k; l++)	/* do each length */
		{
		    total = 0;
		    for (m = k; m < l + k; m++)	/* compute total (unweighted) */
			total += Value[i][j][m];
		    printf(" %5d,",		/* print entry */
			   (int) ((double) total * weights[i][j][l] / l));
		}				/* for each length */
		printf(" },\t/* start number = %d */\n",k);		/* end of lengths */
	    }					/* for each start */
	    printf("        },\n");		/* end of starting locations
						 * (= one type) */
	}					/* for each type */
	printf("    },\n");			/* end of types (=one table) */
    }						/* for each table */
    printf("};\n");
    return 0;
}
