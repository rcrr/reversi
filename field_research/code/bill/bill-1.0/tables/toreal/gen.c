/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * the program to generate the tables that convert bytes of legal moves to bit
 * strings (like virtual -> physical address translation). This program will
 * output a file containing structure initialization stuff. Bill can use this
 * as a union by coercion (see /../h/usr/mahajan/info/union_??) 
 */

#define MAX_PIECES 64
#define LEN 64
#define NUM_TABLES 32
#include <stdio.h>

extern int      Sq_Map[MAX_PIECES];

long		masks[MAX_PIECES][2];		/* will contain, for each
						 * square on board, a 64-bit
						 * string that means: wherever
						 * there is a 1, that square
						 * is represented in the bit
						 * string there. */

extern struct one
{
    char           *name;		/* the human names for this table */
    int             map[8],		/* the squares that go with in bits
					 * 0-7 */
                    size;		/* total ternary digits */
}               data[NUM_TABLES];

long		bit_setter[32];		/* bit_setter[i] = 2^i */

main()
{
    char           *name;
    register        j,
                    size,
                    k,
                   *map,
                    i;
    long temp[2];

    for (j = 0; j < 32; j++)
	bit_setter[j] = 1 << j;
    fill_masks();
    printf ("typedef struct {unsigned long emoves, rmoves; } moves_t;\n");
    for (i = 0; i < NUM_TABLES; i++)	/* do each table */
    {
	map = data[i].map;
	size = data[i].size;
	name = data[i].name;
    /*
     * 1<<size (2^size) is the number of elements, becuase it is a
     * binary-based array 
     */
	printf("moves_t %s[%d] =\n{\n",
	       name, 1 << size);
	for (j = 0; j < (1 << size); j++)	/* print each entry */
	{
	    temp[0] = temp[1] = 0;
	    for (k = 0; k < size; k++)	/* compute mask for given index: j */
	    {
		if (bit_setter[k] & j)	/* if a legal move at bit k, byte #j */
		{
		    temp[0] |= masks[map[k]][0];	/* update by or'ing */
		    temp[1] |= masks[map[k]][1];
		}
	    }
	/* print 64-bits out */
	    printf("    {%11uU,%11uU },\n",
		   (unsigned) temp[0], (unsigned) temp[1]);
	}				/* for (j=0..) */
	printf("\n};\n\n");		/* end of one array */
    }					/* for (i=0..) */
    return 0;
}

fill_masks()
{
    register        i,
                    j;

    for (i = 0; i < 64; i++)		/* do each string */
    {
	for (j = 0; j < 64; j++)	/* check if each bit needs to be 1 */
	    if (Sq_Map[j] == i)		/* needs to be one? */
	    {
		if (j < 32)
		    masks[i][0] |= bit_setter[j];
		else
		    masks[i][1] |= bit_setter[j - 32];
	    }
    }
}
