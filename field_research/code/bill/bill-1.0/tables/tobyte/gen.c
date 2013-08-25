/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* Generate tables to convert an index (integer) into a byte (unsigned char)
   representing where the legal moves are. (1 for legal, 0 for not)
   This is assuming BLACK to move.
 */

#include <stdio.h>

#include "tobyte-tables.h"
#define NUM_TABLES 5

struct one
{
    char name[80];		/* name of table. E.G. mv_table44 */
    int  len1,len2,		/* sizes of bigger, then smaller. e.g. 5,3 */
    	 has_two,		/* is like a 5+3 or 4+4 (TRUE), or a 7-0,8-0, etc */
	 size;			/* number of entries required */
} all[NUM_TABLES] =
{ { "mv_table44", 4, 4, TRUE , 6561},
  { "mv_table53", 5, 3, TRUE , 6561},
  { "mv_table60", 6, 0, FALSE,  729},
  { "mv_table70", 7, 0, FALSE, 2187},
  { "mv_table80", 8, 0, FALSE, 6561}
};

main()
{
    int             line1[10], line2[10];
    char           *name;
    int             tb;
    register struct one *temp;
    register        i, len1, len2,size,byte;
    FILE           *out;

    for (tb = 0; tb < NUM_TABLES; tb++)
    {
	temp = &all[tb];		/* the current structure */
	len1 = temp->len1;
	len2 = temp->len2;
	name = temp->name;
	size = temp->size;
	printf("/* Table to convert index to legals.  This is the %1d+%1d table */\n",
	       len1, len2);
	printf("\nunsigned char tobyte%1d%1d[%d] = \n{", len1, len2, size);
	for (i = 0; i < size; i++)
	{
	    toline(i, line1, line2, len1, len2);
	    byte = (find_moves(line1, len1) << len2) | find_moves(line2, len2);
	    if (i % 20 == 0)
		printf("\n    ");
	    if (i < size - 1)
		printf(" %3d,", byte);
	    else
		printf(" %3d\n};\n", byte);
	}
	printf("\n");		/* just to give seperation */
	fflush(stdout);
    }
    return 0;
}

/* cvt an index to a sequence of black, white and empty squares.
   i is the index
   line1 is the high line
   line2 is the low line
 */
   
toline(i,line1,line2,l1_size,l2_size)
register i;
register *line1,*line2;
int l1_size,l2_size;
{
    register x;

    for (x=l2_size-1;x>=0;x--)
    {
	line2[x] = i%3;
	i /= 3;
    }
    for (x=l1_size-1;x>=0;x--)
    {
	line1[x] = i%3;
	i /= 3;
    }
}

/* return a byte representing the moves for BLACK */

find_moves(line,line_size)
int line_size;
register *line;
{
    int      byte[20],	/* fake byte */
             i;
    int      real_byte;

    for (i = 0; i < line_size; i++)/* check if each square is legal */
    {
	register     j;
	int     found_white;

	byte[i] = FALSE;
	if (line[i] != EMPTY)
	    continue;

	found_white = FALSE;
	for (j = i + 1; j < line_size; j++)/* go forward */
	{
	    if (line[j] == WHITE)
		found_white = TRUE;
	    else
		if (line[j] == BLACK)
		{
		    if (found_white)
		    {
			byte[i] = TRUE;
			break;
		    }
		    else
			break;
		}
		else
		    break;
	}			/* END: for (j ...) */

	found_white = FALSE;
	for (j = i - 1; j >= 0; j--)/* go backward */
	{
	    if (line[j] == WHITE)
		found_white = TRUE;
	    else
		if (line[j] == BLACK)
		{
		    if (found_white)
		    {
			byte[i] = TRUE;
			break;
		    }
		    else
			break;
		}
		else
		    break;
	}			/* END: for (j ...) */
    }				/* END: for (i ...) */

    real_byte = 0;
    for (i = 0; i < line_size; i++)
	real_byte = (real_byte << 1) | byte[i];
    return(real_byte);
}
