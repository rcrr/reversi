/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * program to generate the table that evaluates moves on the edges. The input
 * to the table is an index (0 - 4^8-1) that tells it the edge position, and
 * where the legals are. 
 *
 * The rules used in evaluation are: For each move, a penalty is assessed. That
 * penalty is computed by having BLACK make the move and seeing how the value
 * of the edge position changes for BLACK. (by evaluating for WHITE and
 * negating) (call the delta d (< 0 if bad)) 
 *
 * If d < 0, penalty = Min(abs(d)*7/4 , 700) 
 *
 * if d > 0, bonus   = Min(abs(d)*2, 700) 
 *
 * WARNINGS:  edge table must have black = 0, white = 1, empty = 2 and x -squares
 * most significant 
 */

#include <stdio.h>

#define LEGAL 3
#define BLANK 2
#define EMPTY 2
#define WHITE 1
#define BLACK 0
#define MAX_PIECES 8
#define X_EMPTY 52488			/* magic add to index to get pos with
					 * empty x-squares (intead of with
					 * BLACK in both) */

#define Opposite(x) (!(x))

extern short    Edge_Table[59049];
extern double   Mscale;			/* because we compressed mscore in
					 * gen_ot.c */

static int      cvt[4] = {0, 1, 2, 2};	/* convert legal -> empty */

main()
{
    register        i,
                    k,
                    l,
                    delta;
    int             edge[MAX_PIECES],
                    new_edge[MAX_PIECES];
    int             bonus,
                    old;

    int             ind,new;

    printf("short Edge_Penalty[65536] = \n{\n    ");
    for (i = 0; i < 65536; i++)
    {
	old = Edge_Table[to_edge(edge, i)];	/* edge becomes edge with
						 * LEGALS marked.  returns
						 * index into old et (for
						 * getting old) */
	bonus = 0;			/* nothing found yet */
	for (k = 0; k < MAX_PIECES; k++)/* do each move */
	{
	    if (edge[k] == LEGAL)	/* a move? */
	    {
		for (l = 0; l < MAX_PIECES; l++)
		    new_edge[l] = cvt[edge[l]];
		really_make_move(new_edge, k);
	        ind = to_rev(new_edge);
		new = Edge_Table[ind];
		delta = -1 * Edge_Table[ind] - old;
		if (delta >= 0)		/* good move? */
		{
		    if (delta > 350)	/* 700 is max bonus */
			delta = 350;
		    bonus += delta * 2;
		} else			/* bad move */
		{
		    if (delta < -400)
			delta = -400;
		    bonus += (int) ((double) (1.75 * delta));
		}
	    }				/* if edge[k] is legal */
	}
	/* print penalty */
	printf("%6d,", (int) (-bonus * Mscale));
	if (i % 10 == 9)
	    printf("\n    "); 
    }
    printf("\n};\n");
    return 0;
}

to_edge(edge, i)
register *edge,
             i;
{
    register        j,
                    total = 0;

    for (j = 0; j < MAX_PIECES; j++)
    {
	edge[j] = i&3;
	i >>= 2;
    }
    for (j = MAX_PIECES - 1; j >= 0; j--)
	total = total * 3 + cvt[edge[j]];
    return (total + X_EMPTY);		/* want pos with empty x-squares */
}

to_rev(edge)
register *edge;
{
    register        total = 0,
                    j;
    static          rev[3] = {1, 0, 2};

    for (j = MAX_PIECES - 1; j >= 0; j--)
	total = 3 * total + rev[edge[j]];
    return (total + X_EMPTY);		/* want empty x-squares */
}

print_edge(fp, edge)
register FILE  *fp;
register *edge;
{
    register        i;
    static char     name[4] = {'B', 'W', '.', '?'};	/* ? is for Diag8 legal */

    for (i = 0; i < MAX_PIECES; i++)
    {
	putc(name[edge[i]], fp);
	putc(' ', fp);
    }
}

really_make_move(edge, move)
register        *edge;
int             move;
{
    register        i,
                    j;

    if (move != MAX_PIECES - 1)
    {
	if (edge[move + 1] == WHITE)
	    for (i = move + 2; i < MAX_PIECES; i++)
	    {
		if (edge[i] == BLANK)
		{
		    break;
		} else
		if (edge[i] == Opposite(edge[move + 1]))
		{
		    for (j = move + 1; j <= i - 1; j++)
		    {
			edge[j] = BLACK;
		    }
		    break;
		}
	    }
    }
    if (move != 0)
    {
	if (edge[move - 1] == WHITE)
	    for (i = move - 2; i >= 0; i--)
	    {
		if (edge[i] == BLANK)
		{
		    break;
		} else
		if (edge[i] == Opposite(edge[move - 1]))
		{
		    for (j = move - 1; j >= i + 1; j--)
		    {
			edge[j] = BLACK;
		    }
		    break;
		}
	    }
    }
    if (edge[move] != BLANK)
    {
	fprintf(stderr, "Tried to make illegal move.\n");
	kill(getpid(), 11);
    } else
	edge[move] = BLACK;
}
