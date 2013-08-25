/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

 /* this give the penalties for flipping a disc on the given line:
    using it is:
    	x = Move_Penalty[i][j] means that for table i, flipping square j
	is worth -x.
	i is 0-8, with diag3 being 0 and tb3 being 8.
	j is 0-7 (for the full-size things).
    Flip_weights is used to introduce non-linear sequence length
    penalty.   */

#include <tables.h>

int Flip_Penalty[NUM_TABLES][MAX_PIECES-1] =
{
    { 0,  300},					/* diag3 */
    { 0,  160,  160},				/* diag4 */
    { 0,  170,  150,  170},			/* diag5 */
    { 0,  170,  140,  140,  170},		/* diag6 */
    { 0,  150,  100,    0,  100,  150},		/* diag7 */
    { 0,  120,   80,    0,    0,   80,  120},	/* diag8 */
    { 0,  210,  190,  160,  160,  190,  210},	/* tb1 */
    { 0,  170,  150,  100,  100,  150,  170},	/* tb2 */
    { 0,  140,  100,   40,   40,  100,  140},	/* tb3 */
};

double Flip_Weights[MAX_PIECES+1] =		/* based on bart's info */
{
    0, 1.0, 2.0, 2.7, 3.3, 3.9, 4.4,
};
