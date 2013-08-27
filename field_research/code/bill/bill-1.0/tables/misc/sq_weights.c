/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * This is one of the most important tables.  It contains weighted square
 * stuff used for calculating mobility values.  (1150 is the canonical value).
 * All scaling is done by someone else (edge/gen.c and rest/gen.c) 
 */

#define MAX_PIECES 64

/*
 * Sq_Weight[i] is the value of having a move to square i.  The reason only
 * half of the value are filled in is kind of shaky.  It's becuase rest/gen.c
 * doesn't use the upper 4th group of 16 squares in sq_map, but rather the 3rd
 * group.  Thus, there is no way anyone can reference a value higher than ??. 
 * Also, only the northern edge is used by edge/gen.c, so the other edges are
 * also blank. 
 */

int             Sq_Weights[MAX_PIECES] =
{
    450,  850,  850,  850,  850,  850,  850,  450, 	/* northern edge */
      0, 1150, 1050, 1100, 1100, 1050, 1150,    0,
      0, 1050, 1250, 1200, 1200, 1250, 1050,    0,
      0, 1100, 1200,    0,    0, 1200, 1100,    0,
};
