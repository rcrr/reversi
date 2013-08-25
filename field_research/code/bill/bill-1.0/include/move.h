/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* contains definitions needed by the move finding and flipping procedures.
   The old ones, that is.   (Find_Moves, etc.) */

#ifndef MOVE_H

#define move_t int

#define NO_MOVE		-1 
#define REALLY_NO_MOVE  -2
#define NEXT_MAX 	420	/* number of guys in 1-d next-arr */
#define MAX_LEGALS	30
#define LEGAL(move)	((move) >= 0)

/* The list of legal moves */

typedef struct
{
  int max;			/* number of legal moves */
  move_t data[MAX_LEGALS];	/* the moves */
} legal_list;
typedef legal_list *legal_t;

#define MOVE_H
#endif
