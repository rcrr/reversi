/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* stuff for Bill's color naming system */

#ifndef COLOR_H

#define color_t int

#define END 	 	-1	/* marker meaning end of some arrays */
#define BLACK		0
#define WHITE		1
#define EMPTY		2
#define ESSQ  		3	/* empty but adjacent to any piece */

/* Macros for deciding who's in a square */

#define Opposite(Color) ((Color) ^ 0x1)
#define Empty(Color) ((Color) >= EMPTY)
#define Occupied(Color) ((Color) < EMPTY)
#define COLOR(turn) ((turn)&1)	/* the color who is to play at a given turn_num */
#define COLOR_H
#endif
