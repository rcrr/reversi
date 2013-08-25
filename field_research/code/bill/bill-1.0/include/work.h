/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#define TRUE		1
#define FALSE		0
#define MAX_PIECES	64
#define MAX_BLANKS	60

#define Min(x,y) (((x) > (y)) ? (y) : (x))
#define Max(x,y) (((x) > (y)) ? (x) : (y))
#define Abs(x) (((x) > 0) ? (x) : -(x))

/* Game Record is used to represent a node in a game as well as in
   the search  - some elements are only used in the search */

char *uncvt ();
