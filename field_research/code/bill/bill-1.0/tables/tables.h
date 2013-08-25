/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* gives names of various tables */

#define DIAG3 0
#define DIAG4 1
#define DIAG5 2
#define DIAG6 3
#define DIAG7 4
#define DIAG8 5
#define TB1   6
#define TB2   7
#define TB3   8
#define NUM_TABLES 9

#define UNSTABLE 0		/* Indices into the static */
#define NOT_UNSTABLE 1		/* evaluation array */
#define TWO_MOVES 2
#define ONE_MOVE 3
#define NO_MOVES 4
#define STABLE 5
#define NUM_TYPES 6
#define MAX_PIECES 8
#define pieces(i) ((i) > 4 ? 8 : (i)+3)
