/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * contains the mapping of bit positions in the legal string to square
 * numbers. Has important property that the 3rd  group of 16 numbers
 * corresponds with the 4th group.  In other words, only one move weighing
 * table is required. 
 *
 * For example, sq_map[32] (=19) is of the same type as sq_map[48] (=54).  In
 * this case, both are x-squares.  This must always be true for the move
 * weighing to work. IN OTHER WORDS, DON'T MESS WITH THIS ARRAY!
 *
 * Also, the positions of the x-squares are hardwired into eval.c!
 */

#define MAX_PIECES 64

int             Sq_Map[MAX_PIECES] =
{
     0,  1,  2,  3,  4,  5,  6,  7,	/* north, edge */
     7, 15, 23, 31, 39, 47, 55, 63,	/* east, edge */
    63, 62, 61, 60, 59, 58, 57, 56,	/* south, edge */
    56, 48, 40, 32, 24, 16,  8,  0,	/* west, edge */
     9, 10, 11, 12, 13, 14, 17, 18,	/* till, c3 */
    19, 20, 21, 22, 25, 26, 29, 30,	/* till, g4 */
    54, 53, 52, 51, 50, 49, 46, 45,	/* new: till h7 */
    44, 43, 42, 41, 38, 37, 34, 33,	/* new: till e6 */
};
