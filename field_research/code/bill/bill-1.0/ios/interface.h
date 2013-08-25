/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

typedef struct
{
  int to;			/* fd to feed stuff to it's stdin */
  int from;			/* fd to get stuff from it's stdout */
  int pid;			/* who knows, we might need it later */
  int died;			/* dead, alive */
} Child;

#define OTHELLO_PROGRAM "/home/sanjoy/games/othello/bill/Bill --ios"
#define MOVE_TAG "Bill moves to"
#define PASS_TAG "Bill has no moves"
#define VALUE_TAG "Value:"
