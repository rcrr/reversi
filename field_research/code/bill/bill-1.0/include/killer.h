/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* A killer table entry */

#ifndef KILLER_H

#include <general.h>

struct killer_table
{
  struct double_link *resp_addr[MAX_PIECES], *order;
};

/* Doubly linked list used in each killer entry */

struct double_link
{
  move_t response;
  struct double_link *prev, *next;
};

#define KILLER_H
#endif
