/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/* stuff needed by the hash table and things that use it */

#ifndef HASH_H

#define HASH_SIZE	65536
#define HASHBITS	16
#define ALL_ONES	-1

/* Entry in the hash table */

struct hash_entry
{
  int   key,			/* The position hashed */
        best_move;		/* Best move from here */
  eval_t value;
  short   timestamp,		/* The number of discs at this position */
          color;		/* The player to move from here */
  struct hash_entry *next;
};

struct hash_entry *look_up ();
void put_in ();

#define HASH_H
#endif
