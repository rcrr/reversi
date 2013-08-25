/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdlib.h>

#include <node.h>
#include <hash.h>
#include <check.h>

extern Node  Game_Record[MAX_MOVES];
extern struct hash_entry *Hash_Table[HASH_SIZE];
extern int  Turn_Num, Discs;

extern int   Hash_Count;

/* hash_entry looks up a position in the hash table.  It also scans
   everything in the entry, and delete nodes that are out of date.  key
   is the 32-bit key.  index is the least significant 15-bits of the key.
   It's used to index into the array.  timestamp is the depth the board
   position.  color is the player to move.  */

struct hash_entry *look_up (key, timestamp, color)
     int key, timestamp;
     color_t color;
{
  struct hash_entry *ret = NULL;
  unsigned short index = key & 0x7fff;
  register struct hash_entry *prev = NULL, *ptr = Hash_Table[index];

  while (ptr != NULL)
    {
      CHECK(ent_ok (ptr), "bogus entry");
      if (ptr->key == key && ptr->timestamp == timestamp && ptr->color == color)
	{
	  ret = ptr;
	  prev = ptr;
	  ptr = ptr->next;
	}
      else if (ptr->timestamp >= Discs)
	{
	  prev = ptr;
	  ptr = ptr->next;
	}
      else if (prev == NULL)
      {
	Hash_Table[index] = ptr->next;
	Hash_Count--;
	free ((char *)ptr);
	ptr = Hash_Table[index];
      }
      else
	{
	  prev->next = ptr->next;
	  Hash_Count--;
	  free ((char *)ptr);
	  ptr = prev->next;
	}
    }

  return ret;
}


void check_all ()
{
  register int i;
  register struct hash_entry *ent;

  for (i = 0; i < HASH_SIZE; i++)
    for (ent = Hash_Table[i]; ent != NULL; ent = ent->next)
      CHECK(ent_ok (ent), "bad entry");
}	  

int ent_ok (ent)
     struct hash_entry *ent;
{
  return ent->best_move >= NO_MOVE && ent->best_move < MAX_PIECES ||
    ent->timestamp >= 4 && ent->timestamp < MAX_PIECES &&
      ent->color >= BLACK && ent->color <= EMPTY &&
	ent->key != 0;
}


/* put_in puts a position into the hash slot.  Arguments have the
   same meaning as in look_up.  (best_move is the best move for
   color at this position)  The position is always put in the
   beginning of the hash entry. */

void put_in (key, timestamp, color, move)
     int key, timestamp;
     move_t move;
     color_t color;
{
  struct hash_entry *find;
  register int index;

  if (!LEGAL(move))
    return;

  if (key == 0 && timestamp == 0 || move < -1 || move > 63 ||
      color < 0 || color > 2 || timestamp < 0 || timestamp > 64)
    internal_error("bogus args to put_in");
  Hash_Count++;
  if ((find = look_up (key, timestamp, color)) != NULL)
    {
      CHECK(ent_ok (find), "Bogus return from look_up");
      find->best_move = move;
      return;
    }

  find = (struct hash_entry *) malloc (sizeof (struct hash_entry));
  find->key = key;
  find->timestamp = timestamp;
  find->color = color;
  find->best_move = move;

  index = key & 0x7fff;
  if (Hash_Table[index] == NULL)
    {
      Hash_Table[index] = find;
      Hash_Table[index]->next = NULL;
    }
  else
    {
      find->next = Hash_Table[index]->next;
      Hash_Table[index]->next = find;
    }
}
