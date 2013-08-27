/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdio.h>
#include <node.h>
#include <hash.h>

extern int	Turn_Num,
                Discs,
                Hash_Count,
                Illegal_Hash,
                Steal_Time,
                Random[2][MAX_PIECES];
extern struct killer_table Killer_Table[2][MAX_PIECES+1];
extern Node    Game_Record[MAX_MOVES];

extern char init_killer[65][60];


#if MY_MALLOC
/* Buffers malloc calls */

#define BUF_SIZE (10*1024-4)	/* size of malloc buffer */

char *my_malloc(size)
     int size;
{
  static char *buffer = NULL, *buf_ptr;

  if (buffer == NULL || buf_ptr + size > buffer + BUF_SIZE)	/* used up old buffer */
    buffer = buf_ptr = malloc (BUF_SIZE);      /* get new buffer */
  buf_ptr += size;		/* allocate size bytes from buffer */
  return buf_ptr-size;		/* return starting address */
}
#endif


void setup_killer ()
{
  move_t move;
  color_t color;

  for (color = BLACK; color <= WHITE; color++)
    /* move == MAX_PIECES is NO_MOVE */
    for (move = 0; move <= MAX_PIECES; move++)
      {
	register struct double_link *node, *prev_node;
	register char   *order, *stop;

	switch (move)
	  {
	    register move_t response;

	  case 27:
	  case 28:
	  case 35:
	  case 36:
	    Killer_Table[color][move].order = NULL;
	    for (response = 0; response < MAX_PIECES; response++)
	      Killer_Table[color][move].resp_addr[response] = NULL;
	    continue;
	  }

	prev_node = (struct double_link *) malloc (sizeof (struct double_link));
	if (prev_node == NULL)
	    abort ();
	prev_node->prev = NULL;
	prev_node->response = *(order = &init_killer[move][0]);
	Killer_Table[color][move].resp_addr[*order] = Killer_Table[color][move].order = prev_node;

	stop = &init_killer[move + 1][0];
	if (move < MAX_PIECES)
	  stop--;			/* NO_MOVE list has 64 more nodes */
	for (order++; order < stop; order++)
	  {
	    register move_t response = *order;

	    switch (response)
	      {
	      case 27:
	      case 28:
	      case 35:
	      case 36:
		Killer_Table[color][move].resp_addr[response] = NULL;
		continue;
	      }

	    node = (struct double_link *) malloc (sizeof (struct double_link));
	    if (node == NULL)
		abort ();
	    node->response = response;
	    node->prev = prev_node;
	    Killer_Table[color][move].resp_addr[response] = prev_node->next = node;
	    prev_node = node;
	  }
	node->next = NULL;
      }
}
