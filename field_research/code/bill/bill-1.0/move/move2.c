/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

#include <stdio.h>

#include <node.h>
#include <prune.h>
#include <killer.h>
#include <hash.h>

extern struct killer_table Killer_Table[2][MAX_PIECES+1];
extern          Dirs[1008],		/* directions to check for legality */
                Dir_Start[65],		/* indices into Dirs */
                Next_Arr[NEXT_MAX],	/* uses displacements now */
                Next_St[MAX_PIECES + 1],
                C_Neighbor1[9],
                C_Neighbor2[9],
                C_Square[MAX_PIECES],
                X_Square[MAX_PIECES],
                Corner[5],
                C1[5],
                C2[5],
                Num_Legals[MAX_MOVES],
                Discs,
                Illegal_Hash,
                Turn_Num,
                Random[2][MAX_PIECES],
                Random_Xor[MAX_PIECES],
                Node_Count;

extern Node	Game_Record[MAX_MOVES];


void abmake_move2 (prev_node, node, move)
     register ANode *prev_node, *node;
     move_t	     move;
{
  color_t	  color = prev_node->color, opp_color = Opposite(color);
  move_t	  best;
  square_t       *board = node->board;

  node->color = opp_color;
  node->move = NO_MOVE;
  copy_board (prev_node->board, node->board);

  if (move < 0)
    {
      node->bl_pieces = prev_node->bl_pieces;
      node->wh_pieces = prev_node->wh_pieces;
      node->key = prev_node->key;
      node->next_legal_try =
	Killer_Table[opp_color][Game_Record[0].move].order;
    }
  else
    {
      register int *i1, *stop;

      node->to_here = move;
      node->board[move] = color;
      node->next_legal_try = Killer_Table[opp_color][move].order;

      do_flips (prev_node, node, move);

      i1 = &Next_Arr[Next_St[move]];
      stop = &Next_Arr[Next_St[move + 1]];	/* where to stop at */
      for (; i1 < stop; i1++)
	if (board[*i1] == EMPTY)
	  board[*i1] = ESSQ;
    }

  node->next_move = node->already_tried =
    (best = find_best2 (node)) < 0 && (best = next_move2 (node, NO_MOVE)) < 0
      ? REALLY_NO_MOVE : best;
}


/* next_move2 returns the next "best" move for the board passed.  The order of examination is according to the killer table.  Start from node->next_legal_try (change it before return to the next move after the move found).  Skip already_tried since it's the one found in the hash table. */

move_t next_move2 (node, already_tried)
     register ANode *node;
     move_t	     already_tried;
{
  struct double_link *killer;
  color_t color = node->color, opp_color = Opposite(color);

  for (killer = node->next_legal_try; killer != NULL; killer = killer->next)
    {
      move_t try_move = killer->response;
      register int *i1, *end;

      if (try_move == already_tried || node->board[try_move] != ESSQ)
	continue;

      i1 = &Dirs[Dir_Start[try_move]];
      end = &Dirs[Dir_Start[try_move + 1]];
      while (i1 < end)
	{
	  register square_t *sec, *out, *p;
	  int inc;

	  if (*(sec = &node->board[*i1++]) != opp_color)
	    {
	      i1 += 2;
	      continue;
	    }

	  inc = *i1++;
	  out = &node->board[*i1++];
	  for (p = sec + inc; p != out && Occupied(*p); p += inc)
	    if (*p == color)
	      {
		Node_Count++;
		node->next_legal_try = killer->next;
		return try_move;
	      }
	}
    }

  node->next_legal_try = NULL;
  return NO_MOVE;
}


/* find_best2 looks up in the hash table, and return the best move if found, or NO_MOVE if either not found, or the move found is illegal. */

move_t find_best2 (node)
     register ANode *node;
{
  struct hash_entry *entry;
  move_t	  try_move;

  if ((entry = look_up (node->key, num_discs(node), node->color)) == NULL)
    return NO_MOVE;

  try_move = entry->best_move;
  if (try_move / MAX_PIECES == 0 && node->board[try_move] == ESSQ)
    {
      register int *i1 = &Dirs[Dir_Start[try_move]], *end = &Dirs[Dir_Start[try_move + 1]];
      color_t color = node->color, opp_color = Opposite(color);

      while (i1 < end)
	{
	  square_t *sec;
	  register square_t *out, *p;
	  register int inc;

	  if (*(sec = &node->board[*i1++]) != opp_color)
	    {
	      i1 += 2;
	      continue;
	    }

	  inc = *i1++;
	  out = &node->board[*i1++];
	  for (p = sec + inc; p != out && Occupied(*p); p += inc)
	    if (*p == color)
	      {
		Node_Count++;
		return try_move;
	      }
	}
    }

  Illegal_Hash++;
  return NO_MOVE;
}
