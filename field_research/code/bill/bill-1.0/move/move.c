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
#include <prune.h>
#include <check.h>
#include <general.h>

move_t next_move ();
void save_move (move_t move);
move_t find_best ();
void copy_board ();
void simple_do_flips ();
void do_flips ();
void find_legals ();

extern general_info_t General_Info[2];
extern struct killer_table Killer_Table[2][MAX_PIECES+1];
extern int      Dirs[1008],		/* directions to check for legality */
                Dir_Start[65],		/* indices into Dirs */
                Nc_Input[2],
                Next_Arr[NEXT_MAX],	/* uses displacements now */
                Next_St[MAX_PIECES + 1],
                C_Corner[9],
                C_B[9],
                C_A[9],
                Use_Level_Above[2],
                X_Square[MAX_PIECES],
                Corner[5],
                C1[5],
                C2[5],
                Discs,
                Illegal_Hash,
                Turn_Num,
                Random[2][MAX_PIECES],
                Random_Xor[MAX_PIECES],
                Node_Count;
extern Node	Game_Record[MAX_MOVES];


/* make_move updates the board by placing a piece of the given color on int,
   flipping any pieces that need to be, recomputing the legal moves, and updating the empty int "list".
   Should remove all the procedure calls later for speed.  It doesn't return a "list" of flipped pieces. */

void make_move (prev_node, node, move)
     register Node *prev_node, *node;
     move_t move;
{
  color_t color = prev_node->color;

  save_move (move);
  node->color = Opposite(color);
  prev_node->move = move;
  copy_board (prev_node->board, node->board);
#if YOU_LIKE_TO_WASTE_TIME
  check_all ();
#endif

  Turn_Num++;				/* stupid!! */
  if (!LEGAL(move))
    {
      node->bl_pieces = prev_node->bl_pieces;
      node->wh_pieces = prev_node->wh_pieces;
    }
  else
    {					/* if an actual move */
      register int *stop, *ptr;
      simple_do_flips (prev_node, node, move);

      stop = &Next_Arr[Next_St[move + 1]]; /* where to stop at */
      Discs++;				/* another disc is placed on board */
      for (ptr = &Next_Arr[Next_St[move]]; ptr < stop; ++ptr)
	if (node->board[*ptr] == EMPTY)
	  node->board[*ptr] = ESSQ;
    }

  /* Always recompute legal moves (color changes even if board doesn't). */
  find_legals (node, prev_node->move, &node->legals);
  SS_make_move (move);			/* update SS's cached trees */
  prev_node->move = move;
}


/* abmake_move makes a move for the searching and deepening procedures,
   updating the hash key if needed, and doing stuff for the killer table/next move (if needed). */

void abmake_move (prev_node, node, non_leaf)
     register ANode *prev_node,		/* game-record pointer making move from */
		    *node;		/* game_record pointer making move to */
     int non_leaf;			/* not last level of tree? */
{
  square_t       *board = node->board;
  move_t	  move, best;
  color_t	  color = prev_node->color, opp_color = Opposite(color);

  node->color = opp_color;
  node->move = NO_MOVE;
  copy_board (prev_node->board, node->board);

  if (!LEGAL(move = prev_node->move))
    {
      node->key = prev_node->key;
      node->bl_pieces = prev_node->bl_pieces;
      node->wh_pieces = prev_node->wh_pieces;
      node->next_legal_try = Killer_Table[opp_color][MAX_PIECES].order;
    }
  else
    {
      register int *i1 = &Next_Arr[Next_St[move]], *stop = &Next_Arr[Next_St[move + 1]]; /* where to stop at */

      node->to_here = move;
      node->next_legal_try = Killer_Table[opp_color][move].order;
      (non_leaf ? do_flips : simple_do_flips) (prev_node, node, move); /* see WARNING below */

      for (; i1 < stop; i1++)
	if (board[*i1] == EMPTY)
	  board[*i1] = ESSQ;
    }

  /* WARNING: cannot replace `non_leaf' with `node->more > 0' because deepen ()
     and steal () call this procedure without worrying about node->more.
     Only ab () [but not ab2 ()] worries about more. */
  node->next_move = node->already_tried = !non_leaf ? NO_MOVE : LEGAL(best = find_best (node)) || LEGAL(best = next_move (node, NO_MOVE)) ? best : REALLY_NO_MOVE;

  /* Next to last level, so have to get features.  There is one problem with this test:
     it sometimes does a find_features when it doesn't need to, because abmake_move is
     called by deepen() and steal() with garbage node->more's, which could be 1. */
  if (node->more == 1 && General_Info[COLOR(Turn_Num)].eval_stuff.level_above)
    (*General_Info[COLOR(Turn_Num)].eval_stuff.level_above) (node->board, node->color, node->features);
}

/* absetup_move is like abmake_move but it doesn't make a move; it just returns the best move. */

void absetup_move (node, move)
     register ANode *node;
     move_t	     move;
{
  color_t color = node->color;
  move_t best;

  node->move = NO_MOVE;
  if (!LEGAL(move))
    move = MAX_PIECES;
  node->next_legal_try = Killer_Table[color][move].order;
  node->next_move = node->already_tried =
    LEGAL(best = find_best (node)) ||
      LEGAL(best = next_move (node, NO_MOVE)) ? best : REALLY_NO_MOVE;

/* This is just to be safe, in case of fail highs in zero or steal. */
  if (General_Info[COLOR(Turn_Num)].eval_stuff.level_above)
    (*General_Info[COLOR(Turn_Num)].eval_stuff.level_above) (node->board, node->color, node->features);
}


/* simple_do_flips flips the discs that need to be flipped, and updates the counts. */

void simple_do_flips (prev_node, node, move)
     register Node  *prev_node, *node;
     move_t move;
{
  register int *i1 = &Dirs[Dir_Start[move]], *end = &Dirs[Dir_Start[move + 1]];
  color_t color = prev_node->color, opp_color = Opposite(color);
  int flips = 0;

  while (i1 < end)
    {
      register square_t *sec, *out, *p;
      register int inc;

      if (*(sec = &node->board[*i1++]) != opp_color)
	{
	  i1 += 2;
	  continue;
	}

      inc = *i1++;
      out = &node->board[*i1++];

      for (p = sec + inc; p != out && Occupied(*p); p += inc)
	if (*p == color)		/* a sandwich!  Do flips */
	  {
	    do
	      {
		flips++;
		*(p -= inc) = color;
	      }
	    while (p != sec);
	    break;
	  }
    }

  if (color == BLACK)
    {
      node->bl_pieces = prev_node->bl_pieces + flips + 1;
      node->wh_pieces = prev_node->wh_pieces - flips;
    }
  else
    {
      node->bl_pieces = prev_node->bl_pieces - flips;
      node->wh_pieces = prev_node->wh_pieces + flips + 1;
    }
  node->board[move] = color;	/* put guy in */
}


/* do_flips flips the discs that need to be flipped, and updates the counts and hash index. */

void do_flips (prev_node, node, move)
     register ANode *prev_node, *node;
     move_t move;
{
  register int *i1 = &Dirs[Dir_Start[move]], *end = &Dirs[Dir_Start[move + 1]];
  color_t color = prev_node->color, opp_color = Opposite(color);
  int flips = 0;

  node->key = prev_node->key ^ Random[color][move]; /* ??? */

  while (i1 < end)
    {
      register square_t *sec, *out, *p;
      register int inc;

      if (*(sec = &node->board[*i1++]) != opp_color)
	{
	  i1 += 2;
	  continue;
	}

      inc = *i1++;
      out = &node->board[*i1++];

      for (p = sec + inc; p != out && Occupied(*p); p += inc)
	if (*p == color)		/* a sandwich!  Do flips */
	  {
	    do
	      {
		flips++;
		*(p -= inc) = color;
		node->key ^= Random_Xor[p - node->board];
	      }
	    while (p != sec);
	    break;
	  }
    }

  if (color == BLACK)
    {
      node->bl_pieces = prev_node->bl_pieces + flips + 1;
      node->wh_pieces = prev_node->wh_pieces - flips;
    }
  else
    {
      node->bl_pieces = prev_node->bl_pieces - flips;
      node->wh_pieces = prev_node->wh_pieces + flips + 1;
    }
  node->board[move] = color;	/* put guy in */
}


/* find_legals returns the legal moves for a position. */

void find_legals (node, prev_move, legals)
     Node *node;
     move_t	     prev_move;
     legal_t	     legals;
{
  square_t *board= node->board;
  struct double_link *killer;
  color_t color = node->color, opp_color = Opposite(color);
  int num_legals = 0;

  if (!LEGAL(prev_move))
    prev_move = MAX_PIECES;		/* move == MAX_PIECES is NO_MOVE in killer_table */
  for (killer = Killer_Table[color][prev_move].order; killer != NULL; killer = killer->next)
    {
      move_t try_move = killer->response;
      register int *i1, *end;

      if (board[try_move] != ESSQ)
	continue;

      i1 = &Dirs[Dir_Start[try_move]];
      end = &Dirs[Dir_Start[try_move + 1]];
      while (i1 < end)
	{
	  register square_t *sec, *out, *p;
	  register int inc;

	  if (*(sec = &board[*i1++]) != opp_color)
	    {
	      i1 += 2;
	      continue;
	    }

	  inc = *i1++;
	  out = &board[*i1++];

	  for (p = sec + inc; p != out && Occupied(*p); p += inc)
	    if (*p == color)
	      {
		legals->data[num_legals++] = try_move;
		goto legal;
	      }
	}

    legal: ;
    }
  legals->max = num_legals;
}


/* next_move returns the next "best" move for the board passed.  The order
   of examination is according to the killer table.  Start from
   node->next_legal_try (change it before return to the next move after the
   move found).  Skip already_tried since it's the one found in the hash
   table. */

move_t next_move (node, already_tried)
     register ANode *node;
     move_t	     already_tried;
{
  struct double_link *killer;
  color_t color = node->color, opp_color = Opposite(color);

  for (killer = node->next_legal_try; killer != NULL; killer = killer->next)
    {
      move_t try_move = killer->response;
      register int *i1, *end;

#ifdef PRUNE_XC
      if ((xnum = X_Square[try_move]) &&
	  (Empty (node->board[Corner[xnum]])) &&
	  ((Discs <= ALWAYS_PRUNE_X) ||
	   ((Discs <= CONDITIONAL_PRUNE_X) &&
	    (Occupied (node->board[C1[xnum]])) &&
	    (Occupied (node->board[C2[xnum]])))))
	continue;
#endif

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


/* find_best looks up in the hash table, and return the best move if found,
   or NO_MOVE if either not found, or the move found is illegal. */

move_t find_best (node)
     register ANode *node;
{
     square_t *board = node->board;
     color_t color = node->color;
     struct hash_entry *entry;
     move_t	  try_move;

  if ((entry = look_up (node->key, num_discs(node), color)) == NULL)
    return NO_MOVE;

  if ((try_move = entry->best_move) >= 0 && try_move < MAX_PIECES)
    {
#ifdef PRUNE_XC
      if ((xnum = X_Square[try_move]) &&
	  Empty(board[Corner[xnum]]) &&
	  (Discs <= ALWAYS_PRUNE_X ||
	   Discs <= CONDITIONAL_PRUNE_X &&
	    Occupied(board[C1[xnum]]) &&
	    Occupied(board[C2[xnum]])))
	return NO_MOVE;
#endif

      if (board[try_move] == ESSQ)
	{
	  color_t opp_color = Opposite(color);
	  register int *i1 = &Dirs[Dir_Start[try_move]], *end = &Dirs[Dir_Start[try_move + 1]];

	  while (i1 < end)
	    {
	      register square_t *sec, *out, *p;
              register int    inc;

	      if (*(sec = &board[*i1++]) != opp_color)
		{
		  i1 += 2;
		  continue;
		}

	      inc = *i1++;
	      out = &board[*i1++];
	      for (p = sec + inc; p != out && Occupied(*p); p += inc)
		if (*p == color)
		  {
		    Node_Count++;
		    return try_move;
		  }
	    }
	}
    }

  Illegal_Hash++;
  return NO_MOVE;
}
