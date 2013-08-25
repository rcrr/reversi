/* Copyright 1986-1994 by Kai-Fu Lee, Sanjoy Mahajan, and Joe Keane.

   This file is part of Bill and is free software.  You can
   redistribute it and/or modify it under the terms of the GNU General
   Public License as published by the Free Software Foundation; either
   version 2 of the License (see the file COPYING), or (at your
   option) any later version.
 */

/*
 * Opening book for Bill -
 *
 * This version reads in a binary opening book (use bookconv to convert ascii
 * opening book to binary opening book), and does NOT create a tree structure.
 * It just scans the CHARs in the character buffer until the move is found.
 */

#include <stdio.h>
#include <files.h>
#include <general.h>
#include <node.h>
#include <check.h>
#define EOL ((signed char)-1)

extern int      Turn_Num;
extern Node	Game_Record[MAX_MOVES];
extern general_info_t General_Info[2];
extern signed char Black_Opening[], White_Opening[];
void internal_error ()
#ifdef __GNUC__
__attribute__ ((noreturn))
#endif
;

/* setup_opening reads in the opening book into a character buffer,
   and sets has_opening to TRUE, meaning the next move might be found in the opening book.
   This global variable is set to FALSE when a move is not found. */

void setup_opening (color)
     color_t color;
{
  General_Info[color].book_ptr = General_Info[color].book =
    color == BLACK ? Black_Opening : White_Opening;
  General_Info[color].has_opening = TRUE;
}


/* opening_search searches for an opening move for color.
   It is called by the computer player.
   The last move is extracted from Game_Record, and it is used as the key to search for in the opening book. */

move_t opening_search (color)
     color_t color;
{
  move_t rotate ();
  register int	  level = 0;
  register signed char *ptr = General_Info[color].book_ptr;
  register move_t opponent_move = Turn_Num <= 0 ? 0 : rotate (Game_Record[Turn_Num - 1].move);
  register move_t move;

#if 0
  if (ptr == NULL)
  {
      setup_opening (color);
      ptr = General_Info[color].book_ptr;
  }
#endif
  for (;;)
    {
      while (*ptr == EOL)
	{
	  if (--level < 0)
	    {
	      move = NO_MOVE;
	      goto last_move;
	    }
	  ptr++;
	}
      if (level <= 0 && *ptr == opponent_move)
	break;
      level++;
      ptr += 2;
    }
  move = Turn_Num <= 0 ? 44 : rotate (ptr[1]); /* hard-wired E6 */
  if (ptr[2] != EOL)
    General_Info[color].book_ptr = ptr + 2;
  else
    {
    last_move:
      /* last move in book */
      General_Info[color].has_opening = FALSE;
      General_Info[color].book = NULL;
    }
  return move;
}


/* opening_takeback takes back a move for Opposite(color); Game_Record[Turn_Num - 3].book_position contains where we should start searching after the take_back. */

void opening_takeback (color)
     color_t color;
{
  int current_turn = Turn_Num;

  if (General_Info[color].book == NULL) /* have freed book already */
    setup_opening (color);
  for (Turn_Num = color; Turn_Num < current_turn-2; Turn_Num += 2)
    opening_search (Opposite(color));
}


/* rotate rotates a move depending on the first move Black made, because all 4 moves are symmetrical. */

move_t rotate (move)
     move_t move;
{
  switch ((*Game_Record).move)
    {
      static signed char rotate_arr[] =
	{
	  000, 010, 020, 030, 040, 050, 060, 070,
	  001, 011, 021, 031, 041, 051, 061, 071,
	  002, 012, 022, 032, 042, 052, 062, 072,
	  003, 013, 023, 033, 043, 053, 063, 073,
	  004, 014, 024, 034, 044, 054, 064, 074,
	  005, 015, 025, 035, 045, 055, 065, 075,
	  006, 016, 026, 036, 046, 056, 066, 076,
	  007, 017, 027, 037, 047, 057, 067, 077,
	};

    case 023:
      return MAX_PIECES - 1 - move;

    case 032:
      return MAX_PIECES - 1 - rotate_arr[move];

    case 045:
      return rotate_arr[move];

    case 054:
      return move;

    default:
      internal_error ("illegal first move in rotate");
      /*NOTREACHED*/
    }
}
