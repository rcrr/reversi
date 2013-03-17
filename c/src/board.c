/**
 * @file
 *
 * @brief Reversi C - Board implementation
 */

/**
 * @cond
 *
 * board.c
 *
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 *
 * Copyright (c) 2013 Roberto Corradini. All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 *
 * @endcond
 */

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "board.h"

extern int errno;

/**
 * @brief Returns the square state value representing the player's color.
 *
 * Parameter p must be a value belonging to the Player enum, if not the
 * return value is -1 and the errno is set to EINVAL.
 *
 * @param p the player
 * @return  the square state of the player
 */
SquareState color(const Player p)
{
    switch (p) {
    case BLACK_PLAYER: return BLACK_SQUARE;
    case WHITE_PLAYER: return WHITE_SQUARE;
    default:
      errno = EINVAL;
      fprintf(stderr, "Function color in board.c: argument Player p = %d is invalid.\n", p);
      return EXIT_FAILURE;
    }
}

/**
 * @brief Returns the player's opponent.
 *
 * Parameter p must be a value belonging to the Player enum, if not the
 * BLACK_PLAYER is always returned.
 *
 * @param p the player
 * @return  the player's opponent
 */
Player opponent(const Player p)
{
  return (p == BLACK_PLAYER) ? WHITE_PLAYER : BLACK_PLAYER;
}

/**
 * @brief Returns the player's description.
 *
 * Parameter p must be a value belonging to the Player enum, if not the
 * return value is -1 and the errno is set to EINVAL.
 *
 * @param p the player
 * @return  the player's description
 */
char *description(const Player p)
{
    switch (p) {
    case BLACK_PLAYER: return "The Black player";
    case WHITE_PLAYER: return "The White player";
    default:
      errno = EINVAL;
      fprintf(stderr, "Function description in board.c: argument Player p = %d is invalid.\n", p);
      return "Invalid player as argument.";
    }
}

Board *new_board(const SquareSet b, const SquareSet w)
{
  Board *board;
  static const size_t size_of_board = sizeof(Board);

  /*
  if (w & b) {
    errno = EINVAL;
    fprintf(stderr, "Function new_board in board.c: argument are invalid.\n");
    return NULL;
  }
  */
  board = (Board*) malloc(size_of_board);
  assert(board);
  board->blacks = b;
  board->whites = w;
  return board;
}

Board *delete_board(Board *b)
{
  assert(b);
  free(b);
  b = NULL;
  return b;
}

SquareState get_square(const Board *b, const Square sq)
{
  SquareSet bitsquare = 1ULL << sq;
  if (bitsquare & b->blacks)
    return BLACK_SQUARE;
  else if (bitsquare & b->whites)
    return WHITE_SQUARE;
  else
    return EMPTY_SQUARE;
}
