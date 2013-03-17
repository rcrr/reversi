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
#include <stdio.h>
#include <stdlib.h>

#include "board.h"
#include "bit_works.h"

/**
 * @brief Returns the square state value representing the player's color.
 *
 * Parameter p must be a value belonging to the Player enum, the invariant
 * is guarded by an assertion.
 *
 * @param p the player
 * @return  the square state of the player
 */
SquareState player_color(const Player p)
{
  assert(p == BLACK_PLAYER || p == WHITE_PLAYER);
  return (p == BLACK_PLAYER) ? BLACK_SQUARE : WHITE_SQUARE;
}

/**
 * @brief Returns the player's opponent.
 *
 * Parameter p must be a value belonging to the Player enum, the invariant
 * is guarded by an assertion.
 *
 * @param p the player
 * @return  the player's opponent
 */
Player player_opponent(const Player p)
{
  assert(p == BLACK_PLAYER || p == WHITE_PLAYER);
  return (p == BLACK_PLAYER) ? WHITE_PLAYER : BLACK_PLAYER;
}

/**
 * @brief Returns the player's description.
 *
 * Parameter p must be a value belonging to the Player enum, the invariant
 * is guarded by an assertion.
 *
 * @param p the player
 * @return  the player's description
 */
char *player_description(const Player p)
{
  assert(p == BLACK_PLAYER || p == WHITE_PLAYER);
  return (p == BLACK_PLAYER) ? "The Black player" : "The White player";
}

/**
 * @brief Board structure constructor.
 *
 * Parameters b and w cannot have common square set, the invariant
 * is guarded by an assertion.
 *
 * @param b the set of black squares
 * @param w the set of white squares
 * @return  a pointer to a new board structure
 */
Board *new_board(const SquareSet b, const SquareSet w)
{
  assert((w & b) == 0ULL);

  Board *board;
  static const size_t size_of_board = sizeof(Board);

  board = (Board*) malloc(size_of_board);
  assert(board);

  board->blacks = b;
  board->whites = w;

  return board;
}

/**
 * @brief Board structure destructor.
 *
 * Parameter b cannot be null.
 *
 * @param b the pointer to be deallocated
 * @return  always the NULL pointer
 */
Board *delete_board(Board *b)
{
  assert(b);

  free(b);
  b = NULL;

  return b;
}

/**
 * @brief Returns the SquareState value for the given board's square.
 *
 * Parameter b must be not null.
 * Parameter sq must belongs to the Square enum.
 *
 * @param b  a pointer to the board structure
 * @param sq the square to query for
 * @return   the color of the given square
 */
SquareState board_get_square(const Board *b, const Square sq)
{
  assert(b);
  assert(sq >= A1 && sq <= H8);

  SquareSet bitsquare;

  bitsquare = 1ULL << sq;
  if (bitsquare & b->blacks)
    return BLACK_SQUARE;
  else if (bitsquare & b->whites)
    return WHITE_SQUARE;
  else
    return EMPTY_SQUARE;
}

/**
 * @brief Returns the disk count for the color.
 *
 * Parameter b must be not null.
 * Parameter color must belongs to the SquareState enum.
 *
 * @param b     a pointer to the board structure
 * @param color the square color
 * @return      the piece count for the given color
 */
int board_count_pieces(const Board *b, const SquareState color)
{
  assert(b);

  SquareSet squares;

  switch (color) {
  case EMPTY_SQUARE:
    squares = ~(b->blacks | b->whites);
    break;
  case BLACK_SQUARE:
    squares = b->blacks;
    break;
  case WHITE_SQUARE:
    squares = b->whites;
    break;
  default:
    abort();
  }

  return popcount(squares);
}
