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
SquareState board_get_square(const Board * const b, const Square sq)
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
int board_count_pieces(const Board * const b, const SquareState color)
{
  return popcount(board_get_color(b, color));
}

/**
 * @brief Returns the disk difference between the player and her opponent.
 *
 * Parameter b must be not null.
 * Parameter p must be a value belonging to the Player enum.
 *
 * @param b a pointer to the board structure
 * @param p the player for whom the difference is computed
 * @return  the disc count difference
 */
int board_count_difference(const Board * const b, const Player p)
{
  assert(b);
  assert(p == BLACK_PLAYER || p == WHITE_PLAYER);

  int pcount, ocount;
  Player o;

  o = player_opponent(p);
  pcount = board_count_pieces(b, player_color(p));
  ocount = board_count_pieces(b, player_color(o));

  return pcount - ocount;
}

/**
 * @brief Returns 1 if the move, done by the specified player, is legal,
 * otherwise 0.
 *
 * TO BE COMPLETED!
 *
 * @param b    a pointer to the board structure
 * @param move the square where to put the new disk
 * @param p    the player moving
 * @return     1 if the move is legal, otherwise 0
 */
int board_is_move_legal(const Board * const b,
                        const Square move,
                        const Player p)
{
  assert(b);
  assert(move >= A1 && move <= H8);
  assert(p == BLACK_PLAYER || p == WHITE_PLAYER);

  return 0;
}

/**
 * @brief Returns the empty set of squares in the board.
 *
 * @param b a pointer to the board structure
 * @return  the empy set of squares
 */
SquareSet board_empties(const Board * const b)
{
  return ~(b->blacks | b->whites);
}

SquareSet board_blacks(const Board *b)
{
  return b->blacks;
}

SquareSet board_whites(const Board * const b)
{
  return b->whites;
}

SquareSet board_get_color(const Board * const b, const SquareState color)
{
  assert(b);

  SquareSet squares;

  switch (color) {
  case EMPTY_SQUARE:
    squares = board_empties(b);
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

  return squares;

}

/**
 * Returns a new long value by shifting the {@code squares} parameter by one position
 * on the board.
 *
 * @param squares the squares set on the bitboard
 * @return        the shifted squares
 *
public long shiftBitboard(final long squares) {
  switch (this) {
  case NW: return (squares >>> SHIFT_9) & ALL_SQUARES_EXCEPT_COLUMN_H;
  case N:  return (squares >>> SHIFT_8);
  case NE: return (squares >>> SHIFT_7) & ALL_SQUARES_EXCEPT_COLUMN_A;
  case W:  return (squares >>> SHIFT_1) & ALL_SQUARES_EXCEPT_COLUMN_H;
  case E:  return (squares <<  SHIFT_1) & ALL_SQUARES_EXCEPT_COLUMN_A;
  case SW: return (squares <<  SHIFT_7) & ALL_SQUARES_EXCEPT_COLUMN_H;
  case S:  return (squares <<  SHIFT_8);
  case SE: return (squares <<  SHIFT_9) & ALL_SQUARES_EXCEPT_COLUMN_A;
  default: throw new IllegalArgumentException("Undefined value for direction. dir=" + this);
  }
}
*/
