/**
 * @file
 *
 * @brief Board module implementation.
 * @details This module defines functions for the #Player, #SquareState,
 * #Square, #SquareSet, #Board, #GamePosition, #Direction entities.
 *
 * @par board.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013 Roberto Corradini. All rights reserved.
 *
 * @par License
 * <tt>
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 * \n
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * \n
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 * </tt>
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "board.h"
#include "bit_works.h"

/* A square set being all set with the exception of column A. */
static const SquareSet ALL_SQUARES_EXCEPT_COLUMN_A = 0xFEFEFEFEFEFEFEFEULL;

/* A square set being all set with the exception of column H. */
static const SquareSet ALL_SQUARES_EXCEPT_COLUMN_H = 0x7F7F7F7F7F7F7F7FULL;



/***************************************************/
/* Function implementations for the Player entity. */ 
/***************************************************/

/**
 * @brief Returns the square state value representing the player's color.
 *
 * @invariant Parameter `p` must have a value belonging to the `Player` enum.
 * The invariant is guarded by an assertion.
 *
 * @param [in] p the player
 * @return     the square state of the player
 */
SquareState
player_color (const Player p)
{
  g_assert(p == BLACK_PLAYER || p == WHITE_PLAYER);
  return (p == BLACK_PLAYER) ? BLACK_SQUARE : WHITE_SQUARE;
}

/**
 * @brief Returns the player's description.
 *
 * @invariant Parameter `p` must have a value belonging to the `Player` enum.
 * The invariant is guarded by an assertion.
 *
 * @param p the player
 * @return  the player's description
 */
gchar *
player_description (const Player p)
{
  g_assert(p == BLACK_PLAYER || p == WHITE_PLAYER);
  return (p == BLACK_PLAYER) ? "The Black player" : "The White player";
}

/**
 * @brief Returns the player's opponent.
 *
 * @invariant Parameter `p` must have a value belonging to the `Player` enum.
 * The invariant is guarded by an assertion.
 *
 * @param [in] p the player
 * @return     the player's opponent
 */
Player
player_opponent (const Player p)
{
  g_assert(p == BLACK_PLAYER || p == WHITE_PLAYER);
  return (p == BLACK_PLAYER) ? WHITE_PLAYER : BLACK_PLAYER;
}



/**************************************************/
/* Function implementations for the Board entity. */ 
/**************************************************/

/**
 * @brief Board structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * board structure is not `NULL`.
 *
 * @invariant Parameters `b` and `w` cannot have common square set. 
 * The invariant is guarded by an assertion.
 * It means that a square cannot have a white and a black disc set together.
 *
 * @param [in] b the set of black squares
 * @param [in] w the set of white squares
 * @return     a pointer to a new board structure
 */
Board *
board_new (const SquareSet b,
           const SquareSet w)
{
  g_assert((w & b) == 0ULL);

  Board *board;
  static const size_t size_of_board = sizeof(Board);

  board = (Board*) malloc(size_of_board);
  g_assert(board);

  board->blacks = b;
  board->whites = w;

  return board;
}

/**
 * @brief Board structure destructor.
 *
 * @invariant Parameter `b` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] b the pointer to be deallocated
 * @return       always the NULL pointer
 */
Board *
board_free (Board *b)
{
  g_assert(b);

  free(b);
  b = NULL;

  return b;
}

/**
 * @brief Returns the SquareState value for the given board's square.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * Parameter `sq` must belongs to the `Square` enum.
 * Invariants are both guarded by assetions.
 *
 * @param [in] b  a pointer to the board structure
 * @param [in] sq the square to query for
 * @return        the color of the given square
 */
SquareState
board_get_square (const Board  *const b,
                  const Square        sq)
{
  g_assert(b);
  g_assert(sq >= A1 && sq <= H8);

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
 * @invariant Parameter `b` must be not `NULL`.
 * Parameter `color` must belongs to the `SquareState` enum.
 * Both invariants are guarded by assertions.
 *
 * @param [in] b     a pointer to the board structure
 * @param [in] color the square color
 * @return           the piece count for the given color
 */
int
board_count_pieces (const Board       *const b,
                    const SquareState        color)
{
  g_assert(b);
  g_assert(color == EMPTY_SQUARE || color == BLACK_SQUARE || color == WHITE_SQUARE);

  return popcount(board_get_color(b, color));
}

/**
 * @brief Returns the disk difference between the player and her opponent.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * Parameter `p` must be a value belonging to the `Player` enum.
 * Both invariants are guarded by assertions.
 *
 * @param [in] b a pointer to the board structure
 * @param [in] p the player for whom the difference is computed
 * @return       the disc count difference
 */
int
board_count_difference (const Board  *const b,
                        const Player        p)
{
  g_assert(b);
  g_assert(p == BLACK_PLAYER || p == WHITE_PLAYER);

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
 * @todo Function implementation must be completed!
 *
 * @invariant Parameter `b` must be not `NULL`.
 * Parameter `move` must be a value belonging to the `Square` enum.
 * Parameter `p` must be a value belonging to the `Player` enum.
 * All invariants are guarded by assertions.
 *
 * @param [in] b    a pointer to the board structure
 * @param [in] move the square where to put the new disk
 * @param [in] p    the player moving
 * @return          1 if the move is legal, otherwise 0
 */
int
board_is_move_legal (const Board  *const b,
                     const Square        move,
                     const Player        p)
{
  g_assert(b);
  g_assert(move >= A1 && move <= H8);
  g_assert(p == BLACK_PLAYER || p == WHITE_PLAYER);

  return 0;
}

/**
 * @brief Returns the set of empty squares in the board.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] b a pointer to the board structure
 * @return       the set of empty squares
 */
SquareSet
board_empties (const Board *const b)
{
  g_assert(b);

  return ~(b->blacks | b->whites);
}

/**
 * @brief Returns the set of black squares in the board.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] b a pointer to the board structure
 * @return       the set of black squares
 */
SquareSet
board_blacks (const Board *const b)
{
  g_assert(b);

  return b->blacks;
}

/**
 * @brief Returns the set of white squares in the board.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] b a pointer to the board structure
 * @return       the set of white squares
 */
SquareSet
board_whites (const Board *const b)
{
  g_assert(b);

  return b->whites;
}

/**
 * @brief Returns the #SquareSet of the #Board addressed by `b`
 * corresponding to the #SquareState identified by `color`.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * Parameter `color` must belong to the #SquareState enum.
 * Invariants are guarded by assertions.
 *
 * @param [in] b     a pointer to the board structure
 * @param [in] color a given color
 * @return           the set of squares in the board having the given color
 */
SquareSet
board_get_color (const Board       *const b,
                 const SquareState        color)
{
  g_assert(b);
  g_assert(color == EMPTY_SQUARE || color == BLACK_SQUARE || color == WHITE_SQUARE);

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
 * @brief Returns a formatted string showing a 2d graphical represention of the board.
 *
 * The returned string has a dynamic extent. It must then properly
 * garbage collected by a call to `g_free` when no more referenced.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] b a pointer to the board structure
 * @return       a string being a 2d representation of the board
 */
gchar *
board_print (const Board const *b)
{
  g_assert(b);

  char *b_to_string;
  GString *bs;

  bs = g_string_sized_new(220);
  g_string_append(bs, "    a b c d e f g h ");
  for (int row = 0; row < 8; row++) {
    g_string_append_printf(bs, "\n %1d  ", row + 1);
    for (int col = 0; col < 8; col++) {
      g_string_append_printf(bs, "%c ", square_state_symbol(board_get_square(b, (8 * row) + col)));
    }
  }
  g_string_append(bs, "\n");

  b_to_string = bs->str;
  g_string_free(bs, FALSE);

  return b_to_string;
}

/**
 * @brief Compares board `a` and board `b`.
 *
 * When the two board are equal it returns `0`, when `a` is greather then `b` it
 * returns `+1`, otherwise `-1`.
 *
 * Boards are equals when have the same square configuration.
 *
 * The returned string has a dynamic extent set by a call to malloc. It must then properly
 * garbage collected by a call to free when no more referenced.
 *
 * @invariant Parameters `a` and `b` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] a a pointer to a board structure
 * @param [in] b a pointer to another board structure
 * @return       `-1` when `a < b`, `+1` when `a > b`, or `0` when the two boards are equal
 */
int
board_compare (const Board * const a,
               const Board * const b)
{
  g_assert(a);
  g_assert(b);

  if (a->blacks < b->blacks) {
    return -1;
  } else if (a->blacks > b->blacks) {
    return +1;
  } else {          /* Blacks are equal. */
    if (a->whites < b->whites) {
      return -1;
    } else if (a->whites > b->whites) {
      return +1;
    } else {        /* Also whites are equal, and so are a and b boards. */
      return  0;
    }
  }  
}



/******************************************************/
/* Function implementations for the Direction entity. */ 
/******************************************************/

/**
 * @brief Returns a new #SquareSet value by shifting the
 * `squares` parameter by one position on the board.
 *
 * @invariant Parameter `dir` must belong to the #Direction enum.
 * The invariant is guarded by an assertion.
 *
 * @param [in] dir     the direction to shift to
 * @param [in] squares the squares set on the bitboard
 * @return             the shifted squares
*/
SquareSet
direction_shift_square_set (const Direction dir,
                            const SquareSet squares)
{
  g_assert(dir >= NW && dir <= SE);

  switch (dir) {
  case NW: return (squares >> 9) & ALL_SQUARES_EXCEPT_COLUMN_H;
  case N:  return (squares >> 8);
  case NE: return (squares >> 7) & ALL_SQUARES_EXCEPT_COLUMN_A;
  case W:  return (squares >> 1) & ALL_SQUARES_EXCEPT_COLUMN_H;
  case E:  return (squares << 1) & ALL_SQUARES_EXCEPT_COLUMN_A;
  case SW: return (squares << 7) & ALL_SQUARES_EXCEPT_COLUMN_H;
  case S:  return (squares << 8);
  case SE: return (squares << 9) & ALL_SQUARES_EXCEPT_COLUMN_A;
  default: abort();
  }
}



/********************************************************/
/* Function implementations for the SquareState entity. */ 
/********************************************************/

/**
 * @brief Returns the #SquareState printable representation.
 *
 * @invariant Parameter `color` must belong to the #SquareState enum.
 * The invariant is guarded by an assertion.
 *
 * @param [in] color the given color
 * @return     the color's `symbol`
 */
char
square_state_symbol (const SquareState color)
{
  g_assert(color >= EMPTY_SQUARE && color <= WHITE_SQUARE);

  switch (color) {
  case EMPTY_SQUARE: return '.';
  case BLACK_SQUARE: return '@';
  case WHITE_SQUARE: return 'O';
  default: abort();
  }
}



/*********************************************************/
/* Function implementations for the GamePosition entity. */ 
/*********************************************************/

/**
 * @brief GamePosition structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * game position structure is not `NULL`.
 *
 * @invariant Parameter `b` cannot be null. 
 * The invariant is guarded by an assertion.
 *
 * @invariant Parameter `p` must belong to the `Player` enum set. 
 * The invariant is guarded by an assertion.
 *
 * @param [in] b the board field
 * @param [in] p the player field
 * @return     a pointer to a new game position structure
 */
GamePosition *
game_position_new (Board  *b,
                   Player  p)
{
  g_assert(b);
  g_assert(p == BLACK_PLAYER || p == WHITE_PLAYER);

  GamePosition *gp;
  static const size_t size_of_game_position = sizeof(GamePosition);

  gp = (GamePosition*) malloc(size_of_game_position);
  g_assert(gp);

  gp->board = b;
  gp->player = p;

  return gp;
}

/**
 * @brief GamePosition structure destructor.
 *
 * @invariant Parameter `gp` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] gp the pointer to be deallocated
 * @return        always the NULL pointer
 */
GamePosition *
game_position_free (GamePosition *gp)
{
  g_assert(gp);

  board_free(gp->board);

  g_free(gp);
  gp = NULL;

  return gp;
}

int
game_position_compare (const GamePosition * const a,
                       const GamePosition * const b)
{
  if (a->board->blacks < b->board->blacks) {
    return -1;
  } else if (a->board->blacks > b->board->blacks) {
    return +1;
  } else {                    /* Blacks are equal. */
    if (a->board->whites < b->board->whites) {
      return -1;
    } else if (a->board->whites > b->board->whites) {
      return +1;
    } else {                  /* Also whites are equal. */
      if (a->player < b->player) {
        return -1;
      } else if (a->player > b->player) {
        return +1;
      } else {
        return 0;             /* Players are equal, and so are a and b. */
      }
    }
  }  
}

/**
 * @brief Returns a formatted string showing a 2d graphical represention of the game position.
 *
 * The returned string has a dynamic extent set by a call to malloc. It must then properly
 * garbage collected by a call to free when no more referenced.
 *
 * @invariant Parameter `gp` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] gp a pointer to the game position structure
 * @return        a string being a 2d representation of the game position
 */
gchar *
game_position_print (const GamePosition const *gp)
{
  g_assert(gp);

  const gchar *separator = NULL;

  gchar *gp_to_string;
  gchar *b_to_string;

  b_to_string = board_print(gp->board);

  gp_to_string = g_strjoin(separator,
                           b_to_string,
                           "Player to move: ",
                           (gp->player == BLACK_PLAYER) ? "BLACK" : "WHITE",
                           "\n",
                           NULL);

  g_free(b_to_string);

  return gp_to_string;
}
