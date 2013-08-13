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

/*
 * Prototypes for internal functions.
 */

static void
board_initialize_bitrow_changes_for_player_array (uint8 *array);

static uint8
board_bitrow_changes_for_player (int player_row, int opponent_row, int move_position);



/*
 * Internal variables and constants.
 */

/* A square set being all set with the exception of column A. */
static const SquareSet all_squares_except_column_a = 0xFEFEFEFEFEFEFEFEULL;

/* A square set being all set with the exception of column H. */
static const SquareSet all_squares_except_column_h = 0x7F7F7F7F7F7F7F7FULL;

/* A bitboard being set on column A. */
static const SquareSet column_a = 0x0101010101010101ULL;

/* A bitboard being set on diagonal A1-H8. */
static const SquareSet diagonal_a1_h8 = 0x8040201008040201ULL;

/* A bitboard being set on diagonal H1-A8. */
static const SquareSet diagonal_h1_a8 = 0x0102040810204080ULL;

/* A bitboard having set squares B1 F1 A2 E2. */
static const SquareSet squares_b1_f1_a2_e2 = 0x1122;

/*
 * This array is an implementation of the precomputed table that contains the effects of moving
 * a piece in any of the eigth squares in a row.
 * The size is so computed:
 *  - there are 256 arrangments of player discs,
 *  - and 256 arrangements of opponent pieces,
 *  - the potential moves are 8.
 * So the array size is 256 * 256 * 8 = 524,288 Bytes = 512kB.
 * Not all the entries are legal! The first set of eigth bits and the second one (opponent row)
 * must not set the same position.
 *
 * The index of the array is computed by this formula:
 * index = playerRow | (opponentRow << 8) | (movePosition << 16);
 *
 * After initialization the array is never changed.
 */
static uint8 bitrow_changes_for_player_array[256 * 256 * 8]; 

/*
 * This array has sixtyfour entries. The index, having range 0-63, represent one of the squares
 * of the table. Each entry is a bitboard mask having set all the squares that are
 * reachable moving along the eigth directions, when starting from the square identified by
 * the index itself.
 * 
 * Values do not change.
 */
static SquareSet bitboard_mask_for_all_directions[] = {
  0x81412111090503FE, 0x02824222120A07FD, 0x0404844424150EFB, 0x08080888492A1CF7,
  0x10101011925438EF, 0x2020212224A870DF, 0x404142444850E0BF, 0x8182848890A0C07F,
  0x412111090503FE03, 0x824222120A07FD07, 0x04844424150EFB0E, 0x080888492A1CF71C,
  0x101011925438EF38, 0x20212224A870DF70, 0x4142444850E0BFE0, 0x82848890A0C07FC0,
  0x2111090503FE0305, 0x4222120A07FD070A, 0x844424150EFB0E15, 0x0888492A1CF71C2A,
  0x1011925438EF3854, 0x212224A870DF70A8, 0x42444850E0BFE050, 0x848890A0C07FC0A0,
  0x11090503FE030509, 0x22120A07FD070A12, 0x4424150EFB0E1524, 0x88492A1CF71C2A49,
  0x11925438EF385492, 0x2224A870DF70A824, 0x444850E0BFE05048, 0x8890A0C07FC0A090,
  0x090503FE03050911, 0x120A07FD070A1222, 0x24150EFB0E152444, 0x492A1CF71C2A4988,
  0x925438EF38549211, 0x24A870DF70A82422, 0x4850E0BFE0504844, 0x90A0C07FC0A09088,
  0x0503FE0305091121, 0x0A07FD070A122242, 0x150EFB0E15244484, 0x2A1CF71C2A498808,
  0x5438EF3854921110, 0xA870DF70A8242221, 0x50E0BFE050484442, 0xA0C07FC0A0908884,
  0x03FE030509112141, 0x07FD070A12224282, 0x0EFB0E1524448404, 0x1CF71C2A49880808,
  0x38EF385492111010, 0x70DF70A824222120, 0xE0BFE05048444241, 0xC07FC0A090888482,
  0xFE03050911214181, 0xFD070A1222428202, 0xFB0E152444840404, 0xF71C2A4988080808,
  0xEF38549211101010, 0xDF70A82422212020, 0xBFE0504844424140, 0x7FC0A09088848281
};


/************************************/
/* Module initialization functions. */ 
/************************************/

/**
 * @brief This function must be called before any use of other functions contained in this module.
 */
void
board_module_init (void)
{
  board_initialize_bitrow_changes_for_player_array(bitrow_changes_for_player_array);
}



/***************************************************/
/* Function implementations for the Square entity. */
/***************************************************/

gchar *
square_to_string (const Square sq)
{
  g_assert(sq >= A1 && sq <= H8);

  gchar *symbol;

  static const size_t size_of_square_to_string = 3 * sizeof(gchar);
  symbol = (gchar*) malloc(size_of_square_to_string);

  const uint8 col = sq % 8;
  const uint8 row = sq / 8;
  
  *symbol = 'A' + col;
  *(symbol + 1) = '1' + row;
  *(symbol + 2) = '\0';
  
  return symbol;
}



/******************************************************/
/* Function implementations for the SquareSet entity. */ 
/******************************************************/

/**
 * @brief Returns a string representation for the sqare set.
 *
 * @param moves the square set to be converted into a string
 * @return      a string having the moves sorted as the `Square` enum
 */
gchar *
square_set_print_as_moves (SquareSet moves)
{
  char *moves_to_string;
  GString *tmp;

  tmp = g_string_sized_new(10);  

  Square move = 0;
  gboolean passed = FALSE;
  for (SquareSet cursor = 1ULL; cursor != 0ULL; cursor <<= 1) {
    if ((cursor & moves) != 0ULL) {
      const char row = '1' + (move / 8);
      const char col = 'A' + (move % 8);
      if (passed) {
        g_string_append_printf(tmp, " ");
      }
      g_string_append_printf(tmp, "%c%c", col, row);
      passed = TRUE;
    }
    move++;
  }

  moves_to_string = tmp->str;
  g_string_free(tmp, FALSE);

  return moves_to_string;
}



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



/*************************************************/
/* Function implementations for the Axis entity. */ 
/*************************************************/

/**
 * @brief Computes the shift quantity.
 *
 * The parameter `column` and `row` identify a square, and the `axis`
 * then select the line passing by the square and parallel to it.
 * The returned shift quantity is the amount to apply to the bit board
 * The `bit_works_signed_left_shift` function applyed to a bitboard
 * with this computed quantity move the selected line to the main line
 * for the axis.
 *
 * @invariant Parameters `axis` must belong to its enum.
 * Parameters `column` and `row` must stay in the range `0...7`.
 * The invariants are guarded by assertions.
 *
 * @param [in] axis   the selected axis
 * @param [in] column the square's column
 * @param [in] row    the square's row

 * @return     the shift quantity
 */
int
axis_shift_distance (const Axis  axis,
                     const uint8 column,
                     const uint8 row)
{
  g_assert(axis >= HO && axis <= DU);
  g_assert(column >= 0 && column <= 7);
  g_assert(row >= 0 && row <= 7);

  switch (axis) {
  case HO:
    return -row << 3;
  case VE:
    return -column;
  case DD:
    return (column - row) << 3;
  case DU:
    return (7 - column - row) << 3;
  default:
    abort();
    return EXIT_FAILURE;
  }
}

/**
 * @brief Returns the ordinal position of the move.
 *
 * The parameter `column` and `row` identify a move in the bitboard,
 * given the `axis` the function return the ordinal position of the move
 * in the selected line.
 *
 * @invariant Parameters `axis` must belong to its enum.
 * Parameters `column` and `row` must stay in the range `0...7`.
 * The invariants are guarded by assertions.
 *
 * @param [in] axis   the selected axis
 * @param [in] column the move's column
 * @param [in] row    the move's row
 * @return     the shift quantity
 */
uint8
axis_move_ordinal_position_in_bitrow (const Axis  axis,
                                      const uint8 column,
                                      const uint8 row)
{
  g_assert(axis >= HO && axis <= DU);
  g_assert(column >= 0 && column <= 7);
  g_assert(row >= 0 && row <= 7);

  switch (axis) {
  case VE:
    return row;
  default:
    return column;
  }
}

/**
 * @brief Maps the principal line of each axis into row one.
 *
 * Returns an int having the bits from position 0 to position 7, corresponding to Row One in the board,
 * transformed from:
 *  - `ROW 1` for the `HO` axis.
 *  - `COLUMN A` for the `VE` axis.
 *  - `DIAGONAL A1-H8` for the `DD` axis.
 *  - `DIAGONAL A8-H1` for the `DU` axis.
 *
 * @invariant Parameters `axis` must belong to its enum.
 * The invariants are guarded by assertions.
 *
 * @param [in] axis    the given axis
 * @param [in] squares the set of board squares
 * @return             the transformed line
 */
uint8
axis_transform_to_row_one (const Axis      axis,
                           const SquareSet squares)
{
  g_assert(axis >= HO && axis <= DU);

  SquareSet tmp;

  tmp = squares;
  switch (axis) {
  case HO:
    break;
  case VE:
    tmp &= column_a;
    tmp |= tmp >> 28;
    tmp |= tmp >> 14;
    tmp |= tmp >> 7;
    break;
  case DD:
    tmp &= diagonal_a1_h8;
    tmp |= tmp >> 32;
    tmp |= tmp >> 16;
    tmp |= tmp >> 8;
    break;
  case DU:
    tmp &= diagonal_h1_a8;
    tmp |= tmp >> 32;
    tmp |= tmp >> 16;
    tmp |= tmp >> 8;
    break;
  default:
    abort();
  }
  return (uint8) tmp;
}

/**
 * @brief Maps back the principal line of each axis from row one.
 *
 * Returns a square set having the bits along the axis reference file set to
 * the corresponding ones on the `bitrow` parameter.
 *
 * @invariant Parameters `axis` must belong to its enum.
 * The invariants are guarded by assertions.
 *
 * @param [in] axis   the given axis
 * @param [in] bitrow represents row one
 * @return            a bitboard having the axis reference file set as the bitboard parameter,
 *                    all other position are set to zero
 */
SquareSet
axis_transform_back_from_row_one (const Axis   axis,
                                  const uint32 bitrow)
{
  g_assert(axis >= HO && axis <= DU);

  uint32 tmp;
  SquareSet bit_board;

  switch (axis) {
  case HO:
    return (SquareSet) bitrow;
  case VE:
    tmp = bitrow;
    tmp |= tmp << 7;
    tmp |= tmp << 14;
    bit_board = (SquareSet) tmp | ((SquareSet) tmp << 28);
    return bit_board & column_a;
  case DD:
    tmp = bitrow;
    tmp |= tmp << 8;
    bit_board = (SquareSet) tmp | ((SquareSet) tmp << 16);
    bit_board |= bit_board << 32;
    return bit_board & diagonal_a1_h8;
  case DU:
    tmp = bitrow;
    tmp |= tmp << 8;
    tmp |= (tmp & squares_b1_f1_a2_e2) << 16;
    bit_board = (SquareSet) tmp | ((SquareSet) tmp << 32);
    return bit_board & diagonal_h1_a8;
  default:
    abort();
    return EXIT_FAILURE;
  }
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

  return bit_works_popcount(board_get_color(b, color));
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
 * @brief Used for the score at the end of the game.
 * Returns the disk difference between the player and her opponent,
 * assigning the empty squares to the player having most discs.
 *
 * From the World Othello Federation, web site:
 * World Othello Chanpionship Rules.
 * Scoring.
 * At the end of the game, if both players have completed their moves in
 * the allowed time, the winner is the player with the greater number of
 * discs of his colour on the board at the end. The official score of the
 * game will be determined by counting up the discs of each colour on the
 * board, counting empty squares for the winner. In the event of a draw,
 * the score will always be 32-32.
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
board_count_diff_winner_get_empties (const Board  *const b,
                                     const Player        p)
{
  g_assert(b);
  g_assert(p == BLACK_PLAYER || p == WHITE_PLAYER);

  int pcount, ocount, difference, empties;
  Player o;

  o = player_opponent(p);
  pcount = board_count_pieces(b, player_color(p));
  ocount = board_count_pieces(b, player_color(o));

  difference = pcount - ocount; 
  empties = 64 - (pcount + ocount);

  return difference + ((difference > 0) ? +empties : -empties);
}

/**
 * @brief Returns 1 if the move, done by the specified player, is legal,
 * otherwise 0.
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

  SquareSet bit_move;
  SquareSet p_bit_board;
  SquareSet o_bit_board;

  HiLo xy;
  uint8 column, row;

  bit_move = 1ULL << move;

  if ((board_empties(b) & bit_move) == 0ULL) return FALSE;

  p_bit_board = board_get_player(b, p);
  o_bit_board = board_get_player(b, player_opponent(p));

  bit_works_bitscan_MS1B_to_base8(&xy, bit_move);
  column = xy.lo;
  row    = xy.hi;

  for (Axis axis = HO; axis <= DU; axis++) {
    const int move_ordinal_position = axis_move_ordinal_position_in_bitrow(axis, column, row);
    const int shift_distance = axis_shift_distance(axis, column, row);
    const uint8 p_bitrow = axis_transform_to_row_one(axis, bit_works_signed_left_shift(p_bit_board, shift_distance));
    const uint8 o_bitrow = axis_transform_to_row_one(axis, bit_works_signed_left_shift(o_bit_board, shift_distance));
    if (board_bitrow_changes_for_player(p_bitrow, o_bitrow, move_ordinal_position) != p_bitrow) {
      return TRUE;
    }
  }

  /* If no capture on the four directions happens, return false. */
  return FALSE;
}

/**
 * @brief Returns a list holding the legal moves that the player can do at the board position.
 *        When no moves are available to the player the method returns an empty list.
 *       
 * Implements the legal moves call by waveing the potential legal moves up to the bracketing
 * pieces. Directions are computed one by one, squares work in parallel.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * Parameter `p` must be a value belonging to the `Player` enum.
 * All invariants are guarded by assertions.
 *
 * @param [in] b the given board
 * @param [in] p the player that has to move
 * @return     legal moves for the player
 */
SquareSet
board_legal_moves (const Board * const b, const Player p)
{
  g_assert(b);
  g_assert(p == BLACK_PLAYER || p == WHITE_PLAYER);

  register SquareSet result;
  
  result = 0ULL;

  const Player o = player_opponent(p);
  const SquareSet empties = board_empties(b);
  const SquareSet p_bit_board = board_get_player(b, p);
  const SquareSet o_bit_board = board_get_player(b, o);
  
  for (Direction dir = NW; dir <= SE; dir++) {
    const Direction opposite = direction_opposite(dir);
    SquareSet wave = direction_shift_square_set(dir, empties) & o_bit_board;
    int shift = 1;
    while (wave != 0ULL) {
      wave = direction_shift_square_set(dir, wave);
      shift++;
      result |= direction_shift_square_set_by_amount(opposite, (wave & p_bit_board), shift);
      wave &= o_bit_board;
    }
  }

  return result;
}

/**
 * @brief Returns `TRUE` if the board is not final.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * All invariants are guarded by assertions.
 *
 * @param [in] b the given board
 * @return     true if one of the player has one or more legal moves
 */
gboolean
board_has_any_player_any_legal_move (const Board * const b)
{
  g_assert(b);

  return (0ULL == board_legal_moves(b, BLACK_PLAYER) &&
          0ULL == board_legal_moves(b, WHITE_PLAYER)) ? FALSE : TRUE;
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
 * @brief Returns the #SquareSet of the #Board addressed by `b`
 * corresponding to the #Player identified by `p`.
 *
 * @invariant Parameter `b` must be not `NULL`.
 * Parameter `p` must belong to the #Player enum.
 * Invariants are guarded by assertions.
 *
 * @param [in] b a pointer to the board structure
 * @param [in] p a given player
 * @return       the set of squares in the board belonging to the given player
 */
SquareSet
board_get_player (const Board  *const b,
                  const Player        p)
{
  g_assert(b);
  g_assert(p == BLACK_PLAYER || p == WHITE_PLAYER);

  SquareSet squares;

  switch (p) {
  case BLACK_PLAYER:
    squares = b->blacks;
    break;
  case WHITE_PLAYER:
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
  case NW: return (squares >> 9) & all_squares_except_column_h;
  case N:  return (squares >> 8);
  case NE: return (squares >> 7) & all_squares_except_column_a;
  case W:  return (squares >> 1) & all_squares_except_column_h;
  case E:  return (squares << 1) & all_squares_except_column_a;
  case SW: return (squares << 7) & all_squares_except_column_h;
  case S:  return (squares << 8);
  case SE: return (squares << 9) & all_squares_except_column_a;
  default: abort();
  }
}

/**
 * @brief Returns a new #SquareSet value by shifting the `squares` parameter
 * by a number of positions as given by the `amount` parameter.
 *
 * Amount must be in the 0..8 range, meaning that 0 is equal to no shift, 1 is
 * on position, and 8 always return an empy squares.
 *
 * @invariant Parameter `dir` must belong to the #Direction enum.
 * The invariant is guarded by an assertion.
 *
 * @param [in] dir     the direction to shift to
 * @param [in] squares the squares set on the bitboard
 * @param [in] amount  the amount to shift
 * @return             the shifted squares
*/
SquareSet
direction_shift_square_set_by_amount (const Direction dir,
                                      const SquareSet squares,
                                      const int       amount)
{
  g_assert(dir >= NW && dir <= SE);

  switch (dir) {
  case NW: return (squares >> (9 * amount)) & all_squares_except_column_h;
  case N:  return (squares >> (8 * amount));
  case NE: return (squares >> (7 * amount)) & all_squares_except_column_a;
  case W:  return (squares >> (1 * amount)) & all_squares_except_column_h;
  case E:  return (squares << (1 * amount)) & all_squares_except_column_a;
  case SW: return (squares << (7 * amount)) & all_squares_except_column_h;
  case S:  return (squares << (8 * amount));
  case SE: return (squares << (9 * amount)) & all_squares_except_column_a;
  default: abort();
  }
}

/**
 * @brief Returns the opposite direction of the one given by the `dir` parameter.
 *
 * @invariant Parameter `dir` must belong to the #Direction enum.
 * The invariant is guarded by an assertion.
 *
 * @param [in] dir the given direction
 * @return         the opposite direction
 */
Direction
direction_opposite (const Direction dir)
{
  g_assert(dir >= NW && dir <= SE);

  switch (dir) {
  case NW: return SE;
  case N:  return S;
  case NE: return SW;
  case W:  return E;
  case E:  return W;
  case SW: return NE;
  case S:  return N;
  case SE: return NW;
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

/**
 * @brief Clones a GamePosition structure.
 *
 * @invariant Parameter `gp` cannot be null. 
 * The invariant is guarded by an assertion.
 *
 * @param [in] gp the game position to clone
 * @return     a pointer to a new game position structure
 */
GamePosition *
game_position_clone (const GamePosition * const gp)
{
  g_assert(gp);

  return game_position_new(board_new(gp->board->blacks,
                                     gp->board->whites),
                           gp->player);
}

/**
 * @brief Compares game positions `a` and board `b`.
 *
 * When the two game position are equal it returns `0`, when `a` is greather then `b` it
 * returns `+1`, otherwise `-1`.
 *
 * Game positions are equals when have the same board and player.
 *
 * @invariant Parameters `a` and `b` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] a a pointer to a game position structure
 * @param [in] b a pointer to a second structure
 * @return       `-1` when `a < b`, `+1` when `a > b`, or `0` when the two game position are equal
 */
int
game_position_compare (const GamePosition * const a,
                       const GamePosition * const b)
{
  g_assert(a);
  g_assert(b);

  if ( a == b) return 0;

  const int board_comp = board_compare(a->board, b->board);
  if (board_comp != 0) {
    return board_comp;
  } else {
    if (a->player < b->player) {
      return -1;
    } else if (a->player > b->player) {
      return +1;
    } else {
      return 0;             /* Players are equal, and so are a and b. */
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

/**
 * @brief Returns the disk difference between the player and her opponent.
 *
 * @invariant Parameter `gp` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] gp the given game position
 * @return        the disc count difference
 */
int
game_position_count_difference (const GamePosition *gp)
{
  g_assert(gp);

  return board_count_difference(gp->board, gp->player);
}

/**
 * @brief Returns a list holding the legal moves for the game position.
 *
 * @invariant Parameter `gp` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] gp the given game position
 * @return        a square set holding the legal moves
 */
SquareSet
game_position_legal_moves (const GamePosition *gp)
{
  g_assert(gp);

  return board_legal_moves(gp->board, gp->player);
}

/**
 * @brief Returns if the game state admit one or more legal moves.
 *
 * @invariant Parameter `gp` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] gp the given game position
 * @return        true if the game state admit a legal move
 */
gboolean
game_position_has_any_legal_move (const GamePosition * const gp)
{
  g_assert(gp);

  return (0ULL == game_position_legal_moves(gp)) ? FALSE : TRUE;
}

/**
 * @brief Returns `TRUE` if the game position is not final.
 *
 * @invariant Parameter `gp` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] gp the given game position
 * @return        true if the game state admit a legal move for at last one player
 */
gboolean
game_position_has_any_player_any_legal_move (const GamePosition * const gp)
{
  g_assert(gp);

  return board_has_any_player_any_legal_move(gp->board);
}

/**
 * @brief Returns true if the `move` is legal for the game position.
 *
 * @invariant Parameter `gp` must be not `NULL`.
 * Parameter `move` must be a value belonging to the `Square` enum.
 * Invariants are guarded by assertions.
 *
 * @param [in] gp   the given game position
 * @param [in] move the square where to put the new disk
 * @return          true if the move is legal
 */
gboolean
game_position_is_move_legal (const GamePosition * const gp, const Square move)
{
  g_assert(gp);

  return board_is_move_legal(gp->board, move, gp->player);
}

/**
 * @brief Executes a game move on the given position.
 *
 * @invariant Parameter `gp` must be not `NULL`.
 * Parameter `move` must be a value belonging to the `Square` enum.
 * Invariants are guarded by assertions.
 *
 * @param [in] gp   the given game position
 * @param [in] move the square where to put the new disk
 * @return          a pointer to a newly created game position as a rusult of the move
 */
GamePosition *
game_position_make_move (const GamePosition * const gp, const Square move)
{
  g_assert(gp);
  g_assert(move >= A1 && move <= H8);
  g_assert(game_position_is_move_legal(gp, move));

  const Player p = gp->player;
  const Player o = player_opponent(p);
  const Board *b = gp->board;
  const SquareSet p_bit_board = board_get_player(b, p);
  const SquareSet o_bit_board = board_get_player(b, o);
  const int column = move % 8;
  const int row = move / 8;

  SquareSet new_bit_board[2];
  const SquareSet unmodified_mask = ~bitboard_mask_for_all_directions[move];
  new_bit_board[p] = p_bit_board & unmodified_mask;
  new_bit_board[o] = o_bit_board & unmodified_mask;

  /*
  for (Axis axis = HO; axis <= DU; axis++) {
    const int move_ordinal_position = axis_move_ordinal_position_in_bitrow(axis, column, row);
    const int shift_distance = axis_shift_distance(axis, column, row);
    uint8 p_bitrow = axis_transform_to_row_one(axis, bit_works_signed_left_shift(p_bit_board, shift_distance));
    uint8 o_bitrow = axis_transform_to_row_one(axis, bit_works_signed_left_shift(o_bit_board, shift_distance));
    p_bitrow = board_bitrow_changes_for_player(p_bitrow, o_bitrow, move_ordinal_position);
    o_bitrow &= ~p_bitrow;
    new_bit_board[p] |= bit_works_signed_left_shift(axis_transform_back_from_row_one(axis, p_bitrow), -shift_distance);
    new_bit_board[o] |= bit_works_signed_left_shift(axis_transform_back_from_row_one(axis, o_bitrow), -shift_distance);
  }
  */

  int shift_distance;
  uint8 p_bitrow;
  uint8 o_bitrow;

  /* Axis HO. */
  const uint8 right_shift_for_HO = 8 * row;
  p_bitrow = axis_transform_to_row_one(HO, p_bit_board >> right_shift_for_HO);
  o_bitrow = axis_transform_to_row_one(HO, o_bit_board >> right_shift_for_HO);
  p_bitrow = board_bitrow_changes_for_player(p_bitrow, o_bitrow, column);
  o_bitrow &= ~p_bitrow;
  new_bit_board[p] |= ((SquareSet) p_bitrow << right_shift_for_HO);
  new_bit_board[o] |= ((SquareSet) o_bitrow << right_shift_for_HO);

  /* Axis VE. */
  p_bitrow = axis_transform_to_row_one(VE, p_bit_board >> column);
  o_bitrow = axis_transform_to_row_one(VE, o_bit_board >> column);
  p_bitrow = board_bitrow_changes_for_player(p_bitrow, o_bitrow, row);
  o_bitrow &= ~p_bitrow;
  new_bit_board[p] |= axis_transform_back_from_row_one(VE, p_bitrow) << column;
  new_bit_board[o] |= axis_transform_back_from_row_one(VE, o_bitrow) << column;

  /* Axis DD. */
  shift_distance = axis_shift_distance(DD, column, row);
  p_bitrow = axis_transform_to_row_one(DD, bit_works_signed_left_shift(p_bit_board, shift_distance));
  o_bitrow = axis_transform_to_row_one(DD, bit_works_signed_left_shift(o_bit_board, shift_distance));
  p_bitrow = board_bitrow_changes_for_player(p_bitrow, o_bitrow, column);
  o_bitrow &= ~p_bitrow;
  new_bit_board[p] |= bit_works_signed_left_shift(axis_transform_back_from_row_one(DD, p_bitrow), -shift_distance);
  new_bit_board[o] |= bit_works_signed_left_shift(axis_transform_back_from_row_one(DD, o_bitrow), -shift_distance);

  /* Axis DU. */
  shift_distance = axis_shift_distance(DU, column, row);
  p_bitrow = axis_transform_to_row_one(DU, bit_works_signed_left_shift(p_bit_board, shift_distance));
  o_bitrow = axis_transform_to_row_one(DU, bit_works_signed_left_shift(o_bit_board, shift_distance));
  p_bitrow = board_bitrow_changes_for_player(p_bitrow, o_bitrow, column);
  o_bitrow &= ~p_bitrow;
  new_bit_board[p] |= bit_works_signed_left_shift(axis_transform_back_from_row_one(DU, p_bitrow), -shift_distance);
  new_bit_board[o] |= bit_works_signed_left_shift(axis_transform_back_from_row_one(DU, o_bitrow), -shift_distance);

  return game_position_new(board_new(new_bit_board[0], new_bit_board[1]), o);
}

GamePosition *
game_position_pass (const GamePosition * const gp)
{
  g_assert(gp);
  g_assert(TRUE != game_position_has_any_legal_move(gp));

  return game_position_new(board_new(gp->board->blacks, gp->board->whites), player_opponent(gp->player));
}

/*
 * Internal functions.
 */

/**
 * @brief Used to initialize the `bitrow_changes_for_player_array`.
 *
 * @param array a uint8 array having the row changes for the given index value
 */
void
board_initialize_bitrow_changes_for_player_array (uint8 *array)
{
  for (int player_row_count = 0; player_row_count < 256; player_row_count++) {
    const uint8 player_row = (uint8) player_row_count;
    for (int opponent_row_count = 0; opponent_row_count < 256; opponent_row_count++) {
      const uint8 opponent_row = (uint8) opponent_row_count;
      const uint8 filled_in_row = player_row | opponent_row;
      const uint8 empties_in_row = ~filled_in_row;
      for (uint8 move_position = 0; move_position < 8; move_position++) {
        const uint8 move = 1 << move_position;
        const int array_index = player_row
          | (opponent_row << 8)
          | (move_position << 16);

        uint8 player_row_after_move;

        /*
         * It checks two conditions that cannot happen because are illegal.
         * First player and opponent cannot have overlapping discs.
         * Second the move cannot overlap existing discs.
         * When either one of the two condition applies the result is set being equal
         * to the player row index. Otherwise when black and white do not overlap,
         * and the move is on an empy square it procede with the else block.
         */
        if (((player_row & opponent_row) != 0) || ((move & filled_in_row) != 0)) {
          player_row_after_move = player_row;
        } else {

          /* The square of the move is added to the player configuration of the row after the move. */
          player_row_after_move = player_row | move;

          /*
           * The potential bracketing disc on the right is the first player disc found moving
           * on the left starting from the square of the move.
           */
          const uint8 potential_bracketing_disc_on_the_left = bit_works_highest_bit_set_8(player_row & (move - 1));

          /*
           * The left rank is the sequence of adiacent discs that start from the bracketing disc and end
           * with the move disc.
           */
          const uint8 left_rank = bit_works_fill_in_between(potential_bracketing_disc_on_the_left | move);

          /*
           * If the rank contains empy squares, this is a fake flip, and it doesn't do anything.
           * If the rank is full, it cannot be full of anything different than opponent discs, so
           * it adds the discs to the after move player configuration.
           */
            if ((left_rank & empties_in_row) == 0x00) {
              player_row_after_move |= left_rank;
            }

            /* Here it does the same procedure computed on the left also on the right. */
            const uint8 potential_bracketing_disc_on_the_right = bit_works_lowest_bit_set_8(player_row & ~(move - 1));
            const uint8 right_rank = bit_works_fill_in_between(potential_bracketing_disc_on_the_right | move);
            if ((right_rank & empties_in_row) == 0x00) {
              player_row_after_move |= right_rank;
            }

            /*
             * It checks that the after move configuration is different from
             * the starting one for the player.
             * This case can happen because it never checked that
             * the bracketing piece was not adjacent to the move disc,
             * on such a case, on both side, the move is illegal, and it is recorded setting
             * the result configuation appropriately.
             */
            if (player_row_after_move == (player_row | move)) {
              player_row_after_move = player_row;
            }
        }

        /* Assigns the computed player row to the proper array position. */
        array[array_index] = player_row_after_move;

      }
    }
  }
}

/**
 * Returns an 8-bit row representation of the player pieces after applying the move.
 *
 * @param player_row    8-bit bitboard corrosponding to player pieces
 * @param opponent_row  8-bit bitboard corrosponding to opponent pieces
 * @param move_position square to move
 * @return              the new player's row index after making the move
 */
uint8
board_bitrow_changes_for_player (int player_row, int opponent_row, int move_position) {
  const int array_index = player_row | (opponent_row << 8) | (move_position << 16);
  return bitrow_changes_for_player_array[array_index];
}
