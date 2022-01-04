/**
 * @file
 *
 * @brief Board module definitions.
 * @details This module defines the #Player, #SquareState,
 * #Square, #SquareSet, #GamePositionX entities,
 * and the function prototypes that operate on them.
 *
 * @par board.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014, 2017 Roberto Corradini. All rights reserved.
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

#ifndef BOARD_H
#define BOARD_H

#include <stdbool.h>

#include "bit_works.h"
#include "prng.h"



/**********************************************/
/* Type declarations.                         */
/**********************************************/

/**
 * @enum Player
 * @brief The player is one of the two competitors in the game.
 *
 * She is either one among the Black and the White opponents.
 */
typedef enum {
  BLACK_PLAYER,   /**< The Black player. */
  WHITE_PLAYER    /**< The White player. */
} Player;

/**
 * @enum SquareState
 * @brief The `SquareState` identifies the state, or as a synonym the "color",
 * of each board square.
 */
typedef enum {
  EMPTY_SQUARE,   /**< An empty square. */
  BLACK_SQUARE,   /**< A black piece. */
  WHITE_SQUARE    /**< A white piece. */
} SquareState;

/**
 * @enum Square
 * @brief Square is an enum that realize the base unit of the game board.
 *
 * Squares are represented by two characters, a letter and a numeric digit.
 *
 * For instance, let's take a square: D4.
 * This symbol identifies the square at the cross of column d and row 4.
 *
 * Here is represented the collection of the 64 Square as them are
 * organized in the game board:
 * @code
 * .    a    b    c    d    e    f    g    h
 *   =========================================
 * 1 = A1 = B1 = C1 = D1 = E1 = F1 = G1 = H1 =
 *   =========================================
 * 2 = A2 = B2 = C2 = D2 = E2 = F2 = G2 = H2 =
 *   =========================================
 * 3 = A3 = B3 = C3 = D3 = E3 = F3 = G3 = H3 =
 *   =========================================
 * 4 = A4 = B4 = C4 = D4 = E4 = F4 = G4 = H4 =
 *   =========================================
 * 5 = A5 = B5 = C5 = D5 = E5 = F5 = G5 = H5 =
 *   =========================================
 * 6 = A6 = B6 = C6 = D6 = E6 = F6 = G6 = H6 =
 *   =========================================
 * 7 = A7 = B7 = C7 = D7 = E7 = F7 = G7 = H7 =
 *   =========================================
 * 8 = A8 = B8 = C8 = D8 = E8 = F8 = G8 = H8 =
 *   =========================================
 * @endcode
 * Has to be noticed that the sequence of the squares is organized by rows. It means that
 * the square ordered list is as follow:
 *
 * @code (A1, B1, C1, D1, E1, F1, G1, H1, A2, ... H8) @endcode.
 */
typedef enum {
  A1, B1, C1, D1, E1, F1, G1, H1,
  A2, B2, C2, D2, E2, F2, G2, H2,
  A3, B3, C3, D3, E3, F3, G3, H3,
  A4, B4, C4, D4, E4, F4, G4, H4,
  A5, B5, C5, D5, E5, F5, G5, H5,
  A6, B6, C6, D6, E6, F6, G6, H6,
  A7, B7, C7, D7, E7, F7, G7, H7,
  A8, B8, C8, D8, E8, F8, G8, H8
} Square;

/**
 * @typedef SquareSet
 * @brief The set of sixtyfour squares held by the board.
 */
typedef uint64_t SquareSet;

/**
 * @brief A game position variant where instead of the board pointer its field are directly collected.
 *
 * @details This game position representation is used in conjunction with functions
 * that do not malloc/free, these function use the prefix `game_position_x`.
 */
typedef struct {
  SquareSet blacks;   /**< @brief The set of squares occupied by blank discs. */
  SquareSet whites;   /**< @brief The set of squares occupied by white discs. */
  Player    player;   /**< @brief Next player to move. */
} GamePositionX;



/**********************************************/
/* Global constants.                          */
/**********************************************/

/**
 * @brief The count of squares on the board.
 */
static const int square_cardinality = 64;

/**
 * @brief The empty square set.
 */
static const SquareSet empty_square_set = 0;

/**
 * @brief The full square set.
 */
static const SquareSet full_square_set = 0xFFFFFFFFFFFFFFFF;

/**
 * @brief The pass move.
 */
static const Square pass_move = (Square) 64;

/**
 * @brief The invalid move.
 */
static const Square invalid_move = (Square) 65;

/**
 * @brief The unknown move.
 */
static const Square unknown_move = (Square) 66;

/**
 * @brief The invalid square.
 */
static const Square invalid_square = (Square) 65;



/**********************************************/
/* Module initialization prototype functions. */
/**********************************************/

extern void
board_module_init (void);



/**********************************************/
/* Function prototypes for the Square entity. */
/**********************************************/

extern const char *
square_to_string (const Square sq);

extern const char *
square_as_move_to_string (const Square move);

extern size_t
square_array_to_string (char *const to_string,
                        const Square sqa[],
                        const int length);

extern size_t
square_as_move_array_to_string (char *const to_string,
                                const Square mova[],
                                const int length);

extern bool
square_belongs_to_enum_set (const Square sq);

extern bool
square_is_valid_move (const Square move);



/*************************************************/
/* Function prototypes for the SquareSet entity. */
/*************************************************/

extern size_t
square_set_to_string (char *const to_string,
                      const SquareSet squares);


extern Square
square_set_random_selection (prng_mt19937_t *const prng,
                             const SquareSet squares);

extern void
square_set_to_array (int *sq_count,
                     Square **sq_array,
                     const SquareSet squares);

extern SquareSet
square_set_from_array (const Square sq_array[],
                       const int sq_count);

extern size_t
square_set_print (char *const to_string,
                  const SquareSet s);



/**********************************************/
/* Function prototypes for the Player entity. */
/**********************************************/

extern SquareState
player_color (const Player p);

extern char *
player_description (const Player p);

extern Player
player_opponent (const Player p);



/***************************************************/
/* Function prototypes for the SquareState entity. */
/***************************************************/

extern char
square_state_symbol (const SquareState color);



/*****************************************************/
/* Function prototypes for the GamePositionX entity. */
/*****************************************************/

extern GamePositionX *
game_position_x_new (const SquareSet b,
                     const SquareSet w,
                     const Player p);

extern void
game_position_x_free (GamePositionX *gpx);

extern GamePositionX *
game_position_x_clone (const GamePositionX *const gpx);

extern void
game_position_x_copy (const GamePositionX *const from,
                      GamePositionX *const to);

extern SquareSet
game_position_x_empties (const GamePositionX *const gpx);

inline static Player
game_position_x_get_player (const GamePositionX *const gpx)
{
  return gpx->player;
}

inline static SquareSet
game_position_x_get_mover (const GamePositionX *const gpx)
{
  return *((SquareSet *) gpx + gpx->player);
}

inline static SquareSet
game_position_x_get_opponent (const GamePositionX *const gpx)
{
  return *((SquareSet *) gpx + (1 - gpx->player));
}

extern SquareState
game_position_x_get_square (const GamePositionX *const gpx,
                            const Square sq);

extern SquareSet
game_position_x_legal_moves (const GamePositionX *const gpx);

extern int
game_position_x_count_difference (const GamePositionX *const gpx);

extern void
game_position_x_to_string (const GamePositionX *gpx,
                           char *out);

extern int
game_position_x_compare (const GamePositionX *const a,
                         const GamePositionX *const b);

inline static void
game_position_x_pass (const GamePositionX *const current,
                      GamePositionX *const next)
{
  next->blacks = current->blacks;
  next->whites = current->whites;
  next->player = 1 - current->player;
}

extern uint64_t
game_position_x_hash_plain (const GamePositionX *const gpx);

#ifdef __AVX2__
extern uint64_t
game_position_x_hash_vec (const GamePositionX *const gpx);
#endif

extern uint64_t
game_position_x_hash (const GamePositionX *const gpx);

extern uint64_t
game_position_x_delta_hash (const uint64_t old_hash,
                            const Square *const flips,
                            const int flip_count,
                            const Player new_p);

extern void
game_position_x_deltas (const GamePositionX *const parent,
                        const GamePositionX *const child,
                        Square *flips,
                        int *flip_count);

inline static int
game_position_x_final_value (const GamePositionX *const gpx)
{
  const uint8_t b_count = bitw_bit_count_64(gpx->blacks);
  const uint8_t w_count = bitw_bit_count_64(gpx->whites);
  const int8_t difference = b_count - w_count;
  if (difference == 0) return 0;
  const uint8_t empties = 64 - (b_count + w_count);
  const int8_t delta = (difference > 0) ? difference + empties : difference - empties;
  return (gpx->player == BLACK_PLAYER) ? +delta : -delta;
}

extern size_t
game_position_x_print (char *const to_string,
                       const GamePositionX *const gpx);

extern bool
game_position_x_has_any_legal_move (const GamePositionX *const gpx);

extern bool
game_position_x_has_any_player_any_legal_move (const GamePositionX *const gpx);

extern bool
game_position_x_is_move_legal (const GamePositionX *const gpx,
                               const Square move);

extern void
game_position_x_make_move (const GamePositionX *const current,
                           const Square move,
                           GamePositionX *const updated);

extern void
game_position_x_make_move_delta_hash (const GamePositionX *const current,
                                      const Square move,
                                      GamePositionX *const updated,
                                      uint64_t hash,
                                      uint64_t *updated_hash);

extern void
game_position_x_set_initial_position (GamePositionX *const gpx);

extern int
game_position_x_empty_count (const GamePositionX *const gpx);



#endif /* BOARD_H */
