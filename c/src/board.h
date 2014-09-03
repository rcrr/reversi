/**
 * @file
 *
 * @brief Board module definitions.
 * @details This module defines the #Player, #SquareState,
 * #Square, #SquareSet, #Board, #GamePosition, #Direction entities,
 * and the function prototypes that operate on them.
 *
 * @par board.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014 Roberto Corradini. All rights reserved.
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

#include <glib.h>

#include "bit_works.h"
#include "random.h"



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
 * @enum Direction
 * @brief The directions that are available in a regular board's square are
 * eight, Up, Down, Left, Right, and the four diagonal between them.
 *
 * Each regular `Square` has eight neighbor ones,
 * each identified by the proper direction. Boundary squares have fewer neighbors.
 *
 * The `Direction` enum is represented by the respective cardinal point literal.
 */
typedef enum {
  NW,   /**< North-West direction. */
  N,    /**< North direction. */
  NE,   /**< North-East direction. */
  W,    /**< West direction. */
  E,    /**< East direction. */
  SW,   /**< South-West direction. */
  S,    /**< South direction. */
  SE    /**< South-Est direction. */
} Direction;

/**
 * @enum Axis
 * @brief The axes are the lines that pass throw a square, a general
 * square has four axes.
 */
typedef enum {
  HO,   /**< Horizontal axis (W-E). */
  VE,   /**< Vertical axis (N-S). */
  DD,   /**< Diagonal Down axis (NW-SE), A1-H8. */
  DU    /**< Diagonal Up axis (NE-SW), A8-H1. */
} Axis;

/**
 * @typedef SquareSet
 * @brief The set of sixtyfour squares held by the board.
 */
typedef unsigned long long int SquareSet;

/**
 * @brief A board is an entity that holds the state of a Reversi's gameboard.
 *
 * @details The state is expressed as the combination of the individual state of each board's square.
 * It is the state that a board has regardless of the player
 * that has to move or the time spent or remaining to each player.
 */
typedef struct {
  SquareSet blacks;   /**< @brief The set of squares occupied by blank discs. */
  SquareSet whites;   /**< @brief The set of squares occupied by white discs. */
} Board;

/**
 * @brief A game position is a value object joining the board and the moving player.
 */
typedef struct {
  Board *board;   /**< @brief Member board contains a pointer to a `Board` structure. */
  Player player;  /**< @brief Member player contains the next to move. */
} GamePosition;

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
 * @brief The empty square set.
 */
static const int square_cardinality = 64;

/**
 * @brief The empty square set.
 */
static const SquareSet empty_square_set = 0;

/**
 * @brief The pass move.
 */
static const Square pass_move = (Square) -1;

/**
 * @brief The invalid move.
 */
static const Square invalid_move = (Square) -2;



/**********************************************/
/* Module initialization prototype functions. */
/**********************************************/

extern void
board_module_init (void);



/**********************************************/
/* Function prototypes for the Square entity. */
/**********************************************/

extern const gchar *
square_to_string (const Square sq);

extern const gchar *
square_as_move_to_string (const Square move);

extern gchar *
square_array_to_string (const Square sqa[],
                        const int length);

extern gchar *
square_as_move_array_to_string (const Square mova[],
                                const int length);

extern gboolean
square_belongs_to_enum_set (const Square sq);

extern gboolean
square_is_valid_move (const Square move);



/*************************************************/
/* Function prototypes for the SquareSet entity. */
/*************************************************/

extern gchar *
square_set_to_pg_json_array (const SquareSet squares);

extern gchar *
square_set_to_string (const SquareSet squares);

extern Square
square_set_random_selection (RandomNumberGenerator *const rng,
                             const SquareSet squares);

extern void
square_set_to_array (int *sq_count,
                     Square **sq_array,
                     const SquareSet squares);

extern SquareSet
square_set_from_array (const Square sq_array[],
                       const int sq_count);



/********************************************/
/* Function prototypes for the Axis entity. */
/********************************************/

extern int
axis_shift_distance (const Axis axis,
                     const uint8_t column,
                     const uint8_t row);

extern uint8_t
axis_move_ordinal_position_in_bitrow (const Axis axis,
                                      const uint8_t column,
                                      const uint8_t row);

extern uint8_t
axis_transform_to_row_one (const Axis axis,
                           const SquareSet squares);

extern SquareSet
axis_transform_back_from_row_one (const Axis axis,
                                  const uint32_t bitrow);



/**********************************************/
/* Function prototypes for the Player entity. */
/**********************************************/

extern SquareState
player_color (const Player p);

extern char *
player_description (const Player p);

extern Player
player_opponent (const Player p);



/*********************************************/
/* Function prototypes for the Board entity. */
/*********************************************/

extern Board *
board_new (const SquareSet b,
           const SquareSet w);

extern void
board_free (Board *b);

extern Board *
board_clone (const Board *const b);

extern SquareState
board_get_square (const Board *const b,
                  const Square sq);

extern int
board_count_pieces(const Board *const b,
                   const SquareState color);

extern int
board_count_difference (const Board *const b,
                        const Player p);

extern int
board_count_diff_winner_get_empties (const Board *const b,
                                     const Player p);

extern int
board_is_move_legal (const Board *const b,
                     const Square move,
                     const Player p);

extern SquareSet
board_legal_moves (const Board *const b,
                   const Player p);

extern SquareSet
board_get_color (const Board *const b,
                 const SquareState color);

extern uint8_t
board_bitrow_changes_for_player (int player_row,
                                 int opponent_row,
                                 int move_position);

extern SquareSet
board_get_player (const Board *const b,
                  const Player p);

extern SquareSet
board_empties (const Board *const b);

extern SquareSet
board_blacks (const Board *const b);

extern SquareSet
board_whites (const Board *const b);

extern char *
board_print (const Board *const b);

extern int
board_compare (const Board *const a,
               const Board *const b);

extern gboolean
board_has_any_player_any_legal_move (const Board *const b);



/*************************************************/
/* Function prototypes for the Direction entity. */
/*************************************************/

extern SquareSet
direction_shift_square_set (const Direction dir,
                            const SquareSet squares);

extern SquareSet
direction_shift_square_set_by_amount (const Direction dir,
                                      const SquareSet squares,
                                      const int amount);

extern Direction
direction_opposite (const Direction dir);



/***************************************************/
/* Function prototypes for the SquareState entity. */
/***************************************************/

extern char square_state_symbol (const SquareState color);



/****************************************************/
/* Function prototypes for the GamePosition entity. */
/****************************************************/

extern GamePosition *
game_position_new (Board *b,
                   Player p);

extern void
game_position_free (GamePosition *gp);

extern GamePosition *
game_position_clone (const GamePosition *const gp);

extern gchar *
game_position_print (const GamePosition *const gp);

extern gchar *
game_position_to_string (const GamePosition *const gp);

extern int
game_position_count_difference (const GamePosition *gp);

extern SquareSet
game_position_legal_moves (const GamePosition *position);

extern int
game_position_compare (const GamePosition *const a,
                       const GamePosition *const b);

extern gboolean
game_position_has_any_legal_move (const GamePosition *const gp);

extern gboolean
game_position_has_any_player_any_legal_move (const GamePosition *const gp);

extern gboolean
game_position_is_move_legal (const GamePosition *const gp,
                             const Square move);

extern GamePosition *
game_position_make_move (const GamePosition *const gp,
                         const Square move);

extern GamePosition *
game_position_pass (const GamePosition *const gp);

extern uint64_t
game_position_hash (const GamePosition *const gp);

extern int
game_position_final_value (const GamePosition *const gp);

extern int
game_position_empty_count (const GamePosition *const gp);



/*****************************************************/
/* Function prototypes for the GamePositionX entity. */
/*****************************************************/

extern GamePositionX *
game_position_x_new (const SquareSet b,
                     const SquareSet w,
                     const Player p);

extern GamePositionX *
game_position_x_free (GamePositionX *gpx);

extern GamePositionX *
game_position_x_clone (const GamePositionX *const gpx);

extern GamePositionX *
game_position_x_gp_to_gpx (const GamePosition *const gp);

extern GamePosition *
game_position_x_gpx_to_gp (const GamePositionX *const gpx);

extern void
game_position_x_copy (const GamePositionX *const from,
                      GamePositionX *const to);

void
game_position_x_copy_from_gp (const GamePosition *const from,
                              GamePositionX *const to);

extern SquareSet
game_position_x_empties (const GamePositionX *const gpx);

extern SquareSet
game_position_x_get_player (const GamePositionX *const gpx);

extern SquareSet
game_position_x_get_opponent (const GamePositionX *const gpx);

extern SquareState
game_position_x_get_square (const GamePositionX *const gpx,
                            const Square sq);

extern SquareSet
game_position_x_legal_moves (const GamePositionX *const gpx);

extern int
game_position_x_count_difference (const GamePositionX *const gpx);

extern void
game_position_x_to_string (const GamePositionX const *gpx,
                           char *out);

extern int
game_position_x_compare (const GamePositionX *const a,
                         const GamePositionX *const b);

extern void
game_position_x_pass (const GamePositionX *const current,
                      GamePositionX *const next);

extern uint64_t
game_position_x_hash (const GamePositionX *const gpx);

extern int
game_position_x_final_value (const GamePositionX *const gpx);

extern gchar *
game_position_x_print (const GamePositionX *const gpx);

extern gboolean
game_position_x_has_any_legal_move (const GamePositionX *const gpx);

extern gboolean
game_position_x_has_any_player_any_legal_move (const GamePositionX *const gpx);

extern gboolean
game_position_x_is_move_legal (const GamePositionX *const gpx,
                               const Square move);

extern void
game_position_x_make_move (const GamePositionX *const current,
                           const Square move,
                           GamePositionX *const updated);



#endif /* BOARD_H */
