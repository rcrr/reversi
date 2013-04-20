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

#ifndef BOARD_H
#define BOARD_H

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
 * @typedef SquareSet
 * @brief The set of sixtyfour squares held by the board.
 */
typedef unsigned long long int SquareSet;

/**
 * @brief A board is an entity that holds the state of a Reversi's gameboard.
 *
 * The state is expressed as the combination of the individual state of each board's square.
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



/**************************************************/
/* Function prototypes for the Player entity. */ 
/**************************************************/

extern SquareState player_color(const Player p);

extern char *player_description(const Player p);

extern Player player_opponent(const Player p);



/**************************************************/
/* Function prototypes for the Board entity. */ 
/**************************************************/

extern Board *board_new(const SquareSet b,
                        const SquareSet w
                        );

extern Board *board_delete(Board *b);

extern SquareState board_get_square(const Board  *const b,
                                    const Square        sq
                                    );

extern int board_count_pieces(const Board       *const b,
                              const SquareState        color
                              );

extern int board_count_difference(const Board  *const b,
                                  const Player        p
                                  );

extern int board_is_move_legal(const Board *const b,
                               const Square       move,
                               const Player       p
                               );

extern SquareSet board_get_color(const Board       *const b,
                                 const SquareState        color
                                 );

extern SquareSet board_empties(const Board *const b);

extern SquareSet board_blacks(const Board *const b);

extern SquareSet board_whites(const Board *const b);

extern char *board_print(const Board const *b);

extern int board_compare(const Board * const a,
                         const Board * const b);



/*************************************************/
/* Function prototypes for the Direction entity. */ 
/*************************************************/

extern SquareSet direction_shift_square_set(const Direction dir,
                                            const SquareSet squares
                                            );



/********************************************************/
/* Function implementations for the SquareState entity. */ 
/********************************************************/

extern char square_state_symbol(const SquareState color);


#endif /* BOARD_H */
