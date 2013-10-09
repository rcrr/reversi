/**
 * @file
 *
 * @brief Improved fast endgame solver module definitions.
 * @details This module defines a solve function.
 *
 * @par improved_fast_endgame_solver.h
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

#include "exact_solver.h"

#ifndef IMPROVED_FAST_ENDGAME_SOLVER_H
#define IMPROVED_FAST_ENDGAME_SOLVER_H

/**
 * @enum IFES_SquareState
 * @brief The `IFES_SquareState` identifies the state, or as a synonym the "color",
 * of each board square.
 */
typedef enum {
  IFES_WHITE,         /**< A white piece. */
  IFES_EMPTY,         /**< An empty square. */
  IFES_BLACK,         /**< A black piece. */
  IFES_DUMMY          /**< A piece out of board. */
} IFES_SquareState;

extern ExactSolution *
game_position_ifes_solve (const GamePosition * const root);

extern GamePosition *
ifes_game_position_translation (uint8 *board, int color);

extern void
game_position_to_ifes_board (const GamePosition * const gp, uint8 *b, int *p_emp, int *p_wc, int *p_bc);

extern IFES_SquareState
game_position_get_ifes_player (const GamePosition * const gp);

#endif /* IMPROVED_FAST_ENDGAME_SOLVER_H */
