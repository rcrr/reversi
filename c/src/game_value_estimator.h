/**
 * @file
 *
 * @brief Game Value Estimator module definitions.
 * @details This module defines the #game_position_value_estimator function.
 *
 * @par game_value_estimator.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2021 Roberto Corradini. All rights reserved.
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

#ifndef GAME_VALUE_ESTIMATOR_H
#define GAME_VALUE_ESTIMATOR_H

#include "endgame_utils.h"



/**
 * @brief Gives an extimation of the game position returning a new exact solution pointer.
 *
 * @details The GVE solver return an exact solution when the search_depth is -1 or greather than the real
 *          depth of the game tree generated starting from the root game position.
 *          Otherwise the solver returns an extimation of the game value obtained by minimaxing the value
 *          estimated on the leaf nodes at depth equal to search_depth.
 *
 *          The solver
 *
 * @param [in] root the starting game position to be solved
 * @param [in] env  parameter envelope
 * @return          a pointer to a new exact solution structure
 */
extern ExactSolution *
game_position_value_estimator (const GamePositionX *const root,
                               const endgame_solver_env_t *const env);



#endif /* GAME_VALUE_ESTIMATOR_H */
