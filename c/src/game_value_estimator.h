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
 * @brief Gives an estimation of the game position returning a new exact solution pointer.
 *
 * @details The GVE solver return an exact solution when the search_depth is -1 or greater than the real
 *          depth of the game tree generated starting from the root game position.
 *          Otherwise the solver returns an estimation of the game value obtained by mini-maxing the value
 *          estimated on the leaf nodes at depth equal to search_depth.
 *
 *          The solver traverses the game tree with the MTD(f) algorithm up to a given limit.
 *          After the limit a simple negascout implementation using the fastest-first heuristic is used.
 *          The MTD(f) adopts several techniques like transposition table, iterative deepening, zero window search.
 *
 *          Many parameters collected into a mandatory configuration file affect and control
 *          the inner working of the function.
 *
 *          Parameters:
 *
 *            Section: gve_solver
 *              check_key: must be true
 *              id_min_empty_count: last level touched by MTD(f), then negascout takes over
 *              id_step: increment step value adopted by iterative deepening
 *              ttab_log_size: transposition table binary logarithm of size
 *              ttab_log_verbosity: if set to 1, outputs statistics for the transposition table
 *              game_position_evaluation_summary: if set to 1, outputs statistics on the calls to the evaluation function
 *              gve_solver_log_level: if set from 1 to 3, outputs the search progress with increasing details
 *
 *            Section: model_weights
 *              check_key: must be true
 *              verbose_loader: if true outputs the data file loading progress
 *              check_digest: if true verifies the SHA checksum of the data files
 *              ec00..ec60: the names of the data file to load
 *
 * @param [in] root the starting game position to be solved
 * @param [in] env  parameter envelope
 * @return          a pointer to a new exact solution structure
 */
extern ExactSolution *
game_position_value_estimator (const GamePositionX *const root,
                               const endgame_solver_env_t *const env);



#endif /* GAME_VALUE_ESTIMATOR_H */
