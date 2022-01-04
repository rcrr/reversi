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
 * @copyright 2021, 2022 Roberto Corradini. All rights reserved.
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

#include "rglm_utils.h"
#include "transposition_table.h"
#include "endgame_utils.h"

/**
 * @brief Collects all the info required by the GVE solver to run.
 *
 * @details The pointer needs to be created by a call to gve_context_new(),
 *          and then initialized by a call to gve_context_init().
 *          After this it can be used to call the gve solver once or multiple times,
 *          in this second case a call to gve_context_set_root() updates the info dependent from
 *          the game position and reset counters.
 *          It could be finally disposed by a call to gve_context_release().
 */
typedef struct gve_context_s *gve_context_t;

/**
 * @brief Gives an estimation, or exact solution, of the `root` game position returning a new exact solution pointer.
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
 * @param [in] root the game position to be evaluated or solved
 * @param [in] env  endgame solver environment
 * @return          a pointer to a new exact solution structure
 */
extern ExactSolution *
game_position_value_estimator (const GamePositionX *root,
                               const endgame_solver_env_t *env);

/**
 * @brief Solves a game position using the gve algorithm.
 *
 * @details It calls the game_position_value_estimator() function.
 *          It requires the appropriate `ctx` object being prepared in advance.
 *          It is a convenience prepared for calling the solver multiple times without
 *          having to construct a new context each time.
 *
 * @param [in,out] ctx  gve solver context
 * @param [in]     env  endgame solver environment
 * @return              a pointer to a new exact solution structure
 */
extern ExactSolution *
game_position_gve_solve (gve_context_t ctx,
                         const endgame_solver_env_t *env);

/**
 * @brief Initialize a gve context object.
 *
 * @param [out] ctx  gve solver context
 * @param [in]  env  endgame solver environment
 * @param [in]  root the game position to be evaluated or solved
 * @return           a status value
 */
extern int
gve_context_init (gve_context_t ctx,
                  const endgame_solver_env_t *env,
                  const GamePositionX *root);

/**
 * @brief Returns a not initialized gve solver context.
 *
 * @details The pointers needs then to be initialized by a call to gve_context_init(),
 *          and finally disposed by a call to gve_context_release().
 *
 * @return a gve solver context
 */
gve_context_t
gve_context_new (void);

/**
 * @brief Releases a gve context object.
 *
 * @details The `ctx` pointer cannot be used after the call.
 *
 * @param [in,out] ctx gve solver context
 */
extern void
gve_context_release (gve_context_t ctx);

/**
 * @brief Sets the game position root and info dependent on it.
 *
 * @details Resets conters.
 *
 * @param [in,out] ctx  gve solver context
 * @param [in]     root the game position to be evaluated or solved
 * @return              a status value
 */
extern int
gve_context_set_root (gve_context_t ctx,
                      const GamePositionX *root);



#endif /* GAME_VALUE_ESTIMATOR_H */
