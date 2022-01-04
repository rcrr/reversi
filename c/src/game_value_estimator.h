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

#include "rglm_utils.h"
#include "transposition_table.h"
#include "endgame_utils.h"

/**
 * @brief Empty Count Size - [0..60] the game stages ...
 */
#define EC_SIZE 61

//typedef struct model_weights_data_s model_weights_data_t;

/**
 * @brief Collects all the info related to the Model Weights used by the GVE solver.
 *
 * @details To be completed ...
 *
 */
typedef struct model_weights_data_s {
  rglmdf_model_weights_t mws_s[EC_SIZE]; /**< Array of model weights structures. */
  rglmdf_model_weights_t *mws[EC_SIZE];  /**< Pointers to the mw structures. A NULL value signals a missing data file. */
  char *file_names[EC_SIZE];             /**< File names for the model weights data. */
  bool verbose_loader;                   /**< When true the file loading process is verbose. */
  bool check_digest;                     /**< When true the data files are checked against the SHA hash. */
  int model_weight_count;                /**< Count of model weight instances declared in the config file. */
  int max_model_weight;                  /**< Maximum empty count value found in the model weighs array. */
  int min_model_weight;                  /**< Minimum empty count value found in the model weighs array.*/
} model_weights_data_t;

/**
 * @brief Collects all the info required by the GVE solver to run.
 *
 * @details The structure is prepared by a call to gve_context_init().
 *          Multiple call to
 *
 */
typedef struct gve_context_s {
  int root_empty_count;                  /**< Empty count at the root node. */
  model_weights_data_t mwd;              /**< Model weights data. */
  uint64_t gp_evaluations[EC_SIZE];      /**< The count of game evaluations categorized by empty count. */
  int id_min_empty_count;                /**< Iterative deepening minimum empty count. */
  int id_step;                           /**< Iterative deepening step. */
  int id_search_depth;                   /**< Search depth iterative deepening. */
  int id_limit;                          /**< Limit of the iterative search depth (maximum id depth). */
  int search_depth;                      /**< Search depth. */
  int search_depth_initial_gap;          /**< It is the difference between root empty count and the ec at the first available model weigths. */
  ttab_t ttab;                           /**< Transposition Table. */
  int ttab_log_size;                     /**< Transposition Table binary logarithm of size. */
  int ttab_log_verbosity;                /**< Transposition Table log verbosity. */
  uint64_t node_count;                   /**< Count of nodes touchd by the algorithm. */
  uint64_t leaf_count;                   /**< Count of leafs touchd by the algorithm. */
  int first_level_evaluation;            /**< This is the empty count level where we start having model weight info available. */
  int last_level_evaluation;             /**< It is the empty count value at the deepest evaluation done using the model weights heuristic. */
  int game_position_evaluation_summary;  /**< Game position evaluation summary. */
  int gve_solver_log_level;              /**< Log level for the solver. */
} gve_context_t;

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
game_position_value_estimator (const GamePositionX *const root,
                               const endgame_solver_env_t *const env);

/**
 * @brief To be completed.
 *
 * @details To be completed.
 *
 * @param [in,out] ctx  gve solver context
 * @param [in]     env  endgame solver environment
 * @param [in]     root the game position to be evaluated or solved
 * @return              a pointer to a new exact solution structure
 */
extern ExactSolution *
game_position_gve_solve (gve_context_t *ctx,
                         const endgame_solver_env_t *const env,
                         const GamePositionX *const root);

/**
 * @brief To be completed.
 *
 * @details To be completed.
 *
 * @param [out] ctx  gve solver context
 * @param [in]  env  endgame solver environment
 * @param [in]  root the game position to be evaluated or solved
 * @return           a status value
 */
extern int
gve_context_init (gve_context_t *ctx,
                  const endgame_solver_env_t *env,
                  const GamePositionX *root);

/**
 * @brief To be completed.
 *
 * @details To be completed.
 *
 * @param [in,out] ctx  gve solver context
 */
extern void
gve_context_release (gve_context_t *ctx);

/**
 * @brief
 *
 * @details Sets values for the following fields:
 *           - root_empty_count
 *           - first_level_evaluation
 *           - last_level_evaluation
 *           - search_depth_initial_gap
 *           - id_limit
 *
 * @param [in,out] ctx  gve solver context
 * @param [in]     root the game position to be evaluated or solved
 * @return              a status value
 */
extern int
gve_context_set_root (gve_context_t *ctx,
                      const GamePositionX *root);



#endif /* GAME_VALUE_ESTIMATOR_H */
