/**
 * @file
 *
 * @brief Endgame utils module definitions.
 *
 * @par endgame_utils.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2015, 2016, 2017, 2018, 2021 Roberto Corradini. All rights reserved.
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

#ifndef ENDGAME_UTILS_H
#define ENDGAME_UTILS_H

#include "cfg.h"
#include "game_tree_utils.h"


/**
 * @brief The endgame solver environment collects all the field neaded to call the different solvers.
 *
 * @details Each solver needs shared or specific arguments in order to be started. This structure is
 *          an envelope that collects all of them.
 *          In this way the call to the solver is standardized by a single signature.
 */
typedef struct {
  char                   *log_file;            /**< @brief When not NULL turns on logging. It defines the log file name prefix. */
  char                   *pve_dump_file;       /**< @brief PVE dump file name and path. */
  unsigned long long int  repeats;             /**< @brief The number of repetitions for the random sampler. */
  bool                    pv_recording;        /**< @brief Turns on the principal variation recording. */
  bool                    pv_full_recording;   /**< @brief Drives the logic governing game tree pruning to consider the branches with equal value. */
  bool                    pv_no_print;         /**< @brief Turns off the PV variants printing when `pv_full_recording` is `true`. */
  int                     board_pattern_index; /**< @brief Defines the pattern id. Used by the rand solver when option -P is active. */
  bool                    prng_seed_is_set;    /**< @brief When true the prng_seed has been defined. */
  uint64_t                prng_seed;           /**< @brief Pseudo Random Number Generator seed. */
  int                     alpha;               /**< @brief Lower bound of the search window. */
  int                     beta;                /**< @brief Upper bound of the search window. */
  bool                    all_moves;           /**< @brief Search the value of all moves. */
  int                     search_depth;        /**< @brief Search depth. */
  cfg_t                  *cfg;                 /**< @brief Configuration key/value pairs. */
} endgame_solver_env_t;

/**
 * @brief Function pointer type for solving a game position.
 *
 * @details This prototype receives a game position and a set of
 *          arguments collected by the env parameter.
 *
 * @param [in] gpx the position to solve
 * @param [in] env arguments and parameters for the solver
 */
typedef ExactSolution *
(*endgame_solver_f) (const GamePositionX *const gpx,
                     const endgame_solver_env_t *const env);



#endif /* ENDGAME_UTILS_H */
