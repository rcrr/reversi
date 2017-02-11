/**
 * @file
 *
 * @brief Random game sampler module implementation.
 * @details Used to generate sample game tree.
 *
 * @par random_game_sampler.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014, 2016, 2017 Roberto Corradini. All rights reserved.
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

#include <stdio.h>
#include <stdlib.h>

#include <glib.h>

#include "board.h"
#include "prng.h"
#include "exact_solver.h"
#include "game_tree_logger.h"
#include "random_game_sampler.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
game_position_random_sammpler_impl (ExactSolution *const result,
                                    GameTreeStack *const stack,
                                    const LogEnv *const log_env,
                                    prng_mt19937_t *const prng,
                                    const unsigned long int sub_run_id);



/*
 * Internal variables and constants.
 */

/**
 * @endcond
 */



/*
 * Public functions.
 */

/**
 * @brief Runs a sequence of random games for number of times equal
 * to the `repeats` parameter, starting from the `root` game position.
 *
 * @param [in] root the starting game position to be solved
 * @param [in] env  parameter envelope
 * @return          a pointer to a new exact solution structure
 */
ExactSolution *
game_position_random_sampler__ (const GamePositionX *const root,
                                const endgame_solver_env_t *const env)
{
  g_assert(root);
  g_assert(env);

  LogEnv *const log_env = game_tree_log_init(env->log_file);

  if (log_env->log_is_on) game_tree_log_open_h(log_env);

  prng_mt19937_t *prng = prng_mt19937_new();
  unsigned long int n_run = 1;
  prng_mt19937_init_by_seed(prng, prng_uint64_from_clock_random_seed());
  if (env->repeats < 1) {
    n_run = 1;
  } else {
    n_run = env->repeats;
  }

  ExactSolution *result = NULL;
  GameTreeStack *stack = game_tree_stack_new();
  for (unsigned long int sub_run_id = 0; sub_run_id < n_run; sub_run_id++) {

    game_tree_stack_init(root, stack);
    if (log_env->log_is_on) stack->hash_is_on = true;

    result = exact_solution_new();
    exact_solution_set_solved_game_position_x(result, root);

    game_position_random_sammpler_impl(result, stack, log_env, prng, sub_run_id);

    result->pv[0] = stack->nodes[1].best_move;
    result->outcome = stack->nodes[1].alpha;

    if (sub_run_id != n_run - 1) exact_solution_free(result);
  }

  prng_mt19937_free(prng);
  game_tree_stack_free(stack);
  game_tree_log_close(log_env);

  return result;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

static void
game_position_random_sammpler_impl (ExactSolution *const result,
                                    GameTreeStack *const stack,
                                    const LogEnv *const log_env,
                                    prng_mt19937_t *const prng,
                                    const unsigned long int sub_run_id)
{
  NodeInfo *c;
  const NodeInfo *const root = stack->active_node;

  while (true) {
    result->node_count++;
    c = ++stack->active_node;

    gts_generate_moves(stack);
    prng_mt19937_shuffle_array_uint8(prng, c->head_of_legal_move_list, c->move_count);
    if (stack->hash_is_on) gts_compute_hash(stack);
    if (log_env->log_is_on) do_log(result, stack, sub_run_id, log_env);

    if (gts_is_terminal_node(stack)) {
      result->leaf_count++;
      c->best_move = invalid_move;
      c->alpha = game_position_x_final_value(&c->gpx);
      goto unroll;
    } else if (!c->move_set) {
      if (stack->hash_is_on) {
        stack->flip_count = 1;
        *stack->flips = pass_move;
      }
    } else {
      gts_make_move(stack);
    }
  }

 unroll:
  while (true) {
    c = --stack->active_node;
    if (stack->active_node == root) return;
    c->alpha = - (c + 1)->alpha;
  }
}

/**
 * @endcond
 */
