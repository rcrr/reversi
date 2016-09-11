/**
 * @file
 *
 * @brief Random alpha-beta solver module implementation.
 * @details It searches the end of the game for an exact outcome using the Alpha-Beta algorithm.
 * Moves are sorted by a random criteria.
 *
 * @par rab_solver.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014 2016 Roberto Corradini. All rights reserved.
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

#include "random.h"
#include "game_tree_logger.h"

#include "rab_solver.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
game_position_solve_impl (ExactSolution* const result,
                          GameTreeStack* const stack,
                          const int sub_run_id);



/*
 * Internal variables and constants.
 */

/* The logging environment structure. */
static LogEnv *log_env = NULL;

/**
 * @endcond
 */



/*********************************************************/
/* Function implementations for the GamePosition entity. */
/*********************************************************/

/**
 * @brief Solves the game position returning a new exact solution pointer.
 *
 * @param [in] root the starting game position to be solved
 * @param [in] env  parameter envelope
 * @return          a pointer to a new exact solution structure
 */
ExactSolution *
game_position_rab_solve (const GamePosition *const root,
                         const endgame_solver_env_t *const env)
{
  g_assert(root);
  g_assert(env);

  ExactSolution* result = NULL;
  int n;

  if (env->repeats < 1) {
    n = 1;
  } else {
    n = env->repeats;
  }

  log_env = game_tree_log_init(env->log_file);

  if (log_env->log_is_on) {
    game_tree_log_open_h(log_env);
  }

  random_init_seed();

  int game_value = out_of_range_defeat_score;
  Square best_move = invalid_move;

  for (int sub_run_id = 0; sub_run_id < n; sub_run_id++) {
    GameTreeStack* stack = game_tree_stack_new();

    game_tree_stack_init(root, stack);
    NodeInfo* first_node_info = &stack->nodes[1];

    result = exact_solution_new();
    result->solved_game_position = game_position_clone(root);

    game_position_solve_impl(result, stack, sub_run_id);

    best_move = first_node_info->best_move;
    game_value = first_node_info->alpha;

    game_tree_stack_free(stack);
  }

  game_tree_log_close(log_env);

  result->pv[0] = best_move;
  result->outcome = game_value;
  return result;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

/**
 * @brief Recursive function used to traverse the game tree.
 *
 * @param [in,out] result     a reference to the exact solution data structure
 * @param [in,out] stack      it holds mostly of the status of the iterative search
 * @param [in]     sub_run_id used to tag the log file with the run counter
 */
static void
game_position_solve_impl (ExactSolution* const result,
                          GameTreeStack* const stack,
                          const int sub_run_id)
{
  result->node_count++;

  const int current_fill_index = stack->fill_index;
  const int next_fill_index = current_fill_index + 1;
  const int previous_fill_index = current_fill_index - 1;

  stack->fill_index++;

  NodeInfo* const current_node_info = &stack->nodes[current_fill_index];
  NodeInfo* const next_node_info = &stack->nodes[next_fill_index];
  NodeInfo* const previous_node_info = &stack->nodes[previous_fill_index];
  const GamePositionX* const current_gpx = &current_node_info->gpx;
  GamePositionX* const next_gpx = &next_node_info->gpx;
  current_node_info->hash = game_position_x_hash(current_gpx);
  const SquareSet move_set = game_position_x_legal_moves(current_gpx);
  game_tree_move_list_from_set(move_set, current_node_info, next_node_info);
  random_shuffle_array_uint8(current_node_info->head_of_legal_move_list, current_node_info->move_count);

  if (log_env->log_is_on) {
    LogDataH log_data;
    log_data.sub_run_id = sub_run_id;
    log_data.call_id = result->node_count;
    log_data.hash = current_node_info->hash;
    log_data.parent_hash = previous_node_info->hash;
    log_data.blacks = current_gpx->blacks;
    log_data.whites = current_gpx->whites;
    log_data.player = current_gpx->player;
    log_data.json_doc = "\"{}\"";
    game_tree_log_write_h(log_env, &log_data);
  }

  if (move_set == empty_square_set) {
    const int previous_move_count = previous_node_info->move_count;
    const SquareSet empties = game_position_x_empties(current_gpx);
    if (empties != empty_square_set && previous_move_count != 0) {
      game_position_x_pass(current_gpx, next_gpx);
      next_node_info->alpha = -current_node_info->beta;
      next_node_info->beta = -current_node_info->alpha;
      game_position_solve_impl(result, stack, sub_run_id);
      current_node_info->alpha = -next_node_info->alpha;
      current_node_info->best_move = next_node_info->best_move;
    } else {
      result->leaf_count++;
      current_node_info->alpha = game_position_x_final_value(current_gpx);
      current_node_info->best_move = invalid_move;
    }
  } else {
    current_node_info->alpha = out_of_range_defeat_score;
    for (int i = 0; i < current_node_info->move_count; i++) {
      const Square move = * (current_node_info->head_of_legal_move_list + i);
      game_position_x_make_move(current_gpx, move, next_gpx);
      next_node_info->alpha = -current_node_info->beta;
      next_node_info->beta = -current_node_info->alpha;
      game_position_solve_impl(result, stack, sub_run_id);
      if (-next_node_info->alpha > current_node_info->alpha) {
        current_node_info->alpha = -next_node_info->alpha;
        current_node_info->best_move = move;
        if (current_node_info->alpha >= current_node_info->beta) {
          goto out;
        }
      }
    }
  }
out:
  stack->fill_index--;
  return;
}

/**
 * @endcond
 */
