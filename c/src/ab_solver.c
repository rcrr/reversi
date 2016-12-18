/**
 * @file
 *
 * @brief Alpha-beta solver module implementation.
 * @details It searches the end of the game for an exact outcome using the Alpha-Beta algorithm.
 * Moves are sorted by their natural order, from A1 to H8 (A1, B1, C1, ... G8, H8).
 *
 * @par ab_solver.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014, 2016 Roberto Corradini. All rights reserved.
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

#include "ab_solver.h"


/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack);



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
game_position_ab_solve (const GamePositionX *const root,
                        const endgame_solver_env_t *const env)
{
  g_assert(root);
  g_assert(env);

  const GamePosition *const root_gp = game_position_x_gpx_to_gp(root);

  log_env = game_tree_log_init(env->log_file);

  if (log_env->log_is_on) {
    game_tree_log_open_h(log_env);
  }

  int game_value = out_of_range_defeat_score;
  Square best_move = invalid_move;

  GameTreeStack *stack = game_tree_stack_new();

  game_tree_stack_init(root, stack);
  NodeInfo *first_node_info = &stack->nodes[1];

  ExactSolution *result = exact_solution_new();
  result->solved_game_position = game_position_clone(root_gp);

  game_position_solve_impl(result, stack);

  best_move = first_node_info->best_move;
  game_value = first_node_info->alpha;

  result->pv[0] = best_move;
  result->outcome = game_value;

  game_tree_stack_free(stack);

  game_tree_log_close(log_env);

  return result;
}



/**
 * Internal functions.
 *
 * @cond
 */

/**
 * @brief Recursive function used to traverse the game tree.
 *
 * @param [in] result  a reference to the exact solution data structure
 * @param [in] stack   a reference to the stack structure
 */
static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack)
{
  result->node_count++;

  NodeInfo *const current_node_info = ++stack->active_node;
  NodeInfo *const next_node_info = current_node_info + 1;
  NodeInfo *const previous_node_info = current_node_info - 1;

  const GamePositionX *const current_gpx = &current_node_info->gpx;
  GamePositionX *const next_gpx = &next_node_info->gpx;
  current_node_info->hash = game_position_x_hash(current_gpx);
  const SquareSet move_set = game_position_x_legal_moves(current_gpx);
  game_tree_move_list_from_set(move_set, current_node_info);

  if (log_env->log_is_on) {
    LogDataH log_data = {
      .sub_run_id = game_tree_log_def_sub_run_id,
      .call_id = result->node_count,
      .hash = current_node_info->hash,
      .parent_hash = previous_node_info->hash,
      .blacks = current_gpx->blacks,
      .whites = current_gpx->whites,
      .player = current_gpx->player,
      .json_doc = "\"{}\"",
      .json_doc_len = 4,
      .call_level = stack->active_node - stack->nodes
    };
    game_tree_log_write_h(log_env, &log_data);
  }

  if (move_set == empty_square_set) {
    //const int previous_move_count = previous_node_info->move_count;
    //const SquareSet empties = game_position_x_empties(current_gpx);
    //if (empties != empty_square_set && previous_move_count != 0) {
    if (game_position_x_has_any_player_any_legal_move(current_gpx)) {
      game_position_x_pass(current_gpx, next_gpx);
      next_node_info->alpha = -current_node_info->beta;
      next_node_info->beta = -current_node_info->alpha;
      game_position_solve_impl(result, stack);
      current_node_info->alpha = -next_node_info->alpha;
      current_node_info->best_move = next_node_info->best_move;
    } else {
      result->leaf_count++;
      current_node_info->alpha = game_position_x_final_value(current_gpx);
      current_node_info->best_move = invalid_move;
    }
  } else {
    //current_node_info->alpha = out_of_range_defeat_score;
    for (int i = 0; i < current_node_info->move_count; i++) {
      const Square move = * (current_node_info->head_of_legal_move_list + i);
      game_position_x_make_move(current_gpx, move, next_gpx);
      next_node_info->alpha = -current_node_info->beta;
      next_node_info->beta = -current_node_info->alpha;
      game_position_solve_impl(result, stack);
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
  stack->active_node--;
  return;
}

/**
 * @endcond
 */
