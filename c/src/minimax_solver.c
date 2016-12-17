/**
 * @file
 *
 *
 * @todo [done] Remove every dependency on GamePosition and Board objects.
 *
 * @todo [done] Remove pass special case.
 *
 * @todo [done] Avoid malloc/free calls.
 *
 * @todo [done] Use the game_tree_utils stack.
 *
 * @todo [done] Use the most advanced Kogge-Stone routines.
 *
 * @todo [done] Transform the code from recursion to iteration.
 *
 * @todo [done] Refactor the logging system:
 *              - Remove the costly json generation.
 *              - Trasform logging from ASCI to binary.
 *                Write a postprocessor that generate the ASCI/csv from the binary file.
 *                Add the json generation to the postprocessor.
 *
 * @todo [done] Write a new hash algorithm that prepares the delta_hash between two game positions.
 *
 * @todo [done] Inline some of game_position_x_... calls:
 *              - game_position_x_pass
 *              - game_position_x_final_value
 *              - game_position_x_get_player
 *              - game_position_x_get_opponent
 *
 *
 * @brief Minimax solver module implementation.
 * @details It searches the end of the game for an exact outcome using the MINIMAX algorithm.
 *
 * @par minimax_solver.c
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
#include <stdbool.h>
#include <inttypes.h>

#include "game_tree_logger.h"
#include "minimax_solver.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack,
                          const LogEnv *const log_env);



/*
 * Internal variables and constants.
 */



/**
 * @endcond
 */



/**
 * @brief Solves the game position returning a new exact solution pointer.
 *
 * @param [in] root the starting game position to be solved
 * @param [in] env      parameter envelope
 * @return          a pointer to a new exact solution structure
 */
ExactSolution *
game_position_minimax_solve (const GamePositionX *const root,
                             const endgame_solver_env_t *const env)
{
  g_assert(root);
  g_assert(env);

  GameTreeStack *stack = game_tree_stack_new();
  game_tree_stack_init(root, stack);

  LogEnv *const log_env = game_tree_log_init(env->log_file);

  if (log_env->log_is_on) {
    game_tree_log_open_h(log_env);
    stack->hash_is_on = true;
  }

  ExactSolution *result = exact_solution_new();
  exact_solution_set_solved_game_position_x(result, root);

  game_position_solve_impl(result, stack, log_env);

  result->pv[0] = stack->nodes[1].best_move;
  result->outcome = stack->nodes[1].alpha;

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
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack,
                          const LogEnv *const log_env)
{
  NodeInfo *c;
  const NodeInfo *const root = stack->active_node;

 begin:
  result->node_count++;
  c = ++stack->active_node;
  gts_generate_moves(stack);
  if (stack->hash_is_on) gts_compute_hash(stack);
  if (log_env->log_is_on) do_log(result, stack, log_env);
  c->alpha = out_of_range_defeat_score;

  if (gts_is_terminal_node(stack)) {
    result->leaf_count++;
    c->best_move = invalid_move;
    c->alpha = game_position_x_final_value(&c->gpx);
    goto end;
  }
  if (!c->move_set) {
    if (stack->hash_is_on) {
      stack->flip_count = 1;
      *stack->flips = pass_move;
    }
    goto begin;
  }

  for ( ; c->move_cursor < (c + 1)->head_of_legal_move_list; c->move_cursor++) {
    gts_make_move(stack);
    goto begin;
  entry:
    if (- (c + 1)->alpha > c->alpha) {
      c->best_move = *(c->move_cursor);
      c->alpha = - (c + 1)->alpha;
    }
  }

 end:
  c = --stack->active_node;
  if (stack->active_node == root) return;
  goto entry;
}

/**
 * @endcond
 */
