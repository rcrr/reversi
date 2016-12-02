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
 * @todo        Transform the code from recursion to iteration.
 *
 * @todo [done] Refactor the logging system:
 *              - Remove the costly json generation.
 *              - Trasform logging from ASCI to binary.
 *                Write a postprocessor that generate the ASCI/csv from the binary file.
 *                Add the json generation to the postprocessor.
 *
 * @todo [done] Write a new hash algorithm that prepares the delta_hash between two game positions.
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
                          GameTreeStack *const stack);



/*
 * Internal variables and constants.
 */

/* The logging environment structure. */
static LogEnv *log_env = NULL;

/* True when hash has to be computed. */
static bool compute_hash = false;

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

  log_env = game_tree_log_init(env->log_file);

  if (log_env->log_is_on) {
    game_tree_log_open_h(log_env);
    compute_hash = true;
  }

  GameTreeStack *stack = game_tree_stack_new();
  game_tree_stack_init(root, stack);

  ExactSolution *result = exact_solution_new();
  exact_solution_set_solved_game_position_x(result, root);

  game_position_solve_impl(result, stack);

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
generate_move_array (int *move_count,
                     Square *moves,
                     SquareSet *move_set,
                     const GamePositionX *const gpx)
{
  Square *move = moves;
  *move_set = game_position_x_legal_moves(gpx);
  SquareSet remaining_moves = *move_set;
  if (!remaining_moves) {
    *move++ = pass_move;
  } else {
    while (remaining_moves) {
      const Square m = bit_works_bitscanLS1B_64_bsf(remaining_moves);
      *move++ = m;
      remaining_moves = bit_works_reset_lowest_bit_set_64_blsr(remaining_moves);
    }
  }
  *move_count = move - moves;
}

static bool
is_terminal_node (const GamePositionX *const current,
                  const SquareSet move_set)
{
  if (move_set) return false;
  const SquareSet empties = ~(current->blacks | current->whites);
  if (!empties) return true;
  GamePositionX next;
  game_position_x_pass(current, &next);
  const SquareSet next_legal_moves = game_position_x_legal_moves(&next);
  if (next_legal_moves) return false;
  return true;
}

static void
make_move (const GamePositionX *const current,
           const Square move,
           GamePositionX *const updated,
           Square *flips,
           uint8_t *flip_count)
{
  flips[0] = move;
  *flip_count = 1;
  if (move == pass_move) {
    game_position_x_pass(current, updated);
  } else {
    game_position_x_make_move(current, move, updated);
    if (compute_hash) {
      const SquareSet bitmove = 1ULL << move;
      const SquareSet cu_p = game_position_x_get_player(current);
      const SquareSet up_o = game_position_x_get_opponent(updated);
      SquareSet flip_set = up_o & ~(cu_p | bitmove);
      while (flip_set) {
        const Square f = bit_works_bitscanLS1B_64_bsf(flip_set);
        flip_set ^= 1ULL << f;
        flips[(*flip_count)++] = f;
      }
    }
  }
}

static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack)
{
  Square *m;
  Square moves[32];
  SquareSet move_set;
  int move_count;

  result->node_count++;

  const int current_fill_index = stack->fill_index;
  const int next_fill_index = current_fill_index + 1;
  const int previous_fill_index = current_fill_index - 1;

  stack->fill_index++;

  NodeInfo *const current_node_info = &stack->nodes[current_fill_index];
  NodeInfo *const next_node_info = &stack->nodes[next_fill_index];
  NodeInfo *const previous_node_info = &stack->nodes[previous_fill_index];

  generate_move_array(&move_count, moves, &move_set, &current_node_info->gpx);

  if (compute_hash) {
    current_node_info->hash = game_position_x_delta_hash(previous_node_info->hash,
                                                         stack->flips,
                                                         stack->flip_count,
                                                         current_node_info->gpx.player);
  }

  if (log_env->log_is_on) {
    LogDataH log_data =
      { .sub_run_id   = game_tree_log_def_sub_run_id,
        .call_id      = result->node_count,
        .hash         = current_node_info->hash,
        .parent_hash  = previous_node_info->hash,
        .blacks       = current_node_info->gpx.blacks,
        .whites       = current_node_info->gpx.whites,
        .player       = current_node_info->gpx.player,
        .json_doc     = NULL,
        .json_doc_len = 0,
        .call_level   = current_fill_index };
    game_tree_log_write_h(log_env, &log_data);
  }

  current_node_info->alpha = out_of_range_defeat_score;
  current_node_info->best_move = invalid_move;

  if (is_terminal_node(&current_node_info->gpx, move_set)) {
    result->leaf_count++;
    current_node_info->alpha = game_position_x_final_value(&current_node_info->gpx);
    goto end;
  }

  for (m = moves; m - moves < move_count; m++) {
    make_move(&current_node_info->gpx, *m, &next_node_info->gpx, stack->flips, &stack->flip_count);
    game_position_solve_impl(result, stack);
    const int value = - next_node_info->alpha;
    if (value > current_node_info->alpha) {
      current_node_info->best_move = *m;
      current_node_info->alpha = value;
    }
  }

 end:
  stack->fill_index--;
  return;
}

/**
 * @endcond
 */
