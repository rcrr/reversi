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
static bool hash_is_on = false;

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
    hash_is_on = true;
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
generate_moves (NodeInfo* const c)
{
  uint8_t *const holml = c->head_of_legal_move_list;
  c->move_set = game_position_x_legal_moves(&c->gpx);
  c->move_cursor = holml;
  SquareSet remaining_moves = c->move_set;
  c->move_count = 0;
  if (!remaining_moves) {
    *(c->move_cursor)++ = pass_move;
  } else {
    while (remaining_moves) {
      *(c->move_cursor)++ = bit_works_bitscanLS1B_64_bsf(remaining_moves);
      remaining_moves = bit_works_reset_lowest_bit_set_64_blsr(remaining_moves);
    }
  }
  c->move_count = c->move_cursor - holml;
  (c + 1)->head_of_legal_move_list = c->move_cursor;
  c->move_cursor = holml;
}

static bool
is_terminal_node (NodeInfo* const c)
{
  const GamePositionX *const current = &c->gpx;
  const SquareSet move_set =  c->move_set;
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
    if (hash_is_on) {
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
compute_hash (NodeInfo* const c,
              const GameTreeStack *const stack)
{
  c->hash = game_position_x_delta_hash((c - 1)->hash,
                                       stack->flips,
                                       stack->flip_count,
                                       c->gpx.player);
}

static void
do_log (const ExactSolution *const result,
        const NodeInfo* const node,
        const GameTreeStack *const stack)
{
  LogDataH log_data =
    { .sub_run_id   = game_tree_log_def_sub_run_id,
      .call_id      = result->node_count,
      .hash         = node->hash,
      .parent_hash  = (node - 1)->hash,
      .blacks       = node->gpx.blacks,
      .whites       = node->gpx.whites,
      .player       = node->gpx.player,
      .json_doc     = NULL,
      .json_doc_len = 0,
      .call_level   = stack->active_node - stack->nodes };
  game_tree_log_write_h(log_env, &log_data);
}

static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack)
{
  NodeInfo *c;
  const NodeInfo *const root = stack->active_node;

 begin:
  result->node_count++;

  c = ++stack->active_node;

  generate_moves(c);

  if (hash_is_on) compute_hash(c, stack);

  if (log_env->log_is_on) do_log(result, c, stack);

  c->alpha = out_of_range_defeat_score;
  c->best_move = invalid_move;

  if (is_terminal_node(c)) {
    result->leaf_count++;
    c->alpha = game_position_x_final_value(&c->gpx);
    goto end;
  }

  for ( ; c->move_cursor < (c + 1)->head_of_legal_move_list; c->move_cursor++) {
    make_move(&c->gpx, *(c->move_cursor), &(c + 1)->gpx, stack->flips, &stack->flip_count);
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
