/**
 * @file
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

#include <glib.h>

#include "game_tree_logger.h"
#include "minimax_solver.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static int
game_position_solve_impl (ExactSolution *const result,
                          Square *best_move,
                          const GamePositionX *const gpx);



/*
 * Internal variables and constants.
 */

/* The logging environment structure. */
static LogEnv *log_env = NULL;

/* The total number of call to the recursive function that traverse the game DAG. */
static uint64_t call_count = 0;

/* The predecessor-successor array of game position hash values. */
static uint64_t gp_hash_stack[128];

/* The index of the last entry into gp_hash_stack. */
static int gp_hash_stack_fill_point = 0;

/* The sub_run_id used for logging. */
static const int sub_run_id = 0;

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
    GamePositionX ground = { .blacks = root->blacks, .whites = root->whites, .player = player_opponent(root->player) };
    gp_hash_stack[0] = game_position_x_hash(&ground);
    game_tree_log_open_h(log_env);
  }

  ExactSolution *result = exact_solution_new();

  exact_solution_set_solved_game_position_x(result, root);
  Square best_move;
  int v = game_position_solve_impl(result, &best_move, root);
  result->pv[0] = best_move;
  result->outcome = v;

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
      const Square m = bit_works_bitscanLS1B_64(remaining_moves);
      remaining_moves ^= 1ULL << m;
      *move++ = m;
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
           GamePositionX *const updated)
{
  if (move == pass_move) {
    game_position_x_pass(current, updated);
  } else {
    game_position_x_make_move(current, move, updated);
  }
}

/*
 * TODO:
 * -        Remove every dependency on GamePosition and Board objects.
 * - [done] Remove pass special case.
 * - [done] Avoid malloc/free calls.
 * -        Use the game_tree_utils stack.
 * - [done] Use the most advanced Kogge-Stone routines.
 * -        Transform the code from recursion to iteration.
 */
static int
game_position_solve_impl (ExactSolution *const result,
                          Square *best_move,
                          const GamePositionX *const gpx)
{
  Square *m;
  Square moves[32];
  Square child_best_move;
  int best_value, v;
  SquareSet move_set;
  int move_count;
  GamePositionX child_gpx;

  result->node_count++;

  generate_move_array(&move_count, moves, &move_set, gpx);

  if (log_env->log_is_on) {
    call_count++;
    gp_hash_stack_fill_point++;
    LogDataH log_data;
    log_data.sub_run_id = 0;
    log_data.call_id = call_count;
    log_data.hash = game_position_x_hash(gpx);
    gp_hash_stack[gp_hash_stack_fill_point] = log_data.hash;
    log_data.parent_hash = gp_hash_stack[gp_hash_stack_fill_point - 1];
    log_data.blacks = gpx->blacks;
    log_data.whites = gpx->whites;
    log_data.player = gpx->player;
    gchar *json_doc = game_tree_log_data_h_json_doc2(gp_hash_stack_fill_point, gpx);
    log_data.json_doc = json_doc;
    game_tree_log_write_h(log_env, &log_data);
    g_free(json_doc);
  }

  best_value = out_of_range_defeat_score;
  *best_move = invalid_move;

  if (is_terminal_node(gpx, move_set)) {
    result->leaf_count++;
    best_value = game_position_x_final_value(gpx);
    goto end;
  }

  for (m = moves; m - moves < move_count; m++) {
    make_move(gpx, *m, &child_gpx);
    v = - game_position_solve_impl(result, &child_best_move, &child_gpx);
    if (v > best_value) {
      *best_move = *m;
      best_value = v;
    }
  }

 end:
  if (log_env->log_is_on) {
    gp_hash_stack_fill_point--;
  }
  return best_value;
}

/**
 * @endcond
 */
