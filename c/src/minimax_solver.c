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
 * @copyright 2014 Roberto Corradini. All rights reserved.
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

#include "game_tree_logger.h"
#include "minimax_solver.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static SearchNode *
game_position_solve_impl (ExactSolution *const result,
                          const GamePosition *const gp);



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
 * @param [in] log_file if not null turns logging on the given file name
 * @return          a pointer to a new exact solution structure
 */
ExactSolution *
game_position_minimax_solve (const GamePosition *const root,
                             const gchar *const log_file)
{
  log_env = game_tree_log_init(log_file);

  if (log_env->log_is_on) {
    GamePosition *ground = game_position_new(board_new(root->board->blacks,
                                                       root->board->whites),
                                             player_opponent(root->player));
    gp_hash_stack[0] = game_position_hash(ground);
    game_position_free(ground);
    game_tree_log_open_h(log_env);
  }

  ExactSolution *result = exact_solution_new();

  result->solved_game_position = game_position_clone(root);

  SearchNode *sn = game_position_solve_impl(result, result->solved_game_position);

  result->pv[0] = sn->move;
  result->outcome = sn->value;
  sn = search_node_free(sn);

  game_tree_log_close(log_env);

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
 * @param [in] result a reference to the exact solution data structure
 * @param [in] gp     the game position to traverse
 * @return            a pointer to a new serch node structure
 */
static SearchNode *
game_position_solve_impl (ExactSolution *const result,
                          const GamePosition *const gp)
{
  SearchNode *node  = NULL;
  SearchNode *node2 = NULL;

  result->node_count++;

  const SquareSet moves = game_position_legal_moves(gp);

  if (log_env->log_is_on) {
    call_count++;
    gp_hash_stack_fill_point++;
    LogDataH log_data;
    log_data.sub_run_id = 0;
    log_data.call_id = call_count;
    log_data.hash = game_position_hash(gp);
    gp_hash_stack[gp_hash_stack_fill_point] = log_data.hash;
    log_data.parent_hash = gp_hash_stack[gp_hash_stack_fill_point - 1];
    log_data.blacks = (gp->board)->blacks;
    log_data.whites = (gp->board)->whites;
    log_data.player = gp->player;
    gchar *json_doc = game_tree_log_data_h_json_doc(gp_hash_stack_fill_point, gp);
    log_data.json_doc = json_doc;
    game_tree_log_write_h(log_env, &log_data);
    g_free(json_doc);
  }

  if (moves == empty_square_set) {
    GamePosition *flipped_players = game_position_pass(gp);
    if (game_position_has_any_legal_move(flipped_players)) {
      node = search_node_negated(game_position_solve_impl(result, flipped_players));
    } else {
      result->leaf_count++;
      node = search_node_new(pass_move, game_position_final_value(gp));
    }
    flipped_players = game_position_free(flipped_players);
  } else {
    node = search_node_new(-1, -65);
    SquareSet remaining_moves = moves;
    while (remaining_moves) {
      const Square move = bit_works_bitscanLS1B_64(remaining_moves);
      remaining_moves ^= 1ULL << move;
      GamePosition *gp2 = game_position_make_move(gp, move);
      node2 = search_node_negated(game_position_solve_impl(result, gp2));
      gp2 = game_position_free(gp2);
      if (node2->value > node->value) {
        search_node_free(node);
        node = node2;
        node->move = move;
        node2 = NULL;
      } else {
        node2 = search_node_free(node2);
      }
    }
  }

  if (log_env->log_is_on) {
    gp_hash_stack_fill_point--;
  }

  return node;
}

/**
 * @endcond
 */
