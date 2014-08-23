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
 * @copyright 2013, 2014 Roberto Corradini. All rights reserved.
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
#include "exact_solver.h"
#include "game_tree_logger.h"
#include "random_game_sampler.h"



/*
 * Prototypes for internal functions.
 */

static SearchNode *
game_position_random_sampler_impl (      ExactSolution * const result,
                                   const GamePosition  * const gp);



/*
 * Internal variables and constants.
 */

/**
 * @brief The logging environment structure.
 */
static LogEnv *log_env = NULL;

/**
 * @brief The total number of call to the recursive function that traverse the game DAG.
 */
static uint64_t call_count = 0;

/**
 * @brief The predecessor-successor array of game position hash values.
 */
static uint64_t gp_hash_stack[128];

/**
 * @brief The index of the last entry into gp_hash_stack.
 */
static int gp_hash_stack_fill_point = 0;

/**
 * @brief The sub_run_id used for logging.
 */
static int sub_run_id = 0;



/*********************************************************/
/* Function implementations for the GamePosition entity. */ 
/*********************************************************/

/**
 * @brief Runs a sequence of random games for number of times equal
 * to the `repeats` parameter, starting from the `root` game position.
 *
 * @param [in] root     the starting game position to be solved
 * @param [in] log_file if not null turns logging on the given file name
 * @param [in] repeats  number of random game to play
 * @return              a pointer to a new exact solution structure
 */
ExactSolution *
game_position_random_sampler (const GamePosition * const root,
                              const gchar        * const log_file,
                              const int                  repeats)
{
  ExactSolution *result; 
  SearchNode    *sn;
  int            n;
  
  if (repeats < 1) {
    n = 1;
  } else {
    n = repeats;
  }

  log_env = game_tree_log_init(log_file);

  if (log_env->log_is_on) {
    GamePosition *ground = game_position_new(board_new(root->board->blacks,
                                                       root->board->whites),
                                             player_opponent(root->player));
    gp_hash_stack[0] = game_position_hash(ground);
    game_position_free(ground);
    game_tree_log_open_h(log_env);
  }

  srand(time(NULL));
  
  result = exact_solution_new();
  result->solved_game_position = game_position_clone(root);

  for (int repetition = 0; repetition < n; repetition++) {
    if (log_env->log_is_on) {
      sub_run_id = repetition;
      call_count = 0;
    }
    sn = game_position_random_sampler_impl(result, result->solved_game_position);  
    if (sn) {
      result->pv[0] = sn->move;
      result->outcome = sn->value;
    }
    sn = search_node_free(sn);
  }

  game_tree_log_close(log_env);

  return result;
}



/*
 * Internal functions.
 */

static SearchNode *
game_position_random_sampler_impl (      ExactSolution * const result,
                                   const GamePosition  * const gp)
{
  result->node_count++;
  SearchNode *node = NULL;

  if (log_env->log_is_on) {
    call_count++;
    gp_hash_stack_fill_point++;
    LogDataH log_data;
    log_data.sub_run_id = sub_run_id;
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

  const SquareSet legal_moves = game_position_legal_moves(gp);
  const int legal_move_count = bit_works_popcount(legal_moves);
  if (game_position_has_any_player_any_legal_move(gp)) { // the game must go on
    if (legal_move_count == 0) { // player has to pass
      GamePosition *flipped_players = game_position_pass(gp);
      node = search_node_negated(game_position_random_sampler_impl(result, flipped_players));
      flipped_players = game_position_free(flipped_players);
    } else { // regular move
      const Square random_move = square_set_random_selection(legal_moves);
      GamePosition *next_gp = game_position_make_move(gp, random_move);
      node = search_node_negated(game_position_random_sampler_impl(result, next_gp));
      next_gp = game_position_free(next_gp);
    }
  } else { // game-over
    result->leaf_count++;
    node = search_node_new(pass_move, game_position_final_value(gp));
  }

  if (log_env->log_is_on) {
    gp_hash_stack_fill_point--;
  }

  return node;
}
