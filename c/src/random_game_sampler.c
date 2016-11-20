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
 * @copyright 2013, 2014, 2016 Roberto Corradini. All rights reserved.
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
#include "board.h"
#include "exact_solver.h"
#include "game_tree_logger.h"
#include "random_game_sampler.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static SearchNode *
game_position_random_sampler_impl (ExactSolution *const result,
                                   const GamePosition  *const gp,
                                   RandomNumberGenerator *const rng);



/*
 * Internal variables and constants.
 */

/* The logging environment structure. */
static LogEnv *log_env = NULL;

/* The total number of call to the recursive function that traverse the game DAG. */
static uint64_t call_count = 0;

/* The predecessor-successor array of game position hash values. */
static uint64_t gpx_hash_stack[128];

/* True when has has to be computed. */
static bool compute_hash = false;

/* The index of the last entry into gpx_hash_stack. */
static uint64_t *gpx_hash = gpx_hash_stack;

/* The sub_run_id used for logging. */
static int sub_run_id = 0;

/**
 * @endcond
 */



/*********************************************************/
/* Function implementations for the GamePosition entity. */
/*********************************************************/

/**
 * @brief Runs a sequence of random games for number of times equal
 * to the `repeats` parameter, starting from the `root` game position.
 *
 * @param [in] root the starting game position to be solved
 * @param [in] env  parameter envelope
 * @return          a pointer to a new exact solution structure
 */
ExactSolution *
game_position_random_sampler (const GamePositionX *const root,
                              const endgame_solver_env_t *const env)
{
  g_assert(root);
  g_assert(env);

  ExactSolution *result;
  SearchNode    *sn;
  int            n;

  const GamePosition *const root_gp = game_position_x_gpx_to_gp(root);

  if (env->repeats < 1) {
    n = 1;
  } else {
    n = env->repeats;
  }

  log_env = game_tree_log_init(env->log_file);

  if (log_env->log_is_on) {
    GamePosition *ground = game_position_new(board_new(root_gp->board->blacks,
                                                       root_gp->board->whites),
                                             player_opponent(root_gp->player));
    *gpx_hash++ = game_position_hash(ground);
    game_position_free(ground);
    game_tree_log_open_h(log_env);
    compute_hash = true;
  }

  RandomNumberGenerator *rng = rng_new(rng_random_seed());

  result = exact_solution_new();
  result->solved_game_position = game_position_clone(root_gp);

  for (int repetition = 0; repetition < n; repetition++) {
    if (log_env->log_is_on) {
      sub_run_id = repetition;
      call_count = 0;
    }
    sn = game_position_random_sampler_impl(result, result->solved_game_position, rng);
    if (sn) {
      result->pv[0] = sn->move;
      result->outcome = sn->value;
    }
    search_node_free(sn);
  }

  game_tree_log_close(log_env);
  rng_free(rng);

  return result;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

static SearchNode *
game_position_random_sampler_impl (ExactSolution *const result,
                                   const GamePosition  *const gp,
                                   RandomNumberGenerator *const rng)
{
  result->node_count++;
  SearchNode *node = NULL;

  if (compute_hash) {
    *gpx_hash = game_position_hash(gp);
    gpx_hash++;
  }

  if (log_env->log_is_on) {
    GamePositionX gpx_structure = { .blacks = gp->board->blacks, .whites = gp->board->whites, .player = gp->player };
    GamePositionX *gpx = &gpx_structure;
    call_count++;
    char json_doc[game_tree_log_max_json_doc_len];
    const size_t json_doc_len = game_tree_log_data_h_json_doc3(json_doc, gpx_hash - gpx_hash_stack - 1, gpx);
    LogDataH log_data =
      { .sub_run_id   = sub_run_id,
        .call_id      = result->node_count,
        .hash         = *(gpx_hash - 1),
        .parent_hash  = *(gpx_hash - 2),
        .blacks       = gpx->blacks,
        .whites       = gpx->whites,
        .player       = gpx->player,
        .json_doc     = json_doc,
        .json_doc_len = json_doc_len,
        .call_level   = gpx_hash - gpx_hash_stack - 1 };
    game_tree_log_write_h(log_env, &log_data);
    log_data.json_doc = NULL;
    game_tree_log_write_dat_h(log_env, &log_data);


    /*
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
    */
  }

  const SquareSet legal_moves = game_position_legal_moves(gp);
  const int legal_move_count = bit_works_bitcount_64(legal_moves);
  if (game_position_has_any_player_any_legal_move(gp)) { // the game must go on
    if (legal_move_count == 0) { // player has to pass
      GamePosition *flipped_players = game_position_pass(gp);
      node = search_node_negated(game_position_random_sampler_impl(result, flipped_players, rng));
      game_position_free(flipped_players);
    } else { // regular move
      const Square random_move = square_set_random_selection(rng, legal_moves);
      GamePosition *next_gp = game_position_make_move(gp, random_move);
      node = search_node_negated(game_position_random_sampler_impl(result, next_gp, rng));
      game_position_free(next_gp);
    }
  } else { // game-over
    result->leaf_count++;
    node = search_node_new(pass_move, game_position_final_value(gp));
  }

  if (compute_hash) gpx_hash--;

  return node;
}

/**
 * @endcond
 */
