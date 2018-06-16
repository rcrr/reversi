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
 * @copyright 2014, 2016, 2017 Roberto Corradini. All rights reserved.
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
#include <assert.h>

#include "game_tree_logger.h"
#include "board_pattern.h"
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
                          const gtl_log_env_t *const log_env,
                          const bool alpha_beta_pruning,
                          const bool randomize_move_order,
                          prng_mt19937_t *const prng,
                          const unsigned long int sub_run_id);

static void
game_position_random_sammpler_impl (ExactSolution *const result,
                                    GameTreeStack *const stack,
                                    const gtl_log_env_t *const log_env,
                                    prng_mt19937_t *const prng,
                                    const unsigned long int sub_run_id,
                                    const board_pattern_t *board_pattern,
                                    uint64_t *pattern_index_frequencies);

static ExactSolution *
game_position_mab_solve (const GamePositionX *const root,
                         const endgame_solver_env_t *const env,
                         const bool alpha_beta_pruning,
                         const bool randomize_move_order,
                         const bool random_sampler);

static void
compute_and_store_pattern_indexes (NodeInfo *c,
                                   const board_pattern_t *board_pattern,
                                   uint64_t *pattern_index_frequencies);



/*
 * Internal variables and constants.
 */

static size_t size_of_pattern_index_array = 0;

static const size_t size_of_empty_count_array = 61;


/**
 * @endcond
 */



/*
 * Public functions.
 */

/**
 * @brief Solves the game position returning a new exact solution pointer.
 *
 * @param [in] root the starting game position to be solved
 * @param [in] env  parameter envelope
 * @return          a pointer to a new exact solution structure
 */
ExactSolution *
game_position_minimax_solve (const GamePositionX *const root,
                             const endgame_solver_env_t *const env)
{
  assert(root);
  assert(env);

  const bool ab_pruning = false;
  const bool randomize_move_order = false;
  const bool random_sampler = false;
  return game_position_mab_solve(root, env, ab_pruning, randomize_move_order, random_sampler);
}

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
  assert(root);
  assert(env);

  const bool ab_pruning = true;
  const bool randomize_move_order = false;
  const bool random_sampler = false;
  return game_position_mab_solve(root, env, ab_pruning, randomize_move_order, random_sampler);
}

/**
 * @brief Solves the game position returning a new exact solution pointer.
 *
 * @param [in] root the starting game position to be solved
 * @param [in] env  parameter envelope
 * @return          a pointer to a new exact solution structure
 */
ExactSolution *
game_position_rab_solve (const GamePositionX *const root,
                         const endgame_solver_env_t *const env)
{
  assert(root);
  assert(env);

  const bool ab_pruning = true;
  const bool randomize_move_order = true;
  const bool random_sampler = false;
  return game_position_mab_solve(root, env, ab_pruning, randomize_move_order, random_sampler);
}

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
  assert(root);
  assert(env);

  const bool ab_pruning = true;
  const bool randomize_move_order = true;
  const bool random_sampler = true;
  return game_position_mab_solve(root, env, ab_pruning, randomize_move_order, random_sampler);
}


/**
 * @cond
 */

/*
 * Internal functions.
 */

ExactSolution *
game_position_mab_solve (const GamePositionX *const root,
                         const endgame_solver_env_t *const env,
                         const bool alpha_beta_pruning,
                         const bool randomize_move_order,
                         const bool random_sampler)
{
  assert(root);
  assert(env);

  gtl_log_env_t *const log_env = gtl_init(env->log_file);

  if (log_env->log_is_on) gtl_open_log(log_env);

  prng_mt19937_t *prng = NULL;
  unsigned long int n_run = 1;
  if (randomize_move_order) {
    uint64_t prng_seed = env->prng_seed_is_set ? env->prng_seed : prng_uint64_from_clock_random_seed();
    prng = prng_mt19937_new();
    prng_mt19937_init_by_seed(prng, prng_seed);
    if (env->repeats < 1) {
      n_run = 1;
    } else {
      n_run = env->repeats;
    }
  }

  uint64_t *pattern_index_frequencies = NULL;
  board_pattern_t *board_pattern = NULL;
  if (random_sampler && env->board_pattern_index != -1) {
    board_pattern = (board_pattern_t *) &board_patterns[env->board_pattern_index];
    /*
     * [0..60] is the epty count range. Sixtyone are the possible values for empty_count.
     * bitw_uipow(3, board_pattern->n_squares) are the possible index_values.
     */
    size_of_pattern_index_array = bitw_uipow(3, board_pattern->n_squares);
    pattern_index_frequencies = (uint64_t *) malloc(sizeof(uint64_t) * size_of_pattern_index_array * size_of_empty_count_array);
    assert(pattern_index_frequencies);
    for (size_t i = 0; i < sizeof(pattern_index_frequencies); i++) pattern_index_frequencies[i] = 0;
  }

  ExactSolution *result = NULL;
  GameTreeStack *stack = game_tree_stack_new();
  for (unsigned long int sub_run_id = 0; sub_run_id < n_run; sub_run_id++) {

    game_tree_stack_init(root, stack);
    if (log_env->log_is_on) stack->hash_is_on = true;

    result = exact_solution_new();
    exact_solution_set_root(result, root);

    if (random_sampler) {
      game_position_random_sammpler_impl(result, stack, log_env, prng, sub_run_id, board_pattern, pattern_index_frequencies);
    } else {
      game_position_solve_impl(result, stack, log_env, alpha_beta_pruning, randomize_move_order, prng, sub_run_id);
    }

    result->best_move = stack->nodes[1].best_move;
    result->outcome = stack->nodes[1].alpha;

    if (sub_run_id != n_run - 1) exact_solution_free(result);
  }

  if (pattern_index_frequencies) {
    FILE *fp;
    char filepath[512];
    int len = snprintf(filepath, 512, "data_pattern_index_frequencies_%s_%zu_%zu.csv", board_pattern->name, env->prng_seed, n_run);
    if (len > 512 - 1) fprintf(stderr, "Warning file name is too long, it has been truncated.\n");
    if ((fp = fopen(filepath, "w"))) {
      fprintf(fp, "EMPTY_COUNT;PATTERN_INDEX;COUNT\n");
      for (size_t ec = 0; ec < size_of_empty_count_array; ec ++)
        for (size_t idx = 0; idx < size_of_pattern_index_array; idx++)
          fprintf(fp, "%zu;%zu;%ld\n", ec, idx, pattern_index_frequencies[size_of_pattern_index_array * ec + idx]);
      fclose(fp);
      fprintf(stdout, "Data file \"%s\" has been written succesfully.\n", filepath);
    } else {
      fprintf(stderr, "Unable to open file: %s. Aborting ...", filepath);
      abort();
    }
  }

  if (randomize_move_order) prng_mt19937_free(prng);
  free(pattern_index_frequencies);
  game_tree_stack_free(stack);
  gtl_close_log(log_env);

  return result;
}

static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack,
                          const gtl_log_env_t *const log_env,
                          const bool alpha_beta_pruning,
                          const bool randomize_move_order,
                          prng_mt19937_t *const prng,
                          const unsigned long int sub_run_id)
{
  NodeInfo *c;
  const NodeInfo *const root = stack->active_node;

 begin:
  result->node_count++;
  c = ++stack->active_node;
  if (alpha_beta_pruning) { c->alpha = - (c - 1)->beta; c->beta = - (c - 1)->alpha; }
  else c->alpha = out_of_range_defeat_score;

  gts_generate_moves(stack);
  if (randomize_move_order) prng_mt19937_shuffle_array_p(prng, c->head_of_legal_move_list, c->move_count);
  if (stack->hash_is_on) gts_compute_hash(stack);
  if (log_env->log_is_on) gtl_do_log_head(result, stack, sub_run_id, log_env);

  if (gts_is_terminal_node(stack)) {
    result->leaf_count++;
    c->alpha = game_position_x_final_value(&c->gpx);
    c->best_move = invalid_move;
    c->move_cursor = c->head_of_legal_move_list;
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
      c->best_move = (*c->move_cursor)->move;
      c->alpha = - (c + 1)->alpha;
      if (alpha_beta_pruning) if (c->alpha >= c->beta) { c->move_cursor++; goto end; }
    }
  }

 end:
  if (log_env->log_is_on) gtl_do_log_tail(result, stack, sub_run_id, log_env);
  c = --stack->active_node;
  if (stack->active_node == root) return;
  goto entry;
}

static void
game_position_random_sammpler_impl (ExactSolution *const result,
                                    GameTreeStack *const stack,
                                    const gtl_log_env_t *const log_env,
                                    prng_mt19937_t *const prng,
                                    const unsigned long int sub_run_id,
                                    const board_pattern_t *board_pattern,
                                    uint64_t *pattern_index_frequencies)
{
  NodeInfo *c;
  const NodeInfo *const root = stack->active_node;

  while (true) {
    result->node_count++;
    c = ++stack->active_node;

    gts_generate_moves(stack);
    prng_mt19937_shuffle_array_p(prng, c->head_of_legal_move_list, c->move_count);
    if (stack->hash_is_on) gts_compute_hash(stack);
    if (log_env->log_is_on) gtl_do_log_head(result, stack, sub_run_id, log_env);
    if (pattern_index_frequencies) compute_and_store_pattern_indexes(c, board_pattern, pattern_index_frequencies);

    if (gts_is_terminal_node(stack)) {
      result->leaf_count++;
      c->alpha = game_position_x_final_value(&c->gpx);
      c->best_move = invalid_move;
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
    if (log_env->log_is_on) gtl_do_log_tail(result, stack, sub_run_id, log_env);
    c = --stack->active_node;
    if (stack->active_node == root) return;
    c->alpha = - (c + 1)->alpha;
  }
}

static void
increment_pattern_index_frequencies (const uint8_t empty_count,
                                     const board_pattern_index_t pattern_index,
                                     uint64_t *pattern_index_frequencies)
{
  const size_t array_index = size_of_pattern_index_array * empty_count + pattern_index;
  pattern_index_frequencies[array_index]++;
}

static void
compute_and_store_pattern_indexes (NodeInfo *c,
                                   const board_pattern_t *bp,
                                   uint64_t *pattern_index_frequencies)
{
  board_pattern_index_t indexes[8];
  board_t board;

  const uint8_t ec = bitw_bit_count_64(game_position_x_empties(&c->gpx));

  const SquareSet blacks = c->gpx.blacks;
  const SquareSet whites = c->gpx.whites;
  const Player player = c->gpx.player;

  board.square_sets[0] = player ? whites : blacks;
  board.square_sets[1] = player ? blacks : whites;

  board_pattern_compute_indexes(indexes, bp, &board);

  for (int i = 0; i < bp->n_instances; i++) {
    increment_pattern_index_frequencies(ec, indexes[i], pattern_index_frequencies);
  }
}

/**
 * @endcond
 */
