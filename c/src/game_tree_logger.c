/**
 * @file
 *
 * @brief Game tree logger module implementation.
 * @details Provides functions to open, close, and write to a log file during the
 * game tree expansion.
 *
 * @par game_tree_logger.c
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
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>
#include <assert.h>

#include "file_utils.h"
#include "game_tree_logger.h"


/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */



/*
 * Internal variables and constants.
 */

/**
 * @endcond
 */



/********************************************************/
/* Function implementations for the GameTreeLog entity. */
/********************************************************/

/**
 * @brief Opens the head file for logging.
 *
 * @invariant Parameter `env` must not be empty.
 * The invariant is guarded by an assertion.
 *
 * @param [in] env a pointer to the logging environment
 */
void
gtl_open_h (gtl_log_env_t *const env)
{
  assert(env);
  if (env->log_is_on) {
    env->h_file = fopen(env->h_file_name, "w");
    assert(env->h_file);
  }
}

/**
 * @brief Opens the tail file for logging and writes the header.
 *
 * @invariant Parameter `env` must not be empty.
 * The invariant is guarded by an assertion.
 *
 * @param [in] env a pointer to the logging environment
 */
void
gtl_open_t (gtl_log_env_t *const env)
{
  assert(env);
  if (env->log_is_on) {
    env->t_file = fopen(env->t_file_name, "w");
    assert(env->t_file);
  }
}

/**
 * @brief Writes one record to the head logging binary file.
 *
 * @invariant Parameter `env` must not be empty.
 * The invariant is guarded by an assertion.
 *
 * @param [in] env  a pointer to the logging environment
 * @param [in] data a pointer to the log record
 */
void
gtl_write_h (const gtl_log_env_t *const env,
             const gtl_log_data_h_t *const data)
{
  assert(env && env->h_file);
  fwrite(&gtl_rec_h, sizeof(gtl_rec_h), 1, env->h_file);
  fwrite(data, sizeof(gtl_log_data_h_t), 1, env->h_file);
}

/**
 * @brief Writes one record to the tail logging file.
 *
 * @invariant Parameter `env` must not be empty.
 * The invariant is guarded by an assertion.
 *
 * @param [in] env  a pointer to the logging environment
 * @param [in] data a pointer to the log record
 */
void
gtl_write_t (const gtl_log_env_t *const env,
             const gtl_log_data_t_t *const data)
{
  assert(env && env->h_file);
  fwrite(&gtl_rec_t, sizeof(gtl_rec_t), 1, env->h_file);
  fwrite(data, sizeof(gtl_log_data_t_t), 1, env->h_file);
}

/**
 * @brief Frees the `env` structure after closing the open files.
 *
 * @invariant Parameter `env` must not be empty.
 * The invariant is guarded by an assertion.
 *
 * @param env the logging environment
 */
void
gtl_close (gtl_log_env_t *const env)
{
  assert(env);
  if (env->log_is_on) {
    free(env->file_name_prefix);
    free(env->h_file_name);
    free(env->t_file_name);
  }
  if (env->h_file) fclose(env->h_file);
  if (env->t_file) fclose(env->t_file);
  free(env);
}

/**
 * @brief Initializes the log env structure.
 *
 * @param [in] file_name_prefix the prefix for the file names
 * @return                      the newly constructed log env
 */
gtl_log_env_t *
gtl_init (const char *const file_name_prefix)
{
  gtl_log_env_t *env;
  static const size_t size_of_log_env = sizeof(gtl_log_env_t);

  const char *h_suffix = "_h.dat";
  const char *t_suffix = "_t.dat";

  env = (gtl_log_env_t*) malloc(size_of_log_env);
  assert(env);

  env->t_file = NULL;
  env->h_file = NULL;

  if (file_name_prefix) {
    env->log_is_on = true;
    const size_t p_len = strlen(file_name_prefix);
    const size_t h_len = strlen(h_suffix);
    const size_t t_len = strlen(t_suffix);
    env->file_name_prefix = malloc(p_len + 1);
    assert(env->file_name_prefix);
    env->h_file_name = malloc(p_len + h_len + 1);
    assert(env->h_file_name);
    env->t_file_name = malloc(p_len + t_len + 1);
    assert(env->t_file_name);
    strcpy(env->file_name_prefix, file_name_prefix);
    strcpy(env->h_file_name, file_name_prefix);
    strcat(env->h_file_name, h_suffix);
    strcpy(env->t_file_name, file_name_prefix);
    strcat(env->t_file_name, t_suffix);
  } else {
    env->log_is_on = false;
    env->file_name_prefix = NULL;
    env->h_file_name = NULL;
    env->t_file_name = NULL;
  }

  return env;
}

bool
gtl_touch_files (const char *const file_name_prefix)
{
  static const size_t buf_size = 4096;

  char buf[buf_size];
  bool res_h, res_t;

  const char *h_suffix = "_h.dat";
  const char *t_suffix = "_t.dat";

  const size_t p_len = strlen(file_name_prefix);
  const size_t h_len = strlen(h_suffix);
  const size_t t_len = strlen(t_suffix);

  if (p_len + h_len +t_len + 1 > buf_size) {
    fprintf(stderr, "buf_size of %zu is too small in function game_tree_logger.c:gtl_touch_files(). Aborting ...\n", buf_size);
    abort();
  }

  strcpy(buf, file_name_prefix);
  strcat(buf, h_suffix);
  res_h = fut_touch_file(buf);

  strcpy(buf, file_name_prefix);
  strcat(buf, t_suffix);
  res_t = fut_touch_file(buf);

  return (res_h && res_t);
}

void
gtl_do_log (const ExactSolution *const result,
            const GameTreeStack *const stack,
            const unsigned long int sub_run_id,
            const gtl_log_env_t *const log_env)
{
  const NodeInfo* const c = stack->active_node;
  const bool is_leaf = !game_position_x_has_any_player_any_legal_move(&c->gpx);
  const SquareSet empties = game_position_x_empties(&c->gpx);
  const SquareSet legal_moves = game_position_x_legal_moves(&c->gpx);
  const uint8_t legal_move_count = bitw_bit_count_64(legal_moves);
  gtl_log_data_h_t log_data =
    { .sub_run_id = sub_run_id,
      .call_id = result->node_count,
      .hash = c->hash,
      .parent_hash = (c - 1)->hash,
      .blacks = c->gpx.blacks,
      .whites = c->gpx.whites,
      .player = c->gpx.player,
      .alpha = c->alpha,
      .beta  = c->beta,
      .call_level = c - stack->nodes,
      .empty_count = bitw_bit_count_64(empties),
      .is_leaf = is_leaf,
      .legal_move_count = legal_move_count,
      .legal_move_count_adjusted = legal_move_count + ((legal_moves == 0 && !is_leaf) ? 1 : 0),
      .parent_move = (*(c - 1)->move_cursor)->move
    };
  uint8_t *m = log_data.legal_move_array;
  SquareSet remaining_moves = legal_moves;
  while (remaining_moves) {
    *m++ = bitw_bit_scan_forward_64(remaining_moves);
    remaining_moves = bitw_reset_lowest_set_bit_64(remaining_moves);
  }
  gtl_write_h(log_env, &log_data);
}

void
gtl_do_log_tail (const ExactSolution *const result,
                 const GameTreeStack *const stack,
                 const unsigned long int sub_run_id,
                 const gtl_log_env_t *const log_env)
{
  const NodeInfo* const c = stack->active_node;
  gtl_log_data_t_t log_data =
    { .call_cnt = result->node_count,
      .alpha = c->alpha,
      .best_move = c->best_move,
      .searched_move_cnt =  c->move_cursor - c->head_of_legal_move_list,
      .call_level = c - stack->nodes,
      .hash = c->hash,
    };
  uint8_t *m = log_data.searched_move_array;
  gts_mle_t **p = c->head_of_legal_move_list;
  while (p < c->move_cursor) *m++ = (*p++)->move;
  gtl_write_t(log_env, &log_data);
}
