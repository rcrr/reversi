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

#include <glib.h>
#include <glib/gstdio.h>

#include "game_tree_logger.h"


/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
game_tree_log_filename_check (const gchar * const filename);

static void
game_tree_log_dirname_recursive_check (const gchar * const filename);



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
game_tree_log_open_h (LogEnv *const env)
{
  g_assert(env);
  if (env->log_is_on) {
    game_tree_log_filename_check(env->h_file_name);
    env->h_file = fopen(env->h_file_name, "w");
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
game_tree_log_open_t (LogEnv *const env)
{
  g_assert(env);
  if (env->log_is_on) {
    game_tree_log_filename_check(env->t_file_name);
    env->t_file = fopen(env->t_file_name, "w");
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
game_tree_log_write_h (const LogEnv *const env,
                       const LogDataH *const data)
{
  g_assert(env && env->h_file);
  fwrite(data, sizeof(LogDataH), 1, env->h_file);
  if (data->json_doc) {
    fwrite(data->json_doc, data->json_doc_len + 1, 1, env->h_file);
  }
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
game_tree_log_write_t (const LogEnv *const env,
                       const LogDataT *const data)
{
  g_assert(env && env->t_file);
  fprintf(env->t_file, "%6d;%8" PRIu64 ";x%s\n",
          data->sub_run_id,
          data->call_id,
          data->json_doc);
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
game_tree_log_close (LogEnv *const env)
{
  g_assert(env);
  if (env->log_is_on) {
    g_free(env->file_name_prefix);
    g_free(env->h_file_name);
    g_free(env->t_file_name);
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
LogEnv *
game_tree_log_init (const gchar *const file_name_prefix)
{
  LogEnv *env;
  static const size_t size_of_log_env = sizeof(LogEnv);

  env = (LogEnv*) malloc(size_of_log_env);
  g_assert(env);

  gchar* file_name_prefix_copy = g_strdup(file_name_prefix);

  env->t_file = NULL;
  env->h_file = NULL;

  if (file_name_prefix_copy) {
    env->log_is_on = TRUE;
    env->file_name_prefix = file_name_prefix_copy;
    env->h_file_name      = g_strconcat(file_name_prefix_copy, "_h.dat", NULL);
    env->t_file_name      = g_strconcat(file_name_prefix_copy, "_t.dat", NULL);
  } else {
    env->log_is_on        = FALSE;
    env->file_name_prefix = NULL;
    env->h_file_name      = NULL;
    env->t_file_name      = NULL;
  }

  return env;
}

/**
 * @brief Returns the json_doc used in the head log by exact_solver and ifes solvers.
 *
 * @param [out] json_doc   the newly constucted json string
 * @param [in]  call_level call level value
 * @param [in]  gpx        the current game position
 * @return                 the length of the json_doc string
 */
int
game_tree_log_data_h_json_doc3 (char *const json_doc,
                                const int call_level,
                                const GamePositionX *const gpx)
{
  const gboolean is_leaf = !game_position_x_has_any_player_any_legal_move(gpx);
  const SquareSet legal_moves = game_position_x_legal_moves(gpx);
  const int legal_move_count = bit_works_bitcount_64(legal_moves);
  const SquareSet empties = game_position_x_empties(gpx);
  const int empty_count = bit_works_bitcount_64(empties);
  const int legal_move_count_adj = legal_move_count + ((legal_moves == 0 && !is_leaf) ? 1 : 0);
  gchar *legal_moves_pg_json_array = square_set_to_pg_json_array(legal_moves);
  /*
   * cl:   call level
   * ec:   empty count
   * il:   is leaf
   * lmc:  legal move count
   * lmca: legal move count adjusted
   * lma:  legal move array ([""A1"", ""B4"", ""H8""])
   */
  const int len = sprintf(json_doc,
                          "\"{ \"\"cl\"\": %2d, \"\"ec\"\": %2d, \"\"il\"\": %s, \"\"lmc\"\": %2d, \"\"lmca\"\": %2d, \"\"lma\"\": %s }\"",
                          call_level,
                          empty_count,
                          is_leaf ? "true" : "false",
                          legal_move_count,
                          legal_move_count_adj,
                          legal_moves_pg_json_array);
  return len;
}

void
do_log (const ExactSolution *const result,
        const GameTreeStack *const stack,
        const unsigned long int sub_run_id,
        const LogEnv *const log_env)
{
  const NodeInfo* const c = stack->active_node;
  LogDataH log_data =
    { .sub_run_id   = sub_run_id,
      .call_id      = result->node_count,
      .hash         = c->hash,
      .parent_hash  = (c - 1)->hash,
      .blacks       = c->gpx.blacks,
      .whites       = c->gpx.whites,
      .player       = c->gpx.player,
      .json_doc     = NULL,
      .json_doc_len = 0,
      .call_level   = c - stack->nodes };
  game_tree_log_write_h(log_env, &log_data);
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

/**
 * @brief Verifies the filename.
 *
 * @param [in] filename the logging file name
 */
static void
game_tree_log_filename_check (const gchar *const filename)
{
  const gchar * const dirname  = g_path_get_dirname(filename);
  if (!g_file_test(filename, G_FILE_TEST_EXISTS)) {
    game_tree_log_dirname_recursive_check(dirname);
  } else {
    if (g_file_test(filename, G_FILE_TEST_IS_REGULAR)) {
      printf("Logging regular file \"%s\" does exist, overwriting it.\n", filename);
    } else {
      printf("Logging file \"%s\" does exist, but it is a directory! Exiting with status -101.\n", filename);
      exit(-101);
    }
  }
}

/**
 * @brief Utility function used by game_tree_log_filename_check.
 * It recursively checks the subdirs in the filename path and creates them if are missing.
 *
 * @param [in] filename the directory path to be checked
 */
static void
game_tree_log_dirname_recursive_check (const gchar * const filename)
{
  const gchar * const dirname  = g_path_get_dirname(filename);
  if (!g_file_test(filename, G_FILE_TEST_EXISTS)) {
    game_tree_log_dirname_recursive_check(dirname);
    g_mkdir(filename, 0755);
  } else {
    if (g_file_test(filename, G_FILE_TEST_IS_REGULAR)) {
      printf("The given \"%s\" path contains an existing file! Exiting with status -102.\n", filename);
      exit(-102);
    }
  }
}

/**
 * @endcond
 */
