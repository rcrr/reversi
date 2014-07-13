/**
 * @file
 *
 * @todo Create a game_tree_log_json_doc_maker function.
 * It receives the env, a game position and returns the json_doc string.
 * In order to have a strong configurability it should receive as argument an hash-table with key-value pairs.
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
#include <glib/gstdio.h>

#include "game_tree_logger.h"



/*
 * Prototypes for internal functions.
 */

static void
game_tree_log_filename_check (const gchar * const filename);

static void
game_tree_log_dirname_recursive_check (const gchar * const filename);

static 
void game_tree_log_write_header (FILE * const file);



/*
 * Internal variables and constants.
 */



/********************************************************/
/* Function implementations for the GameTreeLog entity. */ 
/********************************************************/

/**
 * @brief Opens the h file for logging and writes the header.
 *
 * @invariant Parameter `env` must not be empty.
 * The invariant is guarded by an assertion.
 *
 * @param [in] env a pointer to the logging environment
 */
void
game_tree_log_open_h (LogEnv * const env)
{
  g_assert(env);
  if (env->log_is_on) {
    game_tree_log_filename_check(env->h_file_name);
    env->h_file = fopen(env->h_file_name, "w");
    game_tree_log_write_header(env->h_file);
  }
}

/**
 * @brief Opens the t file for logging and writes the header.
 *
 * @invariant Parameter `env` must not be empty.
 * The invariant is guarded by an assertion.
 *
 * @param [in] env a pointer to the logging environment
 */
void
game_tree_log_open_t (LogEnv * const env)
{
  g_assert(env);
  if (env->log_is_on) {
    game_tree_log_filename_check(env->t_file_name);
    env->t_file = fopen(env->t_file_name, "w");
    game_tree_log_write_header(env->t_file);
  }
}

/**
 * @brief Writes one record to the head logging file.
 *
 * @invariant Parameter `env` must not be empty.
 * The invariant is guarded by an assertion.
 *
 * @param [in] env  a pointer to the logging environment
 * @param [in] data a pointer to the log record
 */
void
game_tree_log_write_h (const LogEnv  * const env,
                       const LogData * const data)
{
  g_assert(env);
  fprintf(env->h_file, "%6d;%8llu;%+20lld;%+20lld;%+20lld;%+20lld;%1d;%s\n",
          data->sub_run_id,
          data->call_id,
          (sint64) data->hash,
          (sint64) data->parent_hash,
          (sint64) data->blacks,
          (sint64) data->whites,
          data->player,
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
game_tree_log_close (LogEnv * const env)
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
game_tree_log_init (const gchar * const file_name_prefix)
{
  LogEnv *env;
  static const size_t size_of_log_env = sizeof(LogEnv);
  
  env = (LogEnv*) malloc(size_of_log_env);
  g_assert(env);
  
  gchar* file_name_prefix_copy = g_strdup(file_name_prefix);

  env->h_file = NULL;
  env->t_file = NULL;
  
  if (file_name_prefix_copy) {
    env->log_is_on = TRUE;
    env->file_name_prefix = file_name_prefix_copy;
    env->h_file_name = g_strconcat(file_name_prefix_copy, "_h.csv", NULL);
    env->t_file_name = g_strconcat(file_name_prefix_copy, "_t.csv", NULL);
  } else {
    env->log_is_on        = FALSE;
    env->file_name_prefix = NULL;
    env->h_file_name      = NULL;
    env->t_file_name      = NULL;
  }

  return env;
}



/*
 * Internal functions.
 */

void
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

void
game_tree_log_filename_check (const gchar * const filename)
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

void
game_tree_log_write_header (FILE * const file)
{
  fprintf(file, "%s;%s;%s;%s;%s;%s;%s;%s\n",
          "SUB_RUN_ID",
          "CALL_ID",
          "HASH",
          "PARENT_HASH",
          "BLACKS",
          "WHITES",
          "PLAYER",
          "JSON_DOC");
}
