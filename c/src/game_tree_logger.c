/**
 * @file
 *
 * @todo Add a game_tree_log_env structure, add to it the FILE field. Pass a pointer to it
 * as an argument to all the module's call.
 *
 * @todo Create a game_tree_log_json_doc_maker function.
 * It receive the env, a game position and returns the json_doc string.
 * In order to have a strong configurability it should receive as argument an hash-table with key-value pairs.
 *
 * @todo Add the file name definition as an option when calling endgame_solver with the -l flag.
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



/*
 * Internal variables and constants.
 */

/**
 * @brief The log file used to record the game DAG traversing.
 */
static FILE *game_tree_log_file = NULL;



/********************************************************/
/* Function implementations for the GameTreeLog entity. */ 
/********************************************************/

/**
 * @brief Opens the file for logging and writes the header.
 *
 * @param [in] filename the name of the logging file
 */
void
game_tree_log_open (const gchar * const filename)
{
  game_tree_log_filename_check(filename);
  game_tree_log_file = fopen(filename, "w");
  fprintf(game_tree_log_file, "%s;%s;%s;%s;%s;%s;%s;%s\n",
          "SUB_RUN_ID",
          "CALL_ID",
          "HASH",
          "PARENT_HASH",
          "BLACKS",
          "WHITES",
          "PLAYER",
          "JSON_DOC");
}

/**
 * @brief Writes one record to the logging file.
 *
 * @param [in] log_data a pointer to the log record
 */
void
game_tree_log_write (const LogData * const log_data)
{
  fprintf(game_tree_log_file, "%6d;%8llu;%+20lld;%+20lld;%+20lld;%+20lld;%1d;%s\n",
          log_data->sub_run_id,
          log_data->call_id,
          (sint64) log_data->hash,
          (sint64) log_data->parent_hash,
          (sint64) log_data->blacks,
          (sint64) log_data->whites,
          log_data->player,
          log_data->json_doc);
}

/**
 * @brief Closes the logging file.
 */
void
game_tree_log_close (void)
{
  fclose(game_tree_log_file);
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
