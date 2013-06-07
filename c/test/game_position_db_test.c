/**
 * @file
 *
 * @brief Game position database module unit test suite.
 * @details Collects tests and helper methods for the game position database module.
 *
 * @todo Function syntax_error_log_destroy_function must be moved in game_position_db.c module
 * @todo The statement  db = gpdb_new(NULL); is really bad with this NULL value.
 * @todo The statement  gpdb_load(fp, NULL, db, syntax_error_log, &error); has the NULL that must be rearranged.
 *
 * @par game_position_db_test.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013 Roberto Corradini. All rights reserved.
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

#include <glib.h>

#include "game_position_db.h"



/* Test function prototypes. */

static void gpdb_load_returned_errors_test (void);
static void gpdb_load_test (void);
static void gpdb_entry_syntax_error_print_test (void);



/* Helper function prototypes. */

static int
contains_error (GSList                             *syntax_error_list,
                char                               *line,
                GamePositionDbEntrySyntaxErrorType  error_type);

static void
assert_gpdb_load_logs_error (char                               *line,
                             GamePositionDbEntrySyntaxErrorType  error_type);

static void
syntax_error_log_destroy_function (gpointer data);


static void
gpdb_print (void)
{
  printf("\ngdb_print: START\n");

  GamePositionDb  *db;
  GSList          *syntax_error_log;
  FILE            *fp;
  GError          *error;

  /* Loads the game position database. */
  fp = fopen("db/gpdb-sample-games.txt", "r");
  if (!fp) {
    printf("Unable to open database test file \"db/gpdb-sample-games.txt\" for reading.\n.");
    g_test_fail();
    return;
  }
  error = NULL;
  db = gpdb_new(NULL);
  syntax_error_log = g_slist_alloc();
  gpdb_load(fp, NULL, db, syntax_error_log, &error);
  fclose(fp);

  GamePositionDbEntry *entry = (GamePositionDbEntry *) g_tree_lookup(db->tree, "early-game-c-12-moves");
  printf("%s", board_print(entry->board));

  /* Removes the tmp file, frees the resources. */
  g_free(error);
  gpdb_delete(db, TRUE);
  g_slist_free_full(syntax_error_log, (GDestroyNotify) syntax_error_log_destroy_function);

  printf("\ngdb_print: END\n");

  g_assert(TRUE);
}

int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/game_position_db/gpdb_load-returned_errors", gpdb_load_returned_errors_test);
  g_test_add_func("/game_position_db/gpdb_entry_syntax_error_print", gpdb_entry_syntax_error_print_test);
  g_test_add_func("/game_position_db/gpdb_load", gpdb_load_test);

  g_test_add_func("/tmp/gpdb_print", gpdb_print);

  return g_test_run();
}



/*
 * Test functions.
 */

static void
gpdb_load_returned_errors_test (void)
{
  assert_gpdb_load_logs_error("I-am-a-not-a-terminated-id",
                              GPDB_ENTRY_SYNTAX_ERROR_ON_ID);

  assert_gpdb_load_logs_error("I-am-a-again a-not-terminated-id\n",
                              GPDB_ENTRY_SYNTAX_ERROR_ON_ID);

  assert_gpdb_load_logs_error("test-error-on-board-size-63;"
                              "ww.wwwwbbwwbbbbbwwbwwwwbwwbwwwbbwwwwwwbb...wwwwb....w..b.......;"
                              "b;"
                              "The board has 63 squares;"
                              "\n",
                              GPDB_ENTRY_SYNTAX_ERROR_BOARD_SIZE_IS_NOT_64);

  assert_gpdb_load_logs_error("test-error-on-board-size-65;"
                              "ww.wwwwbbwwbbbbbwwbwwwwbwwbbbwwwbbwwwwwwbb...wwwwb....w..b.......;"
                              "b;"
                              "The board has 65 squares;"
                              "\n",
                              GPDB_ENTRY_SYNTAX_ERROR_BOARD_SIZE_IS_NOT_64);

  assert_gpdb_load_logs_error("test-error-on-board-size-0;"
                              ";"
                              "b;"
                              "The board has no squares;"
                              "\n",
                              GPDB_ENTRY_SYNTAX_ERROR_BOARD_SIZE_IS_NOT_64);

  assert_gpdb_load_logs_error("test-error-on-board-wrong-square-char;"
                              "ww.wwwwbbwwbbbbbwwbwwwwbwwbwwwbbwwwwwwbb...wwwwb....w..x........;"
                              "b;"
                              "The board has a wrong char as square descriptor;"
                              "\n",
                              GPDB_ENTRY_SYNTAX_ERROR_SQUARE_CHAR_IS_INVALID);

  assert_gpdb_load_logs_error("test-error-on-board-not-terminated;"
                              "ww.wwwwbbwwbbbbbwwbwwwwbwwbwwwbbwwwwwwbb...wwwwb....w..x........"
                              "\n",
                              GPDB_ENTRY_SYNTAX_ERROR_BOARD_FIELD_IS_INVALID);

  assert_gpdb_load_logs_error("test-error-on-player-wrong-size;"
                              "ww.wwwwbbwwbbbbbwwbwwwwbwwbwwwbbwwwwwwbb...wwwwb....w...........;"
                              "bb;"
                              "The player field is made by two chars;"
                              "\n",
                              GPDB_ENTRY_SYNTAX_ERROR_PLAYER_IS_NOT_ONE_CHAR);

  assert_gpdb_load_logs_error("test-error-on-player-wrong-char;"
                              "ww.wwwwbbwwbbbbbwwbwwwwbwwbwwwbbwwwwwwbb...wwwwb....w...........;"
                              ".;"
                              "The player char is invalid;"
                              "\n",
                              GPDB_ENTRY_SYNTAX_ERROR_PLAYER_CHAR_IS_INVALID);

  assert_gpdb_load_logs_error("test-error-on-player-not-terminated;"
                              "ww.wwwwbbwwbbbbbwwbwwwwbwwbwwwbbwwwwwwbb...wwwwb....w...........;"
                              "."
                              "\n",
                              GPDB_ENTRY_SYNTAX_ERROR_PLAYER_FIELD_IS_INVALID);

  assert_gpdb_load_logs_error("test-error-on-description-not-terminated;"
                              "ww.wwwwbbwwbbbbbwwbwwwwbwwbwwwbbwwwwwwbb...wwwwb....w...........;"
                              "w;"
                              "The description is not termianted"
                              "\n",
                              GPDB_ENTRY_SYNTAX_ERROR_DESC_FIELD_IS_INVALID);
}

static void
gpdb_load_test (void)
{
  GamePositionDb  *db;
  GSList          *syntax_error_log;
  FILE            *fp;
  GError          *error;

  /* Loads the game position database. */
  fp = fopen("db/gpdb-test-db.txt", "r");
  if (!fp) {
    printf("Unable to open database test file \"db/gpdb-test-db.txt\" for reading.\n.");
    abort();
  }
  error = NULL;
  db = gpdb_new(NULL);
  syntax_error_log = g_slist_alloc();
  gpdb_load(fp, NULL, db, syntax_error_log, &error);
  fclose(fp);

  /* Removes the tmp file, frees the resources. */
  g_free(error);
  gpdb_delete(db, TRUE);
  g_slist_free_full(syntax_error_log, (GDestroyNotify) syntax_error_log_destroy_function);
}

static void
gpdb_entry_syntax_error_print_test (void)
{
  GamePositionDbEntrySyntaxError *syntax_error;
  GString                        *msg;
  GString                        *expected;

  syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_ON_ID,
                                             g_strdup("dummy-source"),
                                             123,
                                             g_strdup("a-record-line"),
                                             g_strdup("error-message"));
  msg = gpdb_entry_syntax_error_print(syntax_error);
  expected = g_string_new("Error type:    The id field is not correctly assigned.\n"
                          "Error message: error-message\n"
                          "Source label:  dummy-source\n"
                          "Line number:   123\n"
                          "Line:          a-record-line\n");
  g_assert_cmpstr(expected->str, ==, msg->str);
  g_string_free(msg,      TRUE);
  g_string_free(expected, TRUE);
  gpdb_entry_syntax_error_delete(syntax_error);
}



/*
 * Internal functions.
 */

static int
contains_error (GSList                             *syntax_error_list,
                char                               *line,
                GamePositionDbEntrySyntaxErrorType  error_type)
{
  int len;

  len = g_slist_length(syntax_error_list);

    for (int i = 0; i < len; i++) {
      GamePositionDbEntrySyntaxError *error = (GamePositionDbEntrySyntaxError *) g_slist_nth_data(syntax_error_list, i);
      if (error && (strcmp(error->line, line) == 0) && (error->error_type == error_type))
        return TRUE;
    }
  return FALSE;
}

static void
assert_gpdb_load_logs_error (char                               *line,
                             GamePositionDbEntrySyntaxErrorType  error_type)
{
  int              tmp_file_handle;
  gchar           *tmp_file_name;
  GError          *error;
  GIOChannel      *channel;
  gsize            bytes_written;
  GamePositionDb  *db;
  GSList          *syntax_error_log;
  FILE            *tmp_fp;

  /* Prepare the new tmp file. */
  error = NULL;
  tmp_file_name = NULL;
  tmp_file_handle = g_file_open_tmp("gpdb_test_XXXXXX.tmp", &tmp_file_name, &error);
  if (error)
    g_free(error);

  channel = g_io_channel_unix_new(tmp_file_handle);

  /* Writes the database line into the tmp file. */
  error = NULL;
  g_io_channel_write_chars(channel, line, strlen(line), &bytes_written, &error);

  error = NULL;
  g_io_channel_flush(channel, &error);

  /* Closes the open IO stream, removes the file, frees the resources. */
  error = NULL;
  g_io_channel_shutdown(channel, TRUE, &error);
  g_io_channel_unref(channel);

  /* Loads the game position database. */
  tmp_fp = fopen(tmp_file_name, "r");
  error = NULL;
  db = gpdb_new(NULL);
  syntax_error_log = g_slist_alloc();
  gpdb_load(tmp_fp, NULL, db, syntax_error_log, &error);
  fclose(tmp_fp);

  /* Removes the tmp file, frees the resources. */
  remove(tmp_file_name);
  g_free(tmp_file_name);
  g_free(error);

  g_assert(contains_error(syntax_error_log,
                          line,
                          error_type)
           == TRUE);

  // Free the syntax_error_log.
  g_slist_free_full(syntax_error_log, (GDestroyNotify) syntax_error_log_destroy_function);

  // db MUST be freed.
  gboolean free_segment = TRUE;
  gpdb_delete(db, free_segment);

}

static void
syntax_error_log_destroy_function (gpointer data)
{
  GamePositionDbEntrySyntaxError *e = (GamePositionDbEntrySyntaxError *) data;
  if (e)
    gpdb_entry_syntax_error_delete(e);
}
