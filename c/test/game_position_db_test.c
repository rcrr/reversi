/**
 * @file
 *
 * @brief Game position database module unit test suite.
 * @details Collects tests and helper methods for the game position database module.
 *
 * @todo The statement  db = gpdb_new(NULL); is really bad with this NULL value.
 * @todo The statement  gpdb_load(fp, NULL, db, syntax_error_log, &error); has the NULL that must be rearranged.
 *
 * @par game_position_db_test.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2017 Roberto Corradini. All rights reserved.
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
contains_error (gpdb_syntax_error_log_t *syntax_error_log,
                char *line,
                gpdb_entry_syntax_error_type_t error_type);

static void
assert_gpdb_load_logs_error (char *line,
                             gpdb_entry_syntax_error_type_t error_type);




int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/game_position_db/gpdb_entry_syntax_error_print", gpdb_entry_syntax_error_print_test);
  g_test_add_func("/game_position_db/gpdb_load_returned_errors", gpdb_load_returned_errors_test);
  g_test_add_func("/game_position_db/gpdb_load", gpdb_load_test);

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
  GamePositionDb *db;
  gpdb_syntax_error_log_t *syntax_error_log;
  FILE *fp;
  GError *error;
  gchar *source;
  int db_length;
  int error_log_length;
  int expected_db_length;
  int expected_error_log_length;
  gpdb_entry_t *entry;


  source = g_strdup("db/gpdb-test-db.txt");
  expected_db_length = 6;
  expected_error_log_length = 2;

  /* Loads the game position database. */
  fp = fopen(source, "r");
  if (!fp) {
    printf("Unable to open database test file \"%s\" for reading.\n", source);
    g_test_fail();
  }
  db = gpdb_new(g_strdup("Testing Database"));
  syntax_error_log = NULL;
  error = NULL;
  gpdb_load(fp, source, db, &syntax_error_log, &error);
  fclose(fp);
  g_free(source);

  /* Verifies the db length. */
  db_length = gpdb_length(db);
  g_assert(db_length == expected_db_length);

  /* Verifies the error log length. */
  error_log_length = gpdb_syntax_error_log_length(syntax_error_log);
  g_assert(error_log_length == expected_error_log_length);

  /* Verifies that the entry all-black has been properly loaded. */
  entry = NULL;
  entry = gpdb_lookup(db, "all-black");
  g_assert(entry);
  g_assert(!g_strcmp0("all-black", entry->id));
  g_assert(!g_strcmp0("A full black board", entry->desc));
  g_assert(BLACK_PLAYER == entry->gpx->player);
  g_assert(0xFFFFFFFFFFFFFFFFULL == entry->gpx->blacks);
  g_assert(0x0000000000000000ULL == entry->gpx->whites);

  /* Verifies that the entry all-white has been properly loaded. */
  entry = NULL;
  entry = gpdb_lookup(db, "all-white");
  g_assert(entry);
  g_assert(!g_strcmp0("all-white", entry->id));
  g_assert(!g_strcmp0("A full white board", entry->desc));
  g_assert(WHITE_PLAYER == entry->gpx->player);
  g_assert(0x0000000000000000ULL == entry->gpx->blacks);
  g_assert(0xFFFFFFFFFFFFFFFFULL == entry->gpx->whites);

  /* Verifies that the duplicate entry has not been overwritten. */
  entry = NULL;
  entry = gpdb_lookup(db, "duplicate-entry");
  g_assert(entry);
  g_assert(!g_strcmp0("duplicate-entry", entry->id));
  g_assert(!g_strcmp0("Test inserting a position twice: first time", entry->desc));
  g_assert(WHITE_PLAYER == entry->gpx->player);
  g_assert(0x0000000000000000ULL == entry->gpx->blacks);
  g_assert(0x0000000000000000ULL == entry->gpx->whites);

  /* Removes the tmp file, frees the resources. */
  g_free(error);
  gpdb_free(db, TRUE);
  if (syntax_error_log)
    gpdb_syntax_error_log_free(syntax_error_log);
}

static void
gpdb_entry_syntax_error_print_test (void)
{
  gpdb_entry_syntax_error_t *syntax_error;
  char *msg;

  char expected[] =
    "Error type:    The id field is not correctly assigned.\n"
    "Error message: error-message\n"
    "Source label:  dummy-source\n"
    "Line number:   123\n"
    "Line:          a-record-line\n";

  syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_ON_ID,
                                             g_strdup("dummy-source"),
                                             123,
                                             g_strdup("a-record-line"),
                                             g_strdup("error-message"));
  msg = gpdb_entry_syntax_error_print(syntax_error);

  g_assert_cmpstr(expected, ==, msg);

  g_free(msg);
  gpdb_entry_syntax_error_free(syntax_error);
}



/*
 * Internal functions.
 */

static int
contains_error (gpdb_syntax_error_log_t *syntax_error_log,
                char *line,
                gpdb_entry_syntax_error_type_t error_type)
{
  int len;

  len = g_slist_length(syntax_error_log);

    for (int i = 0; i < len; i++) {
      gpdb_entry_syntax_error_t *error = (gpdb_entry_syntax_error_t *) g_slist_nth_data(syntax_error_log, i);
      if (error && (strcmp(error->line, line) == 0) && (error->error_type == error_type))
        return TRUE;
    }
  return FALSE;
}

static void
assert_gpdb_load_logs_error (char *line,
                             gpdb_entry_syntax_error_type_t error_type)
{
  int                           tmp_file_handle;
  gchar                        *tmp_file_name;
  GError                       *error;
  GIOChannel                   *channel;
  gsize                         bytes_written;
  GamePositionDb               *db;
  gpdb_syntax_error_log_t *syntax_error_log;
  FILE                         *tmp_fp;

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
  syntax_error_log = NULL;
  gpdb_load(tmp_fp, NULL, db, &syntax_error_log, &error);
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
  if (syntax_error_log)
    gpdb_syntax_error_log_free(syntax_error_log);

  // db MUST be freed.
  gboolean free_segment = TRUE;
  gpdb_free(db, free_segment);

}
