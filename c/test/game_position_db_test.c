
#include <stdio.h>
#include <string.h>

#include <glib.h>

#include "game_position_db.h"

static void dummy_ok_test(void)
{
  g_assert(TRUE == TRUE);
}

void print_error(gpointer data, gpointer user_data)
{
  GString *error;
  GamePositionDbEntrySyntaxError *syntax_error = data;
  error = gpdb_entry_syntax_error_print(syntax_error);
  printf("%s\n", error->str);
  g_string_free(error, TRUE);
}

int contains_error(GSList *syntax_error_list,
                   char *line,
                   GamePositionDbEntrySyntaxErrorType error_type)
{
  int len;

  len = g_slist_length(syntax_error_list);

    for (int i = 0; i < len; i++) {
      GamePositionDbEntrySyntaxError *error = (GamePositionDbEntrySyntaxError *) g_slist_nth_data(syntax_error_list, i);
      // -to be removed- printf("%s\n", gpdb_entry_syntax_error_print(error)->str);
      if (error && (strcmp(error->line, line) == 0) && (error->error_type == error_type))
        return TRUE;
    }
  return FALSE;
}

void assert_gpdb_load_logs_error(char *line, GamePositionDbEntrySyntaxErrorType error_type)
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
  db = NULL;
  syntax_error_log = g_slist_alloc();
  gpdb_load(tmp_fp, db, syntax_error_log, &error);
  fclose(tmp_fp);

  /* Removes the tmp file, frees the resources. */
  remove(tmp_file_name);
  g_free(tmp_file_name);
  g_free(error);

  g_assert(contains_error(syntax_error_log,
                          line,
                          error_type)
           == TRUE);

  // syntax_error_log MUST be freed.
  // db MUST be freed.

}

static void gpdb_load_test(void)
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

}

static void gpdb_entry_syntax_error_print_test(void)
{
  GamePositionDbEntrySyntaxError *syntax_error;
  GString *msg, *expected;

  syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_ON_ID,
                                             "dummy-source",
                                             123,
                                             "a-record-line",
                                             "error-message");
  msg = gpdb_entry_syntax_error_print(syntax_error);
  expected = g_string_new("Error type:    The id field is not correctly assigned.\n"
                          "Error message: error-message\n"
                          "Source label:  dummy-source\n"
                          "Line number:   123\n"
                          "Line:          a-record-line\n");
  g_assert_cmpstr(expected->str, ==, msg->str);
  g_string_free(msg,      TRUE);
  g_string_free(expected, TRUE);
}

int main(int   argc,
         char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/game_position_db/dummy_ok", dummy_ok_test);
  g_test_add_func("/game_position_db/gpdb_load", gpdb_load_test);
  g_test_add_func("/game_position_db/gpdb_entry_syntax_error_print", gpdb_entry_syntax_error_print_test);

  return g_test_run();
}
