
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
      if (error && (strcmp(error->line, line) == 0) && (error->error_type == error_type))
        return TRUE;
    }
  return FALSE;
}

static void gpdb_load_test(void)
{
  FILE            *fp;
  GError         **error;
  GamePositionDb  *db;
  GSList          *syntax_error_log;

  /* The list of error returned reading the file has to be implementend .... */
  fp = fopen("./db/test-db-error-on-board-size.txt", "r");
  syntax_error_log = g_slist_alloc();
  error = NULL;
  db = NULL;
  gpdb_load(fp, db, syntax_error_log, error);
  fclose(fp);
  //printf("\nSyntax Errors in file \"./db/test-db-error-on-board-size.txt\": %d\n", g_slist_length(syntax_error_log)-1);
  //g_slist_foreach(syntax_error_log, print_error, NULL);
  g_assert(contains_error(syntax_error_log,
                          "test-error-on-board-size-63;ww.wwwwbbwwbbbbbwwbwwwwbwwbwwwbbwwwwwwbb...wwwwb....w..b.......;b;The board has 63 squares;\n",
                          GPDB_ENTRY_SYNTAX_ERROR_BOARD_SIZE_IS_NOT_64)
           == TRUE);
  // syntax_errors must be freed ....
  g_slist_free(syntax_error_log);

  fp = fopen("./db/test-db-error-on-id.txt", "r");
  syntax_error_log = NULL;
  error = NULL;
  db = NULL;
  gpdb_load(fp, db, syntax_error_log, error);
  fclose(fp);

  fp = fopen("./db/test-db.txt", "r");
  syntax_error_log = NULL;
  error = NULL;
  db = NULL;
  gpdb_load(fp, db, syntax_error_log, error);
  fclose(fp);

  g_assert(1 == 1);
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
