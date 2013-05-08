
#include <stdio.h>

#include <glib.h>

#include "game_position_db.h"

static void dummy_ok_test(void)
{
  g_assert(TRUE == TRUE);
}

static void gpdb_load_test(void)
{
  FILE *fp;
  GError **error;
  GamePositionDb *db;

  /* The list of error returned reading the file has to be implementend .... */
  fp = fopen("./db/test-db-error-on-id.txt", "r");
  error = NULL;
  db = NULL;
  gpdb_load(fp, db, error);
  fclose(fp);

  fp = fopen("./db/test-db.txt", "r");
  error = NULL;
  db = NULL;
  gpdb_load(fp, db, error);
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
