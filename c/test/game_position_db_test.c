
#include <stdio.h>
//#include <stdlib.h>
//#include <string.h>
//#include "rb.h"

#include <glib.h>

#include "game_position_db.h"

static void dummy_ok_test(void)
{
  g_assert(TRUE == TRUE);
}

static void gpdb_load_test(void)
{
  FILE *fp = fopen("./db/test-db.txt", "r");
  GError **error = NULL;
  GamePositionDb *db = NULL;
  gpdb_load(fp, db, error);
  g_assert(1 == 1);
}

static void gpdb_entry_syntax_error_print_test(void)
{
  GamePositionDbEntrySyntaxError *syntax_error;
  syntax_error = gpdb_entry_syntax_error_new(GPDB_ENTRY_SYNTAX_ERROR_A,
                                             "dummy-source",
                                             123,
                                             "abcdefgh",
                                             "error-message");

  GString *msg = gpdb_entry_syntax_error_print(syntax_error);
  //printf("\n%s", msg->str);
  GString *expected = g_string_new("Error type:    A\nError message: error-message\nSource label:  dummy-source\nLine number:   123\nLine:          abcdefgh\n");
  g_assert_cmpstr(expected->str, ==, msg->str);
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
