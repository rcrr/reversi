
#include <stdio.h>
//#include <stdlib.h>
//#include <string.h>
//#include "rb.h"

#include <glib.h>

#include "game_position_db.h"

static void dummy_test(void)
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

int main(int   argc,
         char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/game_position_db/dummy", dummy_test);
  g_test_add_func("/game_position_db/gpdb_load", gpdb_load_test);

  return g_test_run();
}
