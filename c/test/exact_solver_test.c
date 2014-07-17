/**
 * @file
 *
 * @brief Exact solver unit test suite.
 * @details Collects tests and helper methods for the exact solver module.
 *
 * @par exact_solver_test.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014 Roberto Corradini. All rights reserved.
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

#include <stdlib.h>
#include <stdio.h>

#include <glib.h>

#include "board.h"
#include "exact_solver.h"
#include "game_position_db.h"


/* GamePositionDb fixture */
typedef struct {
  GamePositionDb *db;
} GamePositionDbFixture;



/* Test function prototypes. */

static void
game_position_solve_test (GamePositionDbFixture *fixture,
                          gconstpointer          test_data);

static void
game_position_solve_ffo_01_test (GamePositionDbFixture *fixture,
                                 gconstpointer          test_data);



/* Helper function prototypes. */

static void
gpdb_fixture_setup (GamePositionDbFixture *fixture,
                    gconstpointer          test_data);

static void
gpdb_fixture_teardown (GamePositionDbFixture *fixture,
                       gconstpointer          test_data);

static GamePosition *
get_gp_from_db (GamePositionDb *db, gchar *id);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  board_module_init();

  g_test_add ("/game_position/game_position_solve_test",
              GamePositionDbFixture,
              (gconstpointer) NULL,
              gpdb_fixture_setup,
              game_position_solve_test,
              gpdb_fixture_teardown);

  if (g_test_slow ()) {
    g_test_add ("/exact_solver/game_position_solve_ffo_01_test",
                GamePositionDbFixture,
                (gconstpointer) NULL,
                gpdb_fixture_setup,
                game_position_solve_ffo_01_test,
                gpdb_fixture_teardown);
  }
  
  return g_test_run();
}



/*
 * Test functions.
 */

static void
game_position_solve_test (GamePositionDbFixture *fixture,
                          gconstpointer          test_data)
{
  GamePositionDb *db = fixture->db;

  /*
   * FFO position #05:
   *
   * a b c d e f g h 
   * 1  . O O O O O . . 
   * 2  . . O @ @ O . @ 
   * 3  @ @ O @ O @ @ . 
   * 4  @ @ O @ O @ @ O 
   * 5  @ @ O O @ O O O 
   * 6  @ @ @ @ O O . O 
   * 7  @ . @ O O O . . 
   * 8  . @ @ @ @ @ . . 
   * Player to move: BLACK
   *
   * Move values: G8:+32. G2:+12. B2:-20. G6:-26. G1:-32. G7:-34.
   *
   * Principal Variation, PV: g8 g7 h8 g2 b2 a2 a1 g6 h7 b7 a8 -- h3
   * Final score is +32
   */
  GamePosition *ffo_05 = get_gp_from_db(db, "ffo-05");
  g_assert(TRUE  == game_position_has_any_legal_move(ffo_05));

  ExactSolution *solution = game_position_solve(ffo_05, FALSE);
  g_assert(+32 == solution->outcome);
  g_assert(G8 == solution->principal_variation[0]);
}

static void
game_position_solve_ffo_01_test (GamePositionDbFixture *fixture,
                                 gconstpointer          test_data)
{
  GamePositionDb *db = fixture->db;
  GamePosition *ffo_01 = get_gp_from_db(db, "ffo-01");
  g_assert(TRUE  == game_position_has_any_legal_move(ffo_01));

  ExactSolution *solution = game_position_solve(ffo_01, FALSE);
  g_assert(+18 == solution->outcome);
  g_assert(G8 == solution->principal_variation[0]);
}



/*
 * Internal functions.
 */

static void
gpdb_fixture_setup (GamePositionDbFixture *fixture,
                    gconstpointer          test_data)
{
  gchar *source = g_strdup("db/gpdb-ffo.txt");

  GamePositionDb               *db;
  GamePositionDbSyntaxErrorLog *syntax_error_log;
  FILE                         *fp;
  GError                       *error;

  /* Loads the game position database. */
  fp = fopen(source, "r");
  if (!fp) {
    g_test_message("Unable to open database test file \"%s\" for reading.\n", source);
    g_test_fail();
  }
  g_assert(fp);
  db = gpdb_new(g_strdup("FFOTEST database"));
  syntax_error_log = NULL;
  error = NULL;
  gpdb_load(fp, source, db, &syntax_error_log, &error);
  fclose(fp);
  g_free(source);

  /* Removes the tmp file, frees the resources. */
  g_free(error);
  if (syntax_error_log)
    gpdb_syntax_error_log_free(syntax_error_log);

  fixture->db = db;
  g_assert (fixture->db != NULL);
}

static void
gpdb_fixture_teardown (GamePositionDbFixture *fixture,
                       gconstpointer          test_data)
{
  g_assert(fixture->db != NULL);
  gpdb_free(fixture->db, TRUE);
}

static GamePosition *
get_gp_from_db (GamePositionDb *db, gchar *id)
{
  GamePositionDbEntry *entry = gpdb_lookup(db, id);
  if (!entry) {
    g_test_message("The entry \"%s\" is missing from game position database.\n", id);
    g_test_fail();
  }
  g_assert(entry);
  return entry->game_position;
}
