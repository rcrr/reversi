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

  GamePosition *ffo_40 = get_gp_from_db(db, "ffo-40");

  g_assert(TRUE  == game_position_has_any_legal_move(ffo_40));

  /*
   * FFO position #40, black to move, Turner vs Monnom, Bruxelles 1997.
   * Principal Variation, PV: a2 b1 c1 -- b6 c7 a7 b7 b8 d7 f8 c6 f7 g7
   * Final score is +38
   */


  printf("\n\n\n---- SOLVING FFO #40 GAME POSITION ---\n");
  printf("\nFFO #40:\n%s\n", game_position_print(ffo_40));

  GamePosition *ffo_40_a2 = game_position_make_move(ffo_40, A2);
  printf("\nFFO #40: A2\n%s\n", game_position_print(ffo_40_a2));

  GamePosition *ffo_40_a2_b1 = game_position_make_move(ffo_40_a2, B1);
  printf("\nFFO #40: A2 B1\n%s\n", game_position_print(ffo_40_a2_b1));

  SquareSet lm = game_position_legal_moves(ffo_40_a2_b1);
  printf("ffo_40_a2_b1 legal moves: %s\n", square_set_print_as_moves(lm));
  // ffo_40_a2_b1 legal moves: C1 A6 B6 C6 C7 D7 F7 G7 D8 E8

  // !!!!!
  GamePosition *ffo_40_a2_b1_a6 = game_position_make_move(ffo_40_a2_b1, A6);
  printf("\nFFO #40: A2 B1 A6\n%s\n", game_position_print(ffo_40_a2_b1_a6));

  // !!!!!
  GamePosition *ffo_40_a2_b1_b6 = game_position_make_move(ffo_40_a2_b1, B6);
  printf("\nFFO #40: A2 B1 B6\n%s\n", game_position_print(ffo_40_a2_b1_b6));

  // !!!!!
  GamePosition *ffo_40_a2_b1_c6 = game_position_make_move(ffo_40_a2_b1, C6);
  printf("\nFFO #40: A2 B1 C6\n%s\n", game_position_print(ffo_40_a2_b1_c6));

  // !!!!!
  GamePosition *ffo_40_a2_b1_c7 = game_position_make_move(ffo_40_a2_b1, C7);
  printf("\nFFO #40: A2 B1 C7\n%s\n", game_position_print(ffo_40_a2_b1_c7));

  // !!!!!
  GamePosition *ffo_40_a2_b1_d7 = game_position_make_move(ffo_40_a2_b1, D7);
  printf("\nFFO #40: A2 B1 D7\n%s\n", game_position_print(ffo_40_a2_b1_d7));

  // !!!!!
  GamePosition *ffo_40_a2_b1_f7 = game_position_make_move(ffo_40_a2_b1, F7);
  printf("\nFFO #40: A2 B1 F7\n%s\n", game_position_print(ffo_40_a2_b1_f7));

  // !!!!!
  GamePosition *ffo_40_a2_b1_g7 = game_position_make_move(ffo_40_a2_b1, G7);
  printf("\nFFO #40: A2 B1 G7\n%s\n", game_position_print(ffo_40_a2_b1_g7));

  // !!!!!
  GamePosition *ffo_40_a2_b1_d8 = game_position_make_move(ffo_40_a2_b1, D8);
  printf("\nFFO #40: A2 B1 D8\n%s\n", game_position_print(ffo_40_a2_b1_d8));

  GamePosition *ffo_40_a2_b1_e8 = game_position_make_move(ffo_40_a2_b1, E8);
  printf("\nFFO #40: A2 B1 E8\n%s\n", game_position_print(ffo_40_a2_b1_e8));





  GamePosition *ffo_40_a2_b1_c1 = game_position_make_move(ffo_40_a2_b1, C1);
  printf("\nFFO #40: A2 B1 C1\n%s\n", game_position_print(ffo_40_a2_b1_c1));

  GamePosition *ffo_40_a2_b1_c1_pass = game_position_pass(ffo_40_a2_b1_c1);
  printf("\nFFO #40: A2 B1 C1 --\n%s\n", game_position_print(ffo_40_a2_b1_c1_pass));

  GamePosition *ffo_40_a2_b1_c1_pass_b6 = game_position_make_move(ffo_40_a2_b1_c1_pass, B6);
  printf("\nFFO #40: A2 B1 C1 -- B6\n%s\n", game_position_print(ffo_40_a2_b1_c1_pass_b6));

  GamePosition *ffo_40_a2_b1_c1_pass_b6_c7 = game_position_make_move(ffo_40_a2_b1_c1_pass_b6, C7);
  printf("\nFFO #40: A2 B1 C1 -- B6 C7\n%s\n", game_position_print(ffo_40_a2_b1_c1_pass_b6_c7));

  GamePosition *ffo_40_a2_b1_c1_pass_b6_c7_a7 = game_position_make_move(ffo_40_a2_b1_c1_pass_b6_c7, A7);
  printf("\nFFO #40: A2 B1 C1 -- B6 C7 A7\n%s\n", game_position_print(ffo_40_a2_b1_c1_pass_b6_c7_a7));

  GamePosition *ffo_40_a2_b1_c1_pass_b6_c7_a7_b7 = game_position_make_move(ffo_40_a2_b1_c1_pass_b6_c7_a7, B7);
  printf("\nFFO #40: A2 B1 C1 -- B6 C7 A7 B7\n%s\n", game_position_print(ffo_40_a2_b1_c1_pass_b6_c7_a7_b7));

  ExactSolution *solution = game_position_solve(ffo_40);
  printf("\n%s\n", exact_solution_print(solution));
  printf("\nsolution outcome: %d\n", solution->outcome);

  //ExactSolution *solution = game_position_solve(ffo_40);
  //printf("\n%s\n", exact_solution_print(solution));

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
  db = gpdb_new(g_strdup("Testing Database"));
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

GamePosition *
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
