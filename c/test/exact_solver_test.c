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


/**
 * @brief GamePositionDb fixture
 */
typedef struct {
  GamePositionDb *db;
} GamePositionDbFixture;

/**
 * @brief A test case is used to automate the execution of a set of test game position.
 */
typedef struct {
  gchar *gpdb_label;   /**< @brief The game position label used to acces the gpdb database. */
  int    outcome;      /**< @brief The expected game position value. */
  Square best_move;    /**< @brief The expected best move. */
} TestCase;

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 1 to 19.
 */
const TestCase ffo_01_19[] =
  {
    { "ffo-01", +18, G8}, // ffo-01;..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwww..wwwww..;b; G8:+18. H1:+12. H7:+6. A2:+6. A3:+4. B1:-4. A4:-22. G2:-24.;
    { "ffo-02", +10, A4}, // ffo-02;.bbbbbb...bwwww..bwbbwwb.wwwwwwwwwwwbbwwwwwbbwwb..bbww....bbbbb.;b; A4:+10. B2:+0. A3:-6. G7:-8. A7:-12. H7:-14. B7:-14. H2:-24.;
    { "ffo-03",  +2, D1}, // ffo-03;....wb....wwbb...wwwbb.bwwbbwwwwwbbwbbwwwbbbwwwwwbbbbwbw..wwwwwb;b; D1:+2. G3:+0. B8:-2. B1:-4. C1:-4. A2:-4. A3:-6. B2:-12.;
    { "ffo-04",  +0, H8}, // ffo-04;.bbbbbb.b.bbbww.bwbbbwwbbbwbwwwb.wbwwbbb..wwwbbb..wwbb....bwbbw.;b; H8:+0. A5:+0. B6:-4. B7:-4. A6:-8. B2:-12. H2:-26.;
    { "ffo-05", +32, G8}, // ffo-05;.wwwww....wbbw.bbbwbwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bwww...bbbbb..;b; G8:+32. G2:+12. B2:-20. G6:-26. G1:-32. G7:-34.;
    { "ffo-06", +14, A1}, // ffo-06;..wbbb..wwwbbb..wwwbwbw.wwbwwwb.wwbbbbbbbwwbbwb..wwwwb...bbbbbb.;b; A1:+14. H3:+14. A8:+12. H2:+8. G2:+8. H4:+4. G7:+4. A7:-22. B1:-24.;
    { "ffo-07",  +8, A6}, // ffo-07;..wbbw..bwbbbb..bwwwbbbbbwwbbbbbbwwwwbbb.bbbbbbb..bbwww....bbww.;b; A6:+8. G1:+0. A1:-2. H8:-6. H7:-14. B1:-30.;
    { "ffo-08",  +8, E1}, // ffo-08;...b.b..b.bbbb..bbbbwbbbbbbwwwwwbbwbbbw.bwbbbbw.bwwbbb..bwwbbw..;w; E1:+8. H2:+4. G2:+4. B2:+4. G7:+4. B1:+2. G1:-6. C1:-8.;
    { "ffo-09",  -8, A4}, // ffo-09;..bwbb..w.wwbbbb.wwwbbbb.bwbbbwbbbwbwwwbwbbwbwbb..wbww....wwww..;w; G7:-8. A4:-8. B1:-16. A7:-16. B7:-26. A3:-30. G1:-38. H7:-40.;
    { "ffo-10", +10, B2}, // ffo-10;.bbbb.....wbbb..bwbwbwbbwbwbbwbbwbbwbwwwbbbwbwwb..wbbw...wwwww..;w; B2:+10. B7:+4. F1:+0. A7:-4. A2:-6. G2:-12. H2:-16. H7:-20.;
    { "ffo-11", +30, B3}, // ffo-11;...w.bwb....bbwb...bbwwbw.bbwbwbbbbwwbwb.bwwbbbbbwwwbb.bwwwwwww.;w; B3:+30. C2:+26. A6:+24. G7:+20. C3:+18. D2:+16. B4:+10. E1:+6.;
    { "ffo-12",  -8, B7}, // ffo-12;..w..w..b.wwwwb.bbwwwbwwbbwbwbwwbbwbbwwwbbbbwwww..wbbb...bbbbb..;w; B7:-8. A7:-10. G7:-14. G8:-14. H2:-16. G1:-16. H1:-20.;
    { "ffo-13", +14, B7}, // ffo-13;..bbbbb..wwwbb...wwwbbbb.wbwbwbbwbbbwbbb..bwbwbb..wbwww..wwwww..;b; B7:+14. A4:+0. A3:-8. B1:-18. G8:-20. H7:-20. A2:-24.;
    { "ffo-14", +18, A3}, // ffo-14;..bbbbb...wwwb...bwwbbbb.wwwwwwwwwwbbbwwwwwbbwwb..bbww....bbbbb.;b; A3:+18. A4:+12. B1:+8. G7:-4. H7:-14. A7:-24. B7:-24. B2:-28.;
    { "ffo-15",  +4, G3}, // ffo-15;....w......wwb...wwwbb.bwwwbwwwwwbbwbbwwwbbbwwwwwbbbwwbw..wwwwwb;b; G3:+4. B8:+4. F1:+0. C1:+0. C2:-2. D1:-4. B2:-8. A3:-8.;
    { "ffo-16", +24, F8}, // ffo-16;.bbbbbb.b.bbbww.bwbbbwwbbwwbbbwb.wwwbbbb..wwbbbb...www....bwb.w.;b; F8:+24. C7:+20. A5:+6. H1:+6. B6:+0. B7:-2. A6:-6. H2:-26.;
    { "ffo-17",  +8, F8}, // ffo-17;.wwwww....wbbw.bbbwwwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bww....bbbb...;b; F8:+8. G2:+6. G6:-24. G1:-32. F7:-32. G7:-34. B2:-38.;
    { "ffo-18",  -2, G2}, // ffo-18;.bbb......wwwb..bwwwwwbbwbwbwwbbwbbwwwwwbbbwbwwb..wbbw...wwwww..;b; G2:-2. B7:-6. F1:-8. E1:-10. H7:-12. G8:-14. G7:-14. A2:-18. B2:-18.;
    { "ffo-19",  +8, B6}, // ffo-19;..wbbw..bwbbbb..bwwwwbbbbwwwbbbbb.wwwbbb..wwwwbb..bbwww....bbww.;b; B6:+8. H8:+4. B7:+0. G1:-6. B5:-16. H7:-16. B1:-24.;
    {NULL, 0, A1}
  };



/* Test function prototypes. */

static void
game_position_solve_test (GamePositionDbFixture *fixture,
                          gconstpointer          test_data);

static void
game_position_solve_ffo_01_19_test (GamePositionDbFixture *fixture,
                                    gconstpointer          test_data);



/* Helper function prototypes. */

static void
gpdb_fixture_setup (GamePositionDbFixture *fixture,
                    gconstpointer          test_data);

static void
gpdb_fixture_teardown (GamePositionDbFixture *fixture,
                       gconstpointer          test_data);

static GamePosition *
get_gp_from_db (GamePositionDb *db,
                gchar          *id);

static void
run_test_case_array (      GamePositionDb *db,
                     const TestCase        tca[]);



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
    g_test_add ("/exact_solver/game_position_solve_ffo_01_19_test",
                GamePositionDbFixture,
                (gconstpointer) NULL,
                gpdb_fixture_setup,
                game_position_solve_ffo_01_19_test,
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
game_position_solve_ffo_01_19_test (GamePositionDbFixture *fixture,
                                    gconstpointer          test_data)
{
  GamePositionDb *db = fixture->db;
  run_test_case_array(db, ffo_01_19);
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
  db = gpdb_new(g_strdup("FFO-TEST database"));
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
get_gp_from_db (GamePositionDb *db,
                gchar          *id)
{
  GamePositionDbEntry *entry = gpdb_lookup(db, id);
  if (!entry) {
    g_test_message("The entry \"%s\" is missing from game position database.\n", id);
    g_test_fail();
  }
  g_assert(entry);
  return entry->game_position;
}

static void
run_test_case_array (      GamePositionDb *db,
                     const TestCase        tca[])
{
  const TestCase *tc = NULL;
  for (int i = 0;; i++) {
    tc = &tca[i];
    if (tc->gpdb_label == NULL) break;
    if (g_test_verbose()) {
      gchar *move_to_s = square_to_string(tc->best_move);
      printf("Test #%3d: data[position=%s, expected_value=%+03d, expected_best_move=%s]; ", i, tc->gpdb_label, tc->outcome, move_to_s);
      g_free(move_to_s);
    }
    const GamePosition * const gp = get_gp_from_db(db, tc->gpdb_label);
    ExactSolution * const solution = game_position_solve(gp, FALSE);
    if (g_test_verbose()) {
      gchar *move_to_s = square_to_string(solution->principal_variation[0]);
      printf("result[outcome=%+03d, move=%s]\n", solution->outcome, move_to_s);
      g_free(move_to_s);
    }
    g_assert_cmpint(tc->outcome, ==, solution->outcome);
    g_assert(tc->best_move == solution->principal_variation[0]);
    exact_solution_free(solution);
  }
}
