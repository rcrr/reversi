/**
 * @file
 *
 * @todo Add all the ffo game position.
 *
 * @todo Add other solvers (ifes, rab, ab, minimax)
 *
 * @todo Add the capability to handle more then one best move!
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
  gchar *gpdb_label;       /**< @brief The game position label used to acces the gpdb database. */
  int    best_move_count;  /**< @brief The count of best moves. */
  int    outcome;          /**< @brief The expected game position value. */
  Square best_move[64];    /**< @brief The expected best move array. */
} TestCase;

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 1 to 19.
 */
const TestCase ffo_01_19[] =
  {
    { "ffo-01", 1, +18, { G8 } }, // ffo-01;..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwww..wwwww..;b; G8:+18. H1:+12. H7:+6. A2:+6. A3:+4. B1:-4. A4:-22. G2:-24.;
    { "ffo-02", 1, +10, { A4 } }, // ffo-02;.bbbbbb...bwwww..bwbbwwb.wwwwwwwwwwwbbwwwwwbbwwb..bbww....bbbbb.;b; A4:+10. B2:+0. A3:-6. G7:-8. A7:-12. H7:-14. B7:-14. H2:-24.;
    { "ffo-03", 1,  +2, { D1 } }, // ffo-03;....wb....wwbb...wwwbb.bwwbbwwwwwbbwbbwwwbbbwwwwwbbbbwbw..wwwwwb;b; D1:+2. G3:+0. B8:-2. B1:-4. C1:-4. A2:-4. A3:-6. B2:-12.;
    { "ffo-04", 1,  +0, { H8 } }, // ffo-04;.bbbbbb.b.bbbww.bwbbbwwbbbwbwwwb.wbwwbbb..wwwbbb..wwbb....bwbbw.;b; H8:+0. A5:+0. B6:-4. B7:-4. A6:-8. B2:-12. H2:-26.;
    { "ffo-05", 1, +32, { G8 } }, // ffo-05;.wwwww....wbbw.bbbwbwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bwww...bbbbb..;b; G8:+32. G2:+12. B2:-20. G6:-26. G1:-32. G7:-34.;
    { "ffo-06", 1, +14, { A1 } }, // ffo-06;..wbbb..wwwbbb..wwwbwbw.wwbwwwb.wwbbbbbbbwwbbwb..wwwwb...bbbbbb.;b; A1:+14. H3:+14. A8:+12. H2:+8. G2:+8. H4:+4. G7:+4. A7:-22. B1:-24.;
    { "ffo-07", 1,  +8, { A6 } }, // ffo-07;..wbbw..bwbbbb..bwwwbbbbbwwbbbbbbwwwwbbb.bbbbbbb..bbwww....bbww.;b; A6:+8. G1:+0. A1:-2. H8:-6. H7:-14. B1:-30.;
    { "ffo-08", 1,  +8, { E1 } }, // ffo-08;...b.b..b.bbbb..bbbbwbbbbbbwwwwwbbwbbbw.bwbbbbw.bwwbbb..bwwbbw..;w; E1:+8. H2:+4. G2:+4. B2:+4. G7:+4. B1:+2. G1:-6. C1:-8.;
    { "ffo-09", 1,  -8, { A4 } }, // ffo-09;..bwbb..w.wwbbbb.wwwbbbb.bwbbbwbbbwbwwwbwbbwbwbb..wbww....wwww..;w; G7:-8. A4:-8. B1:-16. A7:-16. B7:-26. A3:-30. G1:-38. H7:-40.;
    { "ffo-10", 1, +10, { B2 } }, // ffo-10;.bbbb.....wbbb..bwbwbwbbwbwbbwbbwbbwbwwwbbbwbwwb..wbbw...wwwww..;w; B2:+10. B7:+4. F1:+0. A7:-4. A2:-6. G2:-12. H2:-16. H7:-20.;
    { "ffo-11", 1, +30, { B3 } }, // ffo-11;...w.bwb....bbwb...bbwwbw.bbwbwbbbbwwbwb.bwwbbbbbwwwbb.bwwwwwww.;w; B3:+30. C2:+26. A6:+24. G7:+20. C3:+18. D2:+16. B4:+10. E1:+6.;
    { "ffo-12", 2,  -8, { A7, B7 } }, // ffo-12;..w..w..b.wwwwb.bbwwwbwwbbwbwbwwbbwbbwwwbbbbwwww..wbbb...bbbbb..;w; B7:-8. A7:-10. G7:-14. G8:-14. H2:-16. G1:-16. H1:-20.;
    { "ffo-13", 1, +14, { B7 } }, // ffo-13;..bbbbb..wwwbb...wwwbbbb.wbwbwbbwbbbwbbb..bwbwbb..wbwww..wwwww..;b; B7:+14. A4:+0. A3:-8. B1:-18. G8:-20. H7:-20. A2:-24.;
    { "ffo-14", 1, +18, { A3 } }, // ffo-14;..bbbbb...wwwb...bwwbbbb.wwwwwwwwwwbbbwwwwwbbwwb..bbww....bbbbb.;b; A3:+18. A4:+12. B1:+8. G7:-4. H7:-14. A7:-24. B7:-24. B2:-28.;
    { "ffo-15", 1,  +4, { G3 } }, // ffo-15;....w......wwb...wwwbb.bwwwbwwwwwbbwbbwwwbbbwwwwwbbbwwbw..wwwwwb;b; G3:+4. B8:+4. F1:+0. C1:+0. C2:-2. D1:-4. B2:-8. A3:-8.;
    { "ffo-16", 1, +24, { F8 } }, // ffo-16;.bbbbbb.b.bbbww.bwbbbwwbbwwbbbwb.wwwbbbb..wwbbbb...www....bwb.w.;b; F8:+24. C7:+20. A5:+6. H1:+6. B6:+0. B7:-2. A6:-6. H2:-26.;
    { "ffo-17", 1,  +8, { F8 } }, // ffo-17;.wwwww....wbbw.bbbwwwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bww....bbbb...;b; F8:+8. G2:+6. G6:-24. G1:-32. F7:-32. G7:-34. B2:-38.;
    { "ffo-18", 1,  -2, { G2 } }, // ffo-18;.bbb......wwwb..bwwwwwbbwbwbwwbbwbbwwwwwbbbwbwwb..wbbw...wwwww..;b; G2:-2. B7:-6. F1:-8. E1:-10. H7:-12. G8:-14. G7:-14. A2:-18. B2:-18.;
    { "ffo-19", 1,  +8, { B6 } }, // ffo-19;..wbbw..bwbbbb..bwwwwbbbbwwwbbbbb.wwwbbb..wwwwbb..bbwww....bbww.;b; B6:+8. H8:+4. B7:+0. G1:-6. B5:-16. H7:-16. B1:-24.;
    {NULL, 0, 0, {A1}}
  };

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 20 to 29.
 */
const TestCase ffo_20_29[] =
  {
    {"ffo-20", 1,  +6, { H5 } }, // ffo-20;bbbwbbbbwbbbbbbbwwbbbbbbwwwbbbbbwwwbbww.wwwww...wwwwwww.wwwwwww.;b; H5:+6. G6:-2. F6:-4. H6:-10;
    {"ffo-21", 1,  +0, { G5 } }, // ffo-21;wwwwwwwwbwwbbb..bbwwbww.bwbwww..bwwwwb..bwwbww..bwwwww..bbbb....;w; G5:+0. G2:-2. G4:-4. G6:-6;
    {"ffo-22", 1,  +2, { G8 } }, // ffo-22;..wwww..b.wwwww.bbwwbwbbbwbwbbbbbbbwbbbb.bbwbwbb..wbbb.b....b...;w; G8:+2. A6:+0. F8:-4. A7:-4. H2:-4. B2:-6. D8:-8. B7:-14. G7:-26;
    {"ffo-23", 1,  +4, { A2 } }, // ffo-23;..w.......wwb...wwwbbbw.wwwwbwbbbbbwwbwbbbbbbwwbb.bbbbwb..bbbb..;b; A2:+4. D1:-20. H3:-20. B1:-30. G2:-30. E1:-30. F2:-34. G8:-34. B2:-36. H2:-38;
    {"ffo-24", 1,  +0, { C3 } }, // ffo-24;..w..w.....wwwb..b.bwbww..bbbwwwbbbbwwwwbbbwbbwwbbbbbb..bwbb.w..;w; C3:+0. B4:-4. C2:-8. E8:-12. G7:-14. H2:-16. G1:-24;
    {"ffo-25", 2,  +0, { G1, A5 } }, // ffo-25;....b......bbbw..wwwbbbbbwwwwbbw.bbwwbbwwwbwbbbbwwwbb...b.bbbb..;w; G1:+0. A5:+0. F1:-4. D1:-6. F7:-8. C2:-10. G7:-10. H2:-12. H7:-16;
    {"ffo-26", 1,  +0, { D8 } }, // ffo-26;.wwwww....wbbw...wwwwbbw.wwwbwbb.wwbwwbb.bwbbwbb..w.bbbb..w....w;b; D8:+0. A6:-2. A4:-6. B7:-6. A5:-12. G1:-16. A2:-16. A3:-18. H2:-18. B8:-20. G2:-20. B2:-26;
    {"ffo-27", 1,  -2, { B7 } }, // ffo-27;..bw.w....wwww..wwbwbbw.wwwwbbwwwwwbbwb.wbwbbbbb..bbbb....b.w.b.;b; B7:-2. E1:-4. B1:-6. H2:-10. H5:-10. B2:-12. A2:-14. H3:-28. G1:-28. G2:-28;
    {"ffo-28", 3,  +0, { F1, B2, E1 } }, // ffo-28;..w.......www..b.bwwwwbbbbbbwbwb.bbwbwwbbbwbwwbb.wwwww.b...www..;b; F1:+0. B2:+0. E1:+0. B1:-4. F2:-6. G7:-6. D1:-12. C8:-20. G8:-22. B8:-28;
    {"ffo-29", 1, +10, { G2 } }, // ffo-29;.wbbbb....wbbw..bbwwbwwwbbbwwbwwbbwwbwwwbbbbww.bb.bbw...........;b; G2:+10. A1:+4. G6:-10. H2:-12. F8:-12. E8:-12. G7:-24. G1:-24. B2:-30. F7:-34;
    {NULL, 0, 0, {A1}}
  };

/*
ffo-30;.bbb....b.bww...bbwbww..bwbwbw..bwwbwbbbbwwbbwb...wwwww..bbbbb..;b; G3:+0. G2:-12. E1:-16. F2:-18. F1:-22. G4:-22. H6:-24. B7:-24. G8:-28;
ffo-31;.wwwww....wwww..wbbwww...bbbww..bbbbbbw.bbbwww.wb.wwww...wwwww..;b; G6:-2. G3:-4. G4:-8. G7:-14. H5:-14. G2:-16. G1:-30. G8:-32;
ffo-32;..bb....w.bbwb..wwbww...wbwbwww.wwbbwwwbwwbbbwwb..bbbbwb..b..b.b;b; G3:-4. B7:-6. E1:-8. H4:-10. F3:-10. H3:-10. B2:-14. A7:-22;
ffo-33;.bbbbbbb..bwww....wbwwbb.wwbbwbb.wwwwwbb.b.bwwbb...w.b.b..wwww..;b; E7:-8. A3:-8. A6:-12. B2:-12. G7:-12. G2:-12. A4:-14. C6:-20. A5:-22. B3:-28;
ffo-34;.............w.w.wwwwwwwwwwwwbwwwbbwwwbw.bbbwbww..bbbwbw..wbbbbw;b; C2:-2. D2:-6. E2:-6. A3:-10. A2:-10. F1:-12. G2:-14. G1:-16. B2:-20. B8:-26;
ffo-35;..bbb.....bbbb.wwwbbwwwwwwwwwwbw.wwbbbbw.wwwbbbw...bwbb...b.....;w; C7:+0. D8:-8. H8:-8. B2:-12. G1:-14. E8:-20. B1:-20. F8:-24. F1:-32. H7:-32. G8:-38;
ffo-36;...b.w....bbbw.bbbbbbbbbbwwbbwwbbwbwwwbbbbwwww.bb..wwww.........;w; B7:+0. B1:-2. E1:-4. C1:-6. G6:-8. G2:-10. A2:-22. B2:-24;
ffo-37;..wwww..w.wwww..wbbbwww.wbbwbw..wwbbwbb.wwbbbb..w.bbb.....bb.w..;b; G2:-20. G4:-22. B7:-22. H3:-22. G1:-30. H2:-42. B1:-48;
ffo-38;..wwww....wwww...bwbbwwbwwbwwwwb.wwwwwbbbwwbbbbb..b.b...........;b; B2:+4. A5:+0. H2:-4. A3:-10. A7:-18. G2:-20. B7:-22. G1:-24. B1:-26;
ffo-39;w.wwww..bwbbwb..bwwwbbb.bwwwbb..bwwbwb..bwbbb...b.bb............;w; A8:+64. B1:+64. G1:+64. G5:+64. G6:+64. C8:+64. H3:+64. E8:+64. H4:+64. F7:+62. D8:+62. E7:+62. H2:+62. B8:+62. G2:+60. G4:+60. F6:+32;
 */


/* Test function prototypes. */

static void
game_position_solve_test (GamePositionDbFixture *fixture,
                          gconstpointer          test_data);

static void
game_position_solve_ffo_01_19_test (GamePositionDbFixture *fixture,
                                    gconstpointer          test_data);

static void
game_position_solve_ffo_20_29_test (GamePositionDbFixture *fixture,
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

static void
assert_move_is_part_of_array (const Square move,
                              const Square move_array[],
                              const int    move_array_length);



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
    g_test_add ("/exact_solver/game_position_solve_ffo_20_29_test",
                GamePositionDbFixture,
                (gconstpointer) NULL,
                gpdb_fixture_setup,
                game_position_solve_ffo_20_29_test,
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

static void
game_position_solve_ffo_20_29_test (GamePositionDbFixture *fixture,
                                    gconstpointer          test_data)
{
  GamePositionDb *db = fixture->db;
  run_test_case_array(db, ffo_20_29);
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
      gchar *moves_to_s = square_array_to_string(tc->best_move, tc->best_move_count);
      printf("Test #%3d: data[position=%s, expected_value=%+03d, expected_best_moves={%s}]; ", i, tc->gpdb_label, tc->outcome, moves_to_s);
      g_free(moves_to_s);
    }
    const GamePosition * const gp = get_gp_from_db(db, tc->gpdb_label);
    ExactSolution * const solution = game_position_solve(gp, FALSE);
    if (g_test_verbose()) {
      gchar *move_to_s = square_to_string(solution->principal_variation[0]);
      printf("result[outcome=%+03d, move=%s]\n", solution->outcome, move_to_s);
      g_free(move_to_s);
    }
    g_assert_cmpint(tc->outcome, ==, solution->outcome);
    assert_move_is_part_of_array(solution->principal_variation[0], tc->best_move, tc->best_move_count);
    exact_solution_free(solution);
  }
}

static void
assert_move_is_part_of_array (const Square move,
                              const Square move_array[],
                              const int move_array_length)
{
  for (int i = 0; i < move_array_length; i++) {
    if (move == move_array[i]) return;
  }
  g_test_fail();
}
