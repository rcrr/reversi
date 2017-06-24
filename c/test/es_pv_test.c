/**
 * @file
 *
 * @brief Exact solver principal variation unit test suite.
 * @details Collects tests and helper methods that verify the correctness
 *          of the principal variation, vith variants, computed by the
 *          es (exact solver) solver.
 *
 * @par es_pv_test.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2017 Roberto Corradini. All rights reserved.
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
#include <string.h>
#include <assert.h>

#include <glib.h>

#include "board.h"
#include "game_position_db.h"
#include "sha3.h"

#include "exact_solver.h"


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
  char   pv_sha3_256[65];  /**< @brief The expected principal variation digest. */
} TestCase;

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, number 05.
 */
const TestCase ffo_05[] =
  {
    { "ffo-05", 1, +32, { G8 }, "74dc6b02993a1a89db15e96c9b897cb2cb9f1e7c7e9a2d109e06fd92c48ada0a" }, // ffo-05;.wwwww....wbbw.bbbwbwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bwww...bbbbb..;b; G8:+32. G2:+12. B2:-20. G6:-26. G1:-32. G7:-34.;
    {NULL, 0, 0, {A1}, ""}
  };


/*
 *
 * Expected HASH3-256
 *
 * ffo-01: 68a1a543e9d6d978c9acdff70b2da9ad16c7f3bcd9cd32b1d763dd8d7ec3b821
 * ffo-02: def9f78d7abb3bb2d5253cdce249d8b3cf0403bf270349c415ef2c75d37e602d
 * ffo-03: 3c7a58e33fb12f162e1eca788a1c987f8ce290858964ecf7481fab13f7c93d25
 * ffo-04: d6e31bfd10d310bbb92756bfe0219f3cdb5bca49aea99f7a1402e7860e116d8a
 * ffo-05: 74dc6b02993a1a89db15e96c9b897cb2cb9f1e7c7e9a2d109e06fd92c48ada0a
 * ffo-06: b2cf3d1e47f6d27402dcd1a1c54393a6992181b4efa2b6fe5e74532e2a42ea87
 * ffo-07: b264f972520b03237b75ee212cc6490124448c9284763f37325d1e178092013d
 * ffo-08: 97a9f0ff6364275f00557045e762783ff4caa98ad0e8fe22a9157d2af2a2f7d9
 * ffo-09: 520f2aa067d1851426b380c122caa4fbba5a6a13bdba3dc748140fae87c29d39
 * ffo-10: 9a215020e42bc672afd0f50a2df83e096404d6ce9d3ca966230cc80deb8148ee
 * ffo-11: 853e5ffb83b1e0d545cc39b92b42c9c61fb25935ec9881d6da0f6e74c0a5a981
 * ffo-12: 747c459f1ad85f1a94c6159c30616fb3ee6638ecbd1cf660d386d9cf79c0847f
 * ffo-13: a83b0c723ae3e917fca1c3c62d7fe9daf8b3dc9c8b4bcd62d55e26648b49bca3
 * ffo-14: 88c5da1ff1a488faaa5ce8e1a3db4b2af2880496e73a9261e925ab01445bb504
 * ffo-15: 74c154ee212ef867021397e78011989d60baaf0df9a39e26e3beccd4282660ce
 * ffo-16: d1287303f19a0a2e4f193b31fa5f0e46e0c514d3b9ca119054b0860a226c354e
 * ffo-17: 78982c0796df321412e0874100e24fbb93c6e13404e2f8f23375616b12d014bc
 * ffo-18: 47d56b9524c44829d3d3135acccd95f2b80dd5b785a9f99f3b9132c05d6dad58
 * ffo-19: 407895f2e49e48cffdb879c023063ba51a6b32a952c1e990e6b63a7baafaddaf
 * ffo-20: f50a8b973129512968d5853823892c6e2954aaf7504b4c2cab6b0538c709a63b
 * ffo-21: 345d1108d154761edd3d2548a89855037fb15154777e3479c5df359eed6a46f5
 * ffo-22: a37abd8c44bd99a4caeec61558cd3c94e82d01dafaabc3fce7fe9635465c32a3
 * ffo-23: 75797e7a8c10bf4402c72d048d2d0a94934d35f051f863c8f8c56b3d714db35f
 * ffo-24: 9a5f325dab822cff7e7810a4661d02ce5bb5de69ab9213d443df5f5c7873186f
 * ffo-25: bd2a938c87556c7f0cec0840a3a895a61d5345837f5c8f75b0679d1a0112d0b6
 * ffo-26: d1e162de3dc89d039eeabda515e5984827e65c7b6d3b971695dfdf35aafab3af
 * ffo-27: 23fa980ff745e949cf5a32abb9be6a7b071c591f6ffef6c83e88251f5ef59f30
 * ffo-28: 4d86c09d124bc7338ec9d66b616c85337a2d7cc0f3bcc1004a5cef7a73b57901
 * ffo-29: 4f18d1a3f89bedfd288c8832cfad086781aa6388ae3ac2997d5782f0c9ae3d40
 *
 */

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 1 to 19.
 */
const TestCase ffo_01_19[] =
  {
    { "ffo-01", 1, +18, { G8 },     "68a1a543e9d6d978c9acdff70b2da9ad16c7f3bcd9cd32b1d763dd8d7ec3b821" }, // ffo-01;..bbbbb..wwwbb.w.wwwbbwb.wbwbwbbwbbbwbbb..bwbwbb.bbbwww..wwwww..;b; G8:+18. H1:+12. H7:+6. A2:+6. A3:+4. B1:-4. A4:-22. G2:-24.;
    { "ffo-02", 1, +10, { A4 },     "def9f78d7abb3bb2d5253cdce249d8b3cf0403bf270349c415ef2c75d37e602d" }, // ffo-02;.bbbbbb...bwwww..bwbbwwb.wwwwwwwwwwwbbwwwwwbbwwb..bbww....bbbbb.;b; A4:+10. B2:+0. A3:-6. G7:-8. A7:-12. H7:-14. B7:-14. H2:-24.;
    { "ffo-03", 1,  +2, { D1 },     "3c7a58e33fb12f162e1eca788a1c987f8ce290858964ecf7481fab13f7c93d25" }, // ffo-03;....wb....wwbb...wwwbb.bwwbbwwwwwbbwbbwwwbbbwwwwwbbbbwbw..wwwwwb;b; D1:+2. G3:+0. B8:-2. B1:-4. C1:-4. A2:-4. A3:-6. B2:-12.;
    { "ffo-04", 2,  +0, { A5, H8 }, "d6e31bfd10d310bbb92756bfe0219f3cdb5bca49aea99f7a1402e7860e116d8a" }, // ffo-04;.bbbbbb.b.bbbww.bwbbbwwbbbwbwwwb.wbwwbbb..wwwbbb..wwbb....bwbbw.;b; H8:+0. A5:+0. B6:-4. B7:-4. A6:-8. B2:-12. H2:-26.;
    { "ffo-05", 1, +32, { G8 },     "74dc6b02993a1a89db15e96c9b897cb2cb9f1e7c7e9a2d109e06fd92c48ada0a" }, // ffo-05;.wwwww....wbbw.bbbwbwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bwww...bbbbb..;b; G8:+32. G2:+12. B2:-20. G6:-26. G1:-32. G7:-34.;
    { "ffo-06", 2, +14, { A1, H3 }, "b2cf3d1e47f6d27402dcd1a1c54393a6992181b4efa2b6fe5e74532e2a42ea87" }, // ffo-06;..wbbb..wwwbbb..wwwbwbw.wwbwwwb.wwbbbbbbbwwbbwb..wwwwb...bbbbbb.;b; A1:+14. H3:+14. A8:+12. H2:+8. G2:+8. H4:+4. G7:+4. A7:-22. B1:-24.;
    { "ffo-07", 1,  +8, { A6 },     "b264f972520b03237b75ee212cc6490124448c9284763f37325d1e178092013d" }, // ffo-07;..wbbw..bwbbbb..bwwwbbbbbwwbbbbbbwwwwbbb.bbbbbbb..bbwww....bbww.;b; A6:+8. G1:+0. A1:-2. H8:-6. H7:-14. B1:-30.;
    { "ffo-08", 1,  +8, { E1 },     "97a9f0ff6364275f00557045e762783ff4caa98ad0e8fe22a9157d2af2a2f7d9" }, // ffo-08;...b.b..b.bbbb..bbbbwbbbbbbwwwwwbbwbbbw.bwbbbbw.bwwbbb..bwwbbw..;w; E1:+8. H2:+4. G2:+4. B2:+4. G7:+4. B1:+2. G1:-6. C1:-8.;
    { "ffo-09", 2,  -8, { A4, G7 }, "520f2aa067d1851426b380c122caa4fbba5a6a13bdba3dc748140fae87c29d39" }, // ffo-09;..bwbb..w.wwbbbb.wwwbbbb.bwbbbwbbbwbwwwbwbbwbwbb..wbww....wwww..;w; G7:-8. A4:-8. B1:-16. A7:-16. B7:-26. A3:-30. G1:-38. H7:-40.;
    { "ffo-10", 1, +10, { B2 },     "9a215020e42bc672afd0f50a2df83e096404d6ce9d3ca966230cc80deb8148ee" }, // ffo-10;.bbbb.....wbbb..bwbwbwbbwbwbbwbbwbbwbwwwbbbwbwwb..wbbw...wwwww..;w; B2:+10. B7:+4. F1:+0. A7:-4. A2:-6. G2:-12. H2:-16. H7:-20.;
    { "ffo-11", 1, +30, { B3 },     "853e5ffb83b1e0d545cc39b92b42c9c61fb25935ec9881d6da0f6e74c0a5a981" }, // ffo-11;...w.bwb....bbwb...bbwwbw.bbwbwbbbbwwbwb.bwwbbbbbwwwbb.bwwwwwww.;w; B3:+30. C2:+26. A6:+24. G7:+20. C3:+18. D2:+16. B4:+10. E1:+6.;
    { "ffo-12", 2,  -8, { A7, B7 }, "747c459f1ad85f1a94c6159c30616fb3ee6638ecbd1cf660d386d9cf79c0847f" }, // ffo-12;..w..w..b.wwwwb.bbwwwbwwbbwbwbwwbbwbbwwwbbbbwwww..wbbb...bbbbb..;w; B7:-8. A7:-10. G7:-14. G8:-14. H2:-16. G1:-16. H1:-20.;
    { "ffo-13", 1, +14, { B7 },     "a83b0c723ae3e917fca1c3c62d7fe9daf8b3dc9c8b4bcd62d55e26648b49bca3" }, // ffo-13;..bbbbb..wwwbb...wwwbbbb.wbwbwbbwbbbwbbb..bwbwbb..wbwww..wwwww..;b; B7:+14. A4:+0. A3:-8. B1:-18. G8:-20. H7:-20. A2:-24.;
    { "ffo-14", 1, +18, { A3 },     "88c5da1ff1a488faaa5ce8e1a3db4b2af2880496e73a9261e925ab01445bb504" }, // ffo-14;..bbbbb...wwwb...bwwbbbb.wwwwwwwwwwbbbwwwwwbbwwb..bbww....bbbbb.;b; A3:+18. A4:+12. B1:+8. G7:-4. H7:-14. A7:-24. B7:-24. B2:-28.;
    { "ffo-15", 2,  +4, { G3, B8 }, "74c154ee212ef867021397e78011989d60baaf0df9a39e26e3beccd4282660ce" }, // ffo-15;....w......wwb...wwwbb.bwwwbwwwwwbbwbbwwwbbbwwwwwbbbwwbw..wwwwwb;b; G3:+4. B8:+4. F1:+0. C1:+0. C2:-2. D1:-4. B2:-8. A3:-8.;
    { "ffo-16", 1, +24, { F8 },     "d1287303f19a0a2e4f193b31fa5f0e46e0c514d3b9ca119054b0860a226c354e" }, // ffo-16;.bbbbbb.b.bbbww.bwbbbwwbbwwbbbwb.wwwbbbb..wwbbbb...www....bwb.w.;b; F8:+24. C7:+20. A5:+6. H1:+6. B6:+0. B7:-2. A6:-6. H2:-26.;
    { "ffo-17", 1,  +8, { F8 },     "78982c0796df321412e0874100e24fbb93c6e13404e2f8f23375616b12d014bc" }, // ffo-17;.wwwww....wbbw.bbbwwwbb.bbwbwbbwbbwwbwwwbbbbww.wb.bww....bbbb...;b; F8:+8. G2:+6. G6:-24. G1:-32. F7:-32. G7:-34. B2:-38.;
    { "ffo-18", 1,  -2, { G2 },     "47d56b9524c44829d3d3135acccd95f2b80dd5b785a9f99f3b9132c05d6dad58" }, // ffo-18;.bbb......wwwb..bwwwwwbbwbwbwwbbwbbwwwwwbbbwbwwb..wbbw...wwwww..;b; G2:-2. B7:-6. F1:-8. E1:-10. H7:-12. G8:-14. G7:-14. A2:-18. B2:-18.;
    { "ffo-19", 1,  +8, { B6 },     "407895f2e49e48cffdb879c023063ba51a6b32a952c1e990e6b63a7baafaddaf" }, // ffo-19;..wbbw..bwbbbb..bwwwwbbbbwwwbbbbb.wwwbbb..wwwwbb..bbwww....bbww.;b; B6:+8. H8:+4. B7:+0. G1:-6. B5:-16. H7:-16. B1:-24.;
    {NULL, 0, 0, {A1}}
  };

/**
 * @brief Expected results for test cases coming from French Federation Othello game positions, from number 20 to 29.
 */
const TestCase ffo_20_29[] =
  {
    {"ffo-20", 1,  +6, { H5 },         "f50a8b973129512968d5853823892c6e2954aaf7504b4c2cab6b0538c709a63b" }, // ffo-20;bbbwbbbbwbbbbbbbwwbbbbbbwwwbbbbbwwwbbww.wwwww...wwwwwww.wwwwwww.;b; H5:+6. G6:-2. F6:-4. H6:-10;
    {"ffo-21", 1,  +0, { G5 },         "345d1108d154761edd3d2548a89855037fb15154777e3479c5df359eed6a46f5" }, // ffo-21;wwwwwwwwbwwbbb..bbwwbww.bwbwww..bwwwwb..bwwbww..bwwwww..bbbb....;w; G5:+0. G2:-2. G4:-4. G6:-6;
    {"ffo-22", 1,  +2, { G8 },         "a37abd8c44bd99a4caeec61558cd3c94e82d01dafaabc3fce7fe9635465c32a3" }, // ffo-22;..wwww..b.wwwww.bbwwbwbbbwbwbbbbbbbwbbbb.bbwbwbb..wbbb.b....b...;w; G8:+2. A6:+0. F8:-4. A7:-4. H2:-4. B2:-6. D8:-8. B7:-14. G7:-26;
    {"ffo-23", 1,  +4, { A2 },         "75797e7a8c10bf4402c72d048d2d0a94934d35f051f863c8f8c56b3d714db35f" }, // ffo-23;..w.......wwb...wwwbbbw.wwwwbwbbbbbwwbwbbbbbbwwbb.bbbbwb..bbbb..;b; A2:+4. D1:-20. H3:-20. B1:-30. G2:-30. E1:-30. F2:-34. G8:-34. B2:-36. H2:-38;
    {"ffo-24", 1,  +0, { C3 },         "9a5f325dab822cff7e7810a4661d02ce5bb5de69ab9213d443df5f5c7873186f" }, // ffo-24;..w..w.....wwwb..b.bwbww..bbbwwwbbbbwwwwbbbwbbwwbbbbbb..bwbb.w..;w; C3:+0. B4:-4. C2:-8. E8:-12. G7:-14. H2:-16. G1:-24;
    {"ffo-25", 2,  +0, { G1, A5 },     "bd2a938c87556c7f0cec0840a3a895a61d5345837f5c8f75b0679d1a0112d0b6" }, // ffo-25;....b......bbbw..wwwbbbbbwwwwbbw.bbwwbbwwwbwbbbbwwwbb...b.bbbb..;w; G1:+0. A5:+0. F1:-4. D1:-6. F7:-8. C2:-10. G7:-10. H2:-12. H7:-16;
    {"ffo-26", 1,  +0, { D8 },         "d1e162de3dc89d039eeabda515e5984827e65c7b6d3b971695dfdf35aafab3af" }, // ffo-26;.wwwww....wbbw...wwwwbbw.wwwbwbb.wwbwwbb.bwbbwbb..w.bbbb..w....w;b; D8:+0. A6:-2. A4:-6. B7:-6. A5:-12. G1:-16. A2:-16. A3:-18. H2:-18. B8:-20. G2:-20. B2:-26;
    {"ffo-27", 1,  -2, { B7 },         "23fa980ff745e949cf5a32abb9be6a7b071c591f6ffef6c83e88251f5ef59f30" }, // ffo-27;..bw.w....wwww..wwbwbbw.wwwwbbwwwwwbbwb.wbwbbbbb..bbbb....b.w.b.;b; B7:-2. E1:-4. B1:-6. H2:-10. H5:-10. B2:-12. A2:-14. H3:-28. G1:-28. G2:-28;
    {"ffo-28", 3,  +0, { F1, B2, E1 }, "4d86c09d124bc7338ec9d66b616c85337a2d7cc0f3bcc1004a5cef7a73b57901" }, // ffo-28;..w.......www..b.bwwwwbbbbbbwbwb.bbwbwwbbbwbwwbb.wwwww.b...www..;b; F1:+0. B2:+0. E1:+0. B1:-4. F2:-6. G7:-6. D1:-12. C8:-20. G8:-22. B8:-28;
    {"ffo-29", 1, +10, { G2 },         "4f18d1a3f89bedfd288c8832cfad086781aa6388ae3ac2997d5782f0c9ae3d40" }, // ffo-29;.wbbbb....wbbw..bbwwbwwwbbbwwbwwbbwwbwwwbbbbww.bb.bbw...........;b; G2:+10. A1:+4. G6:-10. H2:-12. F8:-12. E8:-12. G7:-24. G1:-24. B2:-30. F7:-34;
    {NULL, 0, 0, {A1}}
  };



/* Test function prototypes. */

static void
game_position_es_solve_test (GamePositionDbFixture *fixture,
                             gconstpointer test_data);



/* Helper function prototypes. */

static GamePositionDb *
gpdb_setup (gchar *source);

static void
gpdb_ffo_fixture_setup (GamePositionDbFixture *fixture,
                        gconstpointer test_data);

static void
gpdb_fixture_teardown (GamePositionDbFixture *fixture,
                       gconstpointer test_data);

static GamePositionX *
get_gpx_from_db (GamePositionDb *db,
                 gchar *id);

static void
run_test_case_array (GamePositionDb *db,
                     const TestCase tca[],
                     ExactSolution* (*solver)(const GamePositionX *const gpx,
                                              const endgame_solver_env_t *const env));

static bool
is_move_part_of_array (const Square move,
                       const Square move_array[],
                       const int move_array_length);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  board_module_init();

  g_test_add("/es_pv/ffo_05",
             GamePositionDbFixture,
             (gconstpointer) ffo_05,
             gpdb_ffo_fixture_setup,
             game_position_es_solve_test,
             gpdb_fixture_teardown);

  if (g_test_slow ()) {
    g_test_add("/es_pv/ffo_01_19",
               GamePositionDbFixture,
               (gconstpointer) ffo_01_19,
               gpdb_ffo_fixture_setup,
               game_position_es_solve_test,
               gpdb_fixture_teardown);

    g_test_add("/es_pv/ffo_20_29",
               GamePositionDbFixture,
               (gconstpointer) ffo_20_29,
               gpdb_ffo_fixture_setup,
               game_position_es_solve_test,
               gpdb_fixture_teardown);
  }

  return g_test_run();
}



/*
 * Test functions.
 */

static void
game_position_es_solve_test (GamePositionDbFixture *fixture,
                             gconstpointer test_data)
{
  GamePositionDb *db = fixture->db;
  TestCase *tcap = (TestCase *) test_data;
  run_test_case_array(db, tcap, game_position_es_solve);
}



/*
 * Internal functions.
 */

static GamePositionDb *
gpdb_setup (gchar *source)
{
  GamePositionDb *db;
  gpdb_syntax_error_log_t *syntax_error_log;
  FILE *fp;
  GError *error;

  /* Loads the game position database. */
  fp = fopen(source, "r");
  if (!fp) {
    g_test_message("Unable to open database test file \"%s\" for reading.\n", source);
    g_test_fail();
  }
  g_assert(fp);
  db = gpdb_new(g_strdup(source));
  syntax_error_log = NULL;
  error = NULL;
  gpdb_load(fp, source, db, &syntax_error_log, &error);
  fclose(fp);
  g_free(source);

  /* Removes the tmp file, frees the resources. */
  g_free(error);
  if (syntax_error_log)
    gpdb_syntax_error_log_free(syntax_error_log);

  g_assert (db);
  return db;
}

static void
gpdb_ffo_fixture_setup (GamePositionDbFixture *fixture,
                        gconstpointer test_data)
{
  gchar *source = g_strdup("db/gpdb-ffo.txt");
  fixture->db = gpdb_setup(source);
}

static void
gpdb_fixture_teardown (GamePositionDbFixture *fixture,
                       gconstpointer test_data)
{
  g_assert(fixture->db != NULL);
  gpdb_free(fixture->db, TRUE);
}

static GamePositionX *
get_gpx_from_db (GamePositionDb *db,
                 gchar *id)
{
  gpdb_entry_t *entry = gpdb_lookup(db, id);
  if (!entry) {
    g_test_message("The entry \"%s\" is missing from game position database.\n", id);
    g_test_fail();
  }
  //g_assert(entry);
  return gpdb_get_gpx(entry);
}

static void
run_test_case_array (GamePositionDb *db,
                     const TestCase tca[],
                     ExactSolution* (*solver)(const GamePositionX *const gpx,
                                              const endgame_solver_env_t *const env))
{
  const endgame_solver_env_t endgame_solver_env =
    { .log_file = NULL,
      .pve_dump_file = NULL,
      .repeats = 0,
      .pv_recording = true,
      .pv_full_recording = true,
      .pv_no_print = true
    };

  const size_t buffer_size = 4096;
  char buffer[buffer_size];
  size_t nc;

  char pv_out_file_path[1024];

  sha3_ctx_t ctx;
  char pv_digest[sha3_256_digest_lenght];
  char pv_digest_as_string[sha3_256_digest_lenght * 2 + 1];

  char moves_to_s[256];

  const TestCase *tc = NULL;
  for (int i = 0;; i++) {
    tc = &tca[i];
    if (tc->gpdb_label == NULL) break;

    if (g_test_verbose()) {
      square_array_to_string(moves_to_s, tc->best_move, tc->best_move_count);
      printf("Test #%3d: data[position=%s, expected_value=%+03d, expected_best_moves={%s}]; ", i, tc->gpdb_label, tc->outcome, moves_to_s);
    }

    GamePositionX *const gpx = get_gpx_from_db(db, tc->gpdb_label);
    ExactSolution *const solution = solver(gpx, &endgame_solver_env);

    pve_transform_to_standard_form(solution->pve);

    sprintf(pv_out_file_path, "build/tmp/%s_pv.txt", tc->gpdb_label);
    FILE *fp = fopen(pv_out_file_path, "w+");
    assert(fp);

    pve_line_with_variants_to_stream(solution->pve, fp);

    rewind(fp);

    sha3_init(&ctx, sha3_256_digest_lenght);
    do {
      nc = fread(buffer, 1, buffer_size, fp);
      sha3_update(&ctx, buffer, nc);
    } while (nc == buffer_size);
    sha3_final(&ctx, pv_digest);
    sha3_msg_digest_to_string(pv_digest_as_string, pv_digest, sha3_256_digest_lenght);

    int fclose_ret = fclose(fp);
    assert(fclose_ret == 0);
    (void) fclose_ret; /* Suppress the warning "unused variable" raised when compiling without assertions. */

    const bool ok_value = tc->outcome == solution->outcome;
    const bool ok_move = is_move_part_of_array(solution->best_move, tc->best_move, tc->best_move_count);
    const bool ok_pv = strcmp(tc->pv_sha3_256, pv_digest_as_string) == 0;
    if (!ok_value || !ok_move || !ok_pv) g_test_fail();

    if (g_test_verbose()) {
      printf("result[outcome=%+03d, move=%s, sha=%s]", solution->outcome, square_to_string(solution->best_move), pv_digest_as_string);
      printf(": %s\n", (ok_value && ok_move && ok_pv) ? "OK": "KO");
    }

    remove(pv_out_file_path);
    exact_solution_free(solution);
    free(gpx);
  }
}

static bool
is_move_part_of_array (const Square move,
                       const Square move_array[],
                       const int move_array_length)
{
  for (int i = 0; i < move_array_length; i++) {
    if (move == move_array[i]) return true;
  }
  return false;
}
