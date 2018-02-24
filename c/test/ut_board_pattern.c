/**
 * @file
 *
 * @brief Board pattern module unit test suite.
 * @details Collects tests and helper methods for the board pattern module.
 *
 * @par ut_board_pattern.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2018 Roberto Corradini. All rights reserved.
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
#include <assert.h>
#include <string.h>

#include "unit_test.h"
#include "board_pattern.h"



static const SquareSet n_edge = 0x00000000000000ff;
static const SquareSet e_edge = 0x8080808080808080;
static const SquareSet s_edge = 0xff00000000000000;
static const SquareSet w_edge = 0x0101010101010101;
static const SquareSet empty  = 0x0000000000000000;
static const SquareSet full   = 0xffffffffffffffff;
static const SquareSet border = 0xff818181818181ff;

static const SquareSet zebra_e = 0x0000000000000055;
static const SquareSet zebra_o = 0x00000000000000aa;


/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
board_pattern_dummy_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_pattern_compute_indexes_edge_t (ut_test_t *const t)
{
  board_t board;
  board_t *b;
  SquareSet m, o;
  board_pattern_index_t indexes[4];
  board_pattern_index_t expected[4];
  b = &board;

  m = empty;
  o = empty;
  expected[0] =    0;
  expected[1] =    0;
  expected[2] =    0;
  expected[3] =    0;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = full;
  o = empty;
  expected[0] = 3280;
  expected[1] = 3280;
  expected[2] = 3280;
  expected[3] = 3280;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = empty;
  o = full;
  expected[0] = 6560;
  expected[1] = 6560;
  expected[2] = 6560;
  expected[3] = 6560;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = border;
  o = empty;
  expected[0] = 3280;
  expected[1] = 3280;
  expected[2] = 3280;
  expected[3] = 3280;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = empty;
  o = border;
  expected[0] = 6560;
  expected[1] = 6560;
  expected[2] = 6560;
  expected[3] = 6560;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = n_edge;
  o = empty;
  expected[0] = 3280;
  expected[1] =    1;
  expected[2] =    0;
  expected[3] = 2187;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = e_edge;
  o = empty;
  expected[0] = 2187;
  expected[1] = 3280;
  expected[2] =    1;
  expected[3] =    0;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = s_edge;
  o = empty;
  expected[0] =    0;
  expected[1] = 2187;
  expected[2] = 3280;
  expected[3] =    1;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = w_edge;
  o = empty;
  expected[0] =    1;
  expected[1] =    0;
  expected[2] = 2187;
  expected[3] = 3280;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = empty;
  o = n_edge;
  expected[0] = 6560;
  expected[1] =    2;
  expected[2] =    0;
  expected[3] = 4374;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = empty;
  o = e_edge;
  expected[0] = 4374;
  expected[1] = 6560;
  expected[2] =    2;
  expected[3] =    0;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = empty;
  o = s_edge;
  expected[0] =    0;
  expected[1] = 4374;
  expected[2] = 6560;
  expected[3] =    2;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = empty;
  o = w_edge;
  expected[0] =    2;
  expected[1] =    0;
  expected[2] = 4374;
  expected[3] = 6560;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = zebra_e;
  o = empty;
  expected[0] =  820;
  expected[1] =    0;
  expected[2] =    0;
  expected[3] = 2187;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = empty;
  o = zebra_e;
  expected[0] = 1640;
  expected[1] =    0;
  expected[2] =    0;
  expected[3] = 4374;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = zebra_o;
  o = empty;
  expected[0] = 2460;
  expected[1] =    1;
  expected[2] =    0;
  expected[3] =    0;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = empty;
  o = zebra_o;
  expected[0] = 4920;
  expected[1] =    2;
  expected[2] =    0;
  expected[3] =    0;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = zebra_e;
  o = zebra_o;
  expected[0] = 5740;
  expected[1] =    2;
  expected[2] =    0;
  expected[3] = 2187;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);

  m = zebra_o;
  o = zebra_e;
  expected[0] = 4100;
  expected[1] =    1;
  expected[2] =    0;
  expected[3] = 4374;
  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(indexes, &board_patterns[BOARD_PATTERN_EDGE], b);
  for (int i = 0; i < 4; i++) ut_assert(t, expected[i] == indexes[i]);
}

static void
board_patterns_t (ut_test_t *const t)
{
  for (unsigned int bp = 0; bp < BOARD_PATTERN_INVALID; bp++) {
    ut_assert(t, bp == board_patterns[bp].id);
    const SquareSet base_mask = board_patterns[bp].masks[0];
    const unsigned int n_instances = board_patterns[bp].n_instances;
    const unsigned int n_squares = board_patterns[bp].n_squares;
    for (unsigned int i = 0; i < n_instances; i++) {
      const SquareSet mask = board_patterns[bp].masks[i];
      const board_trans_f ttp = board_patterns[bp].trans_to_principal_f[i];
      ut_assert(t, n_squares == bitw_bit_count_64(mask));
      ut_assert(t, base_mask == ttp(mask));
    }
  }
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_prog_arg_config_t config;
  ut_init(&config, &argc, &argv);

  ut_suite_t *const s = ut_suite_new(&config, "board_pattern");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_dummy", board_pattern_dummy_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_edge", board_pattern_compute_indexes_edge_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_patterns", board_patterns_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
