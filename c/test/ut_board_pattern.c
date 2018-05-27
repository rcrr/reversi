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

static void
aux_check_expected_indexes (ut_test_t *const t,
                            board_pattern_id_t p_id,
                            SquareSet m,
                            SquareSet o,
                            board_pattern_index_t *expected_indexes)
{
  const board_pattern_t *p;
  int n;
  board_t board;
  board_t *b;
  board_pattern_index_t computed_indexes[8];

  p = &board_patterns[p_id];
  n = p->n_instances;
  b = &board;

  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(computed_indexes, p, b);
  for (int i = 0; i < n; i++) ut_assert(t, expected_indexes[i] == computed_indexes[i]);
}



/*
 * Test functions.
 */

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

static void
board_pattern_compute_indexes_edge_t (ut_test_t *const t)
{
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, empty, empty, (board_pattern_index_t[]) {0, 0, 0, 0});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, full, empty, (board_pattern_index_t[]) {3280, 3280, 3280, 3280});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, empty, full, (board_pattern_index_t[]) {6560, 6560, 6560, 6560});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, border, empty, (board_pattern_index_t[]) {3280, 3280, 3280, 3280});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, empty, border, (board_pattern_index_t[]) {6560, 6560, 6560, 6560});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, n_edge, empty, (board_pattern_index_t[]) {3280, 1, 0, 2187});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, e_edge, empty, (board_pattern_index_t[]) {2187, 3280, 1, 0});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, s_edge, empty, (board_pattern_index_t[]) {0, 2187, 3280, 1});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, w_edge, empty, (board_pattern_index_t[]) {1, 0, 2187, 3280});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, empty, n_edge, (board_pattern_index_t[]) {6560, 2, 0, 4374});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, empty, e_edge, (board_pattern_index_t[]) {4374, 6560, 2, 0});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, empty, s_edge, (board_pattern_index_t[]) {0, 4374, 6560, 2});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, empty, w_edge, (board_pattern_index_t[]) {2, 0, 4374, 6560});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, zebra_e, empty, (board_pattern_index_t[]) {820, 0, 0, 2187});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, empty, zebra_e, (board_pattern_index_t[]) {1640, 0, 0, 4374});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, zebra_o, empty, (board_pattern_index_t[]) {2460, 1, 0, 0});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, empty, zebra_o, (board_pattern_index_t[]) {4920, 2, 0, 0});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, zebra_e, zebra_o, (board_pattern_index_t[]) {5740, 2, 0, 2187});
  aux_check_expected_indexes(t, BOARD_PATTERN_EDGE, zebra_o, zebra_e, (board_pattern_index_t[]) {4100, 1, 0, 4374});
}

static void
board_pattern_compute_indexes_corner_t (ut_test_t *const t)
{
  aux_check_expected_indexes(t, BOARD_PATTERN_CORNER, empty, empty, (board_pattern_index_t[]) {0, 0, 0, 0});
}

static void
board_pattern_compute_indexes_xedge_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_pattern_compute_indexes_r2_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_pattern_compute_indexes_r3_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_pattern_compute_indexes_r4_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_pattern_compute_indexes_diag4_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_pattern_compute_indexes_diag5_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_pattern_compute_indexes_diag6_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_pattern_compute_indexes_diag7_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_pattern_compute_indexes_diag8_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_pattern_compute_indexes_2x5cor_t (ut_test_t *const t)
{
  ut_assert(t, true);
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

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_patterns", board_patterns_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_edge", board_pattern_compute_indexes_edge_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_corner", board_pattern_compute_indexes_corner_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_xedge", board_pattern_compute_indexes_xedge_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_r2", board_pattern_compute_indexes_r2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_r3", board_pattern_compute_indexes_r3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_r4", board_pattern_compute_indexes_r4_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag4", board_pattern_compute_indexes_diag4_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag5", board_pattern_compute_indexes_diag5_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag6", board_pattern_compute_indexes_diag6_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag7", board_pattern_compute_indexes_diag7_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag8", board_pattern_compute_indexes_diag8_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_2x5cor", board_pattern_compute_indexes_2x5cor_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
