/**
 * @file
 *
 * @brief Board transformations module unit test suite.
 * @details Collects tests and helper methods for the board transformations module.
 *
 * @par ut_board_trans.c
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
#include "board_trans.h"



static const SquareSet ar            = 0x22120a0e1222221e;

static const SquareSet ar_flipped_v  = 0x1e2222120e0a1222;
static const SquareSet ar_mirror_h   = 0x4448507048444478;
static const SquareSet ar_flip_h1a8  = 0x00ff888c92610000;
static const SquareSet ar_flip_a1h8  = 0x000086493111ff00;
static const SquareSet ar_rotate_180 = 0x7844444870504844;
static const SquareSet ar_rotate_90c = 0x000061928c88ff00;
static const SquareSet ar_rotate_90a = 0x00ff113149860000;

static const SquareSet full          = 0xffffffffffffffff;
static const SquareSet empty         = 0x0000000000000000;
static const SquareSet half_left     = 0x0f0f0f0f0f0f0f0f;
static const SquareSet half_right    = 0xf0f0f0f0f0f0f0f0;
static const SquareSet half_top      = 0x00000000ffffffff;
static const SquareSet half_bottom   = 0xffffffff00000000;

static const SquareSet row_1         = 0x00000000000000ff;
static const SquareSet row_8         = 0xff00000000000000;

static const SquareSet column_a      = 0x0101010101010101;
static const SquareSet column_h      = 0x8080808080808080;

static const SquareSet diag_h1a8     = 0x0102040810204080;
static const SquareSet diag_a1h8     = 0x8040201008040201;

static const SquareSet square_a1     = 0x0000000000000001;
static const SquareSet square_h1     = 0x0000000000000080;
static const SquareSet square_a8     = 0x0100000000000000;
static const SquareSet square_h8     = 0x8000000000000000;

static const SquareSet squares_a1_h1 = 0x0000000000000081;
static const SquareSet squares_a8_h8 = 0x8100000000000000;
static const SquareSet squares_h1_h8 = 0x8000000000000080;
static const SquareSet squares_a1_a8 = 0x0100000000000001;

static const SquareSet squares_a1_b1 = 0x0000000000000003;
static const SquareSet squares_g8_h8 = 0xc000000000000000;
static const SquareSet squares_h1_h2 = 0x0000000000008080;
static const SquareSet squares_a7_a8 = 0x0101000000000000;



/*
 * Auxiliary functions.
 */

static void
aux_verify_transformation (ut_test_t *const t,
                           const SquareSet expected,
                           const SquareSet input,
                           const board_trans_f transformation)
{
  char buf[256];
  const SquareSet transformed = transformation(input);
  const bool result = (expected == transformed);
  if (!result) {
    square_set_print(buf, input);
    printf("\n\ninput:\n%s\n\n", buf);
    square_set_print(buf, expected);
    printf("expected:\n%s\n\n", buf);
    square_set_print(buf, transformed);
    printf("transformed:\n%s\n\n", buf);
  }
  ut_assert(t, result);
}



/*
 * Test functions.
 */

static void
board_trans_dummy_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
board_trans_flip_fertical_t (ut_test_t *const t)
{
  aux_verify_transformation(t, ar_flipped_v,  ar,            board_trans_flip_vertical);
  aux_verify_transformation(t, full,          full,          board_trans_flip_vertical);
  aux_verify_transformation(t, empty,         empty,         board_trans_flip_vertical);
  aux_verify_transformation(t, half_left,     half_left,     board_trans_flip_vertical);
  aux_verify_transformation(t, half_right,    half_right,    board_trans_flip_vertical);
  aux_verify_transformation(t, half_top,      half_bottom,   board_trans_flip_vertical);
  aux_verify_transformation(t, half_bottom,   half_top,      board_trans_flip_vertical);
  aux_verify_transformation(t, row_1,         row_8,         board_trans_flip_vertical);
  aux_verify_transformation(t, row_8,         row_1,         board_trans_flip_vertical);
  aux_verify_transformation(t, column_a,      column_a,      board_trans_flip_vertical);
  aux_verify_transformation(t, column_h,      column_h,      board_trans_flip_vertical);
  aux_verify_transformation(t, diag_h1a8,     diag_a1h8,     board_trans_flip_vertical);
  aux_verify_transformation(t, diag_a1h8,     diag_h1a8,     board_trans_flip_vertical);
  aux_verify_transformation(t, squares_a1_h1, squares_a8_h8, board_trans_flip_vertical);
  aux_verify_transformation(t, squares_a8_h8, squares_a1_h1, board_trans_flip_vertical);
}

static void
board_trans_mirror_horizontal_t (ut_test_t *const t)
{
  aux_verify_transformation(t, ar_mirror_h,   ar,            board_trans_mirror_horizontal);
  aux_verify_transformation(t, full,          full,          board_trans_mirror_horizontal);
  aux_verify_transformation(t, empty,         empty,         board_trans_mirror_horizontal);
  aux_verify_transformation(t, half_left,     half_right,    board_trans_mirror_horizontal);
  aux_verify_transformation(t, half_right,    half_left,     board_trans_mirror_horizontal);
  aux_verify_transformation(t, half_top,      half_top,      board_trans_mirror_horizontal);
  aux_verify_transformation(t, half_bottom,   half_bottom,   board_trans_mirror_horizontal);
  aux_verify_transformation(t, row_1,         row_1,         board_trans_mirror_horizontal);
  aux_verify_transformation(t, row_8,         row_8,         board_trans_mirror_horizontal);
  aux_verify_transformation(t, column_a,      column_h,      board_trans_mirror_horizontal);
  aux_verify_transformation(t, column_h,      column_a,      board_trans_mirror_horizontal);
  aux_verify_transformation(t, diag_h1a8,     diag_a1h8,     board_trans_mirror_horizontal);
  aux_verify_transformation(t, diag_a1h8,     diag_h1a8,     board_trans_mirror_horizontal);
  aux_verify_transformation(t, squares_a1_h1, squares_a1_h1, board_trans_mirror_horizontal);
  aux_verify_transformation(t, squares_a8_h8, squares_a8_h8, board_trans_mirror_horizontal);
}

static void
board_trans_flip_diag_h1a8_t (ut_test_t *const t)
{
  aux_verify_transformation(t, ar_flip_h1a8,  ar,            board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, full,          full,          board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, empty,         empty,         board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, half_top,      half_right,    board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, half_bottom,   half_left,     board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, half_right,    half_top,      board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, half_left,     half_bottom,   board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, column_h,      row_1,         board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, column_a,      row_8,         board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, row_1,         column_h,      board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, row_8,         column_a,      board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, diag_a1h8,     diag_a1h8,     board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, diag_h1a8,     diag_h1a8,     board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, squares_h1_h8, squares_a1_h1, board_trans_flip_diag_h1a8);
  aux_verify_transformation(t, squares_a1_a8, squares_a8_h8, board_trans_flip_diag_h1a8);
}

static void
board_trans_flip_diag_a1h8_t (ut_test_t *const t)
{
  aux_verify_transformation(t, ar_flip_a1h8,  ar,            board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, full,          full,          board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, empty,         empty,         board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, half_bottom,   half_right,    board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, half_top,      half_left,     board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, half_left,     half_top,      board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, half_right,    half_bottom,   board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, column_a,      row_1,         board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, column_h,      row_8,         board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, row_8,         column_h,      board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, row_1,         column_a,      board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, diag_a1h8,     diag_a1h8,     board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, diag_h1a8,     diag_h1a8,     board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, squares_a1_a8, squares_a1_h1, board_trans_flip_diag_a1h8);
  aux_verify_transformation(t, squares_h1_h8, squares_a8_h8, board_trans_flip_diag_a1h8);
}

static void
board_trans_rotate_180_t (ut_test_t *const t)
{
  aux_verify_transformation(t, ar_rotate_180, ar,            board_trans_rotate_180);
  aux_verify_transformation(t, full,          full,          board_trans_rotate_180);
  aux_verify_transformation(t, empty,         empty,         board_trans_rotate_180);
  aux_verify_transformation(t, half_left,     half_right,    board_trans_rotate_180);
  aux_verify_transformation(t, half_right,    half_left,     board_trans_rotate_180);
  aux_verify_transformation(t, half_bottom,   half_top,      board_trans_rotate_180);
  aux_verify_transformation(t, half_top,      half_bottom,   board_trans_rotate_180);
  aux_verify_transformation(t, row_8,         row_1,         board_trans_rotate_180);
  aux_verify_transformation(t, row_1,         row_8,         board_trans_rotate_180);
  aux_verify_transformation(t, column_a,      column_h,      board_trans_rotate_180);
  aux_verify_transformation(t, column_h,      column_a,      board_trans_rotate_180);
  aux_verify_transformation(t, diag_a1h8,     diag_a1h8,     board_trans_rotate_180);
  aux_verify_transformation(t, diag_h1a8,     diag_h1a8,     board_trans_rotate_180);
  aux_verify_transformation(t, squares_a8_h8, squares_a1_h1, board_trans_rotate_180);
  aux_verify_transformation(t, squares_a1_h1, squares_a8_h8, board_trans_rotate_180);
  aux_verify_transformation(t, squares_g8_h8, squares_a1_b1, board_trans_rotate_180);
}

static void
board_trans_rotate_90c_t (ut_test_t *const t)
{
  aux_verify_transformation(t, ar_rotate_90c, ar,            board_trans_rotate_90c);
  aux_verify_transformation(t, full,          full,          board_trans_rotate_90c);
  aux_verify_transformation(t, empty,         empty,         board_trans_rotate_90c);
  aux_verify_transformation(t, half_bottom,   half_right,    board_trans_rotate_90c);
  aux_verify_transformation(t, half_top,      half_left,     board_trans_rotate_90c);
  aux_verify_transformation(t, half_right,    half_top,      board_trans_rotate_90c);
  aux_verify_transformation(t, half_left,     half_bottom,   board_trans_rotate_90c);
  aux_verify_transformation(t, column_h,      row_1,         board_trans_rotate_90c);
  aux_verify_transformation(t, column_a,      row_8,         board_trans_rotate_90c);
  aux_verify_transformation(t, row_8,         column_h,      board_trans_rotate_90c);
  aux_verify_transformation(t, row_1,         column_a,      board_trans_rotate_90c);
  aux_verify_transformation(t, diag_h1a8,     diag_a1h8,     board_trans_rotate_90c);
  aux_verify_transformation(t, diag_a1h8,     diag_h1a8,     board_trans_rotate_90c);
  aux_verify_transformation(t, squares_h1_h8, squares_a1_h1, board_trans_rotate_90c);
  aux_verify_transformation(t, squares_a1_a8, squares_a8_h8, board_trans_rotate_90c);
  aux_verify_transformation(t, squares_h1_h2, squares_a1_b1, board_trans_rotate_90c);
}

static void
board_trans_rotate_90a_t (ut_test_t *const t)
{
  aux_verify_transformation(t, ar_rotate_90a, ar,            board_trans_rotate_90a);
  aux_verify_transformation(t, full,          full,          board_trans_rotate_90a);
  aux_verify_transformation(t, empty,         empty,         board_trans_rotate_90a);
  aux_verify_transformation(t, half_top,      half_right,    board_trans_rotate_90a);
  aux_verify_transformation(t, half_bottom,   half_left,     board_trans_rotate_90a);
  aux_verify_transformation(t, half_left,     half_top,      board_trans_rotate_90a);
  aux_verify_transformation(t, half_right,    half_bottom,   board_trans_rotate_90a);
  aux_verify_transformation(t, column_a,      row_1,         board_trans_rotate_90a);
  aux_verify_transformation(t, column_h,      row_8,         board_trans_rotate_90a);
  aux_verify_transformation(t, row_1,         column_h,      board_trans_rotate_90a);
  aux_verify_transformation(t, row_8,         column_a,      board_trans_rotate_90a);
  aux_verify_transformation(t, diag_h1a8,     diag_a1h8,     board_trans_rotate_90a);
  aux_verify_transformation(t, diag_a1h8,     diag_h1a8,     board_trans_rotate_90a);
  aux_verify_transformation(t, squares_a1_a8, squares_a1_h1, board_trans_rotate_90a);
  aux_verify_transformation(t, squares_h1_h8, squares_a8_h8, board_trans_rotate_90a);
  aux_verify_transformation(t, squares_a7_a8, squares_a1_b1, board_trans_rotate_90a);
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

  ut_suite_t *const s = ut_suite_new(&config, "board_trans");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_trans_dummy", board_trans_dummy_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_trans_flip_fertical", board_trans_flip_fertical_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_trans_mirror_horizontal", board_trans_mirror_horizontal_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_trans_flip_diag_h1a8", board_trans_flip_diag_h1a8_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_trans_flip_diag_a1h8", board_trans_flip_diag_a1h8_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_trans_rotate_180", board_trans_rotate_180_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_trans_rotate_90c", board_trans_rotate_90c_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_trans_rotate_90a", board_trans_rotate_90a_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
