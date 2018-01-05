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



typedef SquareSet (*board_trans_f) (SquareSet);



static const SquareSet ar            = 0x22120a0e1222221e;
static const SquareSet ar_flipped_v  = 0x1e2222120e0a1222;

static const SquareSet full          = 0xffffffffffffffff;
static const SquareSet empty         = 0x0000000000000000;
static const SquareSet half_left     = 0x0f0f0f0f0f0f0f0f;
static const SquareSet half_right    = 0xf0f0f0f0f0f0f0f0;
static const SquareSet half_top      = 0x00000000ffffffff;
static const SquareSet half_bottom   = 0xffffffff00000000;

static const SquareSet diag_H1_A8    = 0x0102040810204080;
static const SquareSet diag_A1_H8    = 0x8040201008040201;

static const SquareSet squares_A1_H1 = 0x0000000000000081;
static const SquareSet squares_A8_H8 = 0x8100000000000000;



/*
 * Auxiliary functions.
 */

static void
aux_verify_transformation (ut_test_t *const t,
                           const SquareSet expected,
                           const SquareSet input,
                           const board_trans_f transformation)
{
  ut_assert(t, expected == transformation(input));
}



/*
 * Test functions.
 */

static void
board_trans_dummy_t (ut_test_t *const t)
{
  char buf[256];
  GamePositionX gp;

  gp.blacks = ar;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\nar:\n%s\n\n", buf);

  gp.blacks = ar_flipped_v;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\nar_flipped_v:\n%s\n\n", buf);

  gp.blacks = full;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\nfull:\n%s\n\n", buf);

  gp.blacks = empty;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\nempty:\n%s\n\n", buf);

  gp.blacks = half_left;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\nhalf_left:\n%s\n\n", buf);

  gp.blacks = half_right;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\nhalf_right:\n%s\n\n", buf);

  gp.blacks = half_top;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\nhalf_top:\n%s\n\n", buf);

  gp.blacks = half_bottom;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\nhalf_bottom:\n%s\n\n", buf);

  gp.blacks = diag_H1_A8;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\ndiag_H1_A8:\n%s\n\n", buf);

  gp.blacks = diag_A1_H8;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\ndiag_A1_H8:\n%s\n\n", buf);

  gp.blacks = squares_A1_H1;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\nsquares_A1_H1:\n%s\n\n", buf);

  gp.blacks = squares_A8_H8;
  gp.whites = 0x0000000000000000;
  gp.player = 0;
  game_position_x_print(buf, &gp);
  printf("\nsquares_A8_H8:\n%s\n\n", buf);

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
  aux_verify_transformation(t, diag_H1_A8,    diag_A1_H8,    board_trans_flip_vertical);
  aux_verify_transformation(t, diag_A1_H8,    diag_H1_A8,    board_trans_flip_vertical);
  aux_verify_transformation(t, squares_A1_H1, squares_A8_H8, board_trans_flip_vertical);
  aux_verify_transformation(t, squares_A8_H8, squares_A1_H1, board_trans_flip_vertical);
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

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
