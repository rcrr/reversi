/**
 * @file
 *
 * @brief Unit tests for the exact solver principal variation computation.
 * @details Collects tests and helper methods for the es solver.
 *
 * @par ut_es_pv.c
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

#include "board.h"
#include "game_position_db.h"
#include "sha3.h"

#include "exact_solver.h"

#include "unit_test.h"



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
dummy_t (ut_test_t *const t)
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

  ut_suite_t *const s = ut_suite_new(&config, "es_pv");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dummy", dummy_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
