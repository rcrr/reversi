/**
 * @file
 *
 * @brief Utest unit test suite.
 * @details Collects tests and helper methods for the utest program.
 *
 * @par ut_utest.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2015, 2017 Roberto Corradini. All rights reserved.
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

#include "unit_test.h"



/*
 * Auxiliary functions.
 */

static void
aux_dummy (void)
{
  return;
}



/*
 * Test functions.
 */

static void
dummy_t (ut_test_t *const t)
{
  aux_dummy();
  ut_assert(t, true);

  ut_assert(t, true);
  ut_assert(t, 1 == 34);
}

static void
utest_pass_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
utest_fail_t (ut_test_t *const t)
{
  ut_assert(t, false);
}

static void
utest_abort_t (ut_test_t *const t)
{
  abort();
}

static void
utest_exit_t (ut_test_t *const t)
{
  exit(7);
}

static void
utest_assert_false_t (ut_test_t *const t)
{
  assert(false);
}

static void
ut_quickness_range_t (ut_test_t *const t)
{
  timespec_t ts;
  int r;


  /* UT_QUICKNESS_0001 */

  timespec_set(&ts, 0, 0);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_0001);

  timespec_set(&ts, 0, 999999);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_0001);


  /* UT_QUICKNESS_001 */

  timespec_set(&ts, 0, 1000000);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_001);

  timespec_set(&ts, 0, 1000001);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_001);

  timespec_set(&ts, 0, 9999999);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_001);


  /* UT_QUICKNESS_01 */

  timespec_set(&ts, 0, 10000000);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_01);

  timespec_set(&ts, 0, 10000001);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_01);

  timespec_set(&ts, 0, 99999999);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_01);


  /* UT_QUICKNESS_1 */

  timespec_set(&ts, 0, 100000000);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1);

  timespec_set(&ts, 0, 100000001);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1);

  timespec_set(&ts, 0, 999999999);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1);


  /* UT_QUICKNESS_10 */

  timespec_set(&ts, 1, 0);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_10);

  timespec_set(&ts, 1, 1);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_10);

  timespec_set(&ts, 9, 999999999);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_10);


  /* UT_QUICKNESS_100 */

  timespec_set(&ts, 10, 0);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_100);

  timespec_set(&ts, 10, 1);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_100);

  timespec_set(&ts, 99, 999999999);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_100);


  /* UT_QUICKNESS_1000 */

  timespec_set(&ts, 100, 0);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1000);

  timespec_set(&ts, 100, 1);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1000);

  timespec_set(&ts, 999, 999999999);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1000);


  /* UT_QUICKNESS_OUT_OF_RANGE */

  timespec_set(&ts, 1000, 0);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_OUT_OF_RANGE);

  timespec_set(&ts, 1000, 1);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_OUT_OF_RANGE);

  timespec_set(&ts, 3153599999, 999999999);
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_OUT_OF_RANGE);
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

  ut_suite_t *const s = ut_suite_new(&config, "utest");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dummy", dummy_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "pass", utest_pass_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "fail", utest_fail_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "abort", utest_abort_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "exit", utest_exit_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "assert_false", utest_assert_false_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "ut_quickness_range", ut_quickness_range_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
