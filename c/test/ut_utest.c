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

  ts.tv_sec = 0; ts.tv_nsec = 0;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_0001);

  ts.tv_sec = 0; ts.tv_nsec = 999999;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_0001);


  /* UT_QUICKNESS_001 */

  ts.tv_sec = 0; ts.tv_nsec = 1000000;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_001);

  ts.tv_sec = 0; ts.tv_nsec = 1000001;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_001);

  ts.tv_sec = 0; ts.tv_nsec = 9999999;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_001);


  /* UT_QUICKNESS_01 */

  ts.tv_sec = 0; ts.tv_nsec = 10000000;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_01);

  ts.tv_sec = 0; ts.tv_nsec = 10000001;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_01);

  ts.tv_sec = 0; ts.tv_nsec = 99999999;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_01);


  /* UT_QUICKNESS_1 */

  ts.tv_sec = 0; ts.tv_nsec = 100000000;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1);

  ts.tv_sec = 0; ts.tv_nsec = 100000001;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1);

  ts.tv_sec = 0; ts.tv_nsec = 999999999;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1);


  /* UT_QUICKNESS_10 */

  ts.tv_sec = 1; ts.tv_nsec = 0;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_10);

  ts.tv_sec = 1; ts.tv_nsec = 1;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_10);

  ts.tv_sec = 9; ts.tv_nsec = 999999999;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_10);


  /* UT_QUICKNESS_100 */

  ts.tv_sec = 10; ts.tv_nsec = 0;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_100);

  ts.tv_sec = 10; ts.tv_nsec = 1;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_100);

  ts.tv_sec = 99; ts.tv_nsec = 999999999;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_100);


  /* UT_QUICKNESS_1000 */

  ts.tv_sec = 100; ts.tv_nsec = 0;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1000);

  ts.tv_sec = 100; ts.tv_nsec = 1;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1000);

  ts.tv_sec = 999; ts.tv_nsec = 999999999;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_1000);


  /* UT_QUICKNESS_OUT_OF_RANGE */

  ts.tv_sec = 1000; ts.tv_nsec = 0;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_OUT_OF_RANGE);

  ts.tv_sec = 1000; ts.tv_nsec = 1;
  r = ut_quickness_range(&ts);
  ut_assert(t, r == UT_QUICKNESS_OUT_OF_RANGE);

  ts.tv_sec = 9999; ts.tv_nsec = 999999999;
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
  ut_init(&argc, &argv);

  ut_suite_t *const s = ut_suite_new("utest");

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
