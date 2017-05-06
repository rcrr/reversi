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



/*
 * Test functions.
 */

static void
utest_pass_test (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
utest_fail_test (ut_test_t *const t)
{
  ut_assert(t, false);
}

static void
utest_abort_test (ut_test_t *const t)
{
  abort();
}

static void
utest_exit_test (ut_test_t *const t)
{
  exit(7);
}

static void
utest_assert_false_test (ut_test_t *const t)
{
  assert(false);
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

  ut_suite_add_simple_test(s, "pass", utest_pass_test);
  ut_suite_add_simple_test(s, "fail", utest_fail_test);
  ut_suite_add_simple_test(s, "abort", utest_abort_test);
  ut_suite_add_simple_test(s, "exit", utest_exit_test);
  ut_suite_add_simple_test(s, "assert_false", utest_assert_false_test);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
