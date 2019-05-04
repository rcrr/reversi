/**
 * @file
 *
 * @brief Inverse Square Root module unit test suite.
 * @details Collects tests and helper methods for the inverse square root module.
 *
 * @par ut_isqrt.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2019 Roberto Corradini. All rights reserved.
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
#include <math.h>

#include "unit_test.h"
#include "isqrt.h"



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
isqrt_double_newton_t (ut_test_t *const t)
{
  double x, x0, e, r, expected, estimated_error;

  x = 1.;
  expected = 1.;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_newton(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 100.;
  expected = 0.1;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_newton(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 9.;
  expected = 1. / 3.;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_newton(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 2.;
  expected = 0.707106781186548;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_newton(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 0.0001;
  expected = 100.;
  x0 = 1.;
  e = 1.e-12;
  r = isqrt_double_newton(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);
}

static void
isqrt_double_halley_t (ut_test_t *const t)
{
  double x, x0, e, r, expected, estimated_error;

  x = 1.;
  expected = 1.;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_halley(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 100.;
  expected = 0.1;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_halley(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 9.;
  expected = 1. / 3.;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_halley(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 2.;
  expected = 0.707106781186548;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_halley(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 0.0001;
  expected = 100.;
  x0 = 1.;
  e = 1.e-12;
  r = isqrt_double_halley(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);
}

static void
isqrt_double_householder3_t (ut_test_t *const t)
{
  double x, x0, e, r, expected, estimated_error;

  x = 1.;
  expected = 1.;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_householder3(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 100.;
  expected = 0.1;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_householder3(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 9.;
  expected = 1. / 3.;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_householder3(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 2.;
  expected = 0.707106781186548;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_householder3(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 0.0001;
  expected = 100.;
  x0 = 1.;
  e = 1.e-12;
  r = isqrt_double_householder3(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);
}

static void
isqrt_double_halley2_t (ut_test_t *const t)
{
  double x, x0, e, r, expected, estimated_error;

  x = 1.;
  expected = 1.;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_halley2(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 100.;
  expected = 0.1;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_halley2(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 9.;
  expected = 1. / 3.;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_halley2(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 2.;
  expected = 0.707106781186548;
  x0 = 1.;
  e = 1.e-15;
  r = isqrt_double_halley2(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);

  x = 0.0001;
  expected = 100.;
  x0 = 1.;
  e = 1.e-12;
  r = isqrt_double_halley2(x, e, x0, &estimated_error);
  ut_assert(t, fabs(r - expected) < e);
  ut_assert(t, estimated_error < e);
}

static void
isqrt_double_quake_t (ut_test_t *const t)
{
  double r, x, e, expected;

  x = 1.;
  expected = 1.;
  e = 1.e-15;
  r = isqrt_double_quake(x);
  ut_assert(t, fabs(r - expected) < e);

  x = 100.;
  expected = 0.1;
  e = 1.e-15;
  r = isqrt_double_quake(x);
  ut_assert(t, fabs(r - expected) < e);

  x = 9.;
  expected = 1. / 3.;
  e = 1.e-15;
  r = isqrt_double_quake(x);
  ut_assert(t, fabs(r - expected) < e);

  x = 2.;
  expected = 0.707106781186548;
  e = 1.e-15;
  r = isqrt_double_quake(x);
  ut_assert(t, fabs(r - expected) < e);

  x = 0.0001;
  expected = 100.;
  e = 1.e-12;
  r = isqrt_double_quake(x);
  ut_assert(t, fabs(r - expected) < e);
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

  ut_suite_t *const s = ut_suite_new(&config, "isqrt");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "isqrt_double_newton", isqrt_double_newton_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "isqrt_double_halley", isqrt_double_halley_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "isqrt_double_householder3", isqrt_double_householder3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "isqrt_double_halley2", isqrt_double_halley2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "isqrt_double_quake", isqrt_double_quake_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
