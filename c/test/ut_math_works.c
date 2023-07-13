/**
 * @file
 *
 * @brief Math Works unit test suite.
 * @details Collects tests and helper methods for the math works module.
 *
 * @par ut_math_works.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2023 Roberto Corradini. All rights reserved.
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
#include "math_works.h"



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
mathw_dummy_t (ut_test_t *const t)
{
  int a = mathw_dummy(3);
  ut_assert(t, a == 3);
}

static void
mathw_gamma_log_t (ut_test_t *const t)
{
  struct t {
    double x; // x argument
    double g; // gamma(x)
    double y; // log(gamma(x))
  };

  const double epsilon = 0.000001;

  struct t d[] = {
    { 1.0,    1.0,            0.0 },
    { 2.0,    1.0,            0.0 },
    { 3.0,    2.0,            0.6931471806 },
    { 4.0,    6.0,            1.7917594692 },
    { 5.0,   24.0,            3.1780538303 },
    { 0.5,    1.772453850905, 0.5723649429 },
    { 0.125,  7.533941598797, 2.0194183576 },
  };

  const size_t len = sizeof(d) / sizeof(struct t);

  for (size_t i = 0; i < len; i++) {
    const double x = d[i].x;
    const double y = mathw_gamma_log(x);
    const double g = exp(y);
    const double delta = fabs(y - d[i].y);
    const double delta_g = fabs(g - d[i].g);
    if (delta > epsilon || delta_g > epsilon) {
      printf("\n");
      printf("x                     = %f\n", x);
      printf("delta                 = %f\n", delta);
      printf("delta_g               = %f\n", delta_g);
      printf("y = log(gamma(x))     = %f\n", y);
      printf("expected y            = %f\n", d[i].y);
      printf("g = gamma(x) = exp(y) = %f\n", g);
      printf("expected g            = %f\n", d[i].g);
      ut_assert(t, false);
    }
  }
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

  ut_suite_t *const s = ut_suite_new(&config, "math_works");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "mathw_dummy", mathw_dummy_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "mathw_gamma_log_t", mathw_gamma_log_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
