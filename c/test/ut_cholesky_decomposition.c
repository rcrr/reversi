/**
 * @file
 *
 * @brief Cholesky Decomposition module unit test suite.
 * @details Collects tests and helper methods for the cholesky decomposition module.
 *
 * @par ut_cholesky_decomposition.c
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

#include "unit_test.h"
#include "cholesky_decomposition.h"



/*
 * Test structure.
 */



/*
 * Auxiliary functions.
 */



/*
 * Test functions.
 */

static void
chol_fact_naive_i2_t (ut_test_t *const t)
{
  static const size_t n = 2;
  double **a;
  double p[n];

  memset(p, 0, sizeof(p));

  a = chol_allocate_matrix(n, n);

  a[0][0] = 1.0;
  a[0][1] = 0.0;
  a[1][0] = 0.0;
  a[1][1] = 1.0;

  chol_fact_naive(a, n, p);

  ut_assert(t, a[0][0] == 1.0);
  ut_assert(t, a[0][1] == 0.0);
  ut_assert(t, a[1][0] == 0.0);
  ut_assert(t, a[1][1] == 1.0);
  ut_assert(t, p[0]    == 1.0);
  ut_assert(t, p[1]    == 1.0);

  chol_free_matrix(a);
}

static void
chol_fact_naive_3_t (ut_test_t *const t)
{
  static const size_t n = 3;
  double **a;
  double p[n];

  memset(p, 0, sizeof(p));

  a = chol_allocate_matrix(n, n);

  a[0][0] =   4.0;
  a[0][1] =  12.0;
  a[0][2] = -16.0;

  a[1][0] =  12.0;
  a[1][1] =  37.0;
  a[1][2] = -43.0;

  a[2][0] = -16.0;
  a[2][1] = -43.0;
  a[2][2] =  98.0;

  chol_fact_naive(a, n, p);

  ut_assert(t, a[0][0] ==   4.0);
  ut_assert(t, a[0][1] ==  12.0);
  ut_assert(t, a[0][2] == -16.0);
  ut_assert(t, a[1][0] ==   6.0);
  ut_assert(t, a[1][1] ==  37.0);
  ut_assert(t, a[1][2] == -43.0);
  ut_assert(t, a[2][0] ==  -8.0);
  ut_assert(t, a[2][1] ==   5.0);
  ut_assert(t, a[2][2] ==  98.0);
  ut_assert(t, p[0]    ==   2.0);
  ut_assert(t, p[1]    ==   1.0);
  ut_assert(t, p[2]    ==   3.0);

  chol_free_matrix(a);
}

static void
chol_fact_naive_5_t (ut_test_t *const t)
{
  static const size_t n = 5;
  double **a;
  double p[n];
  double b[n];
  double x[n];

  memset(p, 0, sizeof(p));
  memset(b, 0, sizeof(p));
  memset(x, 0, sizeof(p));

  a = chol_allocate_matrix(n, n);

  a[0][0] =   1.0;
  a[0][1] =   2.0;
  a[0][2] =   1.0;
  a[0][3] =  11.0;
  a[0][4] =   8.0;

  a[1][1] =  13.0;
  a[1][2] =  20.0;
  a[1][3] =  49.0;
  a[1][4] =  37.0;

  a[2][2] =  86.0;
  a[2][3] = 100.0;
  a[2][4] = 141.0;

  a[3][3] = 348.0;
  a[3][4] = 238.0;

  a[4][4] = 575.0;

  chol_fact_naive(a, n, p);

  ut_assert(t, a[0][0] ==   1.0);
  ut_assert(t, a[0][1] ==   2.0);
  ut_assert(t, a[0][2] ==   1.0);
  ut_assert(t, a[0][3] ==  11.0);
  ut_assert(t, a[0][4] ==   8.0);

  ut_assert(t, a[1][1] ==  13.0);
  ut_assert(t, a[1][2] ==  20.0);
  ut_assert(t, a[1][3] ==  49.0);
  ut_assert(t, a[1][4] ==  37.0);

  ut_assert(t, a[2][2] ==  86.0);
  ut_assert(t, a[2][3] == 100.0);
  ut_assert(t, a[2][4] == 141.0);

  ut_assert(t, a[3][3] == 348.0);
  ut_assert(t, a[3][4] == 238.0);

  ut_assert(t, a[4][4] == 575.0);

  ut_assert(t, a[1][0] ==   2.0);

  ut_assert(t, a[2][0] ==   1.0);
  ut_assert(t, a[2][1] ==   6.0);

  ut_assert(t, a[3][0] ==  11.0);
  ut_assert(t, a[3][1] ==   9.0);
  ut_assert(t, a[3][2] ==   5.0);

  ut_assert(t, a[4][0] ==   8.0);
  ut_assert(t, a[4][1] ==   7.0);
  ut_assert(t, a[4][2] ==  13.0);
  ut_assert(t, a[4][3] ==   2.0);

  ut_assert(t, p[0]    ==   1.0);
  ut_assert(t, p[1]    ==   3.0);
  ut_assert(t, p[2]    ==   7.0);
  ut_assert(t, p[3]    ==  11.0);
  ut_assert(t, p[4]    ==  17.0);

  b[0] =   -28;
  b[1] =   -68;
  b[2] =  -374;
  b[3] =  -486;
  b[4] = -2857;

  chol_solv_naive(a, n, p, b, x);

  ut_assert(t, p[0]    ==   1.0);
  ut_assert(t, p[1]    ==   3.0);
  ut_assert(t, p[2]    ==   7.0);
  ut_assert(t, p[3]    ==  11.0);
  ut_assert(t, p[4]    ==  17.0);

  ut_assert(t, b[0]    ==   -28.0);
  ut_assert(t, b[1]    ==   -68.0);
  ut_assert(t, b[2]    ==  -374.0);
  ut_assert(t, b[3]    ==  -486.0);
  ut_assert(t, b[4]    == -2857.0);

  ut_assert(t, x[0]    ==   3.0);
  ut_assert(t, x[1]    ==  -1.0);
  ut_assert(t, x[2]    ==   5.0);
  ut_assert(t, x[3]    ==   2.0);
  ut_assert(t, x[4]    ==  -7.0);

  b[0] =   165;
  b[1] =   783;
  b[2] =  2170;
  b[3] =  5202;
  b[4] =  6667;

  chol_solv_naive(a, n, p, b, x);

  ut_assert(t, x[0]    ==   5.0);
  ut_assert(t, x[1]    ==   1.0);
  ut_assert(t, x[2]    ==   3.0);
  ut_assert(t, x[3]    ==   9.0);
  ut_assert(t, x[4]    ==   7.0);

  ut_assert(t, a[0][0] ==   1.0);
  ut_assert(t, a[0][1] ==   2.0);
  ut_assert(t, a[0][2] ==   1.0);
  ut_assert(t, a[0][3] ==  11.0);
  ut_assert(t, a[0][4] ==   8.0);

  ut_assert(t, a[1][1] ==  13.0);
  ut_assert(t, a[1][2] ==  20.0);
  ut_assert(t, a[1][3] ==  49.0);
  ut_assert(t, a[1][4] ==  37.0);

  ut_assert(t, a[2][2] ==  86.0);
  ut_assert(t, a[2][3] == 100.0);
  ut_assert(t, a[2][4] == 141.0);

  ut_assert(t, a[3][3] == 348.0);
  ut_assert(t, a[3][4] == 238.0);

  ut_assert(t, a[4][4] == 575.0);

  ut_assert(t, a[1][0] ==   2.0);

  ut_assert(t, a[2][0] ==   1.0);
  ut_assert(t, a[2][1] ==   6.0);

  ut_assert(t, a[3][0] ==  11.0);
  ut_assert(t, a[3][1] ==   9.0);
  ut_assert(t, a[3][2] ==   5.0);

  ut_assert(t, a[4][0] ==   8.0);
  ut_assert(t, a[4][1] ==   7.0);
  ut_assert(t, a[4][2] ==  13.0);
  ut_assert(t, a[4][3] ==   2.0);

  chol_free_matrix(a);
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

  ut_suite_t *const s = ut_suite_new(&config, "cholesky_decomposition");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_fact_naive_i2", chol_fact_naive_i2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_fact_naive_3", chol_fact_naive_3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_fact_naive_5", chol_fact_naive_5_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
