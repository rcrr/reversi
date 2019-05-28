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
#include <errno.h>
#include <string.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "prng.h"
#include "unit_test.h"
#include "cholesky_decomposition.h"

#define TEST_DIR_NAME_LEN 16

static const char *base_dir_name = "/tmp";

static char test_dir_name[TEST_DIR_NAME_LEN + 1];
static char test_dir_full_path[TEST_DIR_NAME_LEN + 16];

static prng_mt19937_t *prng;

/*
 * Test structure.
 */



/*
 * Auxiliary functions.
 */

static void
aux_setup (void)
{
  int status;

  const unsigned long int random_seed = prng_uint64_from_clock_random_seed();
  prng =  prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, random_seed);

  prng_mt19937_random_string_az(prng, test_dir_name, TEST_DIR_NAME_LEN);

  strcpy(test_dir_full_path, base_dir_name);
  strcat(test_dir_full_path, "/");
  strcat(test_dir_full_path, test_dir_name);

  status = mkdir(test_dir_full_path, S_IRWXU | S_IRWXG | S_IROTH | S_IXOTH);
  if (status != 0) {
    printf("Error creating the directory for testing. Directory name is: %s, %s.\n", test_dir_full_path, strerror(errno));
    abort();
  }
}

static void
aux_teardown (void)
{
  int status;

  status = rmdir(test_dir_full_path);
  if (status != 0) {
    printf("Error removing the directory used tor testing. Directory name is: %s, %s.\n", test_dir_full_path, strerror(errno));
    abort();
  }

  prng_mt19937_free(prng);
}



/*
 * Test functions.
 */


static void
chol_dot_product_t (ut_test_t *const t)
{
  static const size_t n = 101;
  double *a, *b, result, result_avx, expected_plus, expected_minus;

  a = chol_allocate_vector(n);
  b = chol_allocate_vector(n);

  for (size_t i = 0; i < n; i++) {
    a[i] = (1. * i) * (1. / (n - 1));
    b[n - i - 1] = (1. * i) * (1. / (n - 1));
  }

  expected_plus  = (1./6.) * (n - 1);
  expected_minus = (1./6.) * (n - 2);

  result = chol_dot_product(a, b, n);

  result_avx = chol_dot_product_avx(a, b, n);

  ut_assert(t, result > expected_minus);
  ut_assert(t, result < expected_plus);

  ut_assert(t, result_avx > expected_minus);
  ut_assert(t, result_avx < expected_plus);

  chol_free_vector(a);
  chol_free_vector(b);
}

static void
chol_clone_vector_t (ut_test_t *const t)
{
  static const size_t n = 10;
  double *v, *r;
  int ret_code;

  v = chol_allocate_vector(n);

  v[0] =  0.0;
  v[1] =  1.0;
  v[2] =  2.0;
  v[3] =  3.0;
  v[4] =  5.0;
  v[5] =  7.0;
  v[6] = 11.0;
  v[7] = 13.0;
  v[8] = 17.0;
  v[9] = 19.0;

  r = chol_clone_vector(v, n, &ret_code);
  ut_assert(t, ret_code == 0);

  for (size_t i = 0; i < n; i++)
      ut_assert(t, v[i] == r[i]);

  chol_free_vector(v);
  chol_free_vector(r);
}

static void
chol_clone_matrix_t (ut_test_t *const t)
{
  static const size_t nr = 3;
  static const size_t nc = 2;
  double **a, **b;
  int ret_code;

  a = chol_allocate_matrix(nr, nc);

  a[0][0] =  0.0;
  a[0][1] =  0.1;
  a[1][0] =  1.0;
  a[1][1] =  1.1;
  a[2][0] =  2.0;
  a[2][1] =  2.1;

  b = chol_clone_matrix(a, nr, nc, &ret_code);
  ut_assert(t, ret_code == 0);

  for (size_t i = 0; i < nr; i++)
    for (size_t j = 0; j < nc; j++)
      ut_assert(t, a[i][j] == b[i][j]);

  chol_free_matrix(a);
  chol_free_matrix(b);
}

static void
chol_dump_retrieve_vector_t (ut_test_t *const t)
{
  static const size_t n = 7;
  size_t rn;
  double *v, *r;
  int ret_code;

  char pathname[1024];

  const char *file_name = "chol_dump_retrieve_vector_t.tmp";

  strcpy(pathname, test_dir_full_path);
  strcat(pathname, "/");
  strcat(pathname, file_name);

  v = chol_allocate_vector(n);

  v[0] = 0.0;
  v[1] = 1.0;
  v[2] = 2.0;
  v[3] = 3.0;
  v[4] = 4.0;
  v[5] = 5.0;
  v[6] = 6.0;

  chol_dump_vector(v, n, pathname, &ret_code);
  ut_assert(t, ret_code == 0);

  r = chol_retrieve_vector(pathname, &rn, &ret_code);
  ut_assert(t, ret_code == 0);
  ut_assert(t, rn == n);

  for (size_t i = 0; i < n; i++)
      ut_assert(t, v[i] == r[i]);

  chol_free_vector(v);
  chol_free_vector(r);

  unlink(pathname);
}

static void
chol_dump_retrieve_matrix_t (ut_test_t *const t)
{
  static const size_t nr = 2;
  static const size_t nc = 3;
  size_t rnr, rnc;
  double **a, **r;
  int ret_code;

  char pathname[1024];

  const char *file_name = "chol_dump_retrieve_matrix_t.tmp";

  strcpy(pathname, test_dir_full_path);
  strcat(pathname, "/");
  strcat(pathname, file_name);

  a = chol_allocate_matrix(nr, nc);

  a[0][0] = 0.0;
  a[0][1] = 0.1;
  a[0][2] = 0.2;
  a[1][0] = 1.0;
  a[1][1] = 1.1;
  a[1][2] = 1.2;

  chol_dump_matrix(a, nr, nc, pathname, &ret_code);
  ut_assert(t, ret_code == 0);

  r = chol_retrieve_matrix(pathname, &rnr, &rnc, &ret_code);
  ut_assert(t, ret_code == 0);
  ut_assert(t, rnr == nr);
  ut_assert(t, rnc == nc);

  for (size_t i = 0; i < nr; i++)
    for (size_t j = 0; j < nc; j++)
      ut_assert(t, a[i][j] == r[i][j]);

  chol_free_matrix(a);
  chol_free_matrix(r);

  unlink(pathname);
}

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

  aux_setup();

  ut_suite_t *const s = ut_suite_new(&config, "cholesky_decomposition");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_fact_naive_i2", chol_fact_naive_i2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_fact_naive_3", chol_fact_naive_3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_fact_naive_5", chol_fact_naive_5_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_dump_retrieve_vector", chol_dump_retrieve_vector_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_dump_retrieve_matrix", chol_dump_retrieve_matrix_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_clone_vector", chol_clone_vector_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_clone_matrix", chol_clone_matrix_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "chol_dot_product_t", chol_dot_product_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);

  aux_teardown();

  return failure_count;
}
