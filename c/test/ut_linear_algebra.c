/**
 * @file
 *
 * @brief Linear Algebra module unit test suite.
 * @details Collects tests and helper methods for the linear algebra module.
 *
 * @par ut_linear_algebra.c
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
#include <math.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "prng.h"
#include "unit_test.h"
#include "linear_algebra.h"

#define TEST_DIR_NAME_LEN 16

typedef __float128 quad_float_t;

static const char *base_dir_name = "/tmp";

static char test_dir_name[TEST_DIR_NAME_LEN + 1];
static char test_dir_full_path[TEST_DIR_NAME_LEN + 16];

static prng_mt19937_t *prng;



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

static void
aux_create_random_sdf_matrix (prng_mt19937_t *r,
                              double lo,
                              double up,
                              size_t n,
                              double **rand,
                              double **sdf)
{
  assert(lo <= up);

  double sum;
  double v, d;

  d = up - lo;

  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      v = prng_mt19937_get_double_in_o0_o1(r);
      v = v * d + lo;
      rand[i][j] = v;
    }
  }

  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      sum = 0.0;
      for (size_t k = 0; k < n; k++) {
        sum += rand[i][k] * rand[j][k];
      }
      sdf[i][j] = (double) sum;
    }
  }
}

static void
aux_create_random_matrix (prng_mt19937_t *r,
                          double lo,
                          double up,
                          size_t n,
                          double **rand)
{
  assert(lo <= up);

  double v, d;

  d = up - lo;

  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      v = prng_mt19937_get_double_in_o0_o1(r);
      v = v * d + lo;
      rand[i][j] = v;
    }
  }
}

static void
aux_assert_identity (ut_test_t *const t,
                     double **a,
                     double **z,
                     size_t n,
                     double epsilon,
                     bool verbose)
{
  quad_float_t sum;
  size_t i, j, k;
  double expected, delta;

  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++) {
      sum = 0.0;
      for (k = 0; k < n; k++) {
        sum += (quad_float_t) a[i][k] * (quad_float_t) z[k][j];
      }
      if (verbose)
        printf("%6zu, %6zu: %21.15f\n", i, j, (double) sum);
      expected = (i == j) ? 1.0 : 0.0;
      delta = fabs((double) sum - expected);
      if (delta > epsilon) {
        printf("\n");
        printf("aux_assert_identity failed: row n. = %zu, column n. %zu, expected = %24.18f, "
               "value = %24.18f, delta = %24.18f, max_dev = %24.18f\n",
               i, j, expected, (double) sum, delta, epsilon);
        ut_assert(t, false);
      }
    }
  }
}

static void
aux_create_random_sdf_matrix_b (ut_test_t *const t,
                                double ***a,
                                size_t n)
{
  double **rand, **sdf;
  double lo, up;
  prng_mt19937_t *r;
  uint64_t seed;

  lo = -1.;
  up = +1.;
  seed = 45901;

  r = prng_mt19937_new();
  prng_mt19937_init_by_seed(r, seed);

  rand = lial_allocate_square_matrix(n);
  if (!rand) {
    printf("\nUnable to allocate memory for matrix rand\n");
    ut_assert(t, false);
  }

  sdf = lial_allocate_square_matrix(n);
  if (!sdf) {
    printf("\nUnable to allocate memory for matrix sdf\n");
    lial_free_matrix(rand, n);
    ut_assert(t, false);
  }

  aux_create_random_sdf_matrix(r, lo, up, n, rand, sdf);
  *a = sdf;

  lial_free_matrix(rand, n);
  prng_mt19937_free(r);
}

static void
aux_perf_sdf_lapack_t (ut_test_t *const t,
                       const char test_data_file_name[])
{
  static const char ut_lial_upper = 'L';

  bool const debug = false;

  size_t n, nr, nc;
  int nrhs, n0;

  timespec_t start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  double **a;
  double **z;
  double **c;

  double max_delta, normalized_max_row_delta_modulus, mean, standard_deviation;

  double d2, cdm;
  size_t max_delta_i, max_delta_j, normalized_max_row_delta_modulus_i;

  a = lial_retrieve_matrix(test_data_file_name, &nr, &nc, &ret);
  if (ret != 0 || !a || (nr != nc)) {
    printf("\nUnable to read properly matrix a from file: %s\n", test_data_file_name);
    ut_assert(t, false);
  } else {
    n = nr;
    n0 = nr;
    nrhs = nr;
  }
  if (debug) {
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  z = lial_allocate_square_matrix(n);
  if (!z) {
    printf("\nUnable to allocate memory for matrix z\n");
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }
  for (size_t i = 0; i < n; i++)
    for (size_t j = 0; j < n; j++)
      z[i][j] = (i == j) ? 1.0 : 0.0;

  c = lial_clone_matrix(a, n, n);
  if (!c) {
    printf("\nUnable to allocate memory for matrix c\n");
    lial_free_matrix(z, n);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  lial_dpotrf(&ut_lial_upper, &n0, *a, &n0, &ret);
  ut_assert(t, ret == 0);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Factorizing SDF matrix of size %8zu:                    [%6lld.%9ld]\n", n, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Computes inverse matrix z. */
  lial_dpotrs(&ut_lial_upper, &n0, &nrhs, *a, &n0, *z, &n0, &ret);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Computing the inverse matrix:                               [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Multiplies the original SDF matrix and its inverse. */
  double alpha = 1.0;
  double beta = 0.0;
  char trans = 'N';
  lial_dgemm_rowmajor(*z, *c, *a, &n0, &n0, &n0, &alpha, &beta, &trans, &trans, &n0, &n0, &n0);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Multiplying the original random SDF matrix and its inverse: [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /*
   * The standard deviation of the population of elements of the matrix ((A * inv(A)) - I) is computed assuming that the mean is zero.
   *
   */
  max_delta = 0.0;
  max_delta_i = 0;
  max_delta_j = 0;
  normalized_max_row_delta_modulus = 0.0;
  normalized_max_row_delta_modulus_i = 0;
  mean = 0.0;
  standard_deviation = 0.0;
  for (size_t i = 0; i < n; i++) {
    cdm = 0.0;
    a[i][i] -= 1.0;
    for (size_t j = 0; j < n; j++) {
      mean += a[i][j];
      d2 = a[i][j] * a[i][j];
      if (d2 > max_delta) { max_delta = d2; max_delta_i = i; max_delta_j = j; }
      cdm += d2;
    }
    standard_deviation += cdm;
    if (cdm > normalized_max_row_delta_modulus) { normalized_max_row_delta_modulus = cdm; normalized_max_row_delta_modulus_i = i; }
  }
  mean = mean / ((double) n * (double) n);
  standard_deviation = sqrt(standard_deviation / ((double) n * (double) n));
  normalized_max_row_delta_modulus = sqrt(normalized_max_row_delta_modulus / (double) n) ;
  max_delta = sqrt(max_delta);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Error matrix (deviations from the identity matrix) KPI:\n");
    fprintf(stdout, "    mean                             = %28.18f\n", mean);
    fprintf(stdout, "    standard_deviation               = %28.18f\n", standard_deviation);
    fprintf(stdout, "    normalized_max_row_delta_modulus = %28.18f, row #%6zu\n", normalized_max_row_delta_modulus, normalized_max_row_delta_modulus_i);
    fprintf(stdout, "    max_delta                        = %28.18f, i #%6zu, j #%6zu\n", max_delta, max_delta_i, max_delta_j);
  }

  lial_free_matrix(c, n);
  lial_free_matrix(z, n);
  lial_free_matrix(a, n);
}

static void
aux_perf_sdf_lapack_blocked_parallel_t (ut_test_t *const t,
                                        const char test_data_file_name[],
                                        const unsigned int block_size,
                                        const unsigned int thread_count)
{
  static const char ut_lial_upper = 'L';

  bool const debug = false;

  size_t n, nr, nc;
  int nrhs, n0;

  timespec_t start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  double **a;
  double **z;
  double **c;

  double max_delta, normalized_max_row_delta_modulus, mean, standard_deviation;

  double d2, cdm;
  size_t max_delta_i, max_delta_j, normalized_max_row_delta_modulus_i;

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  a = lial_retrieve_matrix(test_data_file_name, &nr, &nc, &ret);
  if (ret != 0 || !a || (nr != nc)) {
    printf("\nUnable to read properly matrix a from file: %s\n", test_data_file_name);
    ut_assert(t, false);
  } else {
    n = nr;
    n0 = nr;
    nrhs = nr;
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Reading from storage SDF matrix:                            [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  if (debug) {
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  z = lial_allocate_square_matrix(n);
  if (!z) {
    printf("\nUnable to allocate memory for matrix z\n");
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }
  for (size_t i = 0; i < n; i++)
    for (size_t j = 0; j < n; j++)
      z[i][j] = (i == j) ? 1.0 : 0.0;

  c = lial_clone_matrix(a, n, n);
  if (!c) {
    printf("\nUnable to allocate memory for matrix c\n");
    lial_free_matrix(z, n);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  lial_dpotrf_bp(&ut_lial_upper, &n0, *a, &n0, &ret, block_size, thread_count);
  ut_assert(t, ret == 0);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Factorizing SDF matrix of size %8zu:                    [%6lld.%9ld]\n", n, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Computes inverse matrix z. */
  lial_dpotrs(&ut_lial_upper, &n0, &nrhs, *a, &n0, *z, &n0, &ret);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Computing the inverse matrix:                               [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Multiplies the original SDF matrix and its inverse. */
  double alpha = 1.0;
  double beta = 0.0;
  char trans = 'N';
  lial_dgemm_rowmajor(*z, *c, *a, &n0, &n0, &n0, &alpha, &beta, &trans, &trans, &n0, &n0, &n0);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Multiplying the original random SDF matrix and its inverse: [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /*
   * The standard deviation of the population of elements of the matrix ((A * inv(A)) - I) is computed assuming that the mean is zero.
   *
   */
  max_delta = 0.0;
  max_delta_i = 0;
  max_delta_j = 0;
  normalized_max_row_delta_modulus = 0.0;
  normalized_max_row_delta_modulus_i = 0;
  mean = 0.0;
  standard_deviation = 0.0;
  for (size_t i = 0; i < n; i++) {
    cdm = 0.0;
    a[i][i] -= 1.0;
    for (size_t j = 0; j < n; j++) {
      mean += a[i][j];
      d2 = a[i][j] * a[i][j];
      if (d2 > max_delta) { max_delta = d2; max_delta_i = i; max_delta_j = j; }
      cdm += d2;
    }
    standard_deviation += cdm;
    if (cdm > normalized_max_row_delta_modulus) { normalized_max_row_delta_modulus = cdm; normalized_max_row_delta_modulus_i = i; }
  }
  mean = mean / ((double) n * (double) n);
  standard_deviation = sqrt(standard_deviation / ((double) n * (double) n));
  normalized_max_row_delta_modulus = sqrt(normalized_max_row_delta_modulus / (double) n) ;
  max_delta = sqrt(max_delta);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Error matrix (deviations from the identity matrix) KPI:\n");
    fprintf(stdout, "    mean                             = %28.18f\n", mean);
    fprintf(stdout, "    standard_deviation               = %28.18f\n", standard_deviation);
    fprintf(stdout, "    normalized_max_row_delta_modulus = %28.18f, row #%6zu\n", normalized_max_row_delta_modulus, normalized_max_row_delta_modulus_i);
    fprintf(stdout, "    max_delta                        = %28.18f, i #%6zu, j #%6zu\n", max_delta, max_delta_i, max_delta_j);
  }

  lial_free_matrix(c, n);
  lial_free_matrix(z, n);
  lial_free_matrix(a, n);
}



/*
 * Test functions.
 */

static void
lial_dummy_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
lial_zero_vector_t (ut_test_t *const t)
{
  size_t n;
  double v[] = { 1., 2., 3. };

  n = 3;
  lial_zero_vector(v, n);

  for (size_t i = 0; i < n; i++) {
    ut_assert(t, v[i] == 0.);
  }
}

static void
lial_vector_magnitude_t (ut_test_t *const t)
{

  size_t n, abs_min_pos, abs_max_pos;
  double abs_min, abs_max;
  double norm2;
  double *v;

  double v0[] = {0., 0., 1.};
  n = 3;
  v = v0;
  norm2 = lial_vector_magnitude(v, n, &abs_min, &abs_min_pos, &abs_max, &abs_max_pos);
  ut_assert(t, norm2 == 1.0);
  ut_assert(t, abs_min == 0.0);
  ut_assert(t, abs_min_pos == 0);
  ut_assert(t, abs_max == 1.0);
  ut_assert(t, abs_max_pos == 2);

  double v1[] = {1., -1., 1., -1., -1.};
  n = 5;
  v = v1;
  norm2 = lial_vector_magnitude(v, n, &abs_min, &abs_min_pos, &abs_max, &abs_max_pos);
  ut_assert(t, norm2 == sqrt(5.0));
  ut_assert(t, abs_min == 1.0);
  ut_assert(t, abs_min_pos == 0);
  ut_assert(t, abs_max == 1.0);
  ut_assert(t, abs_max_pos == 0);

  double v2[] = {1., 0.1, 2.};
  n = 3;
  v = v2;
  norm2 = lial_vector_magnitude(v, n, NULL, NULL, &abs_max, &abs_max_pos);
  ut_assert(t, norm2 == sqrt(5.01));
  ut_assert(t, abs_max == 2.0);
  ut_assert(t, abs_max_pos == 2);

  double v3[] = {1., 0.1, 2.};
  n = 3;
  v = v3;
  norm2 = lial_vector_magnitude(v, n, &abs_min, &abs_min_pos, NULL, NULL);
  ut_assert(t, norm2 == sqrt(5.01));
  ut_assert(t, abs_min == 0.1);
  ut_assert(t, abs_min_pos == 1);

  double v4[] = { 1. };
  n = 1;
  v = v4;
  norm2 = lial_vector_magnitude(v, n, NULL, NULL, NULL, NULL);
  ut_assert(t, norm2 == 1.);

}

static void
lial_lu_singular_t (ut_test_t *const t)
{
  static const size_t n = 2;
  double **a;
  double scale[n];
  double b[n];
  int ret;
  size_t indx[n];

  a = lial_allocate_matrix(n, n);

  a[0][0] = 1.0;
  a[0][1] = 0.0;
  a[1][0] = 0.0;
  a[1][1] = 0.0;

  ret = lial_lu_decom_naive(a, n, indx, scale);
  ut_assert(t, ret == 0);

  a[0][0] = 1.0;
  a[0][1] = 1.0;
  a[1][0] = 1.0;
  a[1][1] = 1.0;

  b[0] = 2.0;
  b[1] = 3.0;

  ret = lial_lu_decom_naive(a, n, indx, scale);
  ut_assert(t, ret == 1);

  lial_lu_bsubst_naive(a, n, indx, scale, b);

  ut_assert(t, isinf(b[0]));
  ut_assert(t, isinf(b[1]));

  lial_free_matrix(a, n);
}

static void
lial_lu_i2_t (ut_test_t *const t)
{
  static const size_t n = 2;
  double **a;
  double scale[n];
  double b[n];
  int ret;
  size_t indx[n];

  memset(b, 0, sizeof(b));

  a = lial_allocate_matrix(n, n);

  a[0][0] = 1.0;
  a[0][1] = 0.0;
  a[1][0] = 0.0;
  a[1][1] = 1.0;

  b[0] = 5.0;
  b[1] = 7.0;

  ret = lial_lu_decom_naive(a, n, indx, scale);
  ut_assert(t, ret == 1);

  lial_lu_bsubst_naive(a, n, indx, scale, b);

  ut_assert(t, a[0][0] == 1.0);
  ut_assert(t, a[0][1] == 0.0);
  ut_assert(t, a[1][0] == 0.0);
  ut_assert(t, a[1][1] == 1.0);

  ut_assert(t, b[0] == 5.0);
  ut_assert(t, b[1] == 7.0);

  lial_free_matrix(a, n);
}

static void
lial_lu_3_t (ut_test_t *const t)
{
  static const size_t n = 3;
  double **a;
  double scale[n];
  double b[n];
  int ret;
  size_t indx[n];

  const double epsilon = 1.E-15;

  memset(b, 0, sizeof(b));

  a = lial_allocate_matrix(n, n);

  a[0][0] =  3.0;
  a[0][1] =  4.0;
  a[0][2] =  2.0;
  a[1][0] =  4.0;
  a[1][1] =  8.0;
  a[1][2] =  7.0;
  a[2][0] =  3.0;
  a[2][1] =  1.0;
  a[2][2] = -1.0;

  b[0] = 23.0;
  b[1] = 22.0;
  b[2] = 29.0;

  ret = lial_lu_decom_naive(a, n, indx, scale);
  ut_assert(t, ret == 1);

  lial_lu_bsubst_naive(a, n, indx, scale, b);

  ut_assert(t, fabs(b[0] - (+9.0)) <  epsilon);
  ut_assert(t, fabs(b[1] - (+0.0)) <  epsilon);
  ut_assert(t, fabs(b[2] - (-2.0)) <  epsilon);

  lial_free_matrix(a, n);
}

static void
lial_lu_5_t (ut_test_t *const t)
{
  static const size_t n = 5;
  double **a;
  double scale[n];
  double b[n];
  int ret;
  size_t indx[n];

  const double epsilon = 1.E-14;

  memset(b, 0, sizeof(b));

  a = lial_allocate_matrix(n, n);

  a[0][0] =   3.0;
  a[0][1] =   4.0;
  a[0][2] =   2.0;
  a[0][3] =   4.0;
  a[0][4] =   9.0;

  a[1][0] =   4.0;
  a[1][1] =   8.0;
  a[1][2] =   7.0;
  a[1][3] =   1.0;
  a[1][4] =   9.0;

  a[2][0] =   3.0;
  a[2][1] =   1.0;
  a[2][2] =  -1.0;
  a[2][3] =  -3.0;
  a[2][4] =   1.0;

  a[3][0] =   3.0;
  a[3][1] = -11.0;
  a[3][2] =  -2.0;
  a[3][3] =   6.0;
  a[3][4] =   0.0;

  a[4][0] =  -1.0;
  a[4][1] =   2.0;
  a[4][2] =  -3.0;
  a[4][3] =   1.0;
  a[4][4] =   7.0;

  b[0] = -38.0;
  b[1] = -65.0;
  b[2] =  10.0;
  b[3] =   7.0;
  b[4] = -12.0;

  ret = lial_lu_decom_naive(a, n, indx, scale);
  ut_assert(t, ret == 1);

  lial_lu_bsubst_naive(a, n, indx, scale, b);

  ut_assert(t, fabs(b[0] - (+3.0)) <  epsilon);
  ut_assert(t, fabs(b[1] - (+2.0)) <  epsilon);
  ut_assert(t, fabs(b[2] - (-7.0)) <  epsilon);
  ut_assert(t, fabs(b[3] - ( 1.0)) <  epsilon);
  ut_assert(t, fabs(b[4] - (-5.0)) <  epsilon);

  lial_free_matrix(a, n);
}

static void
lial_lu_7_t (ut_test_t *const t)
{
  static const size_t n = 7;
  double **a;
  double scale[n];
  int ret;
  size_t indx[n];

  const double epsilon = 1.E-12;

  const double matrix_a[] =
    {
     2.,    7., -2.,  0.,  8.,  3.,  4.,
     11.,   8.,  7.,  1.,  9., -2.,  1.,
     -2.,   1., -1., -2.,  4.,  4.,  5.,
     3.,  -11., -2.,  0.,  0.,  0.,  3.,
     -1.,   2., -3.,  1.,  7., -1., -9.,
     3.,   10.,  4.,  0.,  1.,  6.,  2.,
     -3., -6.,   8., -3., -1., -5., 10.,
    };

  double b[] = { -36., -114., -15., 27., 12., -22., -115. };

  double x[] = { 1., -2., -7., 1., -5., 6., -4. };

  a = lial_allocate_matrix(n, n);
  for (size_t i = 0; i < n * n; i++) {
    *(a[0] + i) = matrix_a[i];
  }

  ret = lial_lu_decom_naive(a, n, indx, scale);
  ut_assert(t, ret == 1);

  lial_lu_bsubst_naive(a, n, indx, scale, b);

  for (size_t i = 0; i < n; i++) {
    ut_assert(t, fabs(b[i] - x[i]) <  epsilon);
  }

  lial_free_matrix(a, n);
}

static void
lial_lu_inv_3_t (ut_test_t *const t)
{
  static const size_t n = 3;
  double **a;
  double **z;
  double **c;
  double scale[n];
  double b[n];
  int ret;
  size_t indx[n];

  const double epsilon = 1.E-15;
  const bool debug = false;

  a = lial_allocate_square_matrix(n);
  if (!a) {
    printf("\nUnable to allocate memory for matrix a\n");
    ut_assert(t, false);
  }

  a[0][0] =  1.0;
  a[0][1] =  0.0;
  a[0][2] =  1.0;
  a[1][0] =  2.0;
  a[1][1] = -1.0;
  a[1][2] =  3.0;
  a[2][0] =  1.0;
  a[2][1] =  4.0;
  a[2][2] =  2.0;

  ret = lial_lu_decom_naive(a, n, indx, scale);
  ut_assert(t, ret == 1);

  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  b[0] = 1.0;
  b[1] = 0.0;
  b[2] = 0.0;

  lial_lu_bsubst_naive(a, n, indx, scale, b);

  ut_assert(t, fabs(b[0] - (+14.0 / 5.0)) <  epsilon);
  ut_assert(t, fabs(b[1] - (+ 1.0 / 5.0)) <  epsilon);
  ut_assert(t, fabs(b[2] - (- 9.0 / 5.0)) <  epsilon);

  b[0] = 0.0;
  b[1] = 1.0;
  b[2] = 0.0;

  lial_lu_bsubst_naive(a, n, indx, scale, b);

  ut_assert(t, fabs(b[0] - (- 4.0 / 5.0)) <  epsilon);
  ut_assert(t, fabs(b[1] - (- 1.0 / 5.0)) <  epsilon);
  ut_assert(t, fabs(b[2] - (+ 4.0 / 5.0)) <  epsilon);

  b[0] = 0.0;
  b[1] = 0.0;
  b[2] = 1.0;

  lial_lu_bsubst_naive(a, n, indx, scale, b);

  ut_assert(t, fabs(b[0] - (- 1.0 / 5.0)) <  epsilon);
  ut_assert(t, fabs(b[1] - (+ 1.0 / 5.0)) <  epsilon);
  ut_assert(t, fabs(b[2] - (+ 1.0 / 5.0)) <  epsilon);

  /* Now restart using the call to lial_lu_inv_naive */

  a[0][0] =  1.0;
  a[0][1] =  0.0;
  a[0][2] =  1.0;
  a[1][0] =  2.0;
  a[1][1] = -1.0;
  a[1][2] =  3.0;
  a[2][0] =  1.0;
  a[2][1] =  4.0;
  a[2][2] =  2.0;

  z = lial_allocate_square_matrix(n);
  if (!z) {
    printf("\nUnable to allocate memory for matrix z\n");
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  for (size_t i = 0; i < n; i++)
    for (size_t j = 0; j < n; j++)
      z[i][j] = (i == j) ? 1.0 : 0.0;

  c = lial_clone_matrix(a, n, n);
  if (!c) {
    printf("\nUnable to allocate memory for matrix c\n");
    lial_free_matrix(z, n);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  ret = lial_lu_inv_naive(a, n, indx, scale, z);
  ut_assert(t, ret == 1);

  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("z[%zu][%zu] = %21.15f\n", i, j, z[i][j]);
  }

  aux_assert_identity(t, c, z, n, epsilon, debug);

  lial_free_matrix(c, n);
  lial_free_matrix(z, n);
  lial_free_matrix(a, n);
}

static void
lial_dot_product_t (ut_test_t *const t)
{
  static const size_t n = 101;
  double *a, *b, result, result_avx, expected_plus, expected_minus;

  a = lial_allocate_vector(n);
  b = lial_allocate_vector(n);

  for (size_t i = 0; i < n; i++) {
    a[i] = (1. * i) * (1. / (n - 1));
    b[n - i - 1] = (1. * i) * (1. / (n - 1));
  }

  expected_plus  = (1./6.) * (n - 1);
  expected_minus = (1./6.) * (n - 2);

  result = lial_dot_product(a, b, n);

  result_avx = lial_dot_product_avx(a, b, n);

  ut_assert(t, result > expected_minus);
  ut_assert(t, result < expected_plus);

  ut_assert(t, result_avx > expected_minus);
  ut_assert(t, result_avx < expected_plus);

  lial_free_vector(a);
  lial_free_vector(b);
}

static void
lial_clone_vector_t (ut_test_t *const t)
{
  static const size_t n = 10;
  double *v, *r;
  int ret_code;

  v = lial_allocate_vector(n);

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

  r = lial_clone_vector(v, n, &ret_code);
  ut_assert(t, ret_code == 0);

  for (size_t i = 0; i < n; i++)
      ut_assert(t, v[i] == r[i]);

  lial_free_vector(v);
  lial_free_vector(r);
}

static void
lial_clone_matrix_t (ut_test_t *const t)
{
  static const size_t nr = 3;
  static const size_t nc = 2;
  double **a, **b;

  a = lial_allocate_matrix(nr, nc);

  a[0][0] =  0.0;
  a[0][1] =  0.1;
  a[1][0] =  1.0;
  a[1][1] =  1.1;
  a[2][0] =  2.0;
  a[2][1] =  2.1;

  b = lial_clone_matrix(a, nr, nc);
  ut_assert(t, b != NULL);

  for (size_t i = 0; i < nr; i++)
    for (size_t j = 0; j < nc; j++)
      ut_assert(t, a[i][j] == b[i][j]);

  lial_free_matrix(a, nr);
  lial_free_matrix(b, nr);
}

static void
lial_dump_retrieve_vector_t (ut_test_t *const t)
{
  static const size_t n = 7;
  size_t rn;
  double *v, *r;
  int ret_code;

  char pathname[1024];

  const char *file_name = "lial_dump_retrieve_vector_t.tmp";

  strcpy(pathname, test_dir_full_path);
  strcat(pathname, "/");
  strcat(pathname, file_name);

  v = lial_allocate_vector(n);

  v[0] = 0.0;
  v[1] = 1.0;
  v[2] = 2.0;
  v[3] = 3.0;
  v[4] = 4.0;
  v[5] = 5.0;
  v[6] = 6.0;

  lial_dump_vector(v, n, pathname, &ret_code);
  ut_assert(t, ret_code == 0);

  r = lial_retrieve_vector(pathname, &rn, &ret_code);
  ut_assert(t, ret_code == 0);
  ut_assert(t, rn == n);

  for (size_t i = 0; i < n; i++)
      ut_assert(t, v[i] == r[i]);

  lial_free_vector(v);
  lial_free_vector(r);

  unlink(pathname);
}

static void
lial_dump_retrieve_matrix_t (ut_test_t *const t)
{
  static const size_t nr = 2;
  static const size_t nc = 3;
  size_t rnr, rnc;
  double **a, **r;
  int ret_code;

  char pathname[1024];

  const char *file_name = "lial_dump_retrieve_matrix_t.tmp";

  strcpy(pathname, test_dir_full_path);
  strcat(pathname, "/");
  strcat(pathname, file_name);

  a = lial_allocate_matrix(nr, nc);

  a[0][0] = 0.0;
  a[0][1] = 0.1;
  a[0][2] = 0.2;
  a[1][0] = 1.0;
  a[1][1] = 1.1;
  a[1][2] = 1.2;

  lial_dump_matrix(a, nr, nc, pathname, &ret_code);
  ut_assert(t, ret_code == 0);

  r = lial_retrieve_matrix(pathname, &rnr, &rnc, &ret_code);
  ut_assert(t, ret_code == 0);
  ut_assert(t, rnr == nr);
  ut_assert(t, rnc == nc);

  for (size_t i = 0; i < nr; i++)
    for (size_t j = 0; j < nc; j++)
      ut_assert(t, a[i][j] == r[i][j]);

  lial_free_matrix(a, nr);
  lial_free_matrix(r, nr);

  unlink(pathname);
}

static void
lial_chol_fact_naive_i2_t (ut_test_t *const t)
{
  static const size_t n = 2;
  double **a;
  double p[n];

  memset(p, 0, sizeof(p));

  a = lial_allocate_matrix(n, n);

  a[0][0] = 1.0;
  a[0][1] = 0.0;
  a[1][0] = 0.0;
  a[1][1] = 1.0;

  lial_chol_fact_naive(a, n, p);

  ut_assert(t, a[0][0] == 1.0);
  ut_assert(t, a[0][1] == 0.0);
  ut_assert(t, a[1][0] == 0.0);
  ut_assert(t, a[1][1] == 1.0);
  ut_assert(t, p[0]    == 1.0);
  ut_assert(t, p[1]    == 1.0);

  lial_free_matrix(a, n);
}

static void
lial_chol_fact_naive_3_t (ut_test_t *const t)
{
  static const size_t n = 3;
  double **a;
  double p[n];

  memset(p, 0, sizeof(p));

  a = lial_allocate_matrix(n, n);

  a[0][0] =   4.0;
  a[0][1] =  12.0;
  a[0][2] = -16.0;

  a[1][0] =  12.0;
  a[1][1] =  37.0;
  a[1][2] = -43.0;

  a[2][0] = -16.0;
  a[2][1] = -43.0;
  a[2][2] =  98.0;

  lial_chol_fact_naive(a, n, p);

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

  lial_free_matrix(a, n);
}

static void
lial_chol_fact_naive_5_t (ut_test_t *const t)
{
  static const size_t n = 5;
  double **a;
  double p[n];
  double b[n];
  double x[n];

  memset(p, 0, sizeof(p));
  memset(b, 0, sizeof(p));
  memset(x, 0, sizeof(p));

  a = lial_allocate_matrix(n, n);

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

  lial_chol_fact_naive(a, n, p);

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

  lial_chol_solv_naive(a, n, p, b, x);

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

  lial_chol_solv_naive(a, n, p, b, x);

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

  lial_free_matrix(a, n);
}

static void
lial_chol_inv_naive_i2_t (ut_test_t *const t)
{
  static const size_t n = 2;
  double **a;
  double **z;
  double p[n];

  const double epsilon = 1.E-15;

  lial_zero_vector(p, n);

  a = lial_allocate_square_matrix(n);

  a[0][0] = 1.0;
  a[0][1] = 0.0;
  a[1][0] = 0.0;
  a[1][1] = 1.0;

  z = lial_allocate_square_matrix(n);

  z[0][0] = 0.0;
  z[0][1] = 0.0;
  z[1][0] = 0.0;
  z[1][1] = 0.0;

  lial_chol_inv_naive(a, n, p, z);

  ut_assert(t, fabs(z[0][0] - 1.0) < epsilon);
  ut_assert(t, fabs(z[0][1] - 0.0) < epsilon);
  ut_assert(t, fabs(z[1][0] - 0.0) < epsilon);
  ut_assert(t, fabs(z[1][1] - 1.0) < epsilon);

  lial_free_matrix(z, n);
  lial_free_matrix(a, n);
}

static void
lial_chol_inv_naive_m2_t (ut_test_t *const t)
{
  static const size_t n = 2;
  double **a; // the matrix to be inverted
  double **z; // the matrix inverse
  double **c; // a copy of the a matrix
  double p[n];

  const double epsilon = 1.E-15;
  const bool debug = false;

  lial_zero_vector(p, n);

  a = lial_allocate_square_matrix(n);

  a[0][0] = 1.2;
  a[0][1] = 0.3;
  a[1][0] = 0.3;
  a[1][1] = 1.8;

  z = lial_allocate_square_matrix(n);

  z[0][0] = 0.0;
  z[0][1] = 0.0;
  z[1][0] = 0.0;
  z[1][1] = 0.0;

  c = lial_clone_matrix(a, n, n);
  if (!c) {
    lial_free_matrix(z, n);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  lial_chol_inv_naive(a, n, p, z);

  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("[%zu][%zu] = %21.15f\n", i, j, z[i][j]);
  }

  ut_assert(t, fabs(z[0][0] - (+0.869565217391304)) < epsilon);
  ut_assert(t, fabs(z[0][1] - (-0.144927536231884)) < epsilon);
  ut_assert(t, fabs(z[1][0] - (-0.144927536231884)) < epsilon);
  ut_assert(t, fabs(z[1][1] - (+0.579710144927536)) < epsilon);

  aux_assert_identity(t, c, z, n, epsilon, debug);

  lial_free_matrix(c, n);
  lial_free_matrix(z, n);
  lial_free_matrix(a, n);
}

static void
lial_lu_inv_m_0_40_t (ut_test_t *const t)
{
  size_t start, end, i, n;

  double **a; // the matrix to be inverted, generated with random values.
  double **z; // the matrix inverse
  double **c; // a copy of the a matrix
  size_t *indx;
  double *scale;
  int ret;

  double lo, up;

  prng_mt19937_t *r;
  uint64_t seed;

  const double epsilon = 1.E-06;
  const bool debug = false;

  start = 1;
  end = 40;
  lo = -1.;
  up = +1.;
  seed = 4532;

  r = prng_mt19937_new();
  prng_mt19937_init_by_seed(r, seed);

  if (debug) printf("\n");
  for (i = start; i <= end; i++) {
    if (debug) printf("lial_lu_inv_m_0_40_t: iteration n. %zu\n", i);

    n = i;

    a = lial_allocate_square_matrix(n);
    if (!a) {
      printf("\nUnable to allocate memory for matrix a\n");
      ut_assert(t, false);
    }

    aux_create_random_matrix(r, lo, up, n, a);

    if (debug) {
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < n; j++)
          printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
    }

    z = lial_allocate_square_matrix(n);
    if (!z) {
      printf("\nUnable to allocate memory for matrix z\n");
      lial_free_matrix(a, n);
      ut_assert(t, false);
    }

    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        z[i][j] = (i == j) ? 1.0 : 0.0;

    c = lial_clone_matrix(a, n, n);
    if (!c) {
      printf("\nUnable to allocate memory for matrix c\n");
      lial_free_matrix(z, n);
      lial_free_matrix(a, n);
      ut_assert(t, false);
    }

    scale = lial_allocate_vector(n);
    if (!scale) {
      printf("\nUnable to allocate memory for vector scale\n");
      lial_free_matrix(c, n);
      lial_free_matrix(z, n);
      lial_free_matrix(a, n);
      ut_assert(t, false);
    }

    indx = (size_t *) malloc(sizeof(size_t) * n);
    if (!indx) {
      printf("\nUnable to allocate memory for vector scale\n");
      lial_free_vector(scale);
      lial_free_matrix(c, n);
      lial_free_matrix(z, n);
      lial_free_matrix(a, n);
      ut_assert(t, false);
    }

    ret = lial_lu_inv_naive(a, n, indx, scale, z);
    ut_assert(t, ret == 1);

    if (debug) {
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < n; j++)
          printf("z[%zu][%zu] = %21.15f\n", i, j, z[i][j]);
    }

    aux_assert_identity(t, c, z, n, epsilon, debug);

    free(indx);
    lial_free_vector(scale);
    lial_free_matrix(c, n);
    lial_free_matrix(z, n);
    lial_free_matrix(a, n);
  }

  ut_assert(t, true);

  prng_mt19937_free(r);
}

static void
lial_chol_inv_naive_m_0_40_t (ut_test_t *const t)
{
  size_t start, end, i, n;

  double **g; // a matrix generated with random values. a = r * r transposed
  double **a; // the matrix to be inverted
  double **z; // the matrix inverse
  double **c; // a copy of the a matrix
  double *p;

  double lo, up;

  prng_mt19937_t *r;
  uint64_t seed;

  const double epsilon = 1.E-09;
  const bool debug = false;

  start = 1;
  end = 40;
  lo = -1.;
  up = +1.;
  seed = 8670;

  r = prng_mt19937_new();
  prng_mt19937_init_by_seed(r, seed);

  if (debug) printf("\n");
  for (i = start; i <= end; i++) {
    if (debug) printf("lial_chol_inv_naive_m_0_40_t: iteration n. %zu\n", i);

    n = i;

    g = lial_allocate_square_matrix(n);
    if (!g) {
      printf("\nUnable to allocate memory for matrix g\n");
      ut_assert(t, false);
    }

    a = lial_allocate_square_matrix(n);
    if (!a) {
      printf("\nUnable to allocate memory for matrix a\n");
      lial_free_matrix(g, n);
      ut_assert(t, false);
    }

    z = lial_allocate_square_matrix(n);
    if (!z) {
      printf("\nUnable to allocate memory for matrix z\n");
      lial_free_matrix(a, n);
      lial_free_matrix(g, n);
      ut_assert(t, false);
    }

    aux_create_random_sdf_matrix(r, lo, up, n, g, a);

    if (debug) {
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < n; j++)
          printf("g[%zu][%zu] = %21.15f\n", i, j, g[i][j]);
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < n; j++)
          printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
    }

    c = lial_clone_matrix(a, n, n);
    if (!c) {
      printf("\nUnable to allocate memory for matrix c\n");
      lial_free_matrix(z, n);
      lial_free_matrix(a, n);
      lial_free_matrix(g, n);
      ut_assert(t, false);
    }

    p = lial_allocate_vector(n);
    if (!p) {
      printf("\nUnable to allocate memory for vector p\n");
      lial_free_matrix(c, n);
      lial_free_matrix(z, n);
      lial_free_matrix(a, n);
      lial_free_matrix(g, n);
      ut_assert(t, false);
    }

    if (debug) {
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < n; j++)
          printf("z[%zu][%zu] = %21.15f\n", i, j, z[i][j]);
    }

    lial_chol_inv_naive(a, n, p, z);

    if (debug) {
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < n; j++)
          printf("z[%zu][%zu] = %21.15f\n", i, j, z[i][j]);
    }

    aux_assert_identity(t, c, z, n, epsilon, debug);

    lial_free_vector(p);
    lial_free_matrix(c, n);
    lial_free_matrix(z, n);
    lial_free_matrix(a, n);
    lial_free_matrix(g, n);
  }

  prng_mt19937_free(r);
}

static void
lial_chol_fact_and_solve_lapack_i2_t (ut_test_t *const t)
{
  static const size_t n = 2;
  double **a;
  int ret;
  double b[n];

  static const bool debug = false;

  a = lial_allocate_square_matrix(n);
  if (!a) {
    printf("\nUnable to allocate memory for matrix a\n");
    ut_assert(t, false);
  }

  a[0][0] = 1.0;
  a[0][1] = 0.0;
  a[1][0] = 0.0;
  a[1][1] = 1.0;

  lial_chol_fact_lapack(a, n, &ret);
  ut_assert(t, ret == 0);
  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  b[0] = 5.0;
  b[1] = 7.0;

  lial_chol_solv_lapack(a, n, b, &ret);
  ut_assert(t, ret == 0);

  ut_assert(t, b[0] == 5.0);
  ut_assert(t, b[1] == 7.0);

  lial_free_matrix(a, n);
}

static void
lial_chol_fact_and_solve_lapack_m2_t (ut_test_t *const t)
{
  static const size_t n = 2;
  double **a;
  int ret;
  double b[n];

  const double epsilon = 1.E-15;
  static const bool debug = false;

  a = lial_allocate_square_matrix(n);
  if (!a) {
    printf("\nUnable to allocate memory for matrix a\n");
    ut_assert(t, false);
  }

  a[0][0] = 1.2;
  a[0][1] = 0.3;
  a[1][0] = 0.3;
  a[1][1] = 1.8;

  lial_chol_fact_lapack(a, n, &ret);
  ut_assert(t, ret == 0);
  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  b[0] = 1.0;
  b[1] = 0.0;

  lial_chol_solv_lapack(a, n, b, &ret);
  ut_assert(t, ret == 0);
  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n; i++)
      printf("b[%zu] = %24.18f\n", i, b[i]);
  }

  ut_assert(t, fabs(b[0] - (+0.869565217391304)) < epsilon);
  ut_assert(t, fabs(b[1] - (-0.144927536231884)) < epsilon);

  b[0] = 0.0;
  b[1] = 1.0;

  lial_chol_solv_lapack(a, n, b, &ret);
  ut_assert(t, ret == 0);

  ut_assert(t, fabs(b[0] - (-0.144927536231884)) < epsilon);
  ut_assert(t, fabs(b[1] - (+0.579710144927536)) < epsilon);

  lial_free_matrix(a, n);
}

static void
lial_chol_fact_and_solve_lapack_m3_t (ut_test_t *const t)
{
  static const size_t n = 3;
  double **a;
  int ret;
  double b[n];

  const double epsilon = 1.E-15;
  static const bool debug = false;

  double a11, a12, a13, a21, a22, a23, a31, a32, a33, det;
  double z11, z12, z13, z21, z22, z23, z31, z32, z33;

  a11 =  3.1;
  a12 =  1.7;
  a13 = -0.7;
  a21 =  a12;
  a22 =  5.7;
  a23 =  0.1;
  a31 =  a13;
  a32 =  a23;
  a33 =  7.3;

  det =
    a11 * (a33 * a22 - a32 * a23) -
    a21 * (a33 * a12 - a32 * a13) +
    a31 * (a23 * a12 - a22 * a13);

  z11 =   (a33 * a22 - a32 * a23) / det;
  z12 = - (a33 * a12 - a32 * a13) / det;
  z13 =   (a23 * a12 - a22 * a13) / det;
  z21 = - (a33 * a21 - a31 * a23) / det;
  z22 =   (a33 * a11 - a31 * a13) / det;
  z23 = - (a23 * a11 - a21 * a13) / det;
  z31 =   (a32 * a21 - a31 * a22) / det;
  z32 = - (a32 * a11 - a31 * a12) / det;
  z33 =   (a22 * a11 - a21 * a12) / det;

  a = lial_allocate_square_matrix(n);
  if (!a) {
    printf("\nUnable to allocate memory for matrix a\n");
    ut_assert(t, false);
  }

  a[0][0] = a11;
  a[0][1] = a12;
  a[0][2] = a13;
  a[1][0] = a21;
  a[1][1] = a22;
  a[1][2] = a23;
  a[2][0] = a31;
  a[2][1] = a32;
  a[2][2] = a33;

  lial_chol_fact_lapack(a, n, &ret);
  ut_assert(t, ret == 0);
  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  b[0] = 1.0;
  b[1] = 0.0;
  b[2] = 0.0;
  lial_chol_solv_lapack(a, n, b, &ret);
  ut_assert(t, ret == 0);
  ut_assert(t, fabs(b[0] - z11) < epsilon);
  ut_assert(t, fabs(b[1] - z21) < epsilon);
  ut_assert(t, fabs(b[2] - z31) < epsilon);

  b[0] = 0.0;
  b[1] = 1.0;
  b[2] = 0.0;
  lial_chol_solv_lapack(a, n, b, &ret);
  ut_assert(t, ret == 0);
  ut_assert(t, fabs(b[0] - z12) < epsilon);
  ut_assert(t, fabs(b[1] - z22) < epsilon);
  ut_assert(t, fabs(b[2] - z32) < epsilon);

  b[0] = 0.0;
  b[1] = 0.0;
  b[2] = 1.0;
  lial_chol_solv_lapack(a, n, b, &ret);
  ut_assert(t, ret == 0);
  ut_assert(t, fabs(b[0] - z13) < epsilon);
  ut_assert(t, fabs(b[1] - z23) < epsilon);
  ut_assert(t, fabs(b[2] - z33) < epsilon);
}

static void
lial_chol_fact_and_solve_lapack_shifted_0_m3_t (ut_test_t *const t)
{
  static const char ut_lial_upper = 'L';
  static const int n0 = 10;
  static const int n1 = 3;
  static const int nrhs = 1;
  double **a;
  int ret;
  double b[n0];

  const double epsilon = 1.E-15;
  static const bool debug = false;

  double a11, a12, a13, a21, a22, a23, a31, a32, a33, det;
  double z11, z12, z13, z21, z22, z23, z31, z32, z33;

  a11 =  3.1;
  a12 =  1.7;
  a13 = -0.7;
  a21 =  a12;
  a22 =  5.7;
  a23 =  0.1;
  a31 =  a13;
  a32 =  a23;
  a33 =  7.3;

  det =
    a11 * (a33 * a22 - a32 * a23) -
    a21 * (a33 * a12 - a32 * a13) +
    a31 * (a23 * a12 - a22 * a13);

  z11 =   (a33 * a22 - a32 * a23) / det;
  z12 = - (a33 * a12 - a32 * a13) / det;
  z13 =   (a23 * a12 - a22 * a13) / det;
  z21 = - (a33 * a21 - a31 * a23) / det;
  z22 =   (a33 * a11 - a31 * a13) / det;
  z23 = - (a23 * a11 - a21 * a13) / det;
  z31 =   (a32 * a21 - a31 * a22) / det;
  z32 = - (a32 * a11 - a31 * a12) / det;
  z33 =   (a22 * a11 - a21 * a12) / det;

  a = lial_allocate_square_matrix(n0);
  if (!a) {
    printf("\nUnable to allocate memory for matrix a\n");
    ut_assert(t, false);
  }

  a[0][0] = a11;
  a[0][1] = a12;
  a[0][2] = a13;
  a[1][0] = a21;
  a[1][1] = a22;
  a[1][2] = a23;
  a[2][0] = a31;
  a[2][1] = a32;
  a[2][2] = a33;

  lial_dpotrf(&ut_lial_upper, &n1, *a, &n0, &ret);
  ut_assert(t, ret == 0);
  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n1; i++)
      for (size_t j = 0; j < n1; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  b[0] = 1.0;
  b[1] = 0.0;
  b[2] = 0.0;
  lial_dpotrs(&ut_lial_upper, &n1, &nrhs, *a, &n0, b, &n0, &ret);
  ut_assert(t, ret == 0);
  ut_assert(t, fabs(b[0] - z11) < epsilon);
  ut_assert(t, fabs(b[1] - z21) < epsilon);
  ut_assert(t, fabs(b[2] - z31) < epsilon);

  b[0] = 0.0;
  b[1] = 1.0;
  b[2] = 0.0;
  lial_dpotrs(&ut_lial_upper, &n1, &nrhs, *a, &n0, b, &n0, &ret);
  ut_assert(t, ret == 0);
  ut_assert(t, fabs(b[0] - z12) < epsilon);
  ut_assert(t, fabs(b[1] - z22) < epsilon);
  ut_assert(t, fabs(b[2] - z32) < epsilon);

  b[0] = 0.0;
  b[1] = 0.0;
  b[2] = 1.0;
  lial_dpotrs(&ut_lial_upper, &n1, &nrhs, *a, &n0, b, &n0, &ret);
  ut_assert(t, ret == 0);
  ut_assert(t, fabs(b[0] - z13) < epsilon);
  ut_assert(t, fabs(b[1] - z23) < epsilon);
  ut_assert(t, fabs(b[2] - z33) < epsilon);

  lial_free_matrix(a, n0);
}

static void
lial_chol_fact_and_solve_lapack_shifted_1_m3_t (ut_test_t *const t)
{
  static const char ut_lial_upper = 'L';
  static const int n0 = 10;
  static const int n1 = 3;
  static const int nrhs = 3;
  static const int sr = 2; // shift on rows
  static const int sc = 3; // shift on columns
  double **a;
  double *ap;
  int ret;
  double **b;
  double *bp;

  const double epsilon = 1.E-15;
  static const bool debug = false;

  double a11, a12, a13, a21, a22, a23, a31, a32, a33, det;
  double z11, z12, z13, z21, z22, z23, z31, z32, z33;

  a11 =  3.1;
  a12 =  1.7;
  a13 = -0.7;
  a21 =  a12;
  a22 =  5.7;
  a23 =  0.1;
  a31 =  a13;
  a32 =  a23;
  a33 =  7.3;

  det =
    a11 * (a33 * a22 - a32 * a23) -
    a21 * (a33 * a12 - a32 * a13) +
    a31 * (a23 * a12 - a22 * a13);

  z11 =   (a33 * a22 - a32 * a23) / det;
  z12 = - (a33 * a12 - a32 * a13) / det;
  z13 =   (a23 * a12 - a22 * a13) / det;
  z21 = - (a33 * a21 - a31 * a23) / det;
  z22 =   (a33 * a11 - a31 * a13) / det;
  z23 = - (a23 * a11 - a21 * a13) / det;
  z31 =   (a32 * a21 - a31 * a22) / det;
  z32 = - (a32 * a11 - a31 * a12) / det;
  z33 =   (a22 * a11 - a21 * a12) / det;

  a = lial_allocate_square_matrix(n0);
  if (!a) {
    printf("\nUnable to allocate memory for matrix a\n");
    ut_assert(t, false);
  }
  for (size_t i = 0; i < n0; i++)
    for (size_t j = 0; j < n0; j++)
      a[i][j] = -7.0;

  ap = *a + sr * n0 + sc;

  a[0 + sr][0 + sc] = a11;
  a[0 + sr][1 + sc] = a12;
  a[0 + sr][2 + sc] = a13;
  a[1 + sr][0 + sc] = a21;
  a[1 + sr][1 + sc] = a22;
  a[1 + sr][2 + sc] = a23;
  a[2 + sr][0 + sc] = a31;
  a[2 + sr][1 + sc] = a32;
  a[2 + sr][2 + sc] = a33;

  b = lial_allocate_square_matrix(n0);
  if (!b) {
    printf("\nUnable to allocate memory for matrix b\n");
    ut_assert(t, false);
  }
  for (size_t i = 0; i < n0; i++)
    for (size_t j = 0; j < n0; j++)
      b[i][j] = -3.0;

  bp = *b + sr * n0 + sc;

  b[0 + sr][0 + sc] = 1.0;
  b[0 + sr][1 + sc] = 0.0;
  b[0 + sr][2 + sc] = 0.0;
  b[1 + sr][0 + sc] = 0.0;
  b[1 + sr][1 + sc] = 1.0;
  b[1 + sr][2 + sc] = 0.0;
  b[2 + sr][0 + sc] = 0.0;
  b[2 + sr][1 + sc] = 0.0;
  b[2 + sr][2 + sc] = 1.0;

  lial_dpotrf(&ut_lial_upper, &n1, ap, &n0, &ret);
  ut_assert(t, ret == 0);
  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n1; i++)
      for (size_t j = 0; j < n1; j++)
        printf("ap[%zu][%zu] = %21.15f\n", i, j, a[i + sr][j + sc]);
    printf("\n");
    for (size_t i = 0; i < n0; i++)
      for (size_t j = 0; j < n0; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  lial_dpotrs(&ut_lial_upper, &n1, &nrhs, ap, &n0, bp, &n0, &ret);
  ut_assert(t, ret == 0);
  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n1; i++)
      for (size_t j = 0; j < n1; j++)
        printf("bp[%zu][%zu] = %21.15f\n", i, j, b[i + sr][j + sc]);
    printf("\n");
    printf("z11 = %21.15f\n", z11);
    printf("z12 = %21.15f\n", z12);
    printf("z13 = %21.15f\n", z13);
    printf("z21 = %21.15f\n", z21);
    printf("z22 = %21.15f\n", z22);
    printf("z23 = %21.15f\n", z23);
    printf("z31 = %21.15f\n", z31);
    printf("z32 = %21.15f\n", z32);
    printf("z33 = %21.15f\n", z33);
    printf("\n");
    for (size_t i = 0; i < n0; i++)
      for (size_t j = 0; j < n0; j++)
        printf("b[%zu][%zu] = %21.15f\n", i, j, b[i][j]);
  }

  ut_assert(t, fabs(b[0 + sr][0 + sc] - z11) < epsilon);
  ut_assert(t, fabs(b[0 + sr][1 + sc] - z12) < epsilon);
  ut_assert(t, fabs(b[0 + sr][2 + sc] - z13) < epsilon);

  ut_assert(t, fabs(b[1 + sr][0 + sc] - z21) < epsilon);
  ut_assert(t, fabs(b[1 + sr][1 + sc] - z22) < epsilon);
  ut_assert(t, fabs(b[1 + sr][2 + sc] - z23) < epsilon);

  ut_assert(t, fabs(b[2 + sr][0 + sc] - z31) < epsilon);
  ut_assert(t, fabs(b[2 + sr][1 + sc] - z32) < epsilon);
  ut_assert(t, fabs(b[2 + sr][2 + sc] - z33) < epsilon);

  lial_free_matrix(b, n0);
  lial_free_matrix(a, n0);
}

static void
lial_chol_fact_and_solve_lapack_m_0_40_t (ut_test_t *const t)
{
  size_t start, end, i, n;
  int ret;

  double **g; // a matrix generated with random values. a = r * r transposed
  double **a; // the matrix to be inverted
  double **z; // the matrix inverse
  double **c; // a copy of the a matrix

  double lo, up;

  prng_mt19937_t *r;
  uint64_t seed;

  const double epsilon = 1.E-09;
  const bool debug = false;

  start = 1;
  end = 40;
  lo = -1.;
  up = +1.;
  seed = 8670;

  r = prng_mt19937_new();
  prng_mt19937_init_by_seed(r, seed);

  if (debug) printf("\n");
  for (i = start; i <= end; i++) {
    if (debug) printf("lial_chol_fact_and_solve_lapack_m_0_40_t: iteration n. %zu\n", i);

    n = i;
    ret = 0;

    g = lial_allocate_square_matrix(n);
    if (!g) {
      printf("\nUnable to allocate memory for matrix g\n");
      ut_assert(t, false);
    }

    a = lial_allocate_square_matrix(n);
    if (!a) {
      printf("\nUnable to allocate memory for matrix a\n");
      lial_free_matrix(g, n);
      ut_assert(t, false);
    }

    z = lial_allocate_square_matrix(n);
    if (!z) {
      printf("\nUnable to allocate memory for matrix z\n");
      lial_free_matrix(a, n);
      lial_free_matrix(g, n);
      ut_assert(t, false);
    }
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        z[i][j] = (i == j) ? 1.0 : 0.0;

    aux_create_random_sdf_matrix(r, lo, up, n, g, a);

    if (debug) {
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < n; j++)
          printf("g[%zu][%zu] = %21.15f\n", i, j, g[i][j]);
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < n; j++)
          printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
    }

    c = lial_clone_matrix(a, n, n);
    if (!c) {
      printf("\nUnable to allocate memory for matrix c\n");
      lial_free_matrix(z, n);
      lial_free_matrix(a, n);
      lial_free_matrix(g, n);
      ut_assert(t, false);
    }

    if (debug) {
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < n; j++)
          printf("z[%zu][%zu] = %21.15f\n", i, j, z[i][j]);
    }

    lial_chol_inv_lapack(a, n, z, &ret);

    if (debug) {
      for (size_t i = 0; i < n; i++)
        for (size_t j = 0; j < n; j++)
          printf("z[%zu][%zu] = %21.15f\n", i, j, z[i][j]);
    }

    aux_assert_identity(t, c, z, n, epsilon, debug);

    lial_free_matrix(c, n);
    lial_free_matrix(z, n);
    lial_free_matrix(a, n);
    lial_free_matrix(g, n);
  }

  prng_mt19937_free(r);
}

static void
lial_perf_sdf_chol_naive_1000_t (ut_test_t *const t)
{
  size_t const n = 1000;
  bool const debug = false;

  timespec_t start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  double **a;
  double **z;
  double **c;
  double *p;

  double max_delta, normalized_max_row_delta_modulus, mean, standard_deviation;

  double d2, cdm;
  size_t max_delta_i, max_delta_j, normalized_max_row_delta_modulus_i;

  aux_create_random_sdf_matrix_b(t, &a, n);
  if (debug) {
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  p = lial_allocate_vector(n);
  if (!p) {
    printf("\nUnable to allocate memory for vector p\n");
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  z = lial_allocate_square_matrix(n);
  if (!z) {
    printf("\nUnable to allocate memory for matrix z\n");
    lial_free_vector(p);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }
  for (size_t i = 0; i < n; i++)
    for (size_t j = 0; j < n; j++)
      z[i][j] = (i == j) ? 1.0 : 0.0;

  c = lial_clone_matrix(a, n, n);
  if (!c) {
    printf("\nUnable to allocate memory for matrix c\n");
    lial_free_matrix(z, n);
    lial_free_vector(p);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  lial_chol_fact_naive(a, n, p);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Factorizing SDF matrix of size %8zu:                    [%6lld.%9ld]\n", n, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Computes inverse matrix z. */
  for (size_t i = 0; i < n; i++) {
    lial_chol_solv_naive(a, n, p, z[i], z[i]);
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Computing the inverse matrix:                               [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Multiplies the original random SDF matrix and its inverse. */
  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      a[i][j] = 0.0;
      for (size_t k = 0; k < n; k++) {
        //a[i][j] += c[i][k] * z[k][j];
        a[i][j] += z[i][k] * c[j][k];
      }
    }
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Multiplying the original random SDF matrix and its inverse: [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /*
   * The standard deviation of the population of elements of the matrix ((A * inv(A)) - I) is computed assuming that the mean is zero.
   *
   */
  max_delta = 0.0;
  max_delta_i = 0;
  max_delta_j = 0;
  normalized_max_row_delta_modulus = 0.0;
  normalized_max_row_delta_modulus_i = 0;
  mean = 0.0;
  standard_deviation = 0.0;
  for (size_t i = 0; i < n; i++) {
    cdm = 0.0;
    a[i][i] -= 1.0;
    for (size_t j = 0; j < n; j++) {
      mean += a[i][j];
      d2 = a[i][j] * a[i][j];
      if (d2 > max_delta) { max_delta = d2; max_delta_i = i; max_delta_j = j; }
      cdm += d2;
    }
    standard_deviation += cdm;
    if (cdm > normalized_max_row_delta_modulus) { normalized_max_row_delta_modulus = cdm; normalized_max_row_delta_modulus_i = i; }
  }
  mean = mean / ((double) n * (double) n);
  standard_deviation = sqrt(standard_deviation / ((double) n * (double) n));
  normalized_max_row_delta_modulus = sqrt(normalized_max_row_delta_modulus / (double) n) ;
  max_delta = sqrt(max_delta);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Error matrix (deviations from the identity matrix) KPI:\n");
    fprintf(stdout, "    mean                             = %28.18f\n", mean);
    fprintf(stdout, "    standard_deviation               = %28.18f\n", standard_deviation);
    fprintf(stdout, "    normalized_max_row_delta_modulus = %28.18f, row #%6zu\n", normalized_max_row_delta_modulus, normalized_max_row_delta_modulus_i);
    fprintf(stdout, "    max_delta                        = %28.18f, i #%6zu, j #%6zu\n", max_delta, max_delta_i, max_delta_j);
  }

  lial_free_matrix(c, n);
  lial_free_matrix(z, n);
  lial_free_vector(p);
  lial_free_matrix(a, n);
}

static void
lial_perf_sdf_lu_naive_1000_t (ut_test_t *const t)
{
  size_t const n = 1000;
  bool const debug = false;

  timespec_t start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  double **a;
  double **z;
  double **c;
  double *scale;
  size_t *indx;

  double max_delta, normalized_max_row_delta_modulus, mean, standard_deviation;

  double d2, cdm;
  size_t max_delta_i, max_delta_j, normalized_max_row_delta_modulus_i;

  aux_create_random_sdf_matrix_b(t, &a, n);
  if (debug) {
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  scale = lial_allocate_vector(n);
  if (!scale) {
    printf("\nUnable to allocate memory for vector scale\n");
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  indx = (size_t *) malloc(sizeof(size_t) * n);
  if (!indx) {
    printf("\nUnable to allocate memory for vector scale\n");
    lial_free_vector(scale);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  z = lial_allocate_square_matrix(n);
  if (!z) {
    printf("\nUnable to allocate memory for matrix z\n");
    lial_free_vector(scale);
    free(indx);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }
  for (size_t i = 0; i < n; i++)
    for (size_t j = 0; j < n; j++)
      z[i][j] = (i == j) ? 1.0 : 0.0;

  c = lial_clone_matrix(a, n, n);
  if (!c) {
    printf("\nUnable to allocate memory for matrix c\n");
    lial_free_matrix(z, n);
    lial_free_vector(scale);
    free(indx);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  ret = lial_lu_decom_naive(a, n, indx, scale);
  ut_assert(t, ret == 1);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Factorizing SDF matrix of size %8zu:                    [%6lld.%9ld]\n", n, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Computes inverse matrix z. */
  for (size_t i = 0; i < n; i++) {
    lial_lu_bsubst_naive(a, n, indx, scale, z[i]);
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Computing the inverse matrix:                               [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Multiplies the original random SDF matrix and its inverse. */
  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      a[i][j] = 0.0;
      for (size_t k = 0; k < n; k++) {
        //a[i][j] += c[i][k] * z[k][j];
        a[i][j] += z[i][k] * c[j][k];
      }
    }
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Multiplying the original random SDF matrix and its inverse: [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /*
   * The standard deviation of the population of elements of the matrix ((A * inv(A)) - I) is computed assuming that the mean is zero.
   *
   */
  max_delta = 0.0;
  max_delta_i = 0;
  max_delta_j = 0;
  normalized_max_row_delta_modulus = 0.0;
  normalized_max_row_delta_modulus_i = 0;
  mean = 0.0;
  standard_deviation = 0.0;
  for (size_t i = 0; i < n; i++) {
    cdm = 0.0;
    a[i][i] -= 1.0;
    for (size_t j = 0; j < n; j++) {
      mean += a[i][j];
      d2 = a[i][j] * a[i][j];
      if (d2 > max_delta) { max_delta = d2; max_delta_i = i; max_delta_j = j; }
      cdm += d2;
    }
    standard_deviation += cdm;
    if (cdm > normalized_max_row_delta_modulus) { normalized_max_row_delta_modulus = cdm; normalized_max_row_delta_modulus_i = i; }
  }
  mean = mean / ((double) n * (double) n);
  standard_deviation = sqrt(standard_deviation / ((double) n * (double) n));
  normalized_max_row_delta_modulus = sqrt(normalized_max_row_delta_modulus / (double) n) ;
  max_delta = sqrt(max_delta);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Error matrix (deviations from the identity matrix) KPI:\n");
    fprintf(stdout, "    mean                             = %28.18f\n", mean);
    fprintf(stdout, "    standard_deviation               = %28.18f\n", standard_deviation);
    fprintf(stdout, "    normalized_max_row_delta_modulus = %28.18f, row #%6zu\n", normalized_max_row_delta_modulus, normalized_max_row_delta_modulus_i);
    fprintf(stdout, "    max_delta                        = %28.18f, i #%6zu, j #%6zu\n", max_delta, max_delta_i, max_delta_j);
  }

  lial_free_matrix(c, n);
  lial_free_matrix(z, n);
  lial_free_vector(scale);
  free(indx);
  lial_free_matrix(a, n);
}

static void
lial_perf_sdf_lapack_1000_t (ut_test_t *const t)
{
  size_t const n = 1000;
  bool const debug = false;

  timespec_t start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  double **a;
  double **z;
  double **c;

  double max_delta, normalized_max_row_delta_modulus, mean, standard_deviation;

  double d2, cdm;
  size_t max_delta_i, max_delta_j, normalized_max_row_delta_modulus_i;

  aux_create_random_sdf_matrix_b(t, &a, n);
  if (debug) {
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  z = lial_allocate_square_matrix(n);
  if (!z) {
    printf("\nUnable to allocate memory for matrix z\n");
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }
  for (size_t i = 0; i < n; i++)
    for (size_t j = 0; j < n; j++)
      z[i][j] = (i == j) ? 1.0 : 0.0;

  c = lial_clone_matrix(a, n, n);
  if (!c) {
    printf("\nUnable to allocate memory for matrix c\n");
    lial_free_matrix(z, n);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  lial_chol_fact_lapack(a, n, &ret);
  ut_assert(t, ret == 0);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Factorizing SDF matrix of size %8zu:                    [%6lld.%9ld]\n", n, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Computes inverse matrix z. */
  for (size_t i = 0; i < n; i++) {
    lial_chol_solv_lapack(a, n, z[i], &ret);
    ut_assert(t, ret == 0);
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Computing the inverse matrix:                               [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Multiplies the original random SDF matrix and its inverse. */
  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      a[i][j] = 0.0;
      for (size_t k = 0; k < n; k++) {
        //a[i][j] += c[i][k] * z[k][j];
        a[i][j] += z[i][k] * c[j][k];
      }
    }
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Multiplying the original random SDF matrix and its inverse: [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /*
   * The standard deviation of the population of elements of the matrix ((A * inv(A)) - I) is computed assuming that the mean is zero.
   *
   */
  max_delta = 0.0;
  max_delta_i = 0;
  max_delta_j = 0;
  normalized_max_row_delta_modulus = 0.0;
  normalized_max_row_delta_modulus_i = 0;
  mean = 0.0;
  standard_deviation = 0.0;
  for (size_t i = 0; i < n; i++) {
    cdm = 0.0;
    a[i][i] -= 1.0;
    for (size_t j = 0; j < n; j++) {
      mean += a[i][j];
      d2 = a[i][j] * a[i][j];
      if (d2 > max_delta) { max_delta = d2; max_delta_i = i; max_delta_j = j; }
      cdm += d2;
    }
    standard_deviation += cdm;
    if (cdm > normalized_max_row_delta_modulus) { normalized_max_row_delta_modulus = cdm; normalized_max_row_delta_modulus_i = i; }
  }
  mean = mean / ((double) n * (double) n);
  standard_deviation = sqrt(standard_deviation / ((double) n * (double) n));
  normalized_max_row_delta_modulus = sqrt(normalized_max_row_delta_modulus / (double) n) ;
  max_delta = sqrt(max_delta);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Error matrix (deviations from the identity matrix) KPI:\n");
    fprintf(stdout, "    mean                             = %28.18f\n", mean);
    fprintf(stdout, "    standard_deviation               = %28.18f\n", standard_deviation);
    fprintf(stdout, "    normalized_max_row_delta_modulus = %28.18f, row #%6zu\n", normalized_max_row_delta_modulus, normalized_max_row_delta_modulus_i);
    fprintf(stdout, "    max_delta                        = %28.18f, i #%6zu, j #%6zu\n", max_delta, max_delta_i, max_delta_j);
  }

  lial_free_matrix(c, n);
  lial_free_matrix(z, n);
  lial_free_matrix(a, n);
}

static void
lial_perf_sdf_chol_naive_edge_000_t (ut_test_t *const t)
{
  bool const debug = false;
  const char test_data_file_name[] = "./test/data/ut_linear_algebra/large_binary.sdf_edge_000.dat";

  size_t n, nr, nc;

  timespec_t start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  double **a;
  double **z;
  double **c;
  double *p;

  double max_delta, normalized_max_row_delta_modulus, mean, standard_deviation;

  double d2, cdm;
  size_t max_delta_i, max_delta_j, normalized_max_row_delta_modulus_i;

  a = lial_retrieve_matrix (test_data_file_name, &nr, &nc, &ret);
  if (ret != 0 || !a || (nr != nc)) {
    printf("\nUnable to read properly matrix a from file: %s\n", test_data_file_name);
    ut_assert(t, false);
  } else {
    n = nr;
  }
  if (debug) {
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  p = lial_allocate_vector(n);
  if (!p) {
    printf("\nUnable to allocate memory for vector p\n");
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  z = lial_allocate_square_matrix(n);
  if (!z) {
    printf("\nUnable to allocate memory for matrix z\n");
    lial_free_vector(p);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }
  for (size_t i = 0; i < n; i++)
    for (size_t j = 0; j < n; j++)
      z[i][j] = (i == j) ? 1.0 : 0.0;

  c = lial_clone_matrix(a, n, n);
  if (!c) {
    printf("\nUnable to allocate memory for matrix c\n");
    lial_free_matrix(z, n);
    lial_free_vector(p);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  lial_chol_fact_naive(a, n, p);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Factorizing SDF matrix of size %8zu:                    [%6lld.%9ld]\n", n, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Computes inverse matrix z. */
  for (size_t i = 0; i < n; i++) {
    lial_chol_solv_naive(a, n, p, z[i], z[i]);
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Computing the inverse matrix:                               [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Multiplies the original random SDF matrix and its inverse. */
  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      a[i][j] = 0.0;
      for (size_t k = 0; k < n; k++) {
        //a[i][j] += c[i][k] * z[k][j];
        a[i][j] += z[i][k] * c[j][k];
      }
    }
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Multiplying the original random SDF matrix and its inverse: [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /*
   * The standard deviation of the population of elements of the matrix ((A * inv(A)) - I) is computed assuming that the mean is zero.
   *
   */
  max_delta = 0.0;
  max_delta_i = 0;
  max_delta_j = 0;
  normalized_max_row_delta_modulus = 0.0;
  normalized_max_row_delta_modulus_i = 0;
  mean = 0.0;
  standard_deviation = 0.0;
  for (size_t i = 0; i < n; i++) {
    cdm = 0.0;
    a[i][i] -= 1.0;
    for (size_t j = 0; j < n; j++) {
      mean += a[i][j];
      d2 = a[i][j] * a[i][j];
      if (d2 > max_delta) { max_delta = d2; max_delta_i = i; max_delta_j = j; }
      cdm += d2;
    }
    standard_deviation += cdm;
    if (cdm > normalized_max_row_delta_modulus) { normalized_max_row_delta_modulus = cdm; normalized_max_row_delta_modulus_i = i; }
  }
  mean = mean / ((double) n * (double) n);
  standard_deviation = sqrt(standard_deviation / ((double) n * (double) n));
  normalized_max_row_delta_modulus = sqrt(normalized_max_row_delta_modulus / (double) n) ;
  max_delta = sqrt(max_delta);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Error matrix (deviations from the identity matrix) KPI:\n");
    fprintf(stdout, "    mean                             = %28.18f\n", mean);
    fprintf(stdout, "    standard_deviation               = %28.18f\n", standard_deviation);
    fprintf(stdout, "    normalized_max_row_delta_modulus = %28.18f, row #%6zu\n", normalized_max_row_delta_modulus, normalized_max_row_delta_modulus_i);
    fprintf(stdout, "    max_delta                        = %28.18f, i #%6zu, j #%6zu\n", max_delta, max_delta_i, max_delta_j);
  }

  lial_free_matrix(c, n);
  lial_free_matrix(z, n);
  lial_free_vector(p);
  lial_free_matrix(a, n);
}

static void
lial_perf_sdf_lu_naive_edge_000_t (ut_test_t *const t)
{
  bool const debug = false;
  const char test_data_file_name[] = "./test/data/ut_linear_algebra/large_binary.sdf_edge_000.dat";

  size_t n, nr, nc;

  timespec_t start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  double **a;
  double **z;
  double **c;
  double *scale;
  size_t *indx;

  double max_delta, normalized_max_row_delta_modulus, mean, standard_deviation;

  double d2, cdm;
  size_t max_delta_i, max_delta_j, normalized_max_row_delta_modulus_i;

  a = lial_retrieve_matrix(test_data_file_name, &nr, &nc, &ret);
  if (ret != 0 || !a || (nr != nc)) {
    printf("\nUnable to read properly matrix a from file: %s\n", test_data_file_name);
    ut_assert(t, false);
  } else {
    n = nr;
  }
  if (debug) {
    for (size_t i = 0; i < n; i++)
      for (size_t j = 0; j < n; j++)
        printf("a[%zu][%zu] = %21.15f\n", i, j, a[i][j]);
  }

  scale = lial_allocate_vector(n);
  if (!scale) {
    printf("\nUnable to allocate memory for vector scale\n");
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  indx = (size_t *) malloc(sizeof(size_t) * n);
  if (!indx) {
    printf("\nUnable to allocate memory for vector scale\n");
    lial_free_vector(scale);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  z = lial_allocate_square_matrix(n);
  if (!z) {
    printf("\nUnable to allocate memory for matrix z\n");
    lial_free_vector(scale);
    free(indx);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }
  for (size_t i = 0; i < n; i++)
    for (size_t j = 0; j < n; j++)
      z[i][j] = (i == j) ? 1.0 : 0.0;

  c = lial_clone_matrix(a, n, n);
  if (!c) {
    printf("\nUnable to allocate memory for matrix c\n");
    lial_free_matrix(z, n);
    lial_free_vector(scale);
    free(indx);
    lial_free_matrix(a, n);
    ut_assert(t, false);
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  ret = lial_lu_decom_naive(a, n, indx, scale);
  ut_assert(t, ret == 1);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Factorizing SDF matrix of size %8zu:                    [%6lld.%9ld]\n", n, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Computes inverse matrix z. */
  for (size_t i = 0; i < n; i++) {
    lial_lu_bsubst_naive(a, n, indx, scale, z[i]);
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Computing the inverse matrix:                               [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* Multiplies the original random SDF matrix and its inverse. */
  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      a[i][j] = 0.0;
      for (size_t k = 0; k < n; k++) {
        //a[i][j] += c[i][k] * z[k][j];
        a[i][j] += z[i][k] * c[j][k];
      }
    }
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Multiplying the original random SDF matrix and its inverse: [%6lld.%9ld]\n", (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time));
  }

  /*
   * The standard deviation of the population of elements of the matrix ((A * inv(A)) - I) is computed assuming that the mean is zero.
   *
   */
  max_delta = 0.0;
  max_delta_i = 0;
  max_delta_j = 0;
  normalized_max_row_delta_modulus = 0.0;
  normalized_max_row_delta_modulus_i = 0;
  mean = 0.0;
  standard_deviation = 0.0;
  for (size_t i = 0; i < n; i++) {
    cdm = 0.0;
    a[i][i] -= 1.0;
    for (size_t j = 0; j < n; j++) {
      mean += a[i][j];
      d2 = a[i][j] * a[i][j];
      if (d2 > max_delta) { max_delta = d2; max_delta_i = i; max_delta_j = j; }
      cdm += d2;
    }
    standard_deviation += cdm;
    if (cdm > normalized_max_row_delta_modulus) { normalized_max_row_delta_modulus = cdm; normalized_max_row_delta_modulus_i = i; }
  }
  mean = mean / ((double) n * (double) n);
  standard_deviation = sqrt(standard_deviation / ((double) n * (double) n));
  normalized_max_row_delta_modulus = sqrt(normalized_max_row_delta_modulus / (double) n) ;
  max_delta = sqrt(max_delta);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Error matrix (deviations from the identity matrix) KPI:\n");
    fprintf(stdout, "    mean                             = %28.18f\n", mean);
    fprintf(stdout, "    standard_deviation               = %28.18f\n", standard_deviation);
    fprintf(stdout, "    normalized_max_row_delta_modulus = %28.18f, row #%6zu\n", normalized_max_row_delta_modulus, normalized_max_row_delta_modulus_i);
    fprintf(stdout, "    max_delta                        = %28.18f, i #%6zu, j #%6zu\n", max_delta, max_delta_i, max_delta_j);
  }

  lial_free_matrix(c, n);
  lial_free_matrix(z, n);
  lial_free_vector(scale);
  free(indx);
  lial_free_matrix(a, n);
}

static void
lial_perf_sdf_lapack_edge_000_t (ut_test_t *const t)
{
  aux_perf_sdf_lapack_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_edge_000.dat");
}

static void
lial_perf_sdf_lapack_corner_000_t (ut_test_t *const t)
{
  aux_perf_sdf_lapack_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_corner_000.dat");
}

static void
lial_perf_sdf_lapack_xedge_000_t (ut_test_t *const t)
{
  aux_perf_sdf_lapack_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_xedge_000.dat");
}

static void
lial_perf_sdf_lapack_edge_000_bp_t (ut_test_t *const t)
{
  const unsigned int bs = 512;
  const unsigned int tc = 1;
  aux_perf_sdf_lapack_blocked_parallel_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_edge_000.dat", bs, tc);
}

static void
lial_dgem_1_t (ut_test_t *const t)
{
  static const size_t n0 = 1;

  double **a;
  double **b;
  double **c;
  double **z;

  double alpha, beta;
  int m, n, k, lda, ldb, ldc;

  char transa, transb;

  m = n0;
  n = n0;
  k = n0;
  lda = n0;
  ldb = n0;
  ldc = n0;

  transa = 'N';
  transb = 'N';

  alpha = 1.0;
  beta = 3.0;

  a = lial_allocate_square_matrix(n0);
  if (!a) {
    printf("\nUnable to allocate memory for matrix a\n");
    ut_assert(t, false);
  }

  b = lial_allocate_square_matrix(n0);
  if (!b) {
    printf("\nUnable to allocate memory for matrix b\n");
    lial_free_matrix(a, n0);
    ut_assert(t, false);
  }

  c = lial_allocate_square_matrix(n0);
  if (!c) {
    printf("\nUnable to allocate memory for matrix c\n");
    lial_free_matrix(b, n0);
    lial_free_matrix(a, n0);
    ut_assert(t, false);
  }

  z = lial_allocate_square_matrix(n0);
  if (!z) {
    printf("\nUnable to allocate memory for matrix z\n");
    lial_free_matrix(c, n0);
    lial_free_matrix(b, n0);
    lial_free_matrix(a, n0);
    ut_assert(t, false);
  }

  alpha = 3.0;
  beta = 2.0;
  a[0][0] =  7.0;
  b[0][0] =  5.0;
  c[0][0] = 11.0;

  z[0][0] = 127;

  lial_dgemm(&transa, &transb, &m, &n, &k, &alpha, *a, &lda, *b, &ldb, &beta, *c, &ldc);

  ut_assert(t, c[0][0] == z[0][0]);

  lial_free_matrix(z, n);
  lial_free_matrix(c, n);
  lial_free_matrix(b, n);
  lial_free_matrix(a, n);
}

static void
lial_dgem_3_t (ut_test_t *const t)
{
  static const size_t n0 = 3;

  static const bool debug = false;

  double **a;
  double **b;
  double **c;
  double **z;

  double alpha, beta;
  int m, n, k, lda, ldb, ldc;

  double epsilon;

  char transa, transb;

  epsilon = 1.0E-12;

  m = n0;
  n = n0;
  k = n0;
  lda = n0;
  ldb = n0;
  ldc = n0;

  transa = 'N';
  transb = 'N';

  alpha = 1.0;
  beta = 3.0;

  a = lial_allocate_square_matrix(n0);
  if (!a) {
    printf("\nUnable to allocate memory for matrix a\n");
    ut_assert(t, false);
  }

  b = lial_allocate_square_matrix(n0);
  if (!b) {
    printf("\nUnable to allocate memory for matrix b\n");
    lial_free_matrix(a, n0);
    ut_assert(t, false);
  }

  c = lial_allocate_square_matrix(n0);
  if (!c) {
    printf("\nUnable to allocate memory for matrix c\n");
    lial_free_matrix(b, n0);
    lial_free_matrix(a, n0);
    ut_assert(t, false);
  }

  z = lial_allocate_square_matrix(n0);
  if (!z) {
    printf("\nUnable to allocate memory for matrix z\n");
    lial_free_matrix(c, n0);
    lial_free_matrix(b, n0);
    lial_free_matrix(a, n0);
    ut_assert(t, false);
  }

  alpha = 2.0;
  beta = -1.0;
  a[0][0] =  1.2;
  a[0][1] = -1.1;
  a[0][2] =  0.9;
  a[1][0] =  3.5;
  a[1][1] =  2.1;
  a[1][2] = -3.4;
  a[2][0] = -0.7;
  a[2][1] =  0.4;
  a[2][2] =  1.9;

  b[0][0] =  0.1;
  b[0][1] =  2.0;
  b[0][2] =  1.5;
  b[1][0] = -2.7;
  b[1][1] =  1.4;
  b[1][2] = -3.1;
  b[2][0] =  1.9;
  b[2][1] =  0.5;
  b[2][2] =  0.7;

  c[0][0] = -1.0;
  c[0][1] =  2.0;
  c[0][2] =  3.0;
  c[1][0] =  4.0;
  c[1][1] =  8.0;
  c[1][2] =  3.4;
  c[2][0] = -4.3;
  c[2][1] =  0.1;
  c[2][2] =  4.0;

  z[0][0] =  10.6;
  z[0][1] =   0.62;
  z[0][2] =   8.68;
  z[1][0] = -27.56;
  z[1][1] =   8.48;
  z[1][2] = -10.68;
  z[2][0] =   9.22;
  z[2][1] =   0.12;
  z[2][2] =  -5.92;

  if (false)
    lial_dgemm(&transb, &transa, &n, &m, &k, &alpha, *b, &ldb, *a, &lda, &beta, *c, &ldc);
  else
    lial_dgemm_rowmajor(*a, *b, *c, &m, &n, &k, &alpha, &beta, &transa, &transb, &lda, &ldb, &ldc);

  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n0; i++)
      for (size_t j = 0; j < n0; j++) {
        printf("i = %zu, j=%zu, c[i][j] = %24.18f, z[i][j] = %24.18f\n", i, j, c[i][j], z[i][j]);
        ut_assert(t, fabs(c[i][j] - z[i][j]) < epsilon);
      }
  }

  lial_free_matrix(z, n);
  lial_free_matrix(c, n);
  lial_free_matrix(b, n);
  lial_free_matrix(a, n);
}

static void
aux_print_matrix (char *name,
                  double *a,
                  int n)
{
  printf("\n");
  printf("\n");
  printf("Matrix %s:\n", name);
  printf("________________________________________________________________________________________________________\n");
  printf("\n");
  for (int i = 0; i < n; i++) {
    printf(" .%2d. | ", i);
    for (int j = 0; j < n; j++) {
      int k = i * n + j;
      printf("%6.3f, ", a[k]);
    }
    printf("\n");
  }
  printf("________________________________________________________________________________________________________\n");
}

static void
lial_dtrsm_0_t (ut_test_t *const t)
{
  static const int n = 12;
  static const int bs = 4;
  double a[] =
    {
     8.0, 0.2, 0.1, 0.0, 0.4, 0.2, 0.1, 0.7, 0.3, 0.5, 0.1, 0.2,
     0.0, 3.0, 0.0, 0.4, 0.3, 0.6, 0.1, 0.9, 0.2, 0.1, 0.5, 0.3,
     0.0, 0.0, 7.0, 0.6, 0.2, 0.5, 0.6, 0.6, 0.3, 0.1, 0.8, 0.0,
     0.0, 0.0, 0.0, 8.0, 0.1, 0.3, 0.7, 0.9, 0.0, 0.0, 0.2, 0.2,
     0.0, 0.0, 0.0, 0.0, 9.0, 0.4, 0.1, 0.1, 0.1, 0.5, 0.7, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.1, 0.9, 0.8, 0.3, 0.2, 0.5,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.3, 0.0, 0.1,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.4, 0.7, 0.6, 0.5,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.9, 0.5, 0.1,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 9.0, 0.3, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.8,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0,
    };

  ut_assert(t, sizeof(a) / sizeof(double) == n * n);

  static const char ut_lial_upper = 'L';

  int ret, nx, bsx;

  const bool verbose = false;

  ret = 0;
  nx = n;
  bsx = bs;

  if (verbose) aux_print_matrix("A", a, n);

  double u[n * n];
  for (int i = 0; i < n * n; i++)
    u[i] = a[i];

  lial_dpotrf(&ut_lial_upper, &n, u, &n, &ret);
  ut_assert(t, ret == 0);

  if (verbose) aux_print_matrix("U", u, n);

  double b[n * n];
  for (int i = 0; i < n * n; i++)
    b[i] = a[i];

  double *bd;
  double *bi;

  bd = b;
  bi = b;
  // TILE-OP : 0.11
  lial_dpotrf(&ut_lial_upper, &bs, bd, &n, &ret);
  ut_assert(t, ret == 0);

  if (verbose) aux_print_matrix("B , TILE-OP : TRSM 0.11", b, n);

  char side = 'R';
  char uplo = 'L';
  char transa = 'T';
  char diag = 'N';
  double alpha = 1.0;
  bi += bs;
  // TILE-OP : 0.12
  lial_dtrsm(&side, &uplo, &transa, &diag, &bsx, &bsx, &alpha, bd, &nx, bi, &nx);

  if (verbose) aux_print_matrix("B , TILE-OP : TRSM 0.12", b, n);

  bi += bs;
  // TILE-OP : 0.13
  lial_dtrsm(&side, &uplo, &transa, &diag, &bsx, &bsx, &alpha, bd, &nx, bi, &nx);

  if (verbose) aux_print_matrix("B , TILE-OP : TRSM 0.13", b, n);

  char uplo_syrk = 'L';
  char trans_syrk = 'N';
  double alpha_syrk = -1.0;
  double beta_syrk = 1.0;
  double *a_syrk = b +              1 * bs;
  double *c_syrk = b + 1 * bs * n + 1 * bs;
  int n_syrk = bs;
  int k_syrk = bs;
  // TILE-OP : 0.22
  lial_dsyrk(&uplo_syrk, &trans_syrk, &n_syrk, &k_syrk, &alpha_syrk, a_syrk, &nx, &beta_syrk, c_syrk, &nx);

  if (verbose) aux_print_matrix("B , TILE-OP : SYRK 0.22", b, n);

  char transa_gemm = 'N';
  char transb_gemm = 'T';
  int m_gemm;
  int n_gemm;
  int k_gemm;
  double alpha_gemm = -1.0;
  double beta_gemm = 1.0;
  double *a_gemm, *b_gemm, *c_gemm;
  m_gemm = bs;
  n_gemm = bs;
  k_gemm = bs;
  a_gemm = b + 1 * bs;
  b_gemm = b + 2 * bs;
  c_gemm = b + 1 * bs * n + 2 * bs;
  // TILE-OP : 0.23
  lial_dgemm(&transa_gemm, &transb_gemm, &m_gemm, &n_gemm, &k_gemm, &alpha_gemm, b_gemm, &nx, a_gemm, &nx, &beta_gemm, c_gemm, &nx);

  if (verbose) aux_print_matrix("B , TILE-OP : GEMM 0.23", b, n);

  a_syrk = b +              2 * bs;
  c_syrk = b + 2 * bs * n + 2 * bs;
  // TILE-OP : 0.33
  lial_dsyrk(&uplo_syrk, &trans_syrk, &n_syrk, &k_syrk, &alpha_syrk, a_syrk, &nx, &beta_syrk, c_syrk, &nx);

  if (verbose) aux_print_matrix("B , TILE-OP : SYRK 0.33", b, n);

  bd = b + 1 * bs * n + 1 * bs;
  bi = b + 1 * bs * n + 1 * bs;
  // TILE-OP : 1.22
  lial_dpotrf(&ut_lial_upper, &bs, bd, &n, &ret);
  ut_assert(t, ret == 0);

  if (verbose) aux_print_matrix("B , TILE-OP : TRSM 1.22", b, n);

  bi += bs;
  // TILE-OP : 1.23
  lial_dtrsm(&side, &uplo, &transa, &diag, &bsx, &bsx, &alpha, bd, &nx, bi, &nx);

  if (verbose) aux_print_matrix("B , TILE-OP : TRSM 1.23", b, n);

  a_syrk = b + 1 * bs * n + 2 * bs;
  c_syrk = b + 2 * bs * n + 2 * bs;
  // TILE-OP : 1.33
  lial_dsyrk(&uplo_syrk, &trans_syrk, &n_syrk, &k_syrk, &alpha_syrk, a_syrk, &nx, &beta_syrk, c_syrk, &nx);

  if (verbose) aux_print_matrix("B , TILE-OP : SYRK 1.33", b, n);

  bd = b + 2 * bs * n + 2 * bs;
  bi = b + 2 * bs * n + 2 * bs;
  // TILE-OP : 2.33
  lial_dpotrf(&ut_lial_upper, &bs, bd, &n, &ret);
  ut_assert(t, ret == 0);

  if (verbose) aux_print_matrix("B , TILE-OP : TRSM 2.33", b, n);

  double epsilon;
  epsilon = 1.0E-15;
  for (int i = 0; i < n * n; i++)
    ut_assert(t, fabs(b[i] - u[i]) < epsilon);

  // reset the b matrix
  for (int i = 0; i < n * n; i++)
    b[i] = a[i];

  if (verbose) aux_print_matrix("U", u, n);

  if (verbose) printf("\n");
  int nb;
  int i, j, k, h;
  double *tile_diag, *tile_j, *tile_k, *tile_h;
  nb = n / bs;
  for (i = 0; i < nb; i++) {
    if (verbose) printf("\n\n\n.i loop, (%d)\n", i);
    tile_diag = b + i * bs * (n + 1);
    lial_dpotrf(&ut_lial_upper, &bs, tile_diag, &n, &ret);
    if (verbose) aux_print_matrix("B , I LOOP", b, n);

    for (j = i + 1; j < nb; j++) {
      if (verbose) printf("\n\n\n..j loop trsm, (%d, %d)\n", i, j);
      tile_j = tile_diag + bs * (j - i);
      lial_dtrsm(&side, &uplo, &transa, &diag, &bsx, &bsx, &alpha, tile_diag, &nx, tile_j, &nx);
      if (verbose) aux_print_matrix("B , J TRSM LOOP", b, n);
    }

    for (k = i + 1; k < nb; k++) {
      if (verbose) printf("\n\n\n..k loop syrk-gemm, (%d, %d)\n", i, k);
      tile_j = tile_diag + bs * (k - i); // a_syrk , same row as the tile_diag
      tile_k = tile_j + bs * n * (k - i); // c_syrk , same column as the tile_j , on the diagonal
      lial_dsyrk(&uplo_syrk, &trans_syrk, &bsx, &bsx, &alpha_syrk, tile_j, &nx, &beta_syrk, tile_k, &nx);
      if (verbose) aux_print_matrix("B , K SYRK-GEMM LOOP", b, n);

      for (h = k + 1; h < nb; h++) {
        if (verbose) printf("\n\n\n...h loop gemm, (%d, %d, %d)\n", i, k, h);
        tile_j = tile_j; // a_gemm
        tile_k = tile_j + bs * (h - k); // b_gemm
        tile_h = tile_k + bs * n * (h - k); // c_gemm
        lial_dgemm(&transa_gemm, &transb_gemm, &bsx, &bsx, &bsx, &alpha_gemm, tile_k, &nx, tile_j, &nx, &beta_gemm, tile_h, &nx);
        if (verbose) aux_print_matrix("B , H GEMM LOOP", b, n);
      }
    }
  }

  for (int i = 0; i < n * n; i++)
    ut_assert(t, fabs(b[i] - u[i]) < epsilon);

}

static void
lial_dtrsm_1_t (ut_test_t *const t)
{
  static const int n = 12;
  static const int bs = 3;
  double a[] =
    {
     8.0, 0.2, 0.1, 0.0, 0.4, 0.2, 0.1, 0.7, 0.3, 0.5, 0.1, 0.2,
     0.0, 3.0, 0.0, 0.4, 0.3, 0.6, 0.1, 0.9, 0.2, 0.1, 0.5, 0.3,
     0.0, 0.0, 7.0, 0.6, 0.2, 0.5, 0.6, 0.6, 0.3, 0.1, 0.8, 0.0,
     0.0, 0.0, 0.0, 8.0, 0.1, 0.3, 0.7, 0.9, 0.0, 0.0, 0.2, 0.2,
     0.0, 0.0, 0.0, 0.0, 9.0, 0.4, 0.1, 0.1, 0.1, 0.5, 0.7, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.1, 0.9, 0.8, 0.3, 0.2, 0.5,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.3, 0.0, 0.1,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.4, 0.7, 0.6, 0.5,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.9, 0.5, 0.1,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 9.0, 0.3, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.8,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0,
    };

  ut_assert(t, sizeof(a) / sizeof(double) == n * n);
  double u[n * n];
  double b[n * n];

  int ret, nx, bsx;

  char sym_L = 'L';
  char sym_N = 'N';
  char sym_R = 'R';
  char sym_T = 'T';

  double pone = +1.0;
  double mone = -1.0;

  const bool verbose = true;
  double epsilon;

  int nb;
  int i, j, k, h;
  double *tile_d, *tile_j, *tile_k, *tile_h;

  epsilon = 1.0E-15;

  ret = 0;
  nx = n;
  bsx = bs;

  for (i = 0; i < n * n; i++)
    u[i] = a[i];

  for (i = 0; i < n * n; i++)
    b[i] = a[i];

  lial_dpotrf(&sym_L, &n, u, &n, &ret);
  ut_assert(t, ret == 0);

  if (verbose) aux_print_matrix("A", a, n);
  if (verbose) aux_print_matrix("U", u, n);

  nb = n / bs;
  for (i = 0; i < nb; i++) {
    if (verbose) printf("\n\n\n.i loop, (%d)\n", i);
    tile_d = b + i * bs * (n + 1);
    lial_dpotrf(&sym_L, &bs, tile_d, &n, &ret);
    if (verbose) aux_print_matrix("B , I LOOP", b, n);

    for (j = i + 1; j < nb; j++) {
      if (verbose) printf("\n\n\n..j loop trsm, (%d, %d)\n", i, j);
      tile_j = tile_d + bs * (j - i);
      lial_dtrsm(&sym_R, &sym_L, &sym_T, &sym_N, &bsx, &bsx, &pone, tile_d, &nx, tile_j, &nx);
      if (verbose) aux_print_matrix("B , J TRSM LOOP", b, n);
    }

    for (k = i + 1; k < nb; k++) {
      if (verbose) printf("\n\n\n..k loop syrk-gemm, (%d, %d)\n", i, k);
      tile_j = tile_d + bs * (k - i); // a_syrk , same row as the tile_d
      tile_k = tile_j + bs * n * (k - i); // c_syrk , same column as the tile_j , on the diagonal
      lial_dsyrk(&sym_L, &sym_N, &bsx, &bsx, &mone, tile_j, &nx, &pone, tile_k, &nx);
      if (verbose) aux_print_matrix("B , K SYRK-GEMM LOOP", b, n);

      for (h = k + 1; h < nb; h++) {
        if (verbose) printf("\n\n\n...h loop gemm, (%d, %d, %d)\n", i, k, h);
        tile_j = tile_j; // a_gemm
        tile_k = tile_j + bs * (h - k); // b_gemm
        tile_h = tile_k + bs * n * (k - i); // c_gemm
        lial_dgemm(&sym_N, &sym_T, &bsx, &bsx, &bsx, &mone, tile_k, &nx, tile_j, &nx, &pone, tile_h, &nx);
        if (verbose) aux_print_matrix("B , H GEMM LOOP", b, n);
      }
    }
  }

  for (int i = 0; i < n * n; i++) {
    double delta = fabs(b[i] - u[i]);
    if (delta > epsilon) {
      printf("\n ERROR: i=%d, delta=%f, epsilon=%f, b=%f, u=%f\n", i, delta, epsilon, b[i], u[i]);
      ut_assert(t, false);
    }
  }

}

static void
lial_dtrsm_2_t (ut_test_t *const t)
{
  static const int n = 12;
  static const int bs = 5;
  static const int tc = 1;
  double a[] =
    {
     8.0, 0.2, 0.1, 0.0, 0.4, 0.2, 0.1, 0.7, 0.3, 0.5, 0.1, 0.2,
     0.0, 3.0, 0.0, 0.4, 0.3, 0.6, 0.1, 0.9, 0.2, 0.1, 0.5, 0.3,
     0.0, 0.0, 7.0, 0.6, 0.2, 0.5, 0.6, 0.6, 0.3, 0.1, 0.8, 0.0,
     0.0, 0.0, 0.0, 8.0, 0.1, 0.3, 0.7, 0.9, 0.0, 0.0, 0.2, 0.2,
     0.0, 0.0, 0.0, 0.0, 9.0, 0.4, 0.1, 0.1, 0.1, 0.5, 0.7, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.1, 0.9, 0.8, 0.3, 0.2, 0.5,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.3, 0.0, 0.1,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.4, 0.7, 0.6, 0.5,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.9, 0.5, 0.1,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 9.0, 0.3, 0.0,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.8,
     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0,
    };

  ut_assert(t, sizeof(a) / sizeof(double) == n * n);
  double u[n * n];
  double b[n * n];

  double epsilon;

  int ret;
  char sym_L = 'L';

  epsilon = 1.0E-15;
  ret = 0;

  for (int i = 0; i < n * n; i++)
    u[i] = a[i];

  for (int i = 0; i < n * n; i++)
    b[i] = a[i];

  lial_dpotrf(&sym_L, &n, u, &n, &ret);
  ut_assert(t, ret == 0);

  lial_dpotrf_bp(&sym_L, &n, b, &n, &ret, bs, tc);
  ut_assert(t, ret == 0);

  for (int i = 0; i < n * n; i++) {
    double delta = fabs(b[i] - u[i]);
    if (delta > epsilon) {
      printf("\n ERROR: i=%d, delta=%f, epsilon=%f, b=%f, u=%f\n", i, delta, epsilon, b[i], u[i]);
      ut_assert(t, false);
    }
  }
}

static void
lial_dtrsm_3_t (ut_test_t *const t)
{
  static const int n = 16;
  static const int n0 = 14;
  //static const int bs = 5;
  static const int tc = 1;
  double a[] =
    {
     1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     1.0, 8.0, 0.2, 0.1, 0.0, 0.4, 0.2, 0.1, 0.7, 0.3, 0.5, 0.1, 0.2, 0.1, 0.9, 1.0,
     1.0, 0.0, 3.0, 0.0, 0.4, 0.3, 0.6, 0.1, 0.9, 0.2, 0.1, 0.5, 0.3, 0.7, 0.0, 1.0,
     1.0, 0.0, 0.0, 7.0, 0.6, 0.2, 0.5, 0.6, 0.6, 0.3, 0.1, 0.8, 0.0, 0.6, 0.0, 1.0,
     1.0, 0.0, 0.0, 0.0, 8.0, 0.1, 0.3, 0.7, 0.9, 0.0, 0.0, 0.2, 0.2, 0.1, 0.5, 1.0,
     1.0, 0.0, 0.0, 0.0, 0.0, 9.0, 0.4, 0.1, 0.1, 0.1, 0.5, 0.7, 0.0, 0.0, 0.1, 1.0,
     1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.1, 0.9, 0.8, 0.3, 0.2, 0.5, 0.7, 0.2, 1.0,
     1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.3, 0.0, 0.1, 0.4, 0.0, 1.0,
     1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.4, 0.7, 0.6, 0.5, 0.6, 0.0, 1.0,
     1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, 0.9, 0.5, 0.1, 0.6, 0.0, 1.0,
     1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 9.0, 0.3, 0.0, 0.1, 0.7, 1.0,
     1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.8, 0.8, 0.1, 1.0,
     1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 5.0, 0.2, 0.6, 1.0,
     1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 3.0, 0.6, 1.0,
     1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 7.0, 1.0,
     1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
    };

  ut_assert(t, sizeof(a) / sizeof(double) == n * n);
  double u[n * n];
  double b[n * n];

  double epsilon;

  int bs;

  int ret;
  char sym_L = 'L';

  epsilon = 1.0E-15;
  ret = 0;

  for (bs = 0; bs <= n + 1; bs++) {

    for (int i = 0; i < n * n; i++)
      u[i] = a[i];

    for (int i = 0; i < n * n; i++)
      b[i] = a[i];

    lial_dpotrf(&sym_L, &n0, &u[n+1], &n, &ret);
    ut_assert(t, ret == 0);

    bool verbose = false;
    if (verbose) aux_print_matrix("A", a, n);
    if (verbose) aux_print_matrix("U", u, n);

    lial_dpotrf_bp(&sym_L, &n0, &b[n+1], &n, &ret, bs, tc);
    ut_assert(t, ret == 0);

    for (int i = 0; i < n * n; i++) {
      double delta = fabs(b[i] - u[i]);
      if (delta > epsilon) {
        printf("\n ERROR: i=%d, delta=%f, epsilon=%f, b=%f, u=%f\n", i, delta, epsilon, b[i], u[i]);
        ut_assert(t, false);
      }
    }
  }
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

  ut_suite_t *const s = ut_suite_new(&config, "linear_algebra");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dummy", lial_dummy_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_vector_magnitude", lial_vector_magnitude_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_zero_vector", lial_zero_vector_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dump_retrieve_vector", lial_dump_retrieve_vector_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dump_retrieve_matrix", lial_dump_retrieve_matrix_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_clone_vector", lial_clone_vector_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_clone_matrix", lial_clone_matrix_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dot_product_t", lial_dot_product_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dgem_1_t", lial_dgem_1_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dgem_3_t", lial_dgem_3_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_i2", lial_lu_i2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_3", lial_lu_3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_5", lial_lu_5_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_7", lial_lu_7_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_singular", lial_lu_singular_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_inv_3", lial_lu_inv_3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_01,   "lial_lu_inv_m_0_40_t", lial_lu_inv_m_0_40_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_naive_i2", lial_chol_fact_naive_i2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_naive_3", lial_chol_fact_naive_3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_naive_5", lial_chol_fact_naive_5_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_inv_naive_i2", lial_chol_inv_naive_i2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_inv_naive_m2", lial_chol_inv_naive_m2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_01,   "lial_chol_inv_naive_m_0_40_t", lial_chol_inv_naive_m_0_40_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_and_solve_lapack_i2", lial_chol_fact_and_solve_lapack_i2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_and_solve_lapack_m2", lial_chol_fact_and_solve_lapack_m2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_and_solve_lapack_m3", lial_chol_fact_and_solve_lapack_m3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_01,   "lial_chol_fact_and_solve_lapack_m_0_40", lial_chol_fact_and_solve_lapack_m_0_40_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_and_solve_lapack_shifted_0_m3", lial_chol_fact_and_solve_lapack_shifted_0_m3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_and_solve_lapack_shifted_1_m3", lial_chol_fact_and_solve_lapack_shifted_1_m3_t);

  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "lial_perf_sdf_chol_naive_1000", lial_perf_sdf_chol_naive_1000_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "lial_perf_sdf_lu_naive_1000", lial_perf_sdf_lu_naive_1000_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "lial_perf_sdf_lapack_1000", lial_perf_sdf_lapack_1000_t);

  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_1000, "lial_perf_sdf_chol_naive_edge_000", lial_perf_sdf_chol_naive_edge_000_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_100,  "lial_perf_sdf_lu_naive_edge_000", lial_perf_sdf_lu_naive_edge_000_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_100,  "lial_perf_sdf_lapack_edge_000", lial_perf_sdf_lapack_edge_000_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_1000, "lial_perf_sdf_lapack_corner_000", lial_perf_sdf_lapack_corner_000_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_1000, "lial_perf_sdf_lapack_xedge_000", lial_perf_sdf_lapack_xedge_000_t);

  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_100,  "lial_perf_sdf_lapack_edge_000_bp", lial_perf_sdf_lapack_edge_000_bp_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dtrsm_0", lial_dtrsm_0_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dtrsm_1", lial_dtrsm_1_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dtrsm_2", lial_dtrsm_2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dtrsm_3", lial_dtrsm_3_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);

  aux_teardown();

  return failure_count;
}
