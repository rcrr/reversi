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

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <FLAME.h>

#include "prng.h"
#include "unit_test.h"
#include "linear_algebra.h"

/* ---
 * C interface for the LAPACK routine DPOTRS --
 * Double precision POsitive definite TRiangular Solve
 * Interface documentation at
 *   http://www.netlib.org/lapack/dpotrs.f
 *
 * Requires the linker flag -llapck
extern void
dpotrf_ (char *uplo,
         int *n,
         double *a,
         int *lda,
         int *info);
 */

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

static void
aux_create_sdf_matrix (uint64_t seed,
                       double lo,
                       double up,
                       size_t n,
                       double **factorized,
                       double **sdf)
{
  assert(lo <= up);

  prng_mt19937_t *r;
  double v, d;

  d = up - lo;

  r = prng_mt19937_new();
  prng_mt19937_init_by_seed(r, seed);

  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      factorized[i][j] = 0.;
      sdf[i][j] = 0.;
    }
  }

  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < i + 1; j++) {
      v = prng_mt19937_get_double_in_o0_o1(r);
      v = v * d + lo;
      factorized[i][j] = v;
    }
  }

  for (size_t i = 0; i < n; i++) {
    for (size_t j = 0; j < n; j++) {
      for (size_t k = 0; k < n; k++) {
        sdf[i][j] += factorized[i][k] * factorized[j][k];
      }
    }
  }

  prng_mt19937_free(r);
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
lial_chol_lapack_t (ut_test_t *const t)
{
  uint64_t seed;
  int n;
  double **e, **a;
  double lo, up;
  int lda;
  int info;

  char uplo = 'U';

  bool debug = false;

  seed = 1753;
  lo =   1.0;
  up =  10.0;
  n = 50;

  lda = n;
  info = 0;

  e = lial_allocate_square_matrix(n); // expected
  a = lial_allocate_square_matrix(n); // semi definite positive

  aux_create_sdf_matrix(seed, lo, up, n, e, a);

  if (debug) {
    printf("\n");
    for (size_t j = 0; j < n; j++) {
      printf("c%017zu;", j);
    }
    printf("\n");
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < n; j++) {
        printf("%+18.12f;", a[i][j]);
      }
      printf("\n");
    }
    printf("\n");
  }

  dpotrf_(&uplo, &n, *a, &lda, &info);

  ut_assert(t, info == 0);

  if (debug) {
    printf("\n");
    for (size_t i = 0; i < n; i++) {
      for (size_t j = 0; j < i + 1; j++) {
        printf("[%04zu][%04zu]: a = %+18.14f; e = %+18.14f; delta = %+18.14f\n", i, j, a[i][j], e[i][j], a[i][j] - e[i][j]);
      }
    }
  }

  ut_assert(t, 1 == 1);

  lial_free_matrix(a, n);
  lial_free_matrix(e, n);
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
  int ret_code;

  a = lial_allocate_matrix(nr, nc);

  a[0][0] =  0.0;
  a[0][1] =  0.1;
  a[1][0] =  1.0;
  a[1][1] =  1.1;
  a[2][0] =  2.0;
  a[2][1] =  2.1;

  b = lial_clone_matrix(a, nr, nc, &ret_code);
  ut_assert(t, ret_code == 0);

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
lial_chol_fact_naive_3_lapack_t (ut_test_t *const t)
{
  int  n = 3;
  double **a;

  char uplo = 'U';
  int lda = n;
  int info = 0;

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

  dpotrf_(&uplo, &n, *a, &lda, &info);

  ut_assert(t, info == 0);

  ut_assert(t, a[0][0] ==   2.0);
  ut_assert(t, a[0][1] ==  12.0);
  ut_assert(t, a[0][2] == -16.0);
  ut_assert(t, a[1][0] ==   6.0);
  ut_assert(t, a[1][1] ==   1.0);
  ut_assert(t, a[1][2] == -43.0);
  ut_assert(t, a[2][0] ==  -8.0);
  ut_assert(t, a[2][1] ==   5.0);
  ut_assert(t, a[2][2] ==   3.0);

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

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_i2", lial_lu_i2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_3", lial_lu_3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_5", lial_lu_5_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_7", lial_lu_7_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_lu_singular", lial_lu_singular_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_naive_i2", lial_chol_fact_naive_i2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_naive_3", lial_chol_fact_naive_3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_naive_5", lial_chol_fact_naive_5_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_fact_naive_3_lapack", lial_chol_fact_naive_3_lapack_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_chol_lapack", lial_chol_lapack_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);

  aux_teardown();

  return failure_count;
}
