/**
 * @file
 *
 * @brief Linear Algebra module implementation.
 *
 * @par linear_algebra.c
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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <assert.h>

#include <immintrin.h>
#include <omp.h>

#include "time_utils.h"
#include "file_utils.h"

#include "isqrt.h"
#include "linear_algebra.h"

/*
 * One double is eight bytes, AVX2 uses 256 bit ymm registers, that needs 32 byte alignemnt.
 * Sixty-four is in sight of AVX-512 ...
 */
#define CHOL_DOUBLE_ALIGNMENT 8

#define CHOL_DOUBLE_SIZE 8

#define CHOL_DOUBLE_ELEMENT_X_SLOT CHOL_DOUBLE_ALIGNMENT / CHOL_DOUBLE_SIZE

double
lial_vector_magnitude (double *v,
                       size_t n,
                       double *abs_min,
                       size_t *abs_min_pos,
                       double *abs_max,
                       size_t *abs_max_pos)
{
  double sum, abs;

  if (n > 0) {
    if (abs_min) {
      *abs_min = fabs(v[0]);
      if (abs_min_pos)
        *abs_min_pos = 0;
    }
    if (abs_max) {
      *abs_max = fabs(v[0]);
      if (abs_max_pos)
        *abs_max_pos = 0;
    }
  }

  sum = 0.0;
  for (size_t i = 0; i < n; i++) {
    sum += v[i] * v[i];
    abs = fabs(v[i]);
    if (abs_min) if (abs < *abs_min) { *abs_min = abs; if (abs_min_pos) *abs_min_pos = i; }
    if (abs_max) if (abs > *abs_max) { *abs_max = abs; if (abs_max_pos) *abs_max_pos = i; }
  }
  return sqrt(sum);
}

void
lial_zero_vector (double *v,
                  size_t n)
{
  for (size_t k = 0; k < n; k++)
    v[k] = 0.0;
}

double *
lial_allocate_vector (size_t n)
{
  double *v;

  const size_t element_size = sizeof(double);
  const size_t required_size = n * element_size;

  size_t size;

  size = required_size / CHOL_DOUBLE_ALIGNMENT;
  if (required_size % CHOL_DOUBLE_ALIGNMENT > 0) size += 1;
  size *= CHOL_DOUBLE_ALIGNMENT;

  v = (double *) aligned_alloc(CHOL_DOUBLE_ALIGNMENT, size);

  return v;
}

void
lial_free_vector (double *v)
{
  free(v);
}

double **
lial_allocate_square_matrix (size_t n)
{
  return lial_allocate_matrix(n, n);
}

double **
lial_allocate_matrix (size_t nr,
                      size_t nc)
{
  size_t i, row_size, n_slots_per_row, n_double_per_row;
  double **m;

  if (nr == 0 || nc == 0 ) return NULL;

  /* Allocates pointers to rows. */
  m = (double **) malloc ( nr * sizeof(double *));
  if (!m) return NULL;

  /* Allocates the matrix. */
  /*
  m[0] = (double *) malloc ( nr * nc * sizeof(double));
  if (!m[0]) {
    free(m);
    return NULL;
  }
  for (i = 1; i < nr; i++) m[i] = m[i - 1] + nc;
  */


  const size_t row_required_size = nc * CHOL_DOUBLE_SIZE;

  n_slots_per_row = row_required_size / CHOL_DOUBLE_ALIGNMENT;
  if (row_required_size % CHOL_DOUBLE_ALIGNMENT > 0) n_slots_per_row += 1;
  n_double_per_row = n_slots_per_row * CHOL_DOUBLE_ELEMENT_X_SLOT;
  row_size = n_double_per_row * CHOL_DOUBLE_SIZE;

  m[0] = (double *) aligned_alloc(CHOL_DOUBLE_ALIGNMENT, nr * row_size);

  for (i = 1; i < nr; i++) m[i] = m[i - 1] + n_double_per_row;


  /* Return pointer to array of pointers to rows. */
  return m;
}

void
lial_free_matrix (double **m,
                  size_t nr)
{
  double *p, *p0;

  if (m) {
    if (nr > 0) {
      p0 = m[0];
      for (size_t i = 0; i < nr; i++) {
        p = m[i];
        if (p < p0) p0 = p;
      }
      free(p0);
    }
    free(m);
  }
}

void
lial_chol_fact_naive (double **a,
                      size_t n,
                      double p[])
{
  size_t i, j, k;
  double sum;

  for (i = 0; i < n; i++) {
    for (sum = a[i][i], k = 0; k < i; k++) {
      sum -= a[i][k] * a[i][k];
    }
    p[i] = sqrt(sum);
    for (j = i + 1; j < n; j++) {
      sum = a[i][j] - lial_dot_product(a[i], a[j], i);
      a[j][i] = sum / p[i];
    }
  }
}

void
lial_chol_fact_omp (double **a,
                    size_t n,
                    double p[])
{
  size_t i, j, k;
  double sum;

  for (i = 0; i < n; i++) {
    for (sum = a[i][i], k = 0; k < i; k++) {
      sum -= a[i][k] * a[i][k];
    }
    p[i] = sqrt(sum);
#pragma omp parallel for default(none) private(j, sum) shared(a, p, n, i)
    for (j = i + 1; j < n; j++) {
      sum = a[i][j] - lial_dot_product_avx(a[i], a[j], i);
      a[j][i] = sum / p[i];
    }
  }
}

void
lial_chol_fact_omp_tc (double **a,
                       size_t n,
                       double p[],
                       size_t thread_count)
{
  size_t i, j, k;
  double sum;

  timespec_t time_0, time_1, time_diff;
  FILE *fp;

  fp = fopen("lial_chol_fact_omp_tc.log", "w");

  for (i = 0; i < n; i++) {

    /* Starts the stop-watch. */
    clock_gettime(CLOCK_REALTIME, &time_0);

    for (sum = a[i][i], k = 0; k < i; k++) {
      sum -= a[i][k] * a[i][k];
    }
    p[i] = sqrt(sum);

    /* Stops the stop-watch. */
    clock_gettime(CLOCK_REALTIME, &time_1);

    /* Computes the time taken, and updates the process cpu time. */
    timespec_diff(&time_diff, &time_0, &time_1);
    fprintf(fp, "i = %6zu,   diag - [%6lld.%9ld]\n", i, (long long) timespec_get_sec(&time_diff), timespec_get_nsec(&time_diff));

    /* Starts the stop-watch. */
    clock_gettime(CLOCK_REALTIME, &time_0);

#pragma omp parallel for num_threads(thread_count) default(none) private(j, sum) shared(a, p, n, i)
    for (j = i + 1; j < n; j++) {
      sum = a[i][j] - lial_dot_product_avx(a[i], a[j], i);
      a[j][i] = sum / p[i];
    }

    /* Stops the stop-watch. */
    clock_gettime(CLOCK_REALTIME, &time_1);

    /* Computes the time taken, and updates the process cpu time. */
    timespec_diff(&time_diff, &time_0, &time_1);
    fprintf(fp, "i = %6zu, column - [%6lld.%9ld]\n", i, (long long) timespec_get_sec(&time_diff), timespec_get_nsec(&time_diff));

  }
  fclose(fp);
}

double
lial_dot_product (const double * restrict a,
                  const double * restrict b,
                  size_t n)
{
  size_t i;
  double sum;

  sum = 0.;
  for (i = 0; i < n; i++) {
    sum += a[i] * b[i];
  }

  return sum;
}

double
lial_dot_product_avx (const double * restrict a,
                      const double * restrict b,
                      size_t n)
{
  /*
   * Load 256-bits (composed of 4 packed double-precision (64-bit) floating-point elements) from memory into dst. mem_addr does not need to be aligned on any particular boundary.
   * __m256d _mm256_loadu_pd (double const * mem_addr)
   *
   * Multiply packed double-precision (64-bit) floating-point elements in a and b, add the negated intermediate result to packed elements in c, and store the results in dst.
   * __m256d _mm256_fmadd_pd (__m256d a, __m256d b, __m256d c)
   *
   * Return vector of type __m256d with all elements set to zero.
   * __m256d _mm256_setzero_pd (void)
   *
   * Horizontal sum.
   * __m256d x;
   * __m256d s = _mm256_hadd_pd(x,x);
   * return ((double*)&s)[0] + ((double*)&s)[2];
   */

  size_t i, r, m;
  double sum;

  __m256d pa, pb, ps;

  m = n / 4;
  r = n % 4;

  ps = _mm256_setzero_pd();

  for (i = 0; i < m; i++) {
    pa = _mm256_loadu_pd(a);
    pb = _mm256_loadu_pd(b);
    ps = _mm256_fmadd_pd(pa, pb, ps);
    a += 4;
    b += 4;
  }

  ps = _mm256_hadd_pd(ps, ps);
  sum = ((double*)&ps)[0] + ((double*)&ps)[2];

  for (i = 0; i < r; i++) {
    sum += *a++ * *b++;
  }

  return sum;
}

void
lial_chol_solv_naive (double **a,
                      size_t n,
                      double p[],
                      double b[],
                      double x[])
{
  long long int i, k;
  double sum;

  for (i = 0; i < n; i++) {
    for (sum = b[i], k = i - 1; k >= 0; k--) sum -= a[i][k] * x[k];
    x[i] = sum / p[i];
  }

  for (i = n - 1; i >= 0; i--) {
    for (sum = x[i], k = i + 1; k < n; k++) sum -= a[k][i] * x[k];
    x[i] = sum / p[i];
  }
}

void
lial_dump_matrix (double **a,
                  size_t nr,
                  size_t nc,
                  char *file_name,
                  int *ret_code)
{
  assert(a);
  assert(*a);
  assert(file_name);

  FILE *f;
  size_t n;

  f = fopen(file_name, "w");
  if (!f) {
    if (ret_code) *ret_code = -1;
    return;
  }
  n = fwrite(&nr, sizeof(size_t), 1, f);
  if (n != 1) {
    if (ret_code) *ret_code = -2;
    fclose(f);
    return;
  }
  n = fwrite(&nc, sizeof(size_t), 1, f);
  if (n != 1) {
    if (ret_code) *ret_code = -3;
    fclose(f);
    return;
  }
  for (size_t i = 0; i < nr; i++) {
    n = fwrite(a[i], sizeof(double), nc, f);
    if (n != nc) {
      if (ret_code) *ret_code = -4;
      fclose(f);
      return;
    }
  }
  fclose(f);

  if (ret_code) *ret_code = 0;
}

double **
lial_retrieve_matrix (char *file_name,
                      size_t *nr,
                      size_t *nc,
                      int *ret_code)
{
  assert(file_name);
  assert(nr);
  assert(nc);

  FILE *f;
  size_t lnr, lnc, n;
  double **a;

  f = fopen(file_name, "r");
  if (!f) {
    if (ret_code) *ret_code = -1;
    return NULL;
  }
  n = fread(&lnr, sizeof(size_t), 1, f);
  if (n != 1) {
    if (ret_code) *ret_code = -2;
    fclose(f);
    return NULL;
  }
  n = fread(&lnc, sizeof(size_t), 1, f);
  if (n != 1) {
    if (ret_code) *ret_code = -3;
    fclose(f);
    return NULL;
  }
  a = lial_allocate_matrix(lnr, lnc);
  if (!a) {
    if (ret_code) *ret_code = -4;
    fclose(f);
    return NULL;
  }
  for (size_t i = 0; i < lnr; i++) {
    n = fread(a[i], sizeof(double), lnc, f);
    if (n != lnc) {
      if (ret_code) *ret_code = -5;
      fclose(f);
      lial_free_matrix(a, lnr);
      return NULL;
    }
  }
  fclose(f);
  if (ret_code) *ret_code = 0;
  *nr = lnr;
  *nc = lnc;
  return a;
}

double **
lial_clone_matrix (double **a,
                   size_t nr,
                   size_t nc,
                   int *ret_code)
{
  assert(a);
  assert(*a);

  double **b;

  b = lial_allocate_matrix(nr, nc);
  if (!b) {
    if (ret_code) *ret_code = -1;
    return NULL;
  }

  for (size_t i = 0; i < nr; i++)
    for (size_t j = 0; j < nc; j++)
      b[i][j] = a[i][j];

  if (ret_code) *ret_code = 0;
  return b;
}

void
lial_dump_vector (double *v,
                  size_t n,
                  char *file_name,
                  int *ret_code)
{
  assert(v);
  assert(file_name);

  FILE *f;
  size_t nw;

  f = fopen(file_name, "w");
  if (!f) {
    if (ret_code) *ret_code = -1;
    return;
  }
  nw = fwrite(&n, sizeof(size_t), 1, f);
  if (nw != 1) {
    if (ret_code) *ret_code = -2;
    fclose(f);
    return;
  }
  nw = fwrite(v, sizeof(double), n, f);
  if (nw != n) {
    if (ret_code) *ret_code = -3;
    fclose(f);
    return;
  }
  fclose(f);

  if (ret_code) *ret_code = 0;
}

double *
lial_retrieve_vector (char *file_name,
                      size_t *n,
                      int *ret_code)
{
  assert(file_name);
  assert(n);

  FILE *f;
  size_t ln, nr;
  double *v;

  f = fopen(file_name, "r");
  if (!f) {
    if (ret_code) *ret_code = -1;
    return NULL;
  }
  nr = fread(&ln, sizeof(size_t), 1, f);
  if (nr != 1) {
    if (ret_code) *ret_code = -2;
    fclose(f);
    return NULL;
  }
  v = lial_allocate_vector(ln);
  if (!v) {
    if (ret_code) *ret_code = -3;
    fclose(f);
    return NULL;
  }
  nr = fread(v, sizeof(double), ln, f);
  if (nr != ln) {
    if (ret_code) *ret_code = -4;
    fclose(f);
    lial_free_vector(v);
    return NULL;
  }
  fclose(f);
  if (ret_code) *ret_code = 0;
  *n = ln;
  return v;
}

double *
lial_clone_vector (double *v,
                   size_t n,
                   int *ret_code)
{
  assert(v);

  double *u;

  u = lial_allocate_vector(n);
  if (!u) {
    if (ret_code) *ret_code = -1;
    return NULL;
  }

  for (size_t i = 0; i < n; i++)
      u[i] = v[i];

  if (ret_code) *ret_code = 0;
  return u;
}

int
lial_lu_decom_naive (double **a,
                     size_t n,
                     size_t *indx,
                     double *scale)
{
  int i, j, k, h;

  double max, v;
  int imax;
  double *p;
  size_t q;

  /* Initialize the permutation vector. */
  for (k = 0; k < n; k++) {
    indx[k] = k;
  }

  /* Rescale the rows of the matrix. */
  for (i = 0; i < n; i++) {
    max = 0.0;
    for (j = 0; j < n; j++) {
      v = fabs(a[i][j]);
      if (v > max) max = v;
    }
    if (max == 0.0) return 0;
    scale[i] = max;
    for (j = 0; j < n; j++) {
      a[i][j] /= max;
    }
  }

  for (k = 0; k < n; k++) {

    /* Find the pivot element. */
    max = 0.0;
    imax = k;
    for (i = k; i < n; i++) {
      v = fabs(a[i][k]);
      if (max < v) {
        max = v;
        imax = i;
      }
    }
    if (max == 0.0) return 0;

    /* Apply the row permutation. */
    p = a[k];
    a[k] = a[imax];
    a[imax] = p;
    q = indx[k];
    indx[k] = indx[imax];
    indx[imax] = q;

    for (i = k; i < n; i++) {
      for (h = 0; h < k; h++) {
        a[i][k] = a[i][k] - (a[i][h] * a[h][k]);
      }
    }
    for (j = k + 1; j < n; j++){
      for (h = 0; h < k; h++) {
        a[k][j] = a[k][j] - (a[k][h] * a[h][j]);
      }
      a[k][j] = a[k][j] / a[k][k];
    }
  }

  return 1;
}

void
lial_lu_bsubst_naive (double **a,
                      size_t n,
                      size_t *indx,
                      double scale[],
                      double b[])
{
  int i, j;
  double sum, tmp;

  /* Scale cector b */
  for (i = 0; i < n; i++) {
    b[i] /= scale[i];
  }

  /* Swap b */
  for (i = 0; i < n; i++) {
    j = indx[i];
    if (i > j) {
      tmp = b[i];
      b[i] = b[j];
      b[j] = tmp;
    }
  }

  /* Perform the forward substitution using the LU matrix.
   */
  for (i = 0; i < n; i++) {
    sum = b[i];
    for (j = (i - 1); j >= 0 ; j--) {
      sum -= a[i][j] * b[j];
    }
    b[i] = sum / a[i][i];
  }

  /* Perform the backward substitution using the LU matrix.
   */
  for (i = (n - 1); i >= 0; i--) {
    for (j = (i + 1); j < n ; j++) {
      b[i] -= a[i][j] * b[j];
    }
  }

}
