/**
 * @file
 *
 * @brief Cholesky Decomposition module implementation.
 *
 * @par cholesky_decomposition.c
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

#include "cholesky_decomposition.h"

/*
 * One double is eight bytes, AVX2 uses 256 bit ymm registers, that needs 32 byte alignemnt.
 * Sixty-four is in sight of AVX-512 ...
 */
#define CHOL_DOUBLE_ALIGNMENT 32

#define CHOL_DOUBLE_SIZE 8

#define CHOL_DOUBLE_ELEMENT_X_SLOT 4

static uint64_t nflops = 0;

double
chol_vector_magnitude (double *v,
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
chol_zero_vector (double *v,
                  size_t n)
{
  for (size_t k = 0; k < n; k++)
    v[k] = 0.0;
}

double *
chol_allocate_vector (size_t n)
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
chol_free_vector (double *v)
{
  free(v);
}

double **
chol_allocate_square_matrix (size_t n)
{
  return chol_allocate_matrix(n, n);
}

double **
chol_allocate_matrix (size_t nr,
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
chol_free_matrix (double **m)
{
  if (m) {
    free(m[0]);
    free(m);
  }
}

void
chol_fact_zero (double **a,
                size_t n,
                double p[])
{
  long long int i, j, k;
  double sum;

  for (i = 0; i < n; i++) {
    for (j = i; j < n; j++) {
      for (sum = a[i][j], k = i - 1; k >= 0; k--) sum -= a[i][k] * a[j][k];
      if (i == j) p[i] = sqrt(sum);
      else a[j][i] = sum / p[i];
    }
  }
}

void
chol_fact_naive (double **a,
                 size_t n,
                 double p[])
{
  long long int i, j, k;
  double sum;

  for (i = 0; i < n; i++) {
    for (j = i; j < n; j++) {
      for (sum = a[i][j], k = 0; k < i; k++) sum -= a[i][k] * a[j][k];
      if (i == j) p[i] = sqrt(sum);
      else a[j][i] = sum / p[i];
    }
  }
}

void
chol_fact_v1 (double **a,
              size_t n,
              double p[])
{
  long long int i, j;
  double sum;

  printf("nflops=%zu\n", nflops);

  for (i = 0; i < n; i++) {
    for (j = i; j < n; j++) {

      long long int k;
      for (sum = a[i][j], k = 0; k < i; k++) {
        nflops++;
        sum -= a[i][k] * a[j][k];
      }

      // sum = a[i][j] - chol_dot_product_d(a[i], a[j], i);
      if (i == j) p[i] = sqrt(sum);
      else a[j][i] = sum / p[i];
    }
  }
  printf("nflops=%zu\n", nflops);
}

double
chol_dot_product_a (double *a,
                    double *b,
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
chol_dot_product_b (double *a,
                    double *b,
                    size_t n)
{
  size_t i, j, k;
  double global_sum;
  size_t n_slots, n_reminder;

  double *aa, *bb;

  aa = (double *) __builtin_assume_aligned (a, CHOL_DOUBLE_ALIGNMENT);
  bb = (double *) __builtin_assume_aligned (b, CHOL_DOUBLE_ALIGNMENT);

  double sum[] = { 0., 0., 0., 0. };

  n_slots = n / CHOL_DOUBLE_ELEMENT_X_SLOT;
  n_reminder = n % CHOL_DOUBLE_ELEMENT_X_SLOT;

  for (j = 0; j < n_slots; j++) {
    for (i = 0; i < CHOL_DOUBLE_ELEMENT_X_SLOT; i++) {
      k = j * CHOL_DOUBLE_ELEMENT_X_SLOT + i;
      sum[i] += aa[k] * bb[k];
    }
  }

  global_sum = 0.;
  for (i = 0; i < CHOL_DOUBLE_ELEMENT_X_SLOT; i++)
    global_sum += sum[i];

  for (i = 0; i < n_reminder; i++) {
    k = n_slots * CHOL_DOUBLE_ELEMENT_X_SLOT + i;
    global_sum += aa[k] * bb[k];
  }

  return global_sum;
}

static void
multiply_and_add (const double *a,
                  const double *b,
                  const double *c,
                  double *d)
{
  for (int i = 0; i < CHOL_DOUBLE_ELEMENT_X_SLOT; i++) {
    d[i] = (a[i] * b[i]) + c[i];
  }
}

double
chol_dot_product_c (double *a,
                    double *b,
                    size_t n)
{
  size_t i;
  size_t n_slots, n_reminder;

  double sum[] = { 0., 0., 0., 0. };
  double global_sum = 0.;

  n_slots = n / CHOL_DOUBLE_ELEMENT_X_SLOT;
  n_reminder = n % CHOL_DOUBLE_ELEMENT_X_SLOT;

  for (i = 0; i < n_slots; i++) {
    multiply_and_add(a, b, sum, sum);
    a += CHOL_DOUBLE_ELEMENT_X_SLOT;
    b += CHOL_DOUBLE_ELEMENT_X_SLOT;
  }

  for (i = 0; i < n_reminder; i++) {
    global_sum += *a++ * *b++;
  }

  for (i = 0; i < CHOL_DOUBLE_ELEMENT_X_SLOT; i++)
    global_sum += sum[i];

  return global_sum;
}

double
chol_dot_product_d (double *a,
                    double *b,
                    size_t n)
{
  size_t i, j, r, m;
  double sum;

  __m256d pa, pb, ps;

  /*
   * Load 256-bits (composed of 4 packed double-precision (64-bit) floating-point elements) from memory into dst. mem_addr does not need to be aligned on any particular boundary.
   * __m256d _mm256_loadu_pd (double const * mem_addr)
   *
   * Multiply packed double-precision (64-bit) floating-point elements in a and b, add the negated intermediate result to packed elements in c, and store the results in dst.
   * __m256d _mm256_fnmadd_pd (__m256d a, __m256d b, __m256d c)
   *
   * Return vector of type __m256d with all elements set to zero.
   * __m256d _mm256_setzero_pd (void)
   *
   * Horizontal sum.
   * __m256d x;
   * __m256d s = _mm256_hadd_pd(x,x);
   * return ((double*)&s)[0] + ((double*)&s)[2];
   */

  sum = 0.;
  m = n / 4;
  r = n % 4;

  ps = _mm256_setzero_pd();

  for (i = 0; i < m; i++) {

    nflops++;

    pa = _mm256_loadu_pd(a);
    pb = _mm256_loadu_pd(b);
    ps = _mm256_fnmadd_pd(pa, pb, ps);
    a += 4;
    b += 4;

  }


  ps = _mm256_hadd_pd(ps, ps);
  sum += ((double*)&ps)[0] + ((double*)&ps)[2];

  for (i = 0; i < r; i++) {
    sum += *a++ * *b++;
  }

  return sum;
}

void
chol_solv_naive (double **a,
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
chol_dump_matrix (double **a,
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
chol_retrieve_matrix (char *file_name,
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
  a = chol_allocate_matrix(lnr, lnc);
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
      chol_free_matrix(a);
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
chol_clone_matrix (double **a,
                   size_t nr,
                   size_t nc,
                   int *ret_code)
{
  assert(a);
  assert(*a);

  double **b;

  b = chol_allocate_matrix(nr, nc);
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
chol_dump_vector (double *v,
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
chol_retrieve_vector (char *file_name,
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
  v = chol_allocate_vector(ln);
  if (!v) {
    if (ret_code) *ret_code = -3;
    fclose(f);
    return NULL;
  }
  nr = fread(v, sizeof(double), ln, f);
  if (nr != ln) {
    if (ret_code) *ret_code = -4;
    fclose(f);
    chol_free_vector(v);
    return NULL;
  }
  fclose(f);
  if (ret_code) *ret_code = 0;
  *n = ln;
  return v;
}

double *
chol_clone_vector (double *v,
                   size_t n,
                   int *ret_code)
{
  assert(v);

  double *u;

  u = chol_allocate_vector(n);
  if (!u) {
    if (ret_code) *ret_code = -1;
    return NULL;
  }

  for (size_t i = 0; i < n; i++)
      u[i] = v[i];

  if (ret_code) *ret_code = 0;
  return u;
}
