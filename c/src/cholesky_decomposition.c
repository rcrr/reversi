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
#include <math.h>

#include "cholesky_decomposition.h"

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
  v = (double *) malloc( n * sizeof(double));
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
  size_t i;
  double **m;

  if (nr == 0 || nc == 0 ) return NULL;

  /* Allocates pointers to rows. */
  m = (double **) malloc ( nr * sizeof(double *));
  if (!m) return NULL;

  /* Allocates the matrix. */
  m[0] = (double *) malloc ( nr * nc * sizeof(double));
  if (!m[0]) {
    free(m);
    return NULL;
  }
  for (i = 1; i < nr; i++) m[i] = m[i - 1] + nc;

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
chol_fact_naive (double **a,
                 size_t n,
                 double p[])
{
  long long int i, j, k;
  double sum;

  for (i = 0; i < n; i++) {
    for (j = i; j < n; j++) {
      for (sum = a[i][j], k = i - 1; k >= 0; k--) sum -= a[i][k] * a[j][k];
      if (i == j) {
        if (0) printf("i=%02lld, j=%02lld, sum=%0+20.18f\n",i, j, sum);
          p[i] = sqrt(sum);
        }
      else a[j][i] = sum / p[i];
    }
  }
  if (0) abort();
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
