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



/* --- C interface for the LAPACK routine DPOTRF ---
 * Double precision POsitive definite TRiangular Factorize
 * Interface documentation:
 *   www.netlib.org lapack dpotrf.f
 *
 * Requires the linker flag -llapack
 *
 * DPOTRF computes the Cholesky factorization of a real symmetric
 * positive definite matrix A.
 *
 * The factorization has the form
 *    A = U**T * U,  if UPLO = 'U', or
 *    A = L  * L**T,  if UPLO = 'L',
 * where U is an upper triangular matrix and L is lower triangular.
 *
 * This is the block version of the algorithm, calling Level 3 BLAS.
 *
 * Parameters
 *    [in]	UPLO
 *              UPLO is CHARACTER*1
 *              = 'U':  Upper triangle of A is stored;
 *              = 'L':  Lower triangle of A is stored.
 *
 *    [in]	N
 *              N is INTEGER
 *              The order of the matrix A.  N >= 0.
 *
 *    [in,out]	A
 *              A is DOUBLE PRECISION array, dimension (LDA,N)
 *              On entry, the symmetric matrix A.  If UPLO = 'U', the leading
 *              N-by-N upper triangular part of A contains the upper
 *              triangular part of the matrix A, and the strictly lower
 *              triangular part of A is not referenced.  If UPLO = 'L', the
 *              leading N-by-N lower triangular part of A contains the lower
 *              triangular part of the matrix A, and the strictly upper
 *              triangular part of A is not referenced.
 *
 *              On exit, if INFO = 0, the factor U or L from the Cholesky
 *              factorization A = U**T*U or A = L*L**T.
 *
 *    [in]	LDA
 *              LDA is INTEGER
 *              The leading dimension of the array A.  LDA >= max(1,N).
 *
 *    [out]	INFO
 *              INFO is INTEGER
 *              = 0:  successful exit
 *              < 0:  if INFO = -i, the i-th argument had an illegal value
 *              > 0:  if INFO = i, the leading minor of order i is not
 *                    positive definite, and the factorization could not be
 *                    completed.
 *
 */
extern void
dpotrf_ (const char *uplo,
         const int *n,
         double *a,
         const int *lda,
         int *info);

/* --- C interface for the LAPACK routine DPOTRS ---
 * Double precision POsitive definite TRiangular Solve
 * Interface documentation:
 *   www.netlib.org lapack dpotrs.f
 *
 * Requires the linker flag -llapck
 *
 * DPOTRS solves a system of linear equations A*X = B with a symmetric
 * positive definite matrix A using the Cholesky factorization
 * A = U**T*U or A = L*L**T computed by DPOTRF.
 *
 * Parameters:
 *    [in]	UPLO
 *              UPLO is CHARACTER*1
 *              = 'U':  Upper triangle of A is stored;
 *              = 'L':  Lower triangle of A is stored.
 *
 *    [in]	N
 *              N is INTEGER
 *              The order of the matrix A.  N >= 0.
 *
 *    [in]	NRHS
 *              NRHS is INTEGER
 *              The number of right hand sides, i.e., the number of columns
 *              of the matrix B.  NRHS >= 0.
 *
 *    [in]	A
 *              A is DOUBLE PRECISION array, dimension (LDA,N)
 *              The triangular factor U or L from the Cholesky factorization
 *              A = U**T*U or A = L*L**T, as computed by DPOTRF.
 *
 *    [in]	LDA
 *              LDA is INTEGER
 *              The leading dimension of the array A.  LDA >= max(1,N).
 *
 *    [in,out]	B
 *              B is DOUBLE PRECISION array, dimension (LDB,NRHS)
 *              On entry, the right hand side matrix B.
 *              On exit, the solution matrix X.
 *
 *    [in]	LDB
 *              LDB is INTEGER
 *              The leading dimension of the array B.  LDB >= max(1,N).
 *
 *    [out]	INFO
 *              INFO is INTEGER
 *              = 0:  successful exit
 *              < 0:  if INFO = -i, the i-th argument had an illegal value
 *
 */
extern void
dpotrs_ (const char* uplo,
         const int* n,
         const int* nrhs,
         const double* a,
         const int* lda,
         double* b,
         const int* ldb,
         int* info);

/*
 * Purpose:
 *
 * DGEMM performs one of the matrix-matrix operations
 *
 *    C := alpha*op( A )*op( B ) + beta*C,
 *
 * where  op( X ) is one of
 *
 *    op( X ) = X   or   op( X ) = X**T,
 *
 * alpha and beta are scalars, and A, B and C are matrices, with op( A )
 * an m by k matrix,  op( B )  a  k by n matrix and  C an m by n matrix.
 *
 * Arguments:
 *
 * [in] TRANSA
 *
 *          TRANSA is CHARACTER*1
 *           On entry, TRANSA specifies the form of op( A ) to be used in
 *           the matrix multiplication as follows:
 *
 *              TRANSA = 'N' or 'n',  op( A ) = A.
 *
 *              TRANSA = 'T' or 't',  op( A ) = A**T.
 *
 *              TRANSA = 'C' or 'c',  op( A ) = A**T.
 *
 *
 * [in] TRANSB
 *
 *          TRANSB is CHARACTER*1
 *           On entry, TRANSB specifies the form of op( B ) to be used in
 *           the matrix multiplication as follows:
 *
 *              TRANSB = 'N' or 'n',  op( B ) = B.
 *
 *              TRANSB = 'T' or 't',  op( B ) = B**T.
 *
 *              TRANSB = 'C' or 'c',  op( B ) = B**T.
 *
 *
 * [in] M
 *
 *          M is INTEGER
 *           On entry,  M  specifies  the number  of rows  of the  matrix
 *           op( A )  and of the  matrix  C.  M  must  be at least  zero.
 *
 *
 * [in] N
 *
 *          N is INTEGER
 *           On entry,  N  specifies the number  of columns of the matrix
 *           op( B ) and the number of columns of the matrix C. N must be
 *           at least zero.
 *
 *
 * [in] K
 *
 *          K is INTEGER
 *           On entry,  K  specifies  the number of columns of the matrix
 *           op( A ) and the number of rows of the matrix op( B ). K must
 *           be at least  zero.
 *
 *
 * [in] ALPHA
 *
 *          ALPHA is DOUBLE PRECISION.
 *           On entry, ALPHA specifies the scalar alpha.
 *
 *
 * [in] A
 *
 *          A is DOUBLE PRECISION array, dimension ( LDA, ka ), where ka is
 *           k  when  TRANSA = 'N' or 'n',  and is  m  otherwise.
 *           Before entry with  TRANSA = 'N' or 'n',  the leading  m by k
 *           part of the array  A  must contain the matrix  A,  otherwise
 *           the leading  k by m  part of the array  A  must contain  the
 *           matrix A.
 *
 *
 * [in] LDA
 *
 *          LDA is INTEGER
 *           On entry, LDA specifies the first dimension of A as declared
 *           in the calling (sub) program. When  TRANSA = 'N' or 'n' then
 *           LDA must be at least  max( 1, m ), otherwise  LDA must be at
 *           least  max( 1, k ).
 *
 *
 * [in] B
 *
 *          B is DOUBLE PRECISION array, dimension ( LDB, kb ), where kb is
 *           n  when  TRANSB = 'N' or 'n',  and is  k  otherwise.
 *           Before entry with  TRANSB = 'N' or 'n',  the leading  k by n
 *           part of the array  B  must contain the matrix  B,  otherwise
 *           the leading  n by k  part of the array  B  must contain  the
 *           matrix B.
 *
 *
 * [in] LDB
 *
 *          LDB is INTEGER
 *           On entry, LDB specifies the first dimension of B as declared
 *           in the calling (sub) program. When  TRANSB = 'N' or 'n' then
 *           LDB must be at least  max( 1, k ), otherwise  LDB must be at
 *           least  max( 1, n ).
 *
 *
 * [in] BETA
 *
 *          BETA is DOUBLE PRECISION.
 *           On entry,  BETA  specifies the scalar  beta.  When  BETA  is
 *           supplied as zero then C need not be set on input.
 *
 *
 * [in,out] C
 *
 *          C is DOUBLE PRECISION array, dimension ( LDC, N )
 *           Before entry, the leading m by n part of the array C must
 *           contain the matrix C, except when beta is zero, in which
 *           case C need not be set on entry.
 *           On exit, the array C is overwritten by the m by n matrix
 *           ( alpha*op( A )*op( B ) + beta*C ).
 *
 *
 * [in] LDC
 *
 *          LDC is INTEGER
 *           On entry, LDC specifies the first dimension of C as declared
 *           in the calling (sub) program. LDC must be at least
 *           max( 1, m ).
 *
 */
extern void
dgemm_ (char *transa,
        char *transb,
        int *m,
        int *n,
        int *k,
        double *alpha,
        double *a,
        int *lda,
        double *b,
        int *ldb,
        double *beta,
        double *c,
        int *ldc);

/*
 * Purpose:
 *
 *     DSYRK performs one of the symmetric rank k operations
 *
 *        C := alpha*A*A**T + beta*C,
 *
 *     or
 *
 *        C := alpha*A**T*A + beta*C,
 *
 *     where alpha and beta are scalars, C is an n by n symmetric matrix
 *     and A is an n by k matrix in the first case and a k by n matrix
 *     in the second case.
 *
 * Arguments:
 *
 *    [in] UPLO
 *
 *              UPLO is CHARACTER*1
 *               On  entry,   UPLO  specifies  whether  the  upper  or  lower
 *               triangular  part  of the  array  C  is to be  referenced  as
 *               follows:
 *
 *                  UPLO = 'U' or 'u'   Only the  upper triangular part of  C
 *                                      is to be referenced.
 *
 *                  UPLO = 'L' or 'l'   Only the  lower triangular part of  C
 *                                      is to be referenced.
 *
 *
 *    [in] TRANS
 *
 *              TRANS is CHARACTER*1
 *               On entry,  TRANS  specifies the operation to be performed as
 *               follows:
 *
 *                  TRANS = 'N' or 'n'   C := alpha*A*A**T + beta*C.
 *
 *                  TRANS = 'T' or 't'   C := alpha*A**T*A + beta*C.
 *
 *                  TRANS = 'C' or 'c'   C := alpha*A**T*A + beta*C.
 *
 *
 *    [in] N
 *
 *              N is INTEGER
 *               On entry,  N specifies the order of the matrix C.  N must be
 *               at least zero.
 *
 *
 *    [in] K
 *
 *              K is INTEGER
 *               On entry with  TRANS = 'N' or 'n',  K  specifies  the number
 *               of  columns   of  the   matrix   A,   and  on   entry   with
 *               TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
 *               of rows of the matrix  A.  K must be at least zero.
 *
 *
 *    [in] ALPHA
 *
 *              ALPHA is DOUBLE PRECISION.
 *               On entry, ALPHA specifies the scalar alpha.
 *
 *
 *    [in] A
 *
 *              A is DOUBLE PRECISION array, dimension ( LDA, ka ), where ka is
 *               k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
 *               Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
 *               part of the array  A  must contain the matrix  A,  otherwise
 *               the leading  k by n  part of the array  A  must contain  the
 *               matrix A.
 *
 *
 *    [in] LDA
 *
 *              LDA is INTEGER
 *               On entry, LDA specifies the first dimension of A as declared
 *               in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
 *               then  LDA must be at least  max( 1, n ), otherwise  LDA must
 *               be at least  max( 1, k ).
 *
 *
 *    [in] BETA
 *
 *              BETA is DOUBLE PRECISION.
 *               On entry, BETA specifies the scalar beta.
 *
 *
 *    [in,out] C
 *
 *              C is DOUBLE PRECISION array, dimension ( LDC, N )
 *               Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
 *               upper triangular part of the array C must contain the upper
 *               triangular part  of the  symmetric matrix  and the strictly
 *               lower triangular part of C is not referenced.  On exit, the
 *               upper triangular part of the array  C is overwritten by the
 *               upper triangular part of the updated matrix.
 *               Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
 *               lower triangular part of the array C must contain the lower
 *               triangular part  of the  symmetric matrix  and the strictly
 *               upper triangular part of C is not referenced.  On exit, the
 *               lower triangular part of the array  C is overwritten by the
 *               lower triangular part of the updated matrix.
 *
 *
 *    [in] LDC
 *
 *              LDC is INTEGER
 *               On entry, LDC specifies the first dimension of C as declared
 *               in  the  calling  (sub)  program.   LDC  must  be  at  least
 *               max( 1, n ).
 *
 *
 */
extern void
dsyrk_ (char *uplo,
        char *trans,
        int *n,
        int *k,
        double *alpha,
        double *a,
        int *lda,
        double *beta,
        double *c,
        int *ldc);

/*
 * Purpose:
 *
 *     DTRSM  solves one of the matrix equations
 *
 *        op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
 *
 *     where alpha is a scalar, X and B are m by n matrices, A is a unit, or
 *     non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
 *
 *        op( A ) = A   or   op( A ) = A**T.
 *
 *     The matrix X is overwritten on B.
 *
 * Arguments:
 *
 *    [in] SIDE
 *
 *              SIDE is CHARACTER*1
 *               On entry, SIDE specifies whether op( A ) appears on the left
 *               or right of X as follows:
 *
 *                  SIDE = 'L' or 'l'   op( A )*X = alpha*B.
 *
 *                  SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
 *
 *
 *    [in] UPLO
 *
 *              UPLO is CHARACTER*1
 *               On entry, UPLO specifies whether the matrix A is an upper or
 *               lower triangular matrix as follows:
 *
 *                  UPLO = 'U' or 'u'   A is an upper triangular matrix.
 *
 *                  UPLO = 'L' or 'l'   A is a lower triangular matrix.
 *
 *
 *    [in] TRANSA
 *
 *              TRANSA is CHARACTER*1
 *               On entry, TRANSA specifies the form of op( A ) to be used in
 *               the matrix multiplication as follows:
 *
 *                  TRANSA = 'N' or 'n'   op( A ) = A.
 *
 *                  TRANSA = 'T' or 't'   op( A ) = A**T.
 *
 *                  TRANSA = 'C' or 'c'   op( A ) = A**T.
 *
 *
 *    [in] DIAG
 *
 *              DIAG is CHARACTER*1
 *               On entry, DIAG specifies whether or not A is unit triangular
 *               as follows:
 *
 *                  DIAG = 'U' or 'u'   A is assumed to be unit triangular.
 *
 *                  DIAG = 'N' or 'n'   A is not assumed to be unit
 *                                      triangular.
 *
 *
 *    [in] M
 *
 *              M is INTEGER
 *               On entry, M specifies the number of rows of B. M must be at
 *               least zero.
 *
 *
 *    [in] N
 *
 *              N is INTEGER
 *               On entry, N specifies the number of columns of B.  N must be
 *               at least zero.
 *
 *
 *    [in] ALPHA
 *
 *              ALPHA is DOUBLE PRECISION.
 *               On entry,  ALPHA specifies the scalar  alpha. When  alpha is
 *               zero then  A is not referenced and  B need not be set before
 *               entry.
 *
 *
 *    [in] A
 *
 *              A is DOUBLE PRECISION array, dimension ( LDA, k ),
 *               where k is m when SIDE = 'L' or 'l'
 *               and k is n when SIDE = 'R' or 'r'.
 *               Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
 *               upper triangular part of the array  A must contain the upper
 *               triangular matrix  and the strictly lower triangular part of
 *               A is not referenced.
 *               Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
 *               lower triangular part of the array  A must contain the lower
 *               triangular matrix  and the strictly upper triangular part of
 *               A is not referenced.
 *               Note that when  DIAG = 'U' or 'u',  the diagonal elements of
 *               A  are not referenced either,  but are assumed to be  unity.
 *
 *
 *    [in] LDA
 *
 *              LDA is INTEGER
 *               On entry, LDA specifies the first dimension of A as declared
 *               in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
 *               LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
 *               then LDA must be at least max( 1, n ).
 *
 *
 *    [in,out] B
 *
 *              B is DOUBLE PRECISION array, dimension ( LDB, N )
 *               Before entry,  the leading  m by n part of the array  B must
 *               contain  the  right-hand  side  matrix  B,  and  on exit  is
 *               overwritten by the solution matrix  X.
 *
 *
 *    [in] LDB
 *
 *              LDB is INTEGER
 *               On entry, LDB specifies the first dimension of B as declared
 *               in  the  calling  (sub)  program.   LDB  must  be  at  least
 *               max( 1, m ).
 */
extern void
dtrsm_ (char *side,
        char *uplo,
        char *transa,
        char *diag,
        int *m,
        int *n,
        double *alpha,
        double *a,
        int *lda,
        double *b,
        int *ldb);

static void
lial_chol_solv_naive_ident (double **a,
                            size_t n,
                            double p[],
                            size_t ind,
                            double x[])
{
  long long int i, k;
  double sum;

  for (i = 0; i < n; i++) {
    for (sum = (i == ind) ? 1.0 : 0.0, k = i - 1; k >= 0; k--) sum -= a[i][k] * x[k];
    x[i] = sum / p[i];
  }

  for (i = n - 1; i >= 0; i--) {
    for (sum = x[i], k = i + 1; k < n; k++) sum -= a[k][i] * x[k];
    x[i] = sum / p[i];
  }
}

double
lial_vector_magnitude (double *v,
                       size_t n,
                       double *abs_min,
                       size_t *abs_min_pos,
                       double *abs_max,
                       size_t *abs_max_pos)
{
  assert(v);

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
  assert(v);
  for (size_t k = 0; k < n; k++)
    v[k] = 0.0;
}

double *
lial_allocate_vector (size_t n)
{
  double *v;

  const size_t element_size = sizeof(double);
  const size_t size = n * element_size;

  v = (double *) malloc(size);

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
lial_chol_inv_naive (double **a,
                     size_t n,
                     double p[],
                     double **z)
{
  lial_chol_fact_naive(a, n, p);
  for (size_t i = 0; i < n; i++) {
    lial_chol_solv_naive_ident(a, n, p, i, z[i]);
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
                  const char *file_name,
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
lial_retrieve_matrix (const char *file_name,
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
                   size_t nc)
{
  assert(a);
  assert(*a);

  double **b;

  b = lial_allocate_matrix(nr, nc);
  if (!b) return NULL;

  for (size_t i = 0; i < nr; i++)
    for (size_t j = 0; j < nc; j++)
      b[i][j] = a[i][j];

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
  }

  for (k = 0; k < n; k++) {
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
lial_permute_vector (double *v,
                     size_t *indx,
                     size_t n)
{
  size_t i, ix;
  double tmp;

  for (i = 0; i < n - 1; i++) {
    ix = indx[i];
    while (ix < i) ix = indx[ix];
    tmp = v[i];
    v[i] = v[ix];
    v[ix] = tmp;
  }
}

void
lial_lu_bsubst_naive (double **a,
                      size_t n,
                      size_t *indx,
                      double scale[],
                      double b[])
{
  int i, j;
  double sum;

  /* Scale cector b */
  for (i = 0; i < n; i++) {
    b[i] /= scale[i];
  }

  lial_permute_vector(b, indx, n);

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

int
lial_lu_inv_naive (double **a,
                   size_t n,
                   size_t indx[],
                   double scale[],
                   double **z)
{
  int ret;

  ret = lial_lu_decom_naive(a, n, indx, scale);
  if (ret == 0) return ret;
  for (size_t i = 0; i < n; i++) {
    lial_lu_bsubst_naive(a, n, indx, scale, z[i]);
  }
  lial_transpose_square_matrix(z, n);
  return ret;
}

void
lial_transpose_square_matrix (double **a,
                              size_t n)
{
  double tmp;

  for (size_t i = 0; i < n; i++)
    for (size_t j = i + 1; j < n; j++) {
      tmp = a[i][j];
      a[i][j] = a[j][i];
      a[j][i] = tmp;
    }
}

void
lial_dgemm (char *transa,
            char *transb,
            int *m,
            int *n,
            int *k,
            double *alpha,
            double *a,
            int *lda,
            double *b,
            int *ldb,
            double *beta,
            double *c,
            int *ldc)
{
  dgemm_(transa, transb, m, n, k, alpha, a, lda, b, ldb, beta, c, ldc);
}

void
lial_dsyrk (char *uplo,
            char *trans,
            int *n,
            int *k,
            double *alpha,
            double *a,
            int *lda,
            double *beta,
            double *c,
            int *ldc)
{
  dsyrk_(uplo, trans, n, k, alpha, a, lda, beta, c, ldc);
}

void
lial_dtrsm (char *side,
            char *uplo,
            char *transa,
            char *diag,
            int *m,
            int *n,
            double *alpha,
            double *a,
            int *lda,
            double *b,
            int *ldb)
{
  dtrsm_(side, uplo, transa, diag, m, n, alpha, a, lda, b, ldb);
}

void
lial_dpotrf (const char *uplo,
             const int *n,
             double *a,
             const int *lda,
             int *info)
{
  dpotrf_(uplo, n, a, lda, info);
}

void
lial_dpotrs (const char *uplo,
             const int *n,
             const int *nrhs,
             double *a,
             const int *lda,
             double *b,
             const int *ldb,
             int *info)
{
  dpotrs_(uplo, n, nrhs, a, lda, b, ldb, info);
}

static void
aux_print_matrix (char *name,
                  double *a,
                  int n,
                  int lda)
{
  printf("\n");
  printf("\n");
  printf("Matrix %s:\n", name);
  printf("________________________________________________________________________________________________________\n");
  printf("\n");
  for (int i = 0; i < n; i++) {
    printf(" .%2d. | ", i);
    for (int j = 0; j < n; j++) {
      int k = i * lda + j;
      printf("%6.3f, ", a[k]);
    }
    printf("\n");
  }
  printf("________________________________________________________________________________________________________\n");
}

void
lial_dpotrf_bp (const char *uplo,
                const int *n,
                double *a,
                const int *lda,
                int *info,
                const unsigned int block_size,
                const unsigned int thread_count)
{
  /*
   * BE ADVISED , THERE ARE LIMITATIONS.
   *
   * WARNING !!! If n is not a divisible by block_size without a reminder,
   *             the function does not work.
   *
   *             If uplo is not 'L' the function does not work.
   *
   *             If lda is different from n , the function does not work.
   */

  int nx, bsx, nb, nr, ldax;

  int i, j, k, h;
  double *tile_d, *tile_j, *tile_k, *tile_h;

  char sym_L = 'L';
  char sym_N = 'N';
  char sym_R = 'R';
  char sym_T = 'T';
  char sym_U = 'U';

  double pone = +1.0;
  double mone = -1.0;

  nx = *n;
  ldax = *lda;
  bsx = (block_size > 0) ? block_size : *n;

  nb = nx / bsx;
  nr = nx % bsx;

  const bool verbose = false;
  if (verbose) aux_print_matrix("A", a, *n, *lda);

  if (*uplo == 'L') {

    for (i = 0; i < nb; i++) {
      tile_d = a + i * bsx * (ldax + 1);
      lial_dpotrf(&sym_L, &bsx, tile_d, &ldax, info);
      if (verbose) aux_print_matrix("POTRF", a, *n, *lda);

      for (j = i + 1; j < nb; j++) {
        tile_j = tile_d + bsx * (j - i);
        lial_dtrsm(&sym_R, &sym_L, &sym_T, &sym_N, &bsx, &bsx, &pone, tile_d, &ldax, tile_j, &ldax);
        if (verbose) aux_print_matrix("TRSM", a, *n, *lda);
      }
      if (nr) {
        tile_j = tile_d + bsx * (nb - i);
        lial_dtrsm(&sym_R, &sym_L, &sym_T, &sym_N, &nr, &bsx, &pone, tile_d, &ldax, tile_j, &ldax);
        if (verbose) aux_print_matrix("TRSM-R", a, *n, *lda);
      }

      for (k = i + 1; k < nb; k++) {
        tile_j = tile_d + bsx * (k - i);
        tile_k = tile_j + bsx * ldax * (k - i);
        lial_dsyrk(&sym_L, &sym_N, &bsx, &bsx, &mone, tile_j, &ldax, &pone, tile_k, &ldax);
        if (verbose) aux_print_matrix("SYRK", a, *n, *lda);

        for (h = k + 1; h < nb; h++) {
          tile_j = tile_j;
          tile_k = tile_j + bsx * (h - k);
          tile_h = tile_k + bsx * ldax * (k - i);
          lial_dgemm(&sym_N, &sym_T, &bsx, &bsx, &bsx, &mone, tile_k, &ldax, tile_j, &ldax, &pone, tile_h, &ldax);
          if (verbose) aux_print_matrix("GEMM", a, *n, *lda);
        }
        if (nr) {
          tile_j = tile_j;
          tile_k = tile_j + bsx * (nb - k);
          tile_h = tile_k + bsx * ldax * (k - i);
          lial_dgemm(&sym_N, &sym_T, &nr, &bsx, &bsx, &mone, tile_k, &ldax, tile_j, &ldax, &pone, tile_h, &ldax);
          if (verbose) aux_print_matrix("GEMM-R", a, *n, *lda);
        }
      }
      if (nr) {
        tile_j = tile_d + bsx * (nb - i);
        tile_k = tile_j + bsx * ldax * (nb - i);
        lial_dsyrk(&sym_L, &sym_N, &nr, &bsx, &mone, tile_j, &ldax, &pone, tile_k, &ldax);
        if (verbose) aux_print_matrix("SYRK-R", a, *n, *lda);
      }
    }
    if (nr) {
      tile_d = a + nb * bsx * (ldax + 1);
      lial_dpotrf(&sym_L, &nr, tile_d, &ldax, info);
      if (verbose) aux_print_matrix("POTRF-R", a, *n, *lda);
    }

  } else if (*uplo == 'U') {

    for (i = 0; i < nb; i++) {
      tile_d = a + i * bsx * (ldax + 1);
      lial_dpotrf(&sym_U, &bsx, tile_d, &ldax, info);
      if (verbose) aux_print_matrix("POTRF", a, *n, *lda);

      for (j = i + 1; j < nb; j++) {
        tile_j = tile_d + bsx * ldax * (j - i);
        lial_dtrsm(&sym_L, &sym_U, &sym_T, &sym_N, &bsx, &bsx, &pone, tile_d, &ldax, tile_j, &ldax);
        if (verbose) aux_print_matrix("TRSM", a, *n, *lda);
      }
      if (nr) {
        tile_j = tile_d + bsx * ldax * (nb - i);
        lial_dtrsm(&sym_L, &sym_U, &sym_T, &sym_N, &bsx, &nr, &pone, tile_d, &ldax, tile_j, &ldax);
        if (verbose) aux_print_matrix("TRSM-R", a, *n, *lda);
      }

      for (k = i + 1; k < nb; k++) {
        tile_j = tile_d + bsx * ldax * (k - i);
        tile_k = tile_j + bsx * (k - i);
        lial_dsyrk(&sym_U, &sym_T, &bsx, &bsx, &mone, tile_j, &ldax, &pone, tile_k, &ldax);
        if (verbose) aux_print_matrix("SYRK", a, *n, *lda);

        for (h = k + 1; h < nb; h++) {
          tile_j = tile_j;
          tile_k = tile_j + bsx * ldax * (h - k);
          tile_h = tile_k + bsx * (k - i);
          lial_dgemm(&sym_T, &sym_N, &bsx, &bsx, &bsx, &mone, tile_j, &ldax, tile_k, &ldax, &pone, tile_h, &ldax);
          if (verbose) aux_print_matrix("GEMM", a, *n, *lda);
        }
        if (nr) {
          tile_j = tile_j;
          tile_k = tile_j + bsx * ldax * (nb - k);
          tile_h = tile_k + bsx * (k - i);
          lial_dgemm(&sym_T, &sym_N, &bsx, &nr, &bsx, &mone, tile_j, &ldax, tile_k, &ldax, &pone, tile_h, &ldax);
          if (verbose) aux_print_matrix("GEMM-R", a, *n, *lda);
        }
      }
      if (nr) {
        tile_j = tile_d + bsx * ldax * (nb - i);
        tile_k = tile_j + bsx * (nb - i);
        lial_dsyrk(&sym_U, &sym_T, &nr, &bsx, &mone, tile_j, &ldax, &pone, tile_k, &ldax);
        if (verbose) aux_print_matrix("SYRK-R", a, *n, *lda);
      }
    }
    if (nr) {
      tile_d = a + nb * bsx * (ldax + 1);
      lial_dpotrf(&sym_U, &nr, tile_d, &ldax, info);
      if (verbose) aux_print_matrix("POTRF-R", a, *n, *lda);
    }

  } else {
    // case not valid
    ;
  }

}

void
lial_dpotrf_bp_backup (const char *uplo,
                       const int *n,
                       double *a,
                       const int *lda,
                       int *info,
                       const unsigned int block_size,
                       const unsigned int thread_count)
{
  /*
   * BE ADVISED , THERE ARE LIMITATIONS.
   *
   * WARNING !!! If n is not a divisible by block_size without a reminder,
   *             the function does not work.
   *
   *             If uplo is not 'L' the function does not work.
   *
   *             If lda is different from n , the function does not work.
   */

  int nx, bsx, nb, nr, ldax;

  int i, j, k, h;
  double *tile_d, *tile_j, *tile_k, *tile_h;

  char sym_L = 'L';
  char sym_N = 'N';
  char sym_R = 'R';
  char sym_T = 'T';

  double pone = +1.0;
  double mone = -1.0;

  nx = *n;
  ldax = *lda;
  bsx = (block_size > 0) ? block_size : *n;

  nb = nx / bsx;
  nr = nx % bsx;

  const bool verbose = false;
  if (verbose) aux_print_matrix("A", a, *n, *lda);

  for (i = 0; i < nb; i++) {
    tile_d = a + i * bsx * (ldax + 1);
    lial_dpotrf(&sym_L, &bsx, tile_d, &ldax, info);
    if (verbose) aux_print_matrix("POTRF", a, *n, *lda);

    for (j = i + 1; j < nb; j++) {
      tile_j = tile_d + bsx * (j - i);
      lial_dtrsm(&sym_R, &sym_L, &sym_T, &sym_N, &bsx, &bsx, &pone, tile_d, &ldax, tile_j, &ldax);
      if (verbose) aux_print_matrix("TRSM", a, *n, *lda);
    }
    if (nr) {
      tile_j = tile_d + bsx * (nb - i);
      lial_dtrsm(&sym_R, &sym_L, &sym_T, &sym_N, &nr, &bsx, &pone, tile_d, &ldax, tile_j, &ldax);
      if (verbose) aux_print_matrix("TRSM-R", a, *n, *lda);
    }

    for (k = i + 1; k < nb; k++) {
      tile_j = tile_d + bsx * (k - i);
      tile_k = tile_j + bsx * ldax * (k - i);
      lial_dsyrk(&sym_L, &sym_N, &bsx, &bsx, &mone, tile_j, &ldax, &pone, tile_k, &ldax);
      if (verbose) aux_print_matrix("SYRK", a, *n, *lda);

      for (h = k + 1; h < nb; h++) {
        tile_j = tile_j;
        tile_k = tile_j + bsx * (h - k);
        tile_h = tile_k + bsx * ldax * (k - i);
        lial_dgemm(&sym_N, &sym_T, &bsx, &bsx, &bsx, &mone, tile_k, &ldax, tile_j, &ldax, &pone, tile_h, &ldax);
        if (verbose) aux_print_matrix("GEMM", a, *n, *lda);
      }
      if (nr) {
        tile_j = tile_j;
        tile_k = tile_j + bsx * (nb - k);
        tile_h = tile_k + bsx * ldax * (k - i);
        lial_dgemm(&sym_N, &sym_T, &nr, &bsx, &bsx, &mone, tile_k, &ldax, tile_j, &ldax, &pone, tile_h, &ldax);
        if (verbose) aux_print_matrix("GEMM-R", a, *n, *lda);
      }
    }
    if (nr) {
      tile_j = tile_d + bsx * (nb - i);
      tile_k = tile_j + bsx * ldax * (nb - i);
      lial_dsyrk(&sym_L, &sym_N, &nr, &bsx, &mone, tile_j, &ldax, &pone, tile_k, &ldax);
      if (verbose) aux_print_matrix("SYRK-R", a, *n, *lda);
    }
  }
  if (nr) {
    tile_d = a + nb * bsx * (ldax + 1);
    lial_dpotrf(&sym_L, &nr, tile_d, &ldax, info);
    if (verbose) aux_print_matrix("POTRF-R", a, *n, *lda);
  }

}

void
lial_chol_fact_lapack (double **a,
                       size_t nr,
                       int *ret)
{
  /* Being translated from FORTRAN, where matrices are stored by columns,
   * the U value means lower .... and vice versa ....
   */
  const char uplo = 'L';

  int info, n, lda;

  info = 0;
  if (nr > 0) {
    n = nr;
    lda = n;
    lial_dpotrf(&uplo, &n, *a, &lda, &info);
  }

  if (ret) *ret = info;
}

void
lial_chol_solv_lapack (double **a,
                       size_t nr,
                       double *b,
                       int *ret)
{
  const char uplo = 'L';

  int info, n, nrhs, lda, ldb;

  info = 0;
  if (nr > 0) {
    n = nr;
    nrhs = 1;
    lda = n;
    ldb = n;
    lial_dpotrs(&uplo, &n, &nrhs, *a, &lda, b, &ldb, &info);
  }

  if (ret) *ret = info;
}

void
lial_chol_inv_lapack (double **a,
                      size_t n,
                      double **z,
                      int *ret)
{
  lial_chol_fact_lapack(a, n, ret);
  if (ret != NULL && *ret != 0) return;
  for (size_t i = 0; i < n; i++) {
    lial_chol_solv_lapack(a, n, z[i], ret);
    if (ret != NULL && *ret != 0) return;
  }
}
