/**
 * @file
 *
 * @brief TRSM - Linear Algebra module unit test suite.
 * @details Collects tests and helper methods for the TRSM functions.
 *
 * @par ut_trsm.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2020 Roberto Corradini. All rights reserved.
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
#include "linear_algebra.h"

/*
 * TRSM : TRiangular Solve Matrix
 *
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

/*
 * DTRSM data stracture.
 * Storage for the A and B matrices has to be allocated elsewhere.
 */
typedef struct aux_dtrsm_s {
  char side;
  char uplo;
  char transa;
  char diag;
  int m;
  int n;
  double alpha;
  double *a;
  double *b;
  double *x;
  double mkv;
  double epsilon;
  bool verbose;
  bool debug;
} aux_dtrsm_t;



/*
 * Auxiliary data.
 */

static const char lial_trsm_side_values[] = { 'L', 'l', 'R', 'r' };
static const int lial_trsm_side_size = sizeof(lial_trsm_side_values) / sizeof(char);

static const char lial_trsm_uplo_values[] = { 'U', 'u', 'L', 'l' };
const int lial_trsm_uplo_size = sizeof(lial_trsm_uplo_values) / sizeof(char);

static const char lial_trsm_transa_values[] = { 'N', 'n', 'T', 't', 'C', 'c' };
const int lial_trsm_transa_size = sizeof(lial_trsm_transa_values) / sizeof(char);

static const char lial_trsm_diag_values[] = { 'U', 'u', 'N', 'n' };
const int lial_trsm_diag_size = sizeof(lial_trsm_diag_values) / sizeof(char);



/*
 * Auxiliary functions.
 */

/*
 * Prints the matrix to standard output.
 */
static void
aux_print_matrix (char *name,
                  double *a,
                  int n,
                  int m,
                  int lda)
{
  printf("\n");
  printf("\n");
  printf("Matrix ( address = %p, n = %d, m = %d, lda = %d ) %s:\n", (void*) a, n, m, lda, name);
  printf("________________________________________________________________________________________________________\n");
  printf("\n");
  for (int i = 0; i < n; i++) {
    printf(" .%2d. | ", i);
    for (int j = 0; j < m; j++) {
      int k = i * lda + j;
      printf("%8.3f, ", a[k]);
    }
    printf("\n");
  }
  printf("________________________________________________________________________________________________________\n");
}

/*
 * Verifies that the expression: 'abs(exp[i] - val[i]) < epsilon' is TRUE for every i in [0..n).
 * The function aborts if the test fails.
 */
static void
aux_check_array_equivalence (ut_test_t *const t,
                             double *val,          // value
                             double *exp,          // expected value
                             int n,                // size of val and exp arrays
                             double epsilon)       // upper limit , not included, of the accepted deviation
{
  int i;
  double v, e, delta;

  ut_assert(t, val);
  ut_assert(t, exp);
  ut_assert(t, n >= 0);
  ut_assert(t, epsilon >= 0.0);

  for (i = 0; i < n; i++) {
    v = *(val + i);
    e = *(exp + i);
    delta = e - v;
    if (delta < 0.0) delta = -delta;
    if (delta > epsilon) {
      printf("Error detected comparing matrices: %p ; %p ; of size %d\n", (void*) val, (void*) exp, n);
      printf("  position = %d ; value = %+18.15f ; expected = %+18.15f ; delta = %+18.15f\n", i, v, e, delta);
      printf("  aborting ...\n");
      ut_assert(t, false);
    }
  }
}

/*
 * It is a basic testing utility , run by most tests after having prepared the test data.
 *
 * A matrix is given by the args->a pointer
 *
 */
static void
aux_test_dtrsm (ut_test_t *const t,
                aux_dtrsm_t *args)
{
  ut_assert(t, args);
  ut_assert(t, args->side == 'L' || args->side == 'l' || args->side == 'R' || args->side == 'r');
  ut_assert(t, args->uplo == 'U' || args->uplo == 'u' || args->uplo == 'L' || args->uplo == 'l');
  ut_assert(t, args->transa == 'N' || args->transa == 'n' || args->transa == 'T' ||
            args->transa == 't' || args->transa == 'C' || args->transa == 'c');
  ut_assert(t, args->diag == 'U' || args->diag == 'u' || args->diag == 'N' || args->diag == 'n');
  ut_assert(t, args->m >= 0);
  ut_assert(t, args->n >= 0);
  ut_assert(t, args->a);
  ut_assert(t, args->b);

  /* The matrix frame has width equal to one on all four edges. */
  static const int aux_matrix_two_side_frame_width = 2;

  if ( args->n == 0 || args->m == 0) return;

  const bool debug = args->debug;
  const bool verbose = args->verbose;
  const double epsilon = args->epsilon;

  const bool is_side_l = (args->side == 'L' || args->side == 'l') ? true : false;
  const int k = (is_side_l) ? args->m : args->n;
  int lda = k + aux_matrix_two_side_frame_width;
  int ldb = args->m + aux_matrix_two_side_frame_width;
  const size_t size_of_a = lda * (k + aux_matrix_two_side_frame_width);
  const size_t size_of_b = ldb * (args->n + aux_matrix_two_side_frame_width);

  /*
   * Pointers are organized in pairs: 'af' and 'ap' are one pair, and so on.
   * _f is the pointer to the allocated array that hosts the 'framed' matrix.
   * The frame has a width of one value on all four sides. The assigned frame value is
   * set to args->mkv.
   * Applaying this design LDA is always 'K + 2' , and LDB is 'M + 2'.
   * _p is the pointer to the first element of the matrix, and is then passed to the
   * call to DTRSM.
   * 'af' is the framed A matrix (triangular matrix).
   * 'bf' is the framed B matrix (right side matrix before the DTRSM call, result matrix after the call).
   * 'xf' is the framed X matrix (expected result matrix).
   */
  double *af, *ap, *bf, *bp, *xf, *xp;

  af = (double *) malloc(sizeof(double) * size_of_a);
  ut_assert(t, af);
  for (size_t i = 0; i < size_of_a; i++) af[i] = args->mkv;
  if (debug) aux_print_matrix("AF", af, lda, lda, lda);
  ap = af + lda + 1;
  for (size_t i = 0; i < k; i++)
    for (size_t j = 0; j < k; j++)
      af[(i+1)*lda+j+1] = args->a[i*k+j];
  if (debug) aux_print_matrix("AF", af, lda, lda, lda);

  bf = (double *) malloc(sizeof(double) * size_of_b);
  ut_assert(t, bf);
  for (size_t i = 0; i < size_of_b; i++) bf[i] = args->mkv;
  if (debug) aux_print_matrix("BF",bf, args->n + 2, ldb, ldb);
  bp = bf + ldb + 1;
  for (size_t i = 0; i < args->n; i++)
    for (size_t j = 0; j < args->m; j++)
      bf[(i+1)*ldb+j+1] = args->b[i*args->m+j];
  if (debug) aux_print_matrix("BF", bf, args->n + 2, ldb, ldb);

  xf = (double *) malloc(sizeof(double) * size_of_b);
  ut_assert(t, xf);
  for (size_t i = 0; i < size_of_b; i++) xf[i] = args->mkv;
  if (debug) aux_print_matrix("XF",xf, args->n + 2, ldb, ldb);
  xp = xf + ldb + 1;
  for (size_t i = 0; i < args->n; i++)
    for (size_t j = 0; j < args->m; j++)
      xf[(i+1)*ldb+j+1] = args->x[i*args->m+j];
  if (debug) aux_print_matrix("XF",xf, args->n + 2, ldb, ldb);

  if (verbose) {
    printf("\n");
    printf("Parameters:\n");
    printf("  SIDE      = '%c'\n", args->side);
    printf("  UPLO      = '%c'\n", args->uplo);
    printf("  TRANSA    = '%c'\n", args->transa);
    printf("  DIAG      = '%c'\n", args->diag);
    printf("  M         = %d\n", args->m);
    printf("  N         = %d\n", args->n);
    printf("  ALPHA     = %f\n", args->alpha);
    printf("  A         = %p\n", (void*) ap);
    printf("  LDA       = %d\n", lda);
    printf("  B         = %p\n", (void*) bp);
    printf("  LDB       = %d\n", ldb);
    printf("  MKV       = %f\n", args->mkv);
    printf("  K         = %d\n", k);
    printf("  SIZE_OF_A = %zu\n", size_of_a);
    printf("  SIZE_OF_B = %zu\n", size_of_b);
    printf("  EPSILON   = %e\n", args->epsilon);
    printf("\n");
    aux_print_matrix("A", ap, k, k, lda);
    aux_print_matrix("B", bp, args->n, args->m, ldb);
    aux_print_matrix("X_EXPECTED", xp, args->n, args->m, ldb);
    fflush(stdout);
  }
  lial_dtrsm(&args->side, &args->uplo, &args->transa, &args->diag,
             &args->m, &args->n, &args->alpha,
             ap, &lda, bp, &ldb);
  if (verbose) aux_print_matrix("X", bp, args->n, args->m, ldb);
  aux_check_array_equivalence(t, bf, xf, size_of_b, epsilon);

  free(xf);
  free(bf);
  free(af);
}

static void
aux_test_dtrsm_bp (ut_test_t *const t,
                   aux_dtrsm_t *args,
                   int block_size_m,
                   int block_size_n,
                   int thread_count)
{
  ut_assert(t, args);
  ut_assert(t, args->side == 'L' || args->side == 'l' || args->side == 'R' || args->side == 'r');
  ut_assert(t, args->uplo == 'U' || args->uplo == 'u' || args->uplo == 'L' || args->uplo == 'l');
  ut_assert(t, args->transa == 'N' || args->transa == 'n' || args->transa == 'T' ||
            args->transa == 't' || args->transa == 'C' || args->transa == 'c');
  ut_assert(t, args->diag == 'U' || args->diag == 'u' || args->diag == 'N' || args->diag == 'n');
  ut_assert(t, args->m >= 0);
  ut_assert(t, args->n >= 0);
  ut_assert(t, args->a);
  ut_assert(t, args->b);

  /* The matrix frame has width equal to one on all four edges. */
  static const int aux_matrix_two_side_frame_width = 2;

  if ( args->n == 0 || args->m == 0) return;

  const bool debug = args->debug;
  const bool verbose = args->verbose;
  const double epsilon = args->epsilon;

  const bool is_side_l = (args->side == 'L' || args->side == 'l') ? true : false;
  const int k = (is_side_l) ? args->m : args->n;
  int lda = k + aux_matrix_two_side_frame_width;
  int ldb = args->m + aux_matrix_two_side_frame_width;
  const size_t size_of_a = lda * (k + aux_matrix_two_side_frame_width);
  const size_t size_of_b = ldb * (args->n + aux_matrix_two_side_frame_width);

  /*
   * Pointers are organized in pairs: 'af' and 'ap' are one pair, and so on.
   * _f is the pointer to the allocated array that hosts the 'framed' matrix.
   * The frame has a width of one value on all four sides. The assigned frame value is
   * set to args->mkv.
   * Applaying this design LDA is always 'K + 2' , and LDB is 'M + 2'.
   * _p is the pointer to the first element of the matrix, and is then passed to the
   * call to DTRSM.
   * 'af' is the framed A matrix (triangular matrix).
   * 'bf' is the framed B matrix (right side matrix before the DTRSM call, result matrix after the call).
   * 'xf' is the framed X matrix (expected result matrix).
   */
  double *af, *ap, *bf, *bp, *xf, *xp;

  af = (double *) malloc(sizeof(double) * size_of_a);
  ut_assert(t, af);
  for (size_t i = 0; i < size_of_a; i++) af[i] = args->mkv;
  if (debug) aux_print_matrix("AF", af, lda, lda, lda);
  ap = af + lda + 1;
  for (size_t i = 0; i < k; i++)
    for (size_t j = 0; j < k; j++)
      af[(i+1)*lda+j+1] = args->a[i*k+j];
  if (debug) aux_print_matrix("AF", af, lda, lda, lda);

  bf = (double *) malloc(sizeof(double) * size_of_b);
  ut_assert(t, bf);
  for (size_t i = 0; i < size_of_b; i++) bf[i] = args->mkv;
  if (debug) aux_print_matrix("BF",bf, args->n + 2, ldb, ldb);
  bp = bf + ldb + 1;
  for (size_t i = 0; i < args->n; i++)
    for (size_t j = 0; j < args->m; j++)
      bf[(i+1)*ldb+j+1] = args->b[i*args->m+j];
  if (debug) aux_print_matrix("BF", bf, args->n + 2, ldb, ldb);

  xf = (double *) malloc(sizeof(double) * size_of_b);
  ut_assert(t, xf);
  for (size_t i = 0; i < size_of_b; i++) xf[i] = args->mkv;
  if (debug) aux_print_matrix("XF",xf, args->n + 2, ldb, ldb);
  xp = xf + ldb + 1;
  for (size_t i = 0; i < args->n; i++)
    for (size_t j = 0; j < args->m; j++)
      xf[(i+1)*ldb+j+1] = args->x[i*args->m+j];
  if (debug) aux_print_matrix("XF",xf, args->n + 2, ldb, ldb);

  if (verbose) {
    printf("\n");
    printf("Parameters:\n");
    printf("  SIDE     ..  = '%c'\n", args->side);
    printf("  UPLO     ..  = '%c'\n", args->uplo);
    printf("  TRANSA   ..  = '%c'\n", args->transa);
    printf("  DIAG     ..  = '%c'\n", args->diag);
    printf("  M   ..   ..  = %d\n", args->m);
    printf("  N   ..   ..  = %d\n", args->n);
    printf("  ALPHA    ..  = %f\n", args->alpha);
    printf("  A   ..   ..  = %p\n", (void*) ap);
    printf("  LDA      ..  = %d\n", lda);
    printf("  B   ..   ..  = %p\n", (void*) bp);
    printf("  LDB      ..  = %d\n", ldb);
    printf("  MKV      ..  = %f\n", args->mkv);
    printf("  K   ..   ..  = %d\n", k);
    printf("  SIZE_OF_A    = %zu\n", size_of_a);
    printf("  SIZE_OF_B    = %zu\n", size_of_b);
    printf("  EPSILON      = %e\n", args->epsilon);
    printf("  BLOCK_SIZE_M = %d\n", block_size_m);
    printf("  BLOCK_SIZE_N = %d\n", block_size_n);
    printf("  THREAD_COUNT = %d\n", thread_count);
    printf("\n");
    aux_print_matrix("A", ap, k, k, lda);
    aux_print_matrix("B", bp, args->n, args->m, ldb);
    aux_print_matrix("X_EXPECTED", xp, args->n, args->m, ldb);
    fflush(stdout);
  }
  lial_dtrsm_bp(&args->side, &args->uplo, &args->transa, &args->diag,
                &args->m, &args->n, &args->alpha,
                ap, &lda, bp, &ldb,
                block_size_m, block_size_n, thread_count);
  if (verbose) aux_print_matrix("X", bp, args->n, args->m, ldb);
  aux_check_array_equivalence(t, bf, xf, size_of_b, epsilon);

  free(xf);
  free(bf);
  free(af);
}



/*
 * Test functions.
 */
static void
lial_trsm_3x3_3x5_lunn_0_t (ut_test_t *const t)
{
  /*
   * Base configuration:
   *
   * SIDE      = 'L'
   * UPLO      = 'U'
   * TRANSA    = 'N'
   * DIAG      = 'N'
   * M         = 3
   * N         = 5
   * ALPHA     = 1.000000
   *
   * R script:
   *
   * library(MASS)
   *
   * A <- matrix(c(4, 1, 8,
   *               0, 7, 5,
   *               0, 0, 3),
   *             nrow = 3, ncol = 3, byrow = TRUE)
   *
   * B <- matrix(c(1, 3, 9, 2, 3,
   *               5, 2, 8, 5, 1,
   *               8, 1, 7, 6, 4),
   *             nrow = 3, ncol = 5, byrow = TRUE)
   *
   * X <- solve(A, B)
   *
   * Xf <- fractions(X)
   *
   * > A
   *      [,1] [,2] [,3]
   * [1,]    4    1    8
   * [2,]    0    7    5
   * [3,]    0    0    3
   *
   * > B
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]    1    3    9    2    3
   * [2,]    5    2    8    5    1
   * [3,]    8    1    7    6    4
   *
   * > X
   *           [,1]       [,2]       [,3]       [,4]       [,5]
   * [1,] -4.785714 0.07142857 -2.2857143 -3.3214286 -1.7142857
   * [2,] -1.190476 0.04761905 -0.5238095 -0.7142857 -0.8095238
   * [3,]  2.666667 0.33333333  2.3333333  2.0000000  1.3333333
   *
   * > Xf
   *        [,1]   [,2]   [,3]   [,4]   [,5]
   * [1,] -67/14   1/14  -16/7 -93/28  -12/7
   * [2,] -25/21   1/21 -11/21   -5/7 -17/21
   * [3,]    8/3    1/3    7/3      2    4/3
   *
   */

  double a[] =
    {
     4.0, 0.0, 0.0,
     1.0, 7.0, 0.0,
     8.0, 5.0, 3.0,
    };

  double b[] =
    {
     1.0, 5.0, 8.0,
     3.0, 2.0, 1.0,
     9.0, 8.0, 7.0,
     2.0, 5.0, 6.0,
     3.0, 1.0, 4.0,
    };

  double x[] =
    {
     -67.0 / 14.0 ,  -25.0 / 21.0 ,  + 8.0 /  3.0 ,
     + 1.0 / 14.0 ,  + 1.0 / 21.0 ,  + 1.0 /  3.0 ,
     -16.0 /  7.0 ,  -11.0 / 21.0 ,  + 7.0 /  3.0 ,
     -93.0 / 28.0 ,  - 5.0 /  7.0 ,  + 2.0        ,
     -12.0 /  7.0 ,  -17.0 / 21.0 ,  + 4.0 /  3.0 ,
    };

  aux_dtrsm_t args =
    {
     .side = 'L',
     .uplo = 'U',
     .transa = 'N',
     .diag = 'N',
     .m = 3,
     .n = 5,
     .alpha = 1.0,
     .a = a,
     .b = b,
     .x = x,
     .mkv = 2.0,
     .epsilon = 1.0E-15,
     .verbose = ut_run_time_is_verbose(t),
     .debug = false,
    };

  aux_test_dtrsm(t, &args);
}

static void
lial_trsm_3x3_3x5_lunn_1_t (ut_test_t *const t)
{
  /*
   * ALPHA = 2.0
   *
   * The net result of having ALPHA different from the unit ( 1.0 ) is to
   * multiply the computed result X by the value of ALPHA.
   *
   * > A
   *      [,1] [,2] [,3]
   * [1,]    4    1    8
   * [2,]    0    7    5
   * [3,]    0    0    3
   *
   * > B
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]    1    3    9    2    3
   * [2,]    5    2    8    5    1
   * [3,]    8    1    7    6    4
   *
   * > X
   *           [,1]      [,2]      [,3]      [,4]      [,5]
   * [1,] -9.571429 0.1428571 -4.571429 -6.642857 -3.428571
   * [2,] -2.380952 0.0952381 -1.047619 -1.428571 -1.619048
   * [3,]  5.333333 0.6666667  4.666667  4.000000  2.666667
   *
   * > Xf
   *        [,1]   [,2]   [,3]   [,4]   [,5]
   * [1,]  -67/7    1/7  -32/7 -93/14  -24/7
   * [2,] -50/21   2/21 -22/21  -10/7 -34/21
   * [3,]   16/3    2/3   14/3      4    8/3
   *
   */

  double a[] =
    {
     4.0, 0.0, 0.0,
     1.0, 7.0, 0.0,
     8.0, 5.0, 3.0,
    };

  double b[] =
    {
     1.0, 5.0, 8.0,
     3.0, 2.0, 1.0,
     9.0, 8.0, 7.0,
     2.0, 5.0, 6.0,
     3.0, 1.0, 4.0,
    };

  double x[] =
    {
     -67.0 /  7.0 ,  -50.0 / 21.0 ,  +16.0 /  3.0 ,
     + 1.0 /  7.0 ,  + 2.0 / 21.0 ,  + 2.0 /  3.0 ,
     -32.0 /  7.0 ,  -22.0 / 21.0 ,  +14.0 /  3.0 ,
     -93.0 / 14.0 ,  -10.0 /  7.0 ,  + 4.0        ,
     -24.0 /  7.0 ,  -34.0 / 21.0 ,  + 8.0 /  3.0 ,
    };

  aux_dtrsm_t args =
    {
     .side = 'L',
     .uplo = 'U',
     .transa = 'N',
     .diag = 'N',
     .m = 3,
     .n = 5,
     .alpha = 2.0,
     .a = a,
     .b = b,
     .x = x,
     .mkv = 2.0,
     .epsilon = 1.0E-15,
     .verbose = ut_run_time_is_verbose(t),
     .debug = false,
    };

  aux_test_dtrsm(t, &args);
}

static void
lial_trsm_3x3_3x5_lunu_t (ut_test_t *const t)
{
  /*
   * Configuration:
   *
   * SIDE      = 'L'
   * UPLO      = 'U'
   * TRANSA    = 'N'
   * DIAG      = 'U'
   * M         = 3
   * N         = 5
   * ALPHA     = 1.000000
   *
   * Instead of solving the linear system 'A * X = B' , we are solving the
   * equation 'A1 * X = B' , where the A1 matrix is derived from A by substituting
   * all the elements on the diagonal with unit values ( 1.0 ).
   *
   * > A
   *      [,1] [,2] [,3]
   * [1,]    4    1    8
   * [2,]    0    7    5
   * [3,]    0    0    3
   *
   * > A1
   *      [,1] [,2] [,3]
   * [1,]    1    1    8
   * [2,]    0    1    5
   * [3,]    0    0    1
   *
   * > B
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]    1    3    9    2    3
   * [2,]    5    2    8    5    1
   * [3,]    8    1    7    6    4
   *
   * > X
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]  -28   -2  -20  -21  -10
   * [2,]  -35   -3  -27  -25  -19
   * [3,]    8    1    7    6    4
   *
   * > Xf
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]  -28   -2  -20  -21  -10
   * [2,]  -35   -3  -27  -25  -19
   * [3,]    8    1    7    6    4
   *
   */

  double a[] =
    {
     4.0, 0.0, 0.0,
     1.0, 7.0, 0.0,
     8.0, 5.0, 3.0,
    };

  double b[] =
    {
     1.0, 5.0, 8.0,
     3.0, 2.0, 1.0,
     9.0, 8.0, 7.0,
     2.0, 5.0, 6.0,
     3.0, 1.0, 4.0,
    };

  double x[] =
    {
     -28.0,  -35.0,  + 8.0,
     - 2.0,  - 3.0,  + 1.0,
     -20.0,  -27.0,  + 7.0,
     -21.0,  -25.0,  + 6.0,
     -10.0,  -19.0,  + 4.0,
    };

  aux_dtrsm_t args =
    {
     .side = 'L',
     .uplo = 'U',
     .transa = 'N',
     .diag = 'U',
     .m = 3,
     .n = 5,
     .alpha = 1.0,
     .a = a,
     .b = b,
     .x = x,
     .mkv = 2.0,
     .epsilon = 1.0E-15,
     .verbose = ut_run_time_is_verbose(t),
     .debug = false,
    };

  aux_test_dtrsm(t, &args);
}

static void
lial_trsm_3x3_3x5_lltn_t (ut_test_t *const t)
{
  /*
   * Configuration:
   *
   * SIDE      = 'L'
   * UPLO      = 'L'
   * TRANSA    = 'T'
   * DIAG      = 'N'
   * M         = 3
   * N         = 5
   * ALPHA     = 1.000000
   *
   * This configuration obtains the same result of the Base Case by executing two
   * permutations: transposing A and flipping UPLO.
   * LOWER TRANSPOSED is equivalent to UPPER.
   *
   * > A
   *      [,1] [,2] [,3]
   * [1,]    4    0    0
   * [2,]    1    7    0
   * [3,]    8    5    3
   *
   * > AT
   *      [,1] [,2] [,3]
   * [1,]    4    1    8
   * [2,]    0    7    5
   * [3,]    0    0    3
   *
   * > B
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]    1    3    9    2    3
   * [2,]    5    2    8    5    1
   * [3,]    8    1    7    6    4
   *
   * > X
   *           [,1]       [,2]       [,3]       [,4]       [,5]
   * [1,] -4.785714 0.07142857 -2.2857143 -3.3214286 -1.7142857
   * [2,] -1.190476 0.04761905 -0.5238095 -0.7142857 -0.8095238
   * [3,]  2.666667 0.33333333  2.3333333  2.0000000  1.3333333
   *
   * > Xf
   *        [,1]   [,2]   [,3]   [,4]   [,5]
   * [1,] -67/14   1/14  -16/7 -93/28  -12/7
   * [2,] -25/21   1/21 -11/21   -5/7 -17/21
   * [3,]    8/3    1/3    7/3      2    4/3
   *
   */

  double a[] =
    {
     4.0, 1.0, 8.0,
     0.0, 7.0, 5.0,
     0.0, 0.0, 3.0,
    };

  double b[] =
    {
     1.0, 5.0, 8.0,
     3.0, 2.0, 1.0,
     9.0, 8.0, 7.0,
     2.0, 5.0, 6.0,
     3.0, 1.0, 4.0,
    };

  double x[] =
    {
     -67.0 / 14.0 ,  -25.0 / 21.0 ,  + 8.0 /  3.0 ,
     + 1.0 / 14.0 ,  + 1.0 / 21.0 ,  + 1.0 /  3.0 ,
     -16.0 /  7.0 ,  -11.0 / 21.0 ,  + 7.0 /  3.0 ,
     -93.0 / 28.0 ,  - 5.0 /  7.0 ,  + 2.0        ,
     -12.0 /  7.0 ,  -17.0 / 21.0 ,  + 4.0 /  3.0 ,
    };

  aux_dtrsm_t args =
    {
     .side = 'L',
     .uplo = 'L',
     .transa = 'T',
     .diag = 'N',
     .m = 3,
     .n = 5,
     .alpha = 1.0,
     .a = a,
     .b = b,
     .x = x,
     .mkv = 2.0,
     .epsilon = 1.0E-15,
     .verbose = ut_run_time_is_verbose(t),
     .debug = false,
    };

  aux_test_dtrsm(t, &args);
}

static void
lial_trsm_3x3_3x5_llnn_t (ut_test_t *const t)
{
  /*
   * Configuration:
   *
   * SIDE      = 'L'
   * UPLO      = 'L'
   * TRANSA    = 'N'
   * DIAG      = 'N'
   * M         = 3
   * N         = 5
   * ALPHA     = 1.000000
   *
   * A matrix is LOWER. The result is different from the base case, obviously.
   *
   * > A
   *      [,1] [,2] [,3]
   * [1,]    4    0    0
   * [2,]    1    7    0
   * [3,]    8    5    3
   *
   * > B
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]    1    3    9    2    3
   * [2,]    5    2    8    5    1
   * [3,]    8    1    7    6    4
   *
   * > X
   *           [,1]       [,2]       [,3]       [,4]        [,5]
   * [1,] 0.2500000  0.7500000  2.2500000  0.5000000  0.75000000
   * [2,] 0.6785714  0.1785714  0.8214286  0.6428571  0.03571429
   * [3,] 0.8690476 -1.9642857 -5.0357143 -0.4047619 -0.72619048
   *
   * > Xf
   *         [,1]    [,2]    [,3]    [,4]    [,5]
   * [1,]     1/4     3/4     9/4     1/2     3/4
   * [2,]   19/28    5/28   23/28    9/14    1/28
   * [3,]   73/84  -55/28 -141/28  -17/42  -61/84
   *
   */

  double a[] =
    {
     4.0, 1.0, 8.0,
     0.0, 7.0, 5.0,
     0.0, 0.0, 3.0,
    };

  double b[] =
    {
     1.0, 5.0, 8.0,
     3.0, 2.0, 1.0,
     9.0, 8.0, 7.0,
     2.0, 5.0, 6.0,
     3.0, 1.0, 4.0,
    };

  double x[] =
    {
     + 1.0 /  4.0 , + 19.0 / 28.0 , + 73.0 / 84.0 ,
     + 3.0 /  4.0 , +  5.0 / 28.0 , - 55.0 / 28.0 ,
     + 9.0 /  4.0 , + 23.0 / 28.0 , -141.0 / 28.0 ,
     + 1.0 /  2.0 , +  9.0 / 14.0 , - 17.0 / 42.0 ,
     + 3.0 /  4.0 , +  1.0 / 28.0 , - 61.0 / 84.0 ,
    };

  aux_dtrsm_t args =
    {
     .side = 'L',
     .uplo = 'L',
     .transa = 'N',
     .diag = 'N',
     .m = 3,
     .n = 5,
     .alpha = 1.0,
     .a = a,
     .b = b,
     .x = x,
     .mkv = 2.0,
     .epsilon = 1.0E-15,
     .verbose = ut_run_time_is_verbose(t),
     .debug = false,
    };

  aux_test_dtrsm(t, &args);
}

static void
lial_trsm_5x5_3x5_runn_t (ut_test_t *const t)
{
  /*
   * 'SIDE = R' changes the dimensions of the matrix A into A(N,N).
   *
   * 'X * A = B' implies that '(X * A)**T = B**T' and also 'A**T * X**T = B**T'
   * This last equivalence is computed in the R shell, note that the t() function
   * performs transposition.
   * We do also the transposed computation as a further second test.
   *
   * Configuration:
   *
   * SIDE      = 'R'
   * UPLO      = 'U'
   * TRANSA    = 'N'
   * DIAG      = 'N'
   * M         = 3
   * N         = 5
   * ALPHA     = 1.000000
   *
   * R script:
   *
   * library(MASS)
   *
   *
   * A <- matrix(c(4, 1, 8, 1, 7,
   *               0, 7, 5, 9, 2,
   *               0, 0, 3, 4, 6,
   *               0, 0, 0, 1, 2,
   *               0, 0, 0, 0, 5),
   *             nrow = 5, ncol = 5, byrow = TRUE)
   *
   * B <- matrix(c(1, 3, 9, 2, 3,
   *               5, 2, 8, 5, 1,
   *               8, 1, 7, 6, 4),
   *             nrow = 3, ncol = 5, byrow = TRUE)
   *
   * X <- t(solve(t(A), t(B)))
   *
   * Xf <- fractions(X)
   *
   * > A
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]    4    1    8    1    7
   * [2,]    0    7    5    9    2
   * [3,]    0    0    3    4    6
   * [4,]    0    0    0    1    2
   * [5,]    0    0    0    0    5
   *
   * > B
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]    1    3    9    2    3
   * [2,]    5    2    8    5    1
   * [3,]    8    1    7    6    4
   *
   * > X
   *      [,1]       [,2]       [,3]      [,4]      [,5]
   * [1,] 0.25  0.3928571  1.6785714 -8.500000  1.478571
   * [2,] 1.25  0.1071429 -0.8452381  6.166667 -3.045238
   * [3,] 2.00 -0.1428571 -2.7619048 16.333333 -5.161905
   *
   * > Xf
   *           [,1]      [,2]      [,3]      [,4]      [,5]
   * [1,]       1/4     11/28     47/28     -17/2   207/140
   * [2,]       5/4      3/28    -71/84      37/6 -1279/420
   * [3,]         2      -1/7    -58/21      49/3  -542/105
   *
   */

  double a[] =
    {
     4.0, 0.0, 0.0, 0.0, 0.0,
     1.0, 7.0, 0.0, 0.0, 0.0,
     8.0, 5.0, 3.0, 0.0, 0.0,
     1.0, 9.0, 4.0, 1.0, 0.0,
     7.0, 2.0, 6.0, 2.0, 5.0,
    };

  double b[] =
    {
     1.0, 5.0, 8.0,
     3.0, 2.0, 1.0,
     9.0, 8.0, 7.0,
     2.0, 5.0, 6.0,
     3.0, 1.0, 4.0,
    };

  double x[] =
    {
     +  1.0 /   4.0 ,  +   5.0 /   4.0 , +  2.0 /   1.0 ,
     + 11.0 /  28.0 ,  +   3.0 /  28.0 , -  1.0 /   7.0 ,
     + 47.0 /  28.0 ,  -  71.0 /  84.0 , - 58.0 /  21.0 ,
     - 17.0 /   2.0 ,  +  37.0 /   6.0 , + 49.0 /   3.0 ,
     +207.0 / 140.0 ,  -1279.0 / 420.0 , -542.0 / 105.0 ,
    };

  aux_dtrsm_t args =
    {
     .side = 'R',
     .uplo = 'U',
     .transa = 'N',
     .diag = 'N',
     .m = 3,
     .n = 5,
     .alpha = 1.0,
     .a = a,
     .b = b,
     .x = x,
     .mkv = 2.0,
     .epsilon = 1.0E-15,
     .verbose = ut_run_time_is_verbose(t),
     .debug = false,
    };

  aux_test_dtrsm(t, &args);

  /*
   * We do obtain the same result by computing the L-SIDE TRSM computation
   * on the transposed B matrix (offcourse we do obtain the transposed result too).
   * Observe that we need to set TRANSA = T and to swap the values
   * assigned to M and N arguments.
   *
   *
   * Configuration:
   *
   * SIDE      = 'L'
   * UPLO      = 'U'
   * TRANSA    = 'T'
   * DIAG      = 'N'
   * M         = 5
   * N         = 3
   * ALPHA     = 1.000000
   *
   */

  double bt[] =
    {
     1.0, 3.0, 9.0, 2.0, 3.0,
     5.0, 2.0, 8.0, 5.0, 1.0,
     8.0, 1.0, 7.0, 6.0, 4.0,
    };

  double xt[] =
    {
     +1.0 / 4.0 , +11.0 / 28.0 , +47.0 / 28.0 , -17.0 / 2.0 , + 207.0 / 140.0 ,
     +5.0 / 4.0 , + 3.0 / 28.0 , -71.0 / 84.0 , +37.0 / 6.0 , -1279.0 / 420.0 ,
     +2.0 / 1.0 , - 1.0 /  7.0 , -58.0 / 21.0 , +49.0 / 3.0 , - 542.0 / 105.0 ,
    };

  aux_dtrsm_t argst =
    {
     .side = 'L',
     .uplo = 'U',
     .transa = 'T',
     .diag = 'N',
     .m = 5,
     .n = 3,
     .alpha = 1.0,
     .a = a,
     .b = bt,
     .x = xt,
     .mkv = 2.0,
     .epsilon = 1.0E-15,
     .verbose = ut_run_time_is_verbose(t),
     .debug = false,
    };

  aux_test_dtrsm(t, &argst);
}

static void
lial_trsm_5x5_5x3_lunn_0_t (ut_test_t *const t)
{
  /*
   * Base configuration:
   *
   * SIDE      = 'L'
   * UPLO      = 'U'
   * TRANSA    = 'N'
   * DIAG      = 'N'
   * M         = 5
   * N         = 3
   * ALPHA     = 1.000000
   *
   * R script:
   *
   * library(MASS)
   *
   * A <- matrix(c(2, 1, 8, 1, 7,
   *               0, 3, 5, 2, 3,
   *               0, 0, 9, 8, 4,
   *               0, 0, 0, 1, 3,
   *               0, 0, 0, 0, 5),
   *             nrow = 5, ncol = 5, byrow = TRUE)
   *
   * B <- matrix(c(3, 3, 9,
   *               1, 2, 7,
   *               8, 6, 7,
   *               4, 2, 4,
   *               6, 3, 1),
   *             nrow = 5, ncol = 3, byrow = TRUE)
   *
   * X_AB <- solve(A, B)
   *
   * X_ABf <- fractions(X_AB)
   *
   * > A
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]    2    1    8    1    7
   * [2,]    0    3    5    2    3
   * [3,]    0    0    9    8    4
   * [4,]    0    0    0    1    3
   * [5,]    0    0    0    0    5
   *
   * > B
   *      [,1] [,2] [,3]
   * [1,]    3    3    9
   * [2,]    1    2    7
   * [3,]    8    6    7
   * [4,]    4    2    4
   * [5,]    6    3    1
   *
   * > X_AB
   *               [,1]       [,2]      [,3]
   * [1,] -2.333333e+00 -1.3703704  9.555556
   * [2,] -1.133333e+00 -0.4370370  3.755556
   * [3,] -2.960595e-16  0.2222222 -2.333333
   * [4,]  4.000000e-01  0.2000000  3.400000
   * [5,]  1.200000e+00  0.6000000  0.200000
   *
   * > X_ABf
   *         [,1]    [,2]    [,3]
   * [1,]    -7/3  -37/27    86/9
   * [2,]  -17/15 -59/135  169/45
   * [3,]       0     2/9    -7/3
   * [4,]     2/5     1/5    17/5
   * [5,]     6/5     3/5     1/5
   *
   *
   */


  double a[] =
    {
     2.0, 0.0, 0.0, 0.0, 0.0,
     1.0, 3.0, 0.0, 0.0, 0.0,
     8.0, 5.0, 9.0, 0.0, 0.0,
     1.0, 2.0, 8.0, 1.0, 0.0,
     7.0, 3.0, 4.0, 3.0, 5.0,
    };

  double b[] =
    {
     3.0, 1.0, 8.0, 4.0, 6.0,
     3.0, 2.0, 6.0, 2.0, 3.0,
     9.0, 7.0, 7.0, 4.0, 1.0,
    };

  double x[] =
    {
     - 7.0 /  3.0 ,  - 17.0 /  15.0 ,  + 0.0        , +  2.0 / 5.0 , + 6.0 / 5.0 ,
     -37.0 / 27.0 ,  - 59.0 / 135.0 ,  + 2.0 /  9.0 , +  1.0 / 5.0 , + 3.0 / 5.0 ,
     +86.0 /  9.0 ,  +169.0 /  45.0 ,  - 7.0 /  3.0 , + 17.0 / 5.0 , + 1.0 / 5.0 ,
    };

  aux_dtrsm_t args =
    {
     .side = 'L',
     .uplo = 'U',
     .transa = 'N',
     .diag = 'N',
     .m = 5,
     .n = 3,
     .alpha = 1.0,
     .a = a,
     .b = b,
     .x = x,
     .mkv = 2.0,
     .epsilon = 1.0E-14,
     .verbose = ut_run_time_is_verbose(t),
     .debug = false,
    };

  aux_test_dtrsm(t, &args);

}

static void
lial_trsm_bp_t (ut_test_t *const t)
{
  static const int extra_right_columns_of_a = 1;
  static const int extra_left_columns_of_a = 1;
  static const int extra_columns_of_a = extra_right_columns_of_a + extra_left_columns_of_a;
  static const int extra_top_rows_of_a = 1;
  static const int extra_bottom_rows_of_a = 1;
  static const int extra_rows_of_a = extra_top_rows_of_a + extra_bottom_rows_of_a;
  static const int n_0 = 7;
  static const int lda_0 = n_0 + extra_columns_of_a;
  static const int extra_right_columns_of_b = 1;
  static const int extra_left_columns_of_b = 1;
  static const int extra_columns_of_b = extra_right_columns_of_b + extra_left_columns_of_b;
  static const int extra_top_rows_of_b = 1;
  static const int extra_bottom_rows_of_b = 1;
  static const int extra_rows_of_b = extra_top_rows_of_b + extra_bottom_rows_of_b;
  static const int m_0 = 5;
  static const int ldb_0 = m_0 + extra_columns_of_b;
  static const int size_of_a = lda_0 * (n_0 + extra_rows_of_a);
  static const int size_of_b = ldb_0 * (n_0 + extra_rows_of_b);

  static const double mka = 3.0; // Marker for the A matrix.
  static const double mkb = 7.0; // Marker for the B matrix.

  static const double epsilon = 1.0E-15;

  /*
   * a_l_0 is an n x n square ( triangular ) matrix.
   *
   * Being used in the conventional BLAS3 TRSM call, it is a LOWER TRIANGULAR matrix,
   * this is the meaning of the _l_ in its name.
   * Remember that the BLAS3 API endorse the fortran convention that the matrices are
   * stored column-wise, so when passing the a_l_0 pointer to the TRSM function we have also
   * to pass 'L' ( standing for LOWER ) as the UPLO argument.
   *
   * It is constant, it never changes, this is the meaning of the final zero.
   */
  double a_l_0[] =
    {
     mka, mka, mka, mka, mka, mka, mka, mka, mka,
     mka, 2.0, 0.2, 0.1, 0.9, 0.4, 0.2, 0.3, mka,
     mka, 0.0, 9.0, 0.5, 0.4, 0.3, 0.6, 0.1, mka,
     mka, 0.0, 0.0, 3.0, 0.6, 0.2, 0.5, 0.6, mka,
     mka, 0.0, 0.0, 0.0, 2.0, 0.1, 0.3, 0.7, mka,
     mka, 0.0, 0.0, 0.0, 0.0, 5.0, 0.4, 0.1, mka,
     mka, 0.0, 0.0, 0.0, 0.0, 0.0, 7.0, 0.1, mka,
     mka, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 4.0, mka,
     mka, mka, mka, mka, mka, mka, mka, mka, mka,
    };
  ut_assert(t, sizeof(a_l_0) / sizeof(double) == size_of_a);

  /*
   * a_u_0 is the a_l_0 matrix being transposed. It si a UPPER TRIANGULAR matrix.
   */
  double a_u_0[] =
    {
     mka, mka, mka, mka, mka, mka, mka, mka, mka,
     mka, 2.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, mka,
     mka, 0.2, 9.0, 0.0, 0.0, 0.0, 0.0, 0.0, mka,
     mka, 0.1, 0.5, 3.0, 0.0, 0.0, 0.0, 0.0, mka,
     mka, 0.9, 0.4, 0.6, 2.0, 0.0, 0.0, 0.0, mka,
     mka, 0.4, 0.3, 0.2, 0.1, 5.0, 0.0, 0.0, mka,
     mka, 0.2, 0.6, 0.5, 0.3, 0.4, 7.0, 0.0, mka,
     mka, 0.3, 0.1, 0.6, 0.7, 0.1, 0.1, 4.0, mka,
     mka, mka, mka, mka, mka, mka, mka, mka, mka,
    };
  ut_assert(t, sizeof(a_u_0) / sizeof(double) == size_of_a);

  /*
   * a_l_unit_0 matrix is equal to a_l_0 with all the elements on the diagonal turned to be ones.
   */
  double a_l_unit_0[] =
    {
     mka, mka, mka, mka, mka, mka, mka, mka, mka,
     mka, 1.0, 0.2, 0.1, 0.9, 0.4, 0.2, 0.3, mka,
     mka, 0.0, 1.0, 0.5, 0.4, 0.3, 0.6, 0.1, mka,
     mka, 0.0, 0.0, 1.0, 0.6, 0.2, 0.5, 0.6, mka,
     mka, 0.0, 0.0, 0.0, 1.0, 0.1, 0.3, 0.7, mka,
     mka, 0.0, 0.0, 0.0, 0.0, 1.0, 0.4, 0.1, mka,
     mka, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.1, mka,
     mka, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, mka,
     mka, mka, mka, mka, mka, mka, mka, mka, mka,
    };
  ut_assert(t, sizeof(a_l_unit_0) / sizeof(double) == size_of_a);

  /*
   * a_u_unit_0 matrix is the a_l_unit_0 being transposed.
   */
  double a_u_unit_0[] =
    {
     mka, mka, mka, mka, mka, mka, mka, mka, mka,
     mka, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, mka,
     mka, 0.2, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, mka,
     mka, 0.1, 0.5, 1.0, 0.0, 0.0, 0.0, 0.0, mka,
     mka, 0.9, 0.4, 0.6, 1.0, 0.0, 0.0, 0.0, mka,
     mka, 0.4, 0.3, 0.2, 0.1, 1.0, 0.0, 0.0, mka,
     mka, 0.2, 0.6, 0.5, 0.3, 0.4, 1.0, 0.0, mka,
     mka, 0.3, 0.1, 0.6, 0.7, 0.1, 0.1, 1.0, mka,
     mka, mka, mka, mka, mka, mka, mka, mka, mka,
    };
  ut_assert(t, sizeof(a_u_unit_0) / sizeof(double) == size_of_a);

  double b_0[] =
    {
     mkb,  mkb,  mkb,  mkb,  mkb,  mkb,  mkb,
     mkb, +0.5, -0.2, -0.1, +0.8, +0.1,  mkb,
     mkb, +0.1, +0.6, -0.1, -0.8, +0.7,  mkb,
     mkb, -0.3, +0.5, -0.4, +0.1, +0.9,  mkb,
     mkb, +0.8, -0.3, -0.3, +0.2, +0.8,  mkb,
     mkb, +0.6, -0.1, -0.8, +0.4, +0.6,  mkb,
     mkb, +0.9, +0.8, +0.1, -0.6, +0.3,  mkb,
     mkb, -0.7, +0.2, -0.4, +0.7, +0.3,  mkb,
     mkb,  mkb,  mkb,  mkb,  mkb,  mkb,  mkb,
    };
  ut_assert(t, sizeof(b_0) / sizeof(double) == size_of_b);

  static const double lial_trsm_alpha_values[] = { 1.0, -2.0, 3.141592, 0.0 };
  static const int lial_trsm_alpha_size = sizeof(lial_trsm_alpha_values) / sizeof(double);

  static const int lial_trsm_option_combinations = lial_trsm_side_size * lial_trsm_uplo_size * lial_trsm_transa_size * lial_trsm_diag_size * lial_trsm_alpha_size;

  /*
   * Memory allocation for the matrices computed at each iteration of the testing loop.
   *
   * - b_expected : is the expected result of the TRSM call, computed by a call to the function lial_dtrsm
   * - b_recomputed_as_ax : is obtained by multipying matrices A and the B_EXPECTED, it must be equal to B
   * - b_solved : is the result to a call to lial_dtrsm_bp , them verified by comparing it to B_EXPECTED
   */
  double b_expected[size_of_b];
  double b_recomputed_as_ax[size_of_b];
  double b_solved[size_of_b];

  /*
   * b_offset and a_offset are the distance in terms of elements between the first element
   * of the A and B matrices used in the computations and the respective storage allocation.
   *
   * al0 is the pointer to the first real element of the matrix A_L_0
   * au0 refers to A_U_0
   * ...
   */
  int b_offset, a_offset;
  double *al0, *au0, *alu0, *auu0, *b0, *bexp, *brec, *bsol;

  /*
   * Main variables.
   */
  char side, uplo, transa, diag;
  int m, n, lda, ldb;
  double alpha;
  double *ap, *bp, *ap1;

  /*
   * The set of the "g" variables are used as argument to call the lial_dgemm function,
   * that is used to verify that the expected B matrix ( X ) , multipied by the A matrix
   * gives back the original B matrix ( A * X = B ).
   */
  char gtransa, gtransb;
  int gm, gn, gk, glda, gldb, gldc;
  double galpha, gbeta;
  double *gap, *gbp, *gcp;

  /* Loop variables used to enumerate the arguments of the test set. */
  int iside, iuplo, itransa, idiag, ialpha, icount;

  /* Variables used to call the function lial_dtrsm_bp. */
  int block_size_m, block_size_n, thread_count;

  /* Turns on/off (true/false) the output of the log. It is quite verbose. */
  bool verbose;

  /*
   * Assigns the "variable" values of m, n, lda, ldb.
   * Variables m, n, lda, and ldb never change, but the BLAS API requires pointers to variables.
   */
  m = m_0;
  n = n_0;
  lda = lda_0;
  ldb = ldb_0;

  /*
   * The following variables are all constant.
   * All the following pointers are used to pass the address of the first element
   * of the relative matrices to the BLAS functions.
   */
  a_offset = lda * extra_top_rows_of_a + extra_left_columns_of_a;
  b_offset = ldb * extra_top_rows_of_b + extra_left_columns_of_b;
  al0 = &a_l_0[a_offset];
  au0 = &a_u_0[a_offset];
  alu0 = &a_l_unit_0[a_offset];
  auu0 = &a_u_unit_0[a_offset];
  b0 = &b_0[b_offset];
  bexp = &b_expected[b_offset];
  brec = &b_recomputed_as_ax[b_offset];
  bsol = &b_solved[b_offset];

  verbose = ut_run_time_is_verbose(t);

  if (verbose) printf("\n#### Section I : Test data ####\n\n");

  if (verbose) {
    printf("\n");
    printf("Combinations to be tested are the combinatorial explosion of many parameters:\n");
    printf("  SIDE   : [ ");
    for (iside = 0; iside < lial_trsm_side_size; iside++) {
      side = lial_trsm_side_values[iside];
      printf("%c", side);
      if (iside != lial_trsm_side_size - 1) printf(", ");
    }
    printf(" ]\n");
    printf("  UPLO   : [ ");
    for (iuplo = 0; iuplo < lial_trsm_uplo_size; iuplo++) {
      uplo = lial_trsm_uplo_values[iuplo];
      printf("%c", uplo);
      if (iuplo != lial_trsm_uplo_size - 1) printf(", ");
    }
    printf(" ]\n");
    printf("  TRANSA : [ ");
    for (itransa = 0; itransa < lial_trsm_transa_size; itransa++) {
      transa = lial_trsm_transa_values[itransa];
      printf("%c", transa);
      if (itransa != lial_trsm_transa_size - 1) printf(", ");
    }
    printf(" ]\n");
    printf("  DIAG   : [ ");
    for (idiag = 0; idiag < lial_trsm_diag_size; idiag++) {
      diag = lial_trsm_diag_values[idiag];
      printf("%c", diag);
      if (idiag != lial_trsm_diag_size - 1) printf(", ");
    }
    printf(" ]\n");
    printf("  ALPHA  : [ ");
    for (ialpha = 0; ialpha < lial_trsm_alpha_size; ialpha++) {
      printf("%f", lial_trsm_alpha_values[ialpha]);
      if (ialpha != lial_trsm_alpha_size - 1) printf(", ");
    }
    printf(" ]\n");
    printf("Conbinations are %d.\n", lial_trsm_option_combinations);
    printf("Each parameter combination is then tested with different block sizes and thread counts:\n");
    printf("  M      : [ 0..%d ] ( the M argument is always %d, but the tailing block size is tested in the range 0..%d )\n", m, m, m);
    printf("  N      : [ 0..%d ] ( the N argument is always %d, but the tailing block size is tested in the range 0..%d )\n", n, n, n);
    printf("  TCOUNT : [ 1..%d, unlimited ]\n", 4);
    printf("The combination of tests for each parameter set is: %d\n", (m + 1) * (n + 1) * (4 + 1));
  }

  if (verbose) aux_print_matrix("A_L_0", al0, n, n, lda);
  if (verbose) aux_print_matrix("A_U_0", au0, n, n, lda);
  if (verbose) aux_print_matrix("A_L_UNIT_0", alu0, n, n, lda);
  if (verbose) aux_print_matrix("A_U_UNIT_0", auu0, n, n, lda);
  if (verbose) aux_print_matrix("B_0", b0, n, m, ldb);

  if (verbose) printf("\n#### Section II : TRSM tests ####\n\n");

  const int selected_icount = 200;

  icount = 0;

  for (iside = 0; iside < lial_trsm_side_size; iside++) {
    side = lial_trsm_side_values[iside];
    for (iuplo = 0; iuplo < lial_trsm_uplo_size; iuplo++) {
      uplo = lial_trsm_uplo_values[iuplo];
      for (itransa = 0; itransa < lial_trsm_transa_size; itransa++) {
        transa = lial_trsm_transa_values[itransa];
        for (idiag = 0; idiag < lial_trsm_diag_size; idiag++) {
          diag = lial_trsm_diag_values[idiag];
          for (ialpha = 0; ialpha < lial_trsm_alpha_size; ialpha++) {
            alpha = lial_trsm_alpha_values[ialpha];

            if (icount != selected_icount) goto end_test;

            if (verbose) printf("[ COUNT = %06d, SIDE = %c, UPLO = %c, TRANSA = %c, DIAG = %c, ALPHA = %+6.4f ]\n", icount, side, uplo, transa, diag, alpha);
            memcpy(b_expected, b_0, size_of_b * sizeof(double));
            //if (verbose) aux_print_matrix("B_EXPECTED before solution (it must be equal to B_0)", bexp, n, m, ldb);

            if (uplo == 'L' || uplo == 'l') {
              ap = al0;
              ap1 =(diag == 'N' || diag == 'n') ? al0 : alu0;
            } else {
              ap = au0;
              ap1 =(diag == 'N' || diag == 'n') ? au0 : auu0;
            }
            bp = bexp;

            /* Solves the n linear systems A * X = alpha * B. */
            if (verbose) {
              printf("\n");
              printf("Calling TRSM with arguments:\n");
              printf("  SIDE   : %c\n", side);
              printf("  UPLO   : %c\n", uplo);
              printf("  TRANSA : %c\n", transa);
              printf("  DIAG   : %c\n", diag);
              printf("  M      : %d\n", m);
              printf("  N      : %d\n", n);
              printf("  ALPHA  : %f\n", alpha);
              printf("  A      : %p, A(0,0) = %f\n", (void*) ap, *ap);
              printf("  LDA    : %d\n", lda);
              printf("  B      : %p, B(0,0) = %f\n", (void*) bp, *bp);
              printf("  LDB    : %d\n", ldb);
            }
            lial_dtrsm(&side, &uplo, &transa, &diag, &m, &n, &alpha, ap, &lda, bp, &ldb);
            if (verbose) aux_print_matrix("B_EXPECTED after solution computed by lial_dtrsm (it is the expected result)", bexp, n, m, ldb);

            /* Initializes the matrix B_RECOMPUTED_AS_AX with the value mkb as conventional marker. */
            for (double *p = b_recomputed_as_ax; p - b_recomputed_as_ax < size_of_b; p++)
              *p = mkb;
            //if (verbose) aux_print_matrix("B_RECOMPUTED_AS_AX before the matrix multiply operation", brec, n, m, ldb);
            //if (verbose) aux_print_matrix("B_RECOMPUTED_AS_AX FULL before the matrix multiply operation", b_recomputed_as_ax, n + extra_rows_of_b, m + extra_columns_of_b, ldb);

            /*
             * We execute the matrix multiply A * B_EXPECTED = C
             */
            if (side == 'R' || side == 'r') {
              gtransa = 'N';
              gtransb = transa;
              gk = n;
              gap = bp;
              glda = ldb;
              gbp = ap1;
              gldb = lda;
            } else {
              gtransa = transa;
              gtransb = 'N';
              gk = m;
              gap = ap1;
              glda = lda;
              gbp = bp;
              gldb = ldb;
            }
            gm = m;
            gn = n;
            galpha = 1.0 / alpha;
            gbeta = 0.0;
            gcp = brec;
            gldc = ldb;
            lial_dgemm(&gtransa, &gtransb, &gm, &gn, &gk, &galpha, gap, &glda, gbp, &gldb, &gbeta, gcp, &gldc);
            if (verbose) aux_print_matrix("B_RECOMPUTED_AS_AX after the matrix multiply operation", brec, n, m, ldb);
            //if (verbose) aux_print_matrix("B_RECOMPUTED_AS_AX FULL after the matrix multiply operation", b_recomputed_as_ax, n + extra_rows_of_b, m + extra_columns_of_b, ldb);
            aux_check_array_equivalence(t, b_recomputed_as_ax, b_0, size_of_b, epsilon);
            if (verbose) printf("\nCHECK OK: the expected result has been verified. Ready to start testing the OpenMP implementation based on tiles on this argument set ...\n\n");

            // --- BEGIN lial_dtrsm_bp TESTING
            if (verbose) printf("lial_dtrsm_bp testing begin ...\n");

            memcpy(b_solved, b_0, size_of_b * sizeof(double));
            //if (verbose) aux_print_matrix("B_SOLVED before solution (it must be equal to B_0)", bsol, n, m, ldb);

            block_size_m = 2;
            block_size_n = 2;
            thread_count = 1;

            lial_dtrsm_bp(&side, &uplo, &transa, &diag, &m, &n, &alpha, ap, &lda, bsol, &ldb, block_size_m, block_size_n, thread_count);
            if (verbose) aux_print_matrix("B_SOLVED after solution computed by lial_dtrsm_bp (it is the computed result)", bsol, n, m, ldb);

            aux_check_array_equivalence(t, b_solved, b_expected, size_of_b, epsilon);
            if (verbose) printf("\nCHECK OK: the matrices computed by lial_dtrsm and lial_dtrsm_bp function are equal.\n\n");

            if (verbose) printf("lial_dtrsm_bp testing ... end\n");
            // --- END lial_dtrsm_bp TESTING

          end_test:

            icount++;
          }
        }
      }
    }
  }

}

/*
 *
 */
static void
lial_dtrsm_bp_8x8_8x5_lunn_0_t (ut_test_t *const t)
{
  /*
   * Base configuration:
   *
   * SIDE      = 'L'
   * UPLO      = 'U'
   * TRANSA    = 'N'
   * DIAG      = 'N'
   * M         = 8
   * N         = 5
   * ALPHA     = 1.000000
   *
   * > A
   *      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
   * [1,]    4    1    8    3    2    6    2    4
   * [2,]    0    7    5    1    9    2    2    3
   * [3,]    0    0    3    3    1    8    5    9
   * [4,]    0    0    0    2    4    3    4    7
   * [5,]    0    0    0    0    5    9    8    5
   * [6,]    0    0    0    0    0    1    2    1
   * [7,]    0    0    0    0    0    0    6    6
   * [8,]    0    0    0    0    0    0    0    3
   *
   * > B
   *      [,1] [,2] [,3] [,4] [,5]
   * [1,]    1    3    9    2    3
   * [2,]    5    2    8    5    1
   * [3,]    8    1    7    6    4
   * [4,]    2    4    5    1    1
   * [5,]    6    1    2    4    3
   * [6,]    5    5    4    5    6
   * [7,]    1    4    9    4    8
   * [8,]    3    7    5    1    2
   *
   * > X_AB
   *             [,1]       [,2]        [,3]        [,4]        [,5]
   * [1,]  22.1845238  32.196429  16.6488095  17.6428571  21.5178571
   * [2,]  22.5952381  27.980952  15.2047619  18.3619048  20.4285714
   * [3,] -19.1666667 -25.166667 -12.1666667 -15.0000000 -17.1666667
   * [4,]   8.0000000   8.700000   4.6000000   7.2000000   7.5000000
   * [5,]  -8.6666667 -10.266667  -5.8000000  -7.2666667  -8.3333333
   * [6,]   5.6666667   6.000000   2.6666667   4.0000000   4.0000000
   * [7,]  -0.8333333  -1.666667  -0.1666667   0.3333333   0.6666667
   * [8,]   1.0000000   2.333333   1.6666667   0.3333333   0.6666667
   *
   * > X_ABf
   *          [,1]     [,2]     [,3]     [,4]     [,5]
   * [1,] 3727/168  1803/56 2797/168   247/14  1205/56
   * [2,]   949/42 2938/105 3193/210 1928/105    143/7
   * [3,]   -115/6   -151/6    -73/6      -15   -103/6
   * [4,]        8    87/10     23/5     36/5     15/2
   * [5,]    -26/3  -154/15    -29/5  -109/15    -25/3
   * [6,]     17/3        6      8/3        4        4
   * [7,]     -5/6     -5/3     -1/6      1/3      2/3
   * [8,]        1      7/3      5/3      1/3      2/3
   *
   */

  double a[] =
    {
     4.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     1.0, 7.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     8.0, 5.0, 3.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     3.0, 1.0, 3.0, 2.0, 0.0, 0.0, 0.0, 0.0,
     2.0, 9.0, 1.0, 4.0, 5.0, 0.0, 0.0, 0.0,
     6.0, 2.0, 8.0, 3.0, 9.0, 1.0, 0.0, 0.0,
     2.0, 2.0, 5.0, 4.0, 8.0, 2.0, 6.0, 0.0,
     4.0, 3.0, 9.0, 7.0, 5.0, 1.0, 6.0, 3.0,
    };

  double b[] =
    {
     1.0, 5.0, 8.0, 2.0, 6.0, 5.0, 1.0, 3.0,
     3.0, 2.0, 1.0, 4.0, 1.0, 5.0, 4.0, 7.0,
     9.0, 8.0, 7.0, 5.0, 2.0, 4.0, 9.0, 5.0,
     2.0, 5.0, 6.0, 1.0, 4.0, 5.0, 4.0, 1.0,
     3.0, 1.0, 4.0, 1.0, 3.0, 6.0, 8.0, 2.0,
    };

  double x[] =
    {
     +3727.0 / 168.0 , + 949.0 /  42.0 , -115.0 / 6.0 , + 8.0 /  1.0 , - 26.0 /  3.0 , +17.0 / 3.0 , -5.0 / 6.0 , +1.0 / 1.0 ,
     +1803.0 /  56.0 , +2938.0 / 105.0 , -151.0 / 6.0 , +87.0 / 10.0 , -154.0 / 15.0 , + 6.0 / 1.0 , -5.0 / 3.0 , +7.0 / 3.0 ,
     +2797.0 / 168.0 , +3193.0 / 210.0 , - 73.0 / 6.0 , +23.0 /  5.0 , - 29.0 /  5.0 , + 8.0 / 3.0 , -1.0 / 6.0 , +5.0 / 3.0 ,
     + 247.0 /  14.0 , +1928.0 / 105.0 , - 15.0 / 1.0 , +36.0 /  5.0 , -109.0 / 15.0 , + 4.0 / 1.0 , +1.0 / 3.0 , +1.0 / 3.0 ,
     +1205.0 /  56.0 , + 143.0 /   7.0 , -103.0 / 6.0 , +15.0 /  2.0 , - 25.0 /  3.0 , + 4.0 / 1.0 , +2.0 / 3.0 , +2.0 / 3.0 ,
    };

  aux_dtrsm_t args =
    {
     .side = 'L',
     .uplo = 'U',
     .transa = 'N',
     .diag = 'N',
     .m = 8,
     .n = 5,
     .alpha = 1.0,
     .a = a,
     .b = b,
     .x = x,
     .mkv = 2.0,
     .epsilon = 1.0E-14,
     .verbose = ut_run_time_is_verbose(t),
     .debug = false,
    };

  aux_test_dtrsm(t, &args);

  int block_size_m, block_size_n, thread_count;

  block_size_m = 3;
  block_size_n = 2;
  thread_count = 1;

  aux_test_dtrsm_bp(t, &args, block_size_m, block_size_n, thread_count);
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

  ut_suite_t *const s = ut_suite_new(&config, "trsm");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_trsm_3x3_3x5_lunn_0", lial_trsm_3x3_3x5_lunn_0_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_trsm_3x3_3x5_lunn_1", lial_trsm_3x3_3x5_lunn_1_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_trsm_3x3_3x5_lunu", lial_trsm_3x3_3x5_lunu_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_trsm_3x3_3x5_lltn", lial_trsm_3x3_3x5_lltn_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_trsm_3x3_3x5_llnn", lial_trsm_3x3_3x5_llnn_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_trsm_5x5_3x5_runn", lial_trsm_5x5_3x5_runn_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_trsm_5x5_5x3_lunn_0", lial_trsm_5x5_5x3_lunn_0_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_dtrsm_bp_8x8_8x5_lunn_0", lial_dtrsm_bp_8x8_8x5_lunn_0_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_01, "lial_trsm_bp", lial_trsm_bp_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);

  return failure_count;
}
