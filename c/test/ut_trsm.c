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
 * TRSM  solves one of the matrix equations
 *
 *    op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
 *
 * where alpha is a scalar, X and B are m by n matrices, A is a unit, or
 * non-unit, upper or lower triangular matrix and op( A ) is one of
 *
 *    op( A ) = A   or   op( A ) = A**T.
 *
 * The matrix X is overwritten on B.
 */


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
      printf("%6.3f, ", a[k]);
    }
    printf("\n");
  }
  printf("________________________________________________________________________________________________________\n");
}

static void
aux_check_array_equivalence (ut_test_t *const t,
                             double *val,
                             double *exp,
                             int n,
                             double epsilon)
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
 * Test functions.
 */

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

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_01, "lial_trsm_bp", lial_trsm_bp_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);

  return failure_count;
}
