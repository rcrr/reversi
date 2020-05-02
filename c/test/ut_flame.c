/**
 * @file
 *
 * @brief FLAME and BLIS libraries module unit test suite.
 * @details Collects tests and helper methods for the FLAME and BLIS libraries.
 *
 * @par ut_flame.c
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
#include <errno.h>
#include <string.h>
#include <math.h>

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "prng.h"
#include "unit_test.h"
#include "linear_algebra.h"



/*
 * Auxiliary functions.
 */

/* Used to check/debug the function FLASH_Obj_create_hier_copy_of_flat(). */
static void
aux_flame_create_hier_copy_of_flat_t (ut_test_t *const t,
                                      const char test_data_file_name[])
{
  int ret;
  size_t nr, nc;
  double **a;

  bool verbose;

  FLA_Error fla_ret;
  FLA_Obj A, B;
  dim_t bs;

  verbose = (ut_run_time_is_verbose(t)) ? true : false;

  /*
   * STEP (0) - Retrieve from storage the A matrix
   */
  a = lial_retrieve_matrix(test_data_file_name, &nr, &nc, &ret);
  if (ret != 0 || !a || (nr != nc)) {
    printf("Unable to read properly matrix a from file: %s\n", test_data_file_name);
    ut_assert(t, false);
  }  else {
    if (verbose) printf("Reading from storage SDF matrix of size : %zu, from file \"%s\"\n", nr, test_data_file_name);
  }


  FLA_Init();

  fla_ret = FLA_Obj_create_without_buffer(FLA_DOUBLE, nr, nr, &A);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  fla_ret = FLA_Obj_attach_buffer(*a, nr, 1, &A);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  if (verbose) printf("\n");
  for (size_t i = 32; i <= 128; i++) {
    if (verbose) printf("block_size = %zu\n", i);
    bs = i;
    fla_ret = FLASH_Obj_create_hier_copy_of_flat(A, 1, &bs, &B);
    ut_assert(t, fla_ret == FLA_SUCCESS);

    /* --- Cholesky factorization , solution , ... --- */

    FLASH_Obj_free(&B);
  }
  if (verbose) printf("\n");

  fla_ret = FLA_Obj_free_without_buffer(&A);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  FLA_Finalize();

  lial_free_matrix(a, nr);
}

static void
aux_perf_sdf_flame_t (ut_test_t *const t,
                      const char test_data_file_name[])
{
  size_t nr, nc;

  timespec_t delta_time, start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  double **a;

  double max_delta, normalized_max_row_delta_modulus, mean, standard_deviation;
  double d2, cdm;
  size_t max_delta_i, max_delta_j, normalized_max_row_delta_modulus_i;

  FLA_Error fla_ret;
  FLA_Obj A, Z, C;

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /*
   * STEP (0) - Retrieve from storage the A matrix
   */
  a = lial_retrieve_matrix(test_data_file_name, &nr, &nc, &ret);
  if (ret != 0 || !a || (nr != nc)) {
    printf("\nUnable to read properly matrix a from file: %s\n", test_data_file_name);
    ut_assert(t, false);
  } else {
    //n = nr;
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test delta_time. */
  ret = timespec_diff(&delta_time, &start_time, &end_time);
  (void) ret; ut_assert(t, ret == 0);

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; ut_assert(t, ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Reading from storage SDF matrix:                            [%6lld.%9ld][%6lld.%9ld]\n",
            (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time),
            (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  FLA_Init();

  fla_ret = FLA_Obj_create_without_buffer(FLA_DOUBLE, nr, nr, &A);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  fla_ret = FLA_Obj_attach_buffer(*a, nr, 1, &A);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  fla_ret = FLA_Obj_create(FLA_DOUBLE, nr, nr, nr, 1, &Z);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  fla_ret = FLA_Set_to_identity(Z);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  /* Create C as a copy of A. */
  fla_ret = FLA_Obj_create_copy_of(FLA_NO_TRANSPOSE, A, &C);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  //FLA_Obj_show("--- FLAME --- A --- begin ---", A, "%10.3f", "--- FLAME --- A --- end ---");
  //FLA_Obj_show("--- FLAME --- Z --- begin ---", Z, "%10.3f", "--- FLAME --- Z --- end ---");

  /* --- */

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /*
   * STEP (1) - Factorize applying the Cholesky algorithm the A matrix
   */
  fla_ret = FLA_Chol(FLA_UPPER_TRIANGULAR, A);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test delta_time. */
  ret = timespec_diff(&delta_time, &start_time, &end_time);
  (void) ret; ut_assert(t, ret == 0);

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; ut_assert(t, ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Factorizing SDF A matrix of size %8zu:                  [%6lld.%9ld][%6lld.%9ld]\n",
            nr, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time),
            (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  //FLA_Obj_show("--- FLAME --- A --- begin ---", A, "%10.3f", "--- FLAME --- A --- end ---");

  /* --- */

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /*
   * STEP (2) - Computes the inverse matrix Z.
   */
  fla_ret = FLA_Chol_solve(FLA_UPPER_TRIANGULAR, A, Z, Z);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test delta_time. */
  ret = timespec_diff(&delta_time, &start_time, &end_time);
  (void) ret; assert(ret == 0);

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Computing the inverse matrix:                               [%6lld.%9ld][%6lld.%9ld]\n",
            (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time),
            (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  //FLA_Obj_show("--- FLAME --- Z --- begin ---", Z, "%10.3f", "--- FLAME --- Z --- end ---");

  /* --- */

  FLA_Set_to_identity(A);

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /*
   * STEP (3) - Multiplies the original SDF matrix and its inverse.
   *
   * C is a copy of the original A matrix.
   * Z is the inverse of A
   *
   * A := C * Z - I
   */
  FLA_Gemm(FLA_NO_TRANSPOSE, FLA_NO_TRANSPOSE, FLA_ONE, C, Z, FLA_MINUS_ONE, A);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test delta_time. */
  ret = timespec_diff(&delta_time, &start_time, &end_time);
  (void) ret; assert(ret == 0);

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Multiplying the original random SDF matrix and its inverse: [%6lld.%9ld][%6lld.%9ld]\n",
            (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time),
            (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  /* --- */

  /*
   * The standard deviation of the population of elements of the matrix ((A * inv(A)) - I) is computed assuming that the mean is zero.
   *
   * Note: there are no defined functions in the FLAME API to compute the mean , or the standard deviation of columns/rows ...
   * This code is somehow a bit hugly , but it doesn't deserve the effort to be better organized with functions that applies to the FLA_Obj.
   *
   * *** A particular care has to be taken in case the buffer of the A matrix is allocate by FLAME with alignment. ***
   * *** The loop in this case has to consider the leading stride.                                                 ***
   */
  max_delta = 0.0;
  max_delta_i = 0;
  max_delta_j = 0;
  normalized_max_row_delta_modulus = 0.0;
  normalized_max_row_delta_modulus_i = 0;
  mean = 0.0;
  standard_deviation = 0.0;
  for (size_t i = 0; i < nr; i++) {
    cdm = 0.0;
    for (size_t j = 0; j < nr; j++) {
      mean += a[i][j];
      d2 = a[i][j] * a[i][j];
      if (d2 > max_delta) { max_delta = d2; max_delta_i = i; max_delta_j = j; }
      cdm += d2;
    }
    standard_deviation += cdm;
    if (cdm > normalized_max_row_delta_modulus) { normalized_max_row_delta_modulus = cdm; normalized_max_row_delta_modulus_i = i; }
  }
  mean = mean / ((double) nr * (double) nr);
  standard_deviation = sqrt(standard_deviation / ((double) nr * (double) nr));
  normalized_max_row_delta_modulus = sqrt(normalized_max_row_delta_modulus / (double) nr) ;
  max_delta = sqrt(max_delta);

  if (ut_run_time_is_verbose(t)) {
    fprintf(stdout, "  Error matrix (deviations from the identity matrix) KPI:\n");
    fprintf(stdout, "    mean                             = %28.18f\n", mean);
    fprintf(stdout, "    standard_deviation               = %28.18f\n", standard_deviation);
    fprintf(stdout, "    normalized_max_row_delta_modulus = %28.18f, row #%6zu\n", normalized_max_row_delta_modulus, normalized_max_row_delta_modulus_i);
    fprintf(stdout, "    max_delta                        = %28.18f, i #%6zu, j #%6zu\n", max_delta, max_delta_i, max_delta_j);
  }

  fla_ret = FLA_Obj_free(&C);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  fla_ret = FLA_Obj_free(&Z);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  fla_ret = FLA_Obj_free_without_buffer(&A);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  FLA_Finalize();

  lial_free_matrix(a, nr);
}

static void
aux_perf_sdf_flash_t (ut_test_t *const t,
                      const char test_data_file_name[])
{
  size_t nr, nc;

  bool verbose, debug;

  timespec_t delta_time, start_time, end_time, cpu_time, time_0, time_1;
  int ret;

  double **a;

  double max_delta, normalized_max_row_delta_modulus, mean, standard_deviation;
  double d2, cdm;
  size_t max_delta_i, max_delta_j, normalized_max_row_delta_modulus_i;

  FLA_Error fla_ret;
  FLA_Obj A, Z, C, IDENTITY;
  FLA_Bool is_parallel;
  dim_t bs;

  int tc;

  debug = false;

  verbose = (ut_run_time_is_verbose(t)) ? true : false;

  /* Matrix block size. */
  bs = 1024;

  /* Thread count. */
  tc = 8;

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /*
   * STEP (0) - Retrieve from storage the A matrix
   */
  a = lial_retrieve_matrix(test_data_file_name, &nr, &nc, &ret);
  if (ret != 0 || !a || (nr != nc)) {
    printf("\nUnable to read properly matrix a from file: %s\n", test_data_file_name);
    ut_assert(t, false);
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test delta_time. */
  ret = timespec_diff(&delta_time, &start_time, &end_time);
  (void) ret; ut_assert(t, ret == 0);

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; ut_assert(t, ret == 0);

  if (verbose) {
    fprintf(stdout, "  Reading from storage SDF matrix:                            [%6lld.%9ld][%6lld.%9ld]\n",
            (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time),
            (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  FLA_Init();

  fla_ret = FLASH_Queue_enable();
  ut_assert(t, fla_ret == FLA_SUCCESS);

  is_parallel = FLASH_Queue_get_enabled();
  if (debug) printf("is_parallel = %d\n", is_parallel);

  FLASH_Queue_set_num_threads(tc);

  /* Creates the A matrix. */
  fla_ret = FLASH_Obj_create_without_buffer(FLA_DOUBLE, nr, nr, 1, &bs, &A);
  ut_assert(t, fla_ret == FLA_SUCCESS);
  fla_ret = FLASH_Obj_attach_buffer(*a, nr, 1, &A);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  /* Create flat Object IDENTITY , equal to the Identity matrix. */
  fla_ret = FLA_Obj_create(FLA_DOUBLE, nr, nr, nr, 1, &IDENTITY);
  ut_assert(t, fla_ret == FLA_SUCCESS);
  fla_ret = FLA_Set_to_identity(IDENTITY);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  /* Set Z to be the Identity matrix. Unfortunatly there is no function FLASH_Set_to_identity(). */
  fla_ret = FLASH_Obj_create(FLA_DOUBLE, nr, nr, 1, &bs, &Z);
  ut_assert(t, fla_ret == FLA_SUCCESS);
  FLASH_Obj_hierarchify(IDENTITY, Z);

  /* Create C as a copy of A. */
  fla_ret = FLASH_Obj_create_copy_of(FLA_NO_TRANSPOSE, A, &C);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  //FLASH_Obj_show("--- FLASH --- A --- begin ---", A, "%10.3f", "--- FLASH --- A --- end ---");
  //FLASH_Obj_show("--- FLASH --- Z --- begin ---", Z, "%10.3f", "--- FLASH --- Z --- end ---");

  /* --- */

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /*
   * STEP (1) - Factorize applying the Cholesky algorithm the A matrix
   */
  fla_ret = FLASH_Chol(FLA_UPPER_TRIANGULAR, A);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test delta_time. */
  ret = timespec_diff(&delta_time, &start_time, &end_time);
  (void) ret; ut_assert(t, ret == 0);

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; ut_assert(t, ret == 0);

  if (verbose) {
    fprintf(stdout, "  Factorizing SDF A matrix of size %8zu:                  [%6lld.%9ld][%6lld.%9ld]\n",
            nr, (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time),
            (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  //FLASH_Obj_show("--- FLASH --- A --- begin ---", A, "%10.3f", "--- FLASH --- A --- end ---");

  /* --- */

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /*
   * STEP (2) - Computes the inverse matrix Z.
   */
  fla_ret = FLASH_Chol_solve(FLA_UPPER_TRIANGULAR, A, Z, Z);
  ut_assert(t, fla_ret == FLA_SUCCESS);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test delta_time. */
  ret = timespec_diff(&delta_time, &start_time, &end_time);
  (void) ret; assert(ret == 0);

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (verbose) {
    fprintf(stdout, "  Computing the inverse matrix:                               [%6lld.%9ld][%6lld.%9ld]\n",
            (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time),
            (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  //FLASH_Obj_show("--- FLASH --- Z --- begin ---", Z, "%10.3f", "--- FLASH --- Z --- end ---");

  /* --- */

  FLASH_Obj_hierarchify(IDENTITY, A);

  /* Sets the test start time. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  (void) start_time;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /*
   * STEP (3) - Multiplies the original SDF matrix and its inverse.
   *
   * C is a copy of the original A matrix.
   * Z is the inverse of A
   *
   * A := C * Z - I
   */
  FLASH_Gemm(FLA_NO_TRANSPOSE, FLA_NO_TRANSPOSE, FLA_ONE, C, Z, FLA_MINUS_ONE, A);

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Sets the test end time. */
  clock_gettime(CLOCK_REALTIME, &end_time);
  (void) end_time;

  /* Computes the time taken, and updates the test delta_time. */
  ret = timespec_diff(&delta_time, &start_time, &end_time);
  (void) ret; assert(ret == 0);

  /* Computes the time taken, and updates the test cpu_time. */
  ret = timespec_diff(&cpu_time, &time_0, &time_1);
  (void) ret; assert(ret == 0);

  if (verbose) {
    fprintf(stdout, "  Multiplying the original random SDF matrix and its inverse: [%6lld.%9ld][%6lld.%9ld]\n",
            (long long) timespec_get_sec(&cpu_time), timespec_get_nsec(&cpu_time),
            (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  //FLASH_Obj_show("--- FLASH --- A --- begin ---", A, "%10.3f", "--- FLASH --- A --- end ---");

  /* --- */

  /*
   * The standard deviation of the population of elements of the matrix ((A * inv(A)) - I) is computed assuming that the mean is zero.
   *
   * Note: there are no defined functions in the FLAME API to compute the mean , or the standard deviation of columns/rows ...
   * This code is somehow a bit hugly , but it doesn't deserve the effort to be better organized with functions that applies to the FLA_Obj.
   *
   * *** A particular care has to be taken in case the buffer of the A matrix is allocate by FLAME with alignment. ***
   * *** The loop in this case has to consider the leading stride.                                                 ***
   */
  max_delta = 0.0;
  max_delta_i = 0;
  max_delta_j = 0;
  normalized_max_row_delta_modulus = 0.0;
  normalized_max_row_delta_modulus_i = 0;
  mean = 0.0;
  standard_deviation = 0.0;
  for (size_t i = 0; i < nr; i++) {
    cdm = 0.0;
    for (size_t j = 0; j < nr; j++) {
      mean += a[i][j];
      d2 = a[i][j] * a[i][j];
      if (d2 > max_delta) { max_delta = d2; max_delta_i = i; max_delta_j = j; }
      cdm += d2;
    }
    standard_deviation += cdm;
    if (cdm > normalized_max_row_delta_modulus) { normalized_max_row_delta_modulus = cdm; normalized_max_row_delta_modulus_i = i; }
  }
  mean = mean / ((double) nr * (double) nr);
  standard_deviation = sqrt(standard_deviation / ((double) nr * (double) nr));
  normalized_max_row_delta_modulus = sqrt(normalized_max_row_delta_modulus / (double) nr) ;
  max_delta = sqrt(max_delta);

  if (verbose) {
    fprintf(stdout, "  Error matrix (deviations from the identity matrix) KPI:\n");
    fprintf(stdout, "    mean                             = %28.18f\n", mean);
    fprintf(stdout, "    standard_deviation               = %28.18f\n", standard_deviation);
    fprintf(stdout, "    normalized_max_row_delta_modulus = %28.18f, row #%6zu\n", normalized_max_row_delta_modulus, normalized_max_row_delta_modulus_i);
    fprintf(stdout, "    max_delta                        = %28.18f, i #%6zu, j #%6zu\n", max_delta, max_delta_i, max_delta_j);
  }

  FLASH_Obj_free(&IDENTITY);
  FLASH_Obj_free(&C);
  FLASH_Obj_free(&Z);
  FLASH_Obj_free_without_buffer(&A);

  FLA_Finalize();

  lial_free_matrix(a, nr);
}



/*
 * Test functions.
 */

static void
flame_dummy_t (ut_test_t *const t)
{
  ut_assert(t, true);
}

static void
flame_basic_t (ut_test_t *const t)
{
  FLA_Obj A, B, X;
  FLA_Error ret_status;

  const double epsilon = 1.0E-12;

  double buffer_a[] =
    {
     + 4.0, +12.0, -16.0,
     + 0.0, +37.0, -43.0,
     + 0.0, + 0.0, +98.0,
    };

  int m    = 3;
  int rs_a = 3;
  int cs_a = 1;
  int rhsn = 2;
  int rs_b = 2;
  int cs_b = 1;

  double buffer_b[] =
    {
     +2., +7.,
     -1., +2.,
     +5., -3.,
    };

  double buffer_x[] =
    {
     0., 0.,
     0., 0.,
     0., 0.,
    };

  double expected_a[] =
    {
     + 2.0, + 6.0, - 8.0,
     + 0.0, + 1.0, + 5.0,
     + 0.0, + 0.0, + 3.0,
    };

  double expected_x[] =
    {
     + 737.0 / 6.0 , + 3745.0 / 12.0 ,
     - 101.0 / 3.0 , -  257.0 /  3.0 ,
     +  16.0 / 3.0 , +   40.0 /  3.0 ,
    };

  FLA_Init();

  ret_status = FLA_Obj_create_without_buffer(FLA_DOUBLE, m, m, &A);
  ut_assert(t, ret_status == FLA_SUCCESS);

  ret_status = FLA_Obj_attach_buffer(buffer_a, rs_a, cs_a, &A);
  ut_assert(t, ret_status == FLA_SUCCESS);

  ret_status = FLA_Obj_create_without_buffer(FLA_DOUBLE, m, rhsn, &B);
  ut_assert(t, ret_status == FLA_SUCCESS);

  ret_status = FLA_Obj_attach_buffer(buffer_b, rs_b, cs_b, &B);
  ut_assert(t, ret_status == FLA_SUCCESS);

  ret_status = FLA_Obj_create_without_buffer(FLA_DOUBLE, m, rhsn, &X);
  ut_assert(t, ret_status == FLA_SUCCESS);

  ret_status = FLA_Obj_attach_buffer(buffer_x, rs_b, cs_b, &X);
  ut_assert(t, ret_status == FLA_SUCCESS);

  FLA_Chol(FLA_UPPER_TRIANGULAR, A);

  for (int i = 0; i < 9; i++)
    ut_assert(t, buffer_a[i] == expected_a[i]);

  ret_status = FLA_Chol_solve(FLA_UPPER_TRIANGULAR, A, B, X);
  ut_assert(t, ret_status == FLA_SUCCESS);

  for (int i = 0; i < 6; i++)
    ut_assert(t, fabs(buffer_x[i] - expected_x[i]) < epsilon);

  FLA_Obj_free_without_buffer(&X);
  FLA_Obj_free_without_buffer(&B);
  FLA_Obj_free_without_buffer(&A);

  FLA_Finalize();
}

static void
blis_basic_t (ut_test_t *const t)
{
  num_t dt;
  dim_t m, n, k;
  inc_t rs, cs;

  obj_t a, b, c;
  obj_t *alpha;
  obj_t *beta;

  bool verbose;

  verbose = ut_run_time_is_verbose(t);
  if (verbose) printf("\n");

  // Create some matrix operands to work with.
  dt = BLIS_DOUBLE;
  m = 4; n = 5; k = 3; rs = 0; cs = 0;
  bli_obj_create(dt, m, n, rs, cs, &c);
  bli_obj_create(dt, m, k, rs, cs, &a);
  bli_obj_create(dt, k, n, rs, cs, &b);

  // Set the scalars to use.
  alpha = &BLIS_ONE;
  beta  = &BLIS_ONE;

  // Initialize the matrix operands.
  bli_randm(&a);
  bli_setm(&BLIS_ONE, &b);
  bli_setm(&BLIS_ZERO, &c);

  if (verbose) {
    bli_printm("a: randomized", &a, "%4.1f", "");
    bli_printm("b: set to 1.0", &b, "%4.1f", "");
    bli_printm("c: initial value", &c, "%4.1f", "");
  }

  // c := beta * c + alpha * a * b, where 'a', 'b', and 'c' are general.
  bli_gemm(alpha, &a, &b, beta, &c);

  if (verbose) bli_printm("c: after gemm", &c, "%4.1f", "");

  // Free the objects.
  bli_obj_free(&a);
  bli_obj_free(&b);
  bli_obj_free(&c);

  ut_assert(t, true);
}

static void
lial_perf_sdf_edge_000_flame_t (ut_test_t *const t)
{
  aux_perf_sdf_flame_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_edge_000.dat");
}

static void
lial_perf_sdf_corner_000_flame_t (ut_test_t *const t)
{
  aux_perf_sdf_flame_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_corner_000.dat");
}

static void
lial_perf_sdf_xedge_000_flame_t (ut_test_t *const t)
{
  aux_perf_sdf_flame_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_xedge_000.dat");
}

static void
lial_perf_sdf_edge_000_flash_t (ut_test_t *const t)
{
  aux_perf_sdf_flash_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_edge_000.dat");
}

static void
lial_perf_sdf_corner_000_flash_t (ut_test_t *const t)
{
  aux_perf_sdf_flash_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_corner_000.dat");
}

static void
lial_perf_sdf_xedge_000_flash_t (ut_test_t *const t)
{
  aux_perf_sdf_flash_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_xedge_000.dat");
}

static void
lial_flame_create_hier_copy_of_flat_000_t (ut_test_t *const t)
{
  aux_flame_create_hier_copy_of_flat_t(t, "./test/data/ut_linear_algebra/large_binary.sdf_edge_000.dat");
}

static void
lial_FLA_Chol_u_unb_var3_t (ut_test_t *const t)
{
  FLA_Obj A;
  FLA_Error ret_status;

  int m    = 3;
  int rs_a = 3;
  int cs_a = 1;

  double buffer_a[] =
    {
     + 4.0, +12.0, -16.0,
     + 0.0, +37.0, -43.0,
     + 0.0, + 0.0, +98.0,
    };

  double buffer_u[] =
    {
     + 2.0, + 6.0, - 8.0,
     + 0.0, + 1.0, + 5.0,
     + 0.0, + 0.0, + 3.0,
    };

  size_t lba, lbu;

  bool verbose;

  double epsilon = 10E-15;

  lba = sizeof(buffer_a) / sizeof(double);
  lbu = sizeof(buffer_u) / sizeof(double);

  ut_assert(t, lba == lbu);
  ut_assert(t, lba == m * m);

  verbose = ut_run_time_is_verbose(t);
  if (verbose) printf("\n");

  FLA_Init();

  ret_status = FLA_Obj_create_without_buffer(FLA_DOUBLE, m, m, &A);
  ut_assert(t, ret_status == FLA_SUCCESS);

  ret_status = FLA_Obj_attach_buffer(buffer_a, rs_a, cs_a, &A);
  ut_assert(t, ret_status == FLA_SUCCESS);

  if (verbose) FLA_Obj_fshow(stdout,
                             "***   Input SDF Matrix A(3,3)   ***",
                             A,
                             "%8.2f",
                             "*** *** *** *** *** *** *** *** ***");

  lial_FLA_Chol_u_unb_var3(A);

  if (verbose) FLA_Obj_fshow(stdout,
                             "*** Factorized U Matrix A(3,3)  ***",
                             A,
                             "%8.2f",
                             "*** *** *** *** *** *** *** *** ***");

  FLA_Obj_free_without_buffer(&A);

  for (size_t i = 0; i < lba; i++)
    ut_assert(t, fabs(buffer_a[i] - buffer_u[i]) < epsilon);

  FLA_Finalize();
}

static void
lial_FLA_Chol_l_blk_var2_t (ut_test_t *const t)
{
  FLA_Obj A;
  FLA_Error ret_status;

  int m    = 3;
  int rs_a = 3;
  int cs_a = 1;
  int block_size = 2;

  double buffer_a[] =
    {
     + 4.0,   0.0,   0.0,
     +12.0, +37.0,   0.0,
     -16.0, -43.0, +98.0,
    };

  double buffer_l[] =
    {
     + 2.0,   0.0,   0.0,
     + 6.0, + 1.0,   0.0,
     - 8.0, + 5.0, + 3.0,
    };

  size_t lba, lbl;

  bool verbose;

  double epsilon = 10E-15;

  lba = sizeof(buffer_a) / sizeof(double);
  lbl = sizeof(buffer_l) / sizeof(double);

  ut_assert(t, lba == lbl);
  ut_assert(t, lba == m * m);

  verbose = ut_run_time_is_verbose(t);
  if (verbose) printf("\n");

  FLA_Init();

  ret_status = FLA_Obj_create_without_buffer(FLA_DOUBLE, m, m, &A);
  ut_assert(t, ret_status == FLA_SUCCESS);

  ret_status = FLA_Obj_attach_buffer(buffer_a, rs_a, cs_a, &A);
  ut_assert(t, ret_status == FLA_SUCCESS);

  if (verbose) FLA_Obj_fshow(stdout,
                             "***   Input SDF Matrix A(3,3)   ***",
                             A,
                             "%8.2f",
                             "*** *** *** *** *** *** *** *** ***");

  lial_FLA_Chol_l_blk_var2(A, block_size);

  if (verbose) FLA_Obj_fshow(stdout,
                             "*** Factorized L Matrix A(3,3)  ***",
                             A,
                             "%8.2f",
                             "*** *** *** *** *** *** *** *** ***");

  FLA_Obj_free_without_buffer(&A);

  for (size_t i = 0; i < lba; i++)
    ut_assert(t, fabs(buffer_a[i] - buffer_l[i]) < epsilon);

  FLA_Finalize();
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

  ut_suite_t *const s = ut_suite_new(&config, "flame");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "flame_dummy", flame_dummy_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "flame_basic", flame_basic_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "blis_basic", blis_basic_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_FLA_Chol_u_unb_var3", lial_FLA_Chol_u_unb_var3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "lial_FLA_Chol_l_blk_var2", lial_FLA_Chol_l_blk_var2_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_10,   "lial_flame_create_hier_copy_of_flat_000", lial_flame_create_hier_copy_of_flat_000_t);

  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "lial_perf_sdf_edge_000_flame", lial_perf_sdf_edge_000_flame_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_100,  "lial_perf_sdf_corner_000_flame", lial_perf_sdf_corner_000_flame_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_1000, "lial_perf_sdf_xedge_000_flame", lial_perf_sdf_xedge_000_flame_t);

  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "lial_perf_sdf_edge_000_flash", lial_perf_sdf_edge_000_flash_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "lial_perf_sdf_corner_000_flash", lial_perf_sdf_corner_000_flash_t);
  ut_suite_add_simple_test(s, UT_MODE_PERF, UT_QUICKNESS_10,   "lial_perf_sdf_xedge_000_flash", lial_perf_sdf_xedge_000_flash_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);

  return failure_count;
}
