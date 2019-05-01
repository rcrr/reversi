/**
 * @file
 *
 * @brief Cholesky performance analysis.
 * @details This executable reads a file that contains the Hessian matrix
 *          then factorize it.
 *
 * @par chol_perf_analysis.c
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
#include <inttypes.h>
#include <assert.h>
#include <math.h>

#include "time_utils.h"
#include "file_utils.h"
#include "main_option_parse.h"
#include "cholesky_decomposition.h"

/**
 * @cond
 */



/* Static constants. */



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int v_flag = false;

static int f_flag = false;
static char *f_arg = NULL;

static int d_flag = false;
static char *d_arg = NULL;

static mop_options_long_t olist[] = {
  {"help",        'h', MOP_NONE},
  {"verbose",     'v', MOP_NONE},
  {"matrix-file", 'f', MOP_REQUIRED},
  {"diag-file",   'd', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "chol_perf_analysis [OPTION...] - Loads a square semidefinite positive matrix from file, then factorize it.\n"
  "\n"
  "Options:\n"
  "  -h, --help           Show help options\n"
  "  -v, --verbose        Verbose output\n"
  "  -f, --matrix-file    Input file name for the square matrix - Mandatory\n"
  "  -d, --diag-file      Input file name for the diagonal of the factorized matrix - Mandatory\n"
  "\n"
  "Description:\n"
  "Used to test new algorithms and improve performances of the Cholesky decomposition.\n"
  "\n"
  "Author:\n"
  "Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2019 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;

/**
 * @endcond
 */


/**
 * @brief Main entry for the Cholesky Performance Analysis.
 */
int
main (int argc, char *argv[])
{
  /* MOP variables. */
  int opt;
  int oindex = -1;

  /* Stopwatch variables. */
  timespec_t time_0, time_1, time_diff;

  /* Cholesky variables. */
  size_t nr, nc, nv;
  double **a, **b;
  double *aux_diag_a, *aux_diag_b;
  int ret_code;
  size_t zero_count, zero_count_d;
  double epsilon = 1.0e-12;

  /* Verbose output. */
  bool verbose = false;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, olist, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
      break;
    case 'v':
      v_flag = true;
      break;
    case 'f':
      f_flag = true;
      f_arg = options.optarg;
      break;
    case 'd':
      d_flag = true;
      d_arg = options.optarg;
      break;
    case ':':
      fprintf(stderr, "Option parsing failed: %s\n", options.errmsg);
      return -1;
    case '?':
      fprintf(stderr, "Option parsing failed: %s\n", options.errmsg);
      return -2;
    default:
      fprintf(stderr, "Unexpectd error. Aborting ...\n");
      abort();
    }
  }

  /* Prints documentation and returns, when help option is active. */
  if (h_flag) {
    fprintf(stderr, "%s", documentation);
    return 0;
  }

  /* Outpust verbose comments. */
  if (v_flag) verbose = true;

  /* Checks command line options for consistency. */
  if (!f_arg) {
    fprintf(stderr, "Option -f, --matrix-file is mandatory.\n");
    return EXIT_FAILURE;
  }

  /* Checks command line options for consistency. */
  if (!d_arg) {
    fprintf(stderr, "Option -d, --diag-file is mandatory.\n");
    return EXIT_FAILURE;
  }

  /* Checks if the matrix input file exixts. */
  if (!fut_file_exists(f_arg)) {
    fprintf(stderr, "chol_perf_analysis: input file \"%s\" doesn't exixt.\n", f_arg);
    return EXIT_FAILURE;
  }

  /* Checks if the diag input file exixts. */
  if (!fut_file_exists(d_arg)) {
    fprintf(stderr, "chol_perf_analysis: input file \"%s\" doesn't exixt.\n", d_arg);
    return EXIT_FAILURE;
  }

  /* Retrieves the matrix from file. */
  a = chol_retrieve_matrix(f_arg, &nr, &nc, &ret_code);
  if (ret_code != 0) {
    fprintf(stderr, "chol_perf_analysis: error in reading file \"%s\".\n", f_arg);
    return EXIT_FAILURE;
  }
  if (verbose) printf("File \"%s\" has been retrieved succesfully.\n", f_arg);

  /* Retrieves the diagonal of the factorized triangular matrix. */
  aux_diag_a = chol_retrieve_vector(d_arg, &nv, &ret_code);
  if (ret_code != 0) {
    fprintf(stderr, "chol_perf_analysis: error in reading file \"%s\".\n", d_arg);
    chol_free_matrix(a);
    return EXIT_FAILURE;
  }
  if (verbose) printf("File \"%s\" has been retrieved succesfully.\n", d_arg);

  /* Verifies that the matrix is square. */
  if (nr != nc) {
    fprintf(stderr,
            "chol_perf_analysis: the matrix retrieved from file \"%s\" is not square:\n"
            "  the number of rows    is: %zu\n"
            "  the number of columns is: %zu\n",
            f_arg, nr, nc);
    chol_free_vector(aux_diag_a);
    chol_free_matrix(a);
    return EXIT_FAILURE;
  }
  if (verbose) printf("The rank of the matrix is: %zu\n", nr);

  /* Verifies that the matrix is square. */
  if (nv != nr) {
    fprintf(stderr, "chol_perf_analysis: the retrieved diagonal from file \"%s\" has size %zu, that differs from the size of the matrix.\n", d_arg, nv);
    chol_free_vector(aux_diag_a);
    chol_free_matrix(a);
    return EXIT_FAILURE;
  }

  /* Counts the number of elements valued as zero. */
  zero_count_d = 0;
  zero_count = 0;
  for (size_t i = 0; i < nr; i++) {
    if (a[i][i] == 0.) zero_count_d++;
    for (size_t j = i + 1; j < nr; j++)
      if (a[i][j] == 0.) zero_count++;
  }
  if (verbose) {
    printf("The matrix has %zu entries valued zero on the diagonal.\n", zero_count_d);
    printf("The matrix has %zu entries valued zero in the upper triangle escluding the diagonal.\n", zero_count);
  }

  /* Clones matrix a into b. */
  b = chol_clone_matrix(a ,nr, nr, &ret_code);
  if (ret_code != 0) {
    fprintf(stderr, "chol_perf_analysis: error in cloning the matrix.\n");
    chol_free_vector(aux_diag_a);
    chol_free_matrix(a);
    return EXIT_FAILURE;
  }
  if (verbose) printf("The cloned matrix has been prepared.\n");

  /* aux_diag: aux vector for factorizing the matrix a. */
  aux_diag_b = chol_allocate_vector(nr);
  if (!aux_diag_b) {
    fprintf(stderr, "chol_perf_analysis: error in allocating the auxiliary vector.\n");
    chol_free_matrix(b);
    chol_free_vector(aux_diag_a);
    chol_free_matrix(a);
    return EXIT_FAILURE;
  }

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  /* TRACKED ZONE - START */

  /* Factorizes the symmetrical essian matrix applying the Cholesky decomposition. */
  chol_fact_naive(b, nr, aux_diag_b);

  /* TRACKED ZONE - FINISH */

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&time_diff, &time_0, &time_1);
  if (verbose) {
    printf("   Cholesky Solution CPU time:                                    ");
    printf("[%6lld.%9ld] ", (long long) timespec_get_sec(&time_diff), timespec_get_nsec(&time_diff));
    printf("\n");
  }

  /* Verifies that the soluton is correct. */
  for (size_t i = 0; i < nr; i++) {
    if (fabs(aux_diag_a[i] -  aux_diag_b[i]) > epsilon) {
      fprintf(stderr,
              "chol_perf_analysis: the algorithm gives a wrong value.\n"
              "  aux_diag_a[%zu] = %f\n"
              "  aux_diag_b[%zu] = %f\n"
              "  epsilon = %e"
              , i, aux_diag_a[i], i, aux_diag_b[i], epsilon);
      goto end_of_program;
    }
  }
  for (size_t i = 0; i < nr; i++) {
    for (size_t j = 0; j < i; j++)
      if (fabs(a[i][j] - b[i][j]) > epsilon) {
        fprintf(stderr,
                "chol_perf_analysis: the algorithm gives a wrong value.\n"
                "  a[%zu][%zu] = %f\n"
                "  b[%zu][%zu] = %f\n"
                "  fabs(a[%zu][%zu] - b[%zu][%zu]) = %f\n"
                "  epsilon = %e\n"
                , i, j, a[i][j], i, j, b[i][j], i, j, i, j, fabs(a[i][j] - b[i][j]), epsilon);
        goto end_of_program;
      }
  }
  if (verbose) printf("The result of the cholesky decomposition has been checked succesfully.\n");

 end_of_program:
  ;

  /* Frees memory allocated for the matrix. */
  chol_free_vector(aux_diag_b);
  chol_free_vector(aux_diag_a);
  chol_free_matrix(b);
  chol_free_matrix(a);

  return 0;
}
