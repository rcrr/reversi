/**
 * @file
 *
 * @brief RGLM, Revrsi Generalized Linear Model.
 * @details Solves the problem stated in the input file ...
 *
 * @par rglm.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2018, 2019 Roberto Corradini. All rights reserved.
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
#include <time.h>

#include "time_utils.h"
#include "main_option_parse.h"
#include "board_pattern.h"
#include "rglm_data_files.h"
#include "rglm_utils.h"
#include "cholesky_decomposition.h"



/**
 * @cond
 */

/* Static constants. */



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int v_flag = false;

static int s_flag = false;

static int i_flag = false;
static char *i_arg = NULL;

static int A_flag = false;
static char *A_arg = NULL;

static int B_flag = false;
static char *B_arg = NULL;

static int P_flag = false;
static char *P_arg = NULL;

static int Q_flag = false;
static char *Q_arg = NULL;

static int T_flag = false;
static char *T_arg = NULL;

static int W_flag = false;
static char *W_arg = NULL;

static mop_options_long_t olist[] = {
  {"help",              'h', MOP_NONE},
  {"verbose",           'v', MOP_NONE},
  {"solve",             's', MOP_NONE},
  {"input-file",        'i', MOP_REQUIRED},
  {"extract-ps-table",  'A', MOP_REQUIRED},
  {"extract-pfs-table", 'B', MOP_REQUIRED},
  {"extract-gp-table",  'P', MOP_REQUIRED},
  {"extract-gp-ptable", 'Q', MOP_REQUIRED},
  {"extract-gp-ttable", 'T', MOP_REQUIRED},
  {"extract-weights",   'W', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "rglm [OPTION...] - Reversi Generalized Linear Model solver\n"
  "\n"
  "Options:\n"
  "  -h, --help              Show help options\n"
  "  -v, --verbose           Verbose output\n"
  "  -s, --solve             Solve the Generalized Linear Model\n"
  "  -i, --input-file        Input file name - Mandatory\n"
  "  -A, --extract-ps-table  Dumps the position summary table in a CSV format\n"
  "  -B, --extract-pfs-table Dumps the pattern frequency summary table in a CSV format\n"
  "  -P, --extract-gp-table  Dumps the solved and classified game position table in a CSV format, with original pattern indexes\n"
  "  -Q, --extract-gp-ptable Dumps the solved and classified game position table in a CSV format, with principal pattern indexes\n"
  "  -T, --extract-gp-ttable Dumps the solved and classified game position table transformed to glm_variable_id in a CSV format\n"
  "  -W, --extract-weights   Dumps the weights, the optimized value assigned to the glm_variable_id keys in a CSV format\n"
  "\n"
  "Description:\n"
  "The Reversi Generalized Linear Model solver is the main entry to a set of utilities dedicated to process a set of solved and classified game position retrieved from a binary imput file.\n"
  "\n"
  "Author:\n"
  "Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2018, 2019 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;

/* Static functions. */
static void
print_error_and_stop (int ret_code)
{
  fprintf(stderr, "rglm: format error reading file %s\n", i_arg);
  exit(ret_code);
}

/**
 * @endcond
 */



/**
 * @brief Main entry for the RGLM ( Reversi Generalized Linear Model ) program.
 */
int
main (int argc,
      char *argv[])
{
  uint64_t u64, v64, *u64p;
  int16_t i16;
  uint8_t u8;
  char **cpp;
  board_pattern_id_t *bpip;
  char buf[512];
  rglmdf_position_summary_record_t *psrp;
  rglmdf_pattern_freq_summary_record_t *pfsrp;
  rglmdf_solved_and_classified_gp_record_t *scgprp;
  uint32_t *iarrayp;
  uint64_t data_chunk_size;

  rglmdf_general_data_t data;
  rglmdf_general_data_init(&data);

  bool verbose = false;

  FILE *ofp = NULL;

  size_t re;
  int opt;
  int oindex = -1;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, olist, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
      break;
    case 'v':
      v_flag = true;
      break;
    case 's':
      s_flag = true;
      break;
    case 'i':
      i_flag = true;
      i_arg = options.optarg;
      break;
    case 'A':
      A_flag = true;
      A_arg = options.optarg;
      break;
    case 'B':
      B_flag = true;
      B_arg = options.optarg;
      break;
    case 'P':
      P_flag = true;
      P_arg = options.optarg;
      break;
    case 'Q':
      Q_flag = true;
      Q_arg = options.optarg;
      break;
    case 'T':
      T_flag = true;
      T_arg = options.optarg;
      break;
    case 'W':
      W_flag = true;
      W_arg = options.optarg;
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

  /* Outpust verbose comments. */
  if (v_flag) verbose = true;

  /* Prints documentation and returns, when help option is active. */
  if (h_flag) {
    fprintf(stderr, "%s", documentation);
    return 0;
  }

  /* Checks command line options for consistency. */
  if (!i_arg) {
    fprintf(stderr, "Option -i, --input-file is mandatory.\n");
    return -3;
  }

  /* Checks size of types. */
  if (!rglmdf_verify_type_sizes()) {
    fprintf(stderr, "Data types read from, or written to, the binary file have a size that is not consistent with the original one.\n");
    return EXIT_FAILURE;
  }

  /* Opens the binary file for reading. */
  FILE *ifp = fopen(i_arg, "r");
  assert(ifp);

  /* Reads the File Creation time field. */
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(-10);
  rglmdf_set_file_creation_time(&data, u64);
  if (verbose) {
    rglmdf_get_file_creation_time_as_string(&data, buf);
    fprintf(stdout, "Input file started to be written on (UTC) %s", buf);
  }

  /* Reads the batch_id_cnt, batch_ids input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(-20);
  v64 = rglmdf_set_batch_id_cnt(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for batch_ids array.\n");
    return EXIT_FAILURE;
  }
  u64p = rglmdf_get_batch_ids(&data);
  re = fread(u64p, sizeof(uint64_t), u64, ifp);
  if (re != u64) print_error_and_stop(-30);
  if (verbose) {
    rglmdf_batch_ids_to_text_stream(&data, stdout);
  }

  /* Reads the empty_count input field.*/
  re = fread(&u8, sizeof(uint8_t), 1, ifp);
  if (re != 1) print_error_and_stop(-40);
  rglmdf_set_empty_count(&data, u8);
  if (verbose) fprintf(stdout, "Selected empty_count value: %u\n", u8);

  /* Reads the position_status_cnt, position_statuses, position_status_buffer input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(-50);
  v64 = rglmdf_set_position_status_cnt(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for position_statuses array.\n");
    return EXIT_FAILURE;
  }
  cpp = rglmdf_get_position_statuses(&data);
  re = fread(*cpp, RGLM_POSITION_STATUS_BUF_SIZE, u64, ifp);
  if (re != u64) print_error_and_stop(-60);
  if (verbose) rglmdf_position_statuses_to_text_stream(&data, stdout);

  /* Reads the pattern_cnt, patterns input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(-70);
  v64 = rglmdf_set_pattern_cnt(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for patterns array.\n");
    return EXIT_FAILURE;
  }
  bpip = rglmdf_get_patterns(&data);
  for (size_t i = 0; i < u64; i++) {
    re = fread(&i16, sizeof(int16_t), 1, ifp);
    if (re != 1) print_error_and_stop(-80);
    bpip[i] = i16;
  }
  if (verbose) rglmdf_patterns_to_text_stream(&data, stdout);

  /* Reads the position summary table. */
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(-90);
  v64 = rglmdf_set_position_summary_ntuples(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for the records of position summary table.\n");
    return EXIT_FAILURE;
  }
  psrp = rglmdf_get_position_summary_records(&data);
  re = fread(psrp, sizeof(rglmdf_position_summary_record_t), u64, ifp);
  if (re != u64) print_error_and_stop(-100);
  if (verbose) rglmdf_position_summary_cnt_to_text_stream(&data, stdout);

  /* Reads the pattern frequency summary table. */
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(-110);
  v64 = rglmdf_set_pattern_freq_summary_ntuples(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for the records of pattern freq summary table.\n");
    return EXIT_FAILURE;
  }
  pfsrp = rglmdf_get_pattern_freq_summary_records(&data);
  re = fread(pfsrp, sizeof(rglmdf_pattern_freq_summary_record_t), u64, ifp);
  if (re != u64) print_error_and_stop(-120);
  if (verbose) rglmdf_pattern_freq_summary_cnt_to_text_stream(&data, stdout);

  /* Creates the mapping betweeen (pattern_id, principal_index_value) --> glm_variable_id */
  rglmdf_build_reverse_map(&data);
  if (verbose) fprintf(stdout, "Reverse map has been computed.\n");

  /* Read the number of record for the solved and classified game position table. */
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(-130);
  v64 = rglmdf_set_positions_ntuples(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for the positions table.\n");
    return EXIT_FAILURE;
  }
  /* Reads the sequence of chunk of records. Each chunk is organized as: chunk size n, n records, n irecords. */
  scgprp = rglmdf_get_positions_records(&data);
  iarrayp = rglmdf_get_positions_iarray(&data);
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(&data);
  size_t n_record_read = 0;
  for (;;) {
    re = fread(&data_chunk_size, sizeof(uint64_t), 1, ifp);
    if (re != 1) print_error_and_stop(-140);
    n_record_read += data_chunk_size;
    if (n_record_read > u64) {
      fprintf(stderr, "Data chunks cumulated so far are more than expected.\n");
      return EXIT_FAILURE;
    }
    if (data_chunk_size == 0) {
      if (n_record_read != u64) {
        fprintf(stderr, "Data chunks being read are less than expected.\n");
        fprintf(stderr, "Expected = %lu, number of record read = %zu\n", u64, n_record_read);
        return EXIT_FAILURE;
      }
      break;
    }
    re = fread(scgprp, sizeof(rglmdf_solved_and_classified_gp_record_t), data_chunk_size, ifp);
    if (re != data_chunk_size) print_error_and_stop(-150);
    re = fread(iarrayp, sizeof(uint32_t) * ni, data_chunk_size, ifp);
    if (re != data_chunk_size) print_error_and_stop(-160);
    scgprp += data_chunk_size;
    iarrayp += ni * data_chunk_size;
  }
  if (verbose) fprintf(stdout, "All solved and classified game positions has been read succesfully.\n");

  /* Closes the binary imput file. */
  fclose(ifp);

  /* If A flag is turned on, dumps the position summary table to the output file. */
  if (A_arg) {
    ofp = fopen(A_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", A_arg);
      return EXIT_FAILURE;
    }
    rglmdf_ps_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Position summary table dumped to CSV file: \"%s\".\n", A_arg);
  }

  /* If B flag is turned on, dumps the position summary table to the output file. */
  if (B_arg) {
    ofp = fopen(B_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", B_arg);
      return EXIT_FAILURE;
    }
    rglmdf_pfs_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Pattern frequencies summary table dumped to CSV file: \"%s\".\n", B_arg);
  }

  /* If P flag is turned on, dumps the game position table to the output file. */
  if (P_arg) {
    ofp = fopen(P_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", P_arg);
      return EXIT_FAILURE;
    }
    rglmdf_gp_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Game positions dumped to CSV file: \"%s\".\n", P_arg);
  }

  /* Transforms the pattern index values to the principal ones. */
  rglmdf_transform_piv_to_glm_variable_id(&data, true);

  /* If Q flag is turned on, dumps the game position transformed table to the output file. */
  if (Q_arg) {
    ofp = fopen(Q_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", Q_arg);
      return EXIT_FAILURE;
    }
    rglmdf_gp_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Game positions, with pattern indexes being remapped to principal values, dumped to CSV file: \"%s\".\n", Q_arg);
  }

  /* Transforms the pattern principal index values to the corresponding global GLM variable id, in the game position table. */
  rglmdf_transform_piv_to_glm_variable_id(&data, false);
  if (verbose) fprintf(stdout, "Pattern principal index values have been translated to global GLM variable id.\n");

  /* If T flag is turned on, dumps the game position transformed table to the output file. */
  if (T_arg) {
    ofp = fopen(T_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", T_arg);
      return EXIT_FAILURE;
    }
    rglmdf_gp_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Transformed game positions dumped to CSV file: \"%s\".\n", T_arg);
  }

  /*
   *
   * Ready to compute the weights ...
   *
   */

  if (s_flag) {

    /* Max number of iterations allowed to the Newton algorithm. */
    size_t max_newton_iter;
    size_t abs_min_pos, abs_max_pos;

    /* Stopwatch variables. */
    timespec_t time_0, time_1, time_diff;

    /* All the Algorithm variables. */
    double *v, *w, *e, *de, *r, *minus_grad_f, **big_b, *aux_diag_big_b, *p;
    double effe, effe_last, delta_effe, lambda, grad_magnitude, abs_min, abs_max, r_magnitude, p_magnitude;

    double epsilon_on_delta_effe, epsilon_on_delta_w_modulus, epsilon_on_gradient_modulus;

    /* enne: number of parameters to be fitted. */
    const size_t enne = data.pattern_freq_summary.ntuples;

    /* emme: number of solved and classified game positions. */
    const size_t emme = data.positions.ntuples;

    /* v: transformd game value for each solved position. */
    v = chol_allocate_vector(emme);
    if (!v) abort();
    rglmut_gv_init(&data, emme, v);

    /* w: weigths.*/
    w = chol_allocate_vector(enne);
    if (!w) abort();
    for (size_t i = 0; i < enne; i++) w[i] = data.pattern_freq_summary.records[i].weight;

    /* e: evaluation function for the game positions.*/
    e = chol_allocate_vector(emme);
    if (!e) abort();

    /* de: derivative of the evaluation function for the game positions, de = e * (1 - e).*/
    de = chol_allocate_vector(emme);
    if (!de) abort();

    /* r: residual value for the game positions, r = e - v.*/
    r = chol_allocate_vector(emme);
    if (!r) abort();

    /* minus_grad_f: right hand side of the GAUSS-NEWTON liner system. */
    minus_grad_f = chol_allocate_vector(enne);
    if (!minus_grad_f) abort();

    /* big_b: square matrix generated by the matrix multiplication jacobian transposed * jacobian. */
    big_b = chol_allocate_square_matrix(enne);
    if (!big_b) abort();

    /* aux_diag_big_b: aux vector for factorizing big_b. */
    aux_diag_big_b = chol_allocate_vector(enne);
    if (!aux_diag_big_b) abort();

    /* p: the update vector for the Gauss-Newton algorithm. */
    p = chol_allocate_vector(emme);
    if (!p) abort();

    /* Computes the constant v vector. */
    rglmut_gv_init(&data, emme, v);

    /* The first evaluation of effe_last is taken as if the evaluation function is always zero. */
    effe_last = 0.0;
    for (size_t i = 0; i < emme; i++) effe_last += (v[i] - 0.5) * (v[i] - 0.5);
    effe_last *= 0.5;

    /* lambda: scalar parameter used by the Levemberg-Marquardt algorith. */
    lambda = 0.0001;

    /* Termination criteria. */
    epsilon_on_delta_effe       = 1.0e-9;
    epsilon_on_delta_w_modulus  = 1.0e-9;
    epsilon_on_gradient_modulus = 1.0e-9;

    /* max_newton_iter: max number of iterations allowed to the Newton algorithm. */
    max_newton_iter = 10;

    if (verbose) {
      printf("Dumping factor for the diagonal of the Hessian matrix (Levemberg-Marquardt): lambda = %f\n", lambda);
      printf("Max number of Newton algorithm iterations = %zu\n", max_newton_iter);
      printf("Termination criteria:\n");
      printf("   Epsilon on delta effe         : %e\n", epsilon_on_delta_effe);
      printf("   Epsilon on modulus of delta w : %e\n", epsilon_on_delta_w_modulus);
      printf("   Epsilon on modulus of gradient: %e\n", epsilon_on_gradient_modulus);
    }

    for (size_t iter = 0; iter < max_newton_iter; iter++) {

      if (verbose) printf("Iteration[%03zu]:\n", iter);

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      rglmut_evaluation_function_eval(&data, enne, w, emme, e);
      if (0) for (size_t i = 0; i < emme; i++) printf("e[%3zu]=%f\n", i, e[i]);

      rglmut_evaluation_function_derivative_eval(emme, e, de);
      if (0) for (size_t i = 0; i < emme; i++) printf("de[%3zu]=%f\n", i, de[i]);

      rglmut_residual_value_eval(emme, e, v, r);
      if (0) for (size_t i = 0; i < emme; i++) printf("r[%3zu]=%f\n", i, r[i]);

      rglmut_minus_grad_f_eval(&data, minus_grad_f, enne, emme, r, de);
      if (0) for (size_t i = 0; i < enne; i++) printf("minus_grad_f[%3zu]=%f\n", i, minus_grad_f[i]);

      rgmlut_big_b_eval(&data, big_b, enne, emme, e, de, r);
      if (0) for (size_t i = 0; i < enne; i++) printf("big_b[%3zu][%3zu]=%f\n", i, i, big_b[i][i]);
      if (0) {
        for (size_t i = 0; i < enne; i++) {
          printf("big_b[%3zu][%3zu .. %3zu]: ", i, i, enne - 1);
          for (size_t j = i; j < enne; j++) {
            printf("(%3zu, %10.4f) ", j, big_b[i][j]);
          }
          printf("\n");
        }
      }

      grad_magnitude = chol_vector_magnitude(minus_grad_f, enne, &abs_min, &abs_min_pos, &abs_max, &abs_max_pos);
      printf("   Gradient modulus = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", grad_magnitude, abs_min, abs_min_pos, abs_max, abs_max_pos);
      r_magnitude = chol_vector_magnitude(r, emme, &abs_min, &abs_min_pos, &abs_max, &abs_max_pos);
      printf("   Residual modulus = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", r_magnitude, abs_min, abs_min_pos, abs_max, abs_max_pos);

      /* Increases the diagonal of the Hessian matrix. */
      for (size_t i = 0; i < enne; i++) big_b[i][i] *= 1 + lambda;

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

      /* Computes the time taken, and updates the test cpu_time. */
      timespec_diff(&time_diff, &time_0, &time_1);
      if (verbose) {
        printf("   Function, Gradient, Hessian, and Residual evaluation CPU time: ");
        printf("[%6lld.%9ld] ", (long long) timespec_get_sec(&time_diff), timespec_get_nsec(&time_diff));
        printf("\n");
      }

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Factorizes the symmetrical essian matrix applying the Cholesky decomposition. */
      chol_fact_naive(big_b, enne, aux_diag_big_b);

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

      /* Computes the time taken, and updates the test cpu_time. */
      timespec_diff(&time_diff, &time_0, &time_1);
      if (verbose) {
        printf("   Cholesky Factorization and CPU time:                           ");
        printf("[%6lld.%9ld] ", (long long) timespec_get_sec(&time_diff), timespec_get_nsec(&time_diff));
        printf("\n");
      }

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Solve the linear system by back substitution. */
      chol_solv_naive(big_b, enne, aux_diag_big_b, minus_grad_f, p);

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

      /* Computes the time taken, and updates the test cpu_time. */
      timespec_diff(&time_diff, &time_0, &time_1);
      if (verbose) {
        printf("   Cholesky Solution CPU time:                                    ");
        printf("[%6lld.%9ld] ", (long long) timespec_get_sec(&time_diff), timespec_get_nsec(&time_diff));
        printf("\n");
      }

      p_magnitude = chol_vector_magnitude(p, enne, &abs_min, &abs_min_pos, &abs_max, &abs_max_pos);
      printf("   Delta w modulus  = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", p_magnitude, abs_min, abs_min_pos, abs_max, abs_max_pos);

      if (0) {
        for (size_t i = 0; i < enne; i++) {
          printf("w[%02zu]=%0+20.18f, p=%0+20.18f, minus_grad_f=%0+20.18f\n", i, w[i], p[i], minus_grad_f[i]);
        }
      }

      /* Updates the vector of weights with the delta. */
      for (size_t i = 0; i < enne; i++) w[i] += p[i];

      effe = 0.0;
      for (size_t i = 0; i < emme; i++) effe += r[i]*r[i];
      effe *= 0.5;
      delta_effe = effe_last - effe;
      effe_last = effe;
      printf("   Effe             = %30.18f\n", effe);
      printf("   Delta Effe       = %30.18f\n", delta_effe);

      if ( delta_effe <= epsilon_on_delta_effe &&
           p_magnitude <= epsilon_on_delta_w_modulus &&
           grad_magnitude <= epsilon_on_gradient_modulus
           ) break;

    }

    /* Copies the optimized weighs into the general data structure. */
    for (size_t i = 0; i < enne; i++) data.pattern_freq_summary.records[i].weight = w[i];

    chol_free_vector(p);
    chol_free_vector(aux_diag_big_b);
    chol_free_matrix(big_b);
    chol_free_vector(minus_grad_f);
    chol_free_vector(r);
    chol_free_vector(de);
    chol_free_vector(e);
    chol_free_vector(w);
    chol_free_vector(v);

  }

  /*
   *
   * Weights computed.
   *
   */

  /* If W flag is turned on, dumps the weights table to the output file. */
  if (W_arg) {
    ofp = fopen(W_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", W_arg);
      return EXIT_FAILURE;
    }
    rglmdf_pfs_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Optimized Weights table dumped to CSV file: \"%s\".\n", W_arg);
  }

  /* TO DO:
   *  - Add flag to print a specific record in the game position table.
   *  - Optimize with AVX2 the Cholesky decomposition.
   *  - Add the parameter output to binary file.
   *  - Add a further table output to CSV after the solution has been found.
   *  - Reorganize variable declarations.
   *  - Reorganize the verbose output.
   *  - Add the residual output ... means to output all the game positions ....
   *  - Rename cholesky module to linear_algebra ....
   */

  /* Frees resources. */
  rglmdf_general_data_release(&data);

  return EXIT_SUCCESS;
}
