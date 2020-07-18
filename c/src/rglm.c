/**
 * @file
 *
 * @todo Notice: RGLM and REGAB to-do list has to go somehow together.
 *
 * @todo Both REGAB and RGLM main functions have to be reduced to a call to two respective new functions.
 *       These new functions are going to enable an API for the two programs that is then usable by
 *       test modules.
 *
 * @todo Write a function in rglm_data_files that read ( also write ? ) the rglm
 *       binary data file.
 *       Formats for binary data files are more than one. There is the 'general' format,
 *       'weights', 'game-positions', and the 'hessian' format.
 *       Should be all documented and rationalized.
 *
 * @todo The algorithm used to compute the weights is Minimum Mean Square Error (MMSE),
 *       move to Maximum Likelihood Estimation (MLE), or offer the selection among the two.
 *
 * @todo The INTERCEPT is 'hyper-static', and do not take the value corresponding to the average game value.
 *       This behavior is not expected, and should be investigated further.
 *
 * @todo A much more powerful mobility measure could in principle better asses positions like 35900656.
 *       This position has a value of +64, but the best evaluator with ALL the patterns give an evaluation of
 *       a DRAW.
 *       Look at the position for details ....
 *
 *       tst_regab=> SELECT game_position_pp_mop(mover, opponent, player) FROM regab_prng_gp WHERE seq = 35900656;
 *       game_position_pp_mop
 *       -----------------------
 *           a b c d e f g h   +
 *        1  . O @ @ @ @ @ @   +
 *        2  @ @ @ @ @ @ @ .   +
 *        3  @ @ @ @ @ @ @ @   +
 *        4  @ @ @ @ @ . @ .   +
 *        5  . . @ @ @ O O O   +
 *        6  . . @ . O . @ .   +
 *        7  . . . O @ @ @ O   +
 *        8  . . O . @ . . .   +
 *        Player to move: BLACK
 *       (1 row)
 *
 * @todo Classify the legal moves into types.
 * @code
 * .    a    b    c    d    e    f    g    h
 *   =========================================
 * 1 =  V .. C .. A .. B .. B .. A .. C .. V =
 *   =========================================
 * 2 =  C .. X .. F .. H .. H .. F .. X .. C =
 *   =========================================
 * 3 =  A .. F .. S .. E .. E .. S .. F .. A =
 *   =========================================
 * 4 =  B .. H .. E .. O .. O .. E .. H .. B =
 *   =========================================
 * 5 =  B .. H .. E .. O .. O .. E .. H .. B =
 *   =========================================
 * 6 =  A .. F .. S .. E .. E .. S .. F .. A =
 *   =========================================
 * 7 =  C .. X .. F .. H .. H .. F .. X .. C =
 *   =========================================
 * 8 =  V .. C .. A .. B .. B .. A .. C .. V =
 *   =========================================
 * @endcode
 *
 * @todo Evaluate if it would be better to insert the checksum data into the rglm files.
 *
 * @todo More comments are needed in order to better explain the algorithm.
 *
 * @todo Functions are not commented in file rglm_utils.h
 *
 * @todo A new flag has to be introduced that assign the TV value for the regresion.
 *
 * @todo The evaluation function in the current implementation computes the expected outcome.
 *       This is improper at best, incorrect most likely.
 *       The logistic regression applied by the algorithm should model the probability
 *       of a dichotomous outcome.
 *       Here we have two options:
 *         - transform the implementation to a proper dichotomous logit model
 *         - develop the model as an ordinal logit regression
 *       We are going to focus on the first one first.
 *       The idea is to identify a Target game Value (TV) and classify the Position Data-set
 *       using the recorded Game True Value (GTV): Pr(GTV > TV) = Pr(y = 1) = e(w) , where
 *       e(w) is the value of the evaluation function given the vector of weights w.
 *       Solve then the GLM problem for TV in the range [-64..+62] (when TV is 64, all positions
 *       are classified as y=0, fail, loss, defeat, ...). there are 64 regressions to carry out in
 *       order to fully profile the model.
 *       This approach, being formally sound, brings in many challenges and changes.
 *
 *       The first comment regards a formal issue that is going to emerge.
 *       There will be values of TV, most likely at the extreme of the range, that shall
 *       incur into the "Complete Separation" issue. What does it mean ?
 *       There will be feature configurations occurring only on positions classified either
 *       as win or loss. The model doesn't have a solution then, the corresponding parameters
 *       will go to plus or minus infinite. The interpretation is that the feature configuration
 *       classifies the outcome without any possible error as a win or a loss, of course given
 *       the data-set.
 *       The proposed solution is to filter these configurations from the regression.
 *       When found during the application of the model to new positions the evaluation
 *       functions has to pre-check the configurations and return immediately a value of
 *       1 or 0 accordingly.
 *       It means that these configuration has to be detected upfront, and stored in the
 *       binary file. There is a change to be applied in the REGAB program as well as in the
 *       binary file format and in the RGLM utility that read the file.
 *       The pattern_freq_summary table has to be extended with the frequencies of positions
 *       by outcome value, it is an array of 65 integers for each glm variable.
 *
 *       A more automated procedure is then needed to orchestrate all the regressions, storing the
 *       results ( I mean the weights ) by TV, and also the regression KPI for future understanding.
 *
 * @todo A regression convergence createria has to be studied. The goodness of fit statistics, either
 *       residuals, deviance, or chi-square, or something else.
 *
 * --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 *
 * @todo [2020-05-15 - done] There is an error in the definition of one vector of size emme instead of enne.
 *                           Change done.
 *
 * @todo [2020-05-15 - done] The vector p is not needed, most likely. Removed.
 *
 * @todo [2020-05-16 - done] There is an error in the math formula used in function rglmut_big_b_eval
 *                           The formula was correct , but written in an alternative way.
 *                           The formula has been written as in the documentation.
 *                           The definition of residual is changed from r = e - v to r = v - e.
 *
 * @todo [2020-06-19 - done] The optimization loop, that implements the Newton-Raphson algorithm
 *                           doesn't have the proper order of printf statements.
 *                           Measured quantities are computed and printed in the wrong place or order.
 *
 * @todo [2020-06-19 - done] Reorganize verbose output and variable declarations.
 *
 * @todo [2020-06-19 - done] CSV files dumped from the program are somehow redundant and not well organized.
 *                           Moreover the FATURES are not dumped ...
 *
 * @todo [2020-06-19 - done] Add the intercept parameter w_0
 *                           Add the new concept of feature.
 *                           Add to features the legal_move_count as MOBILITY, MOBILITY2, MOBILITY3 and INTERCEPT as the w_0 intercept parameter.
 *                           These three changes has been joined together.
 *
 * @todo [2020-06-21 - done] Consider if is better to remove the end_game positions.
 *                           Are strong outliers.
 *                           Were not that many! And the change reveled to be not so relevant. But doing it I have found a terrible bug in the
 *                           game_value generation, when the first move in the game position is a pass, the stack was not initialized properly
 *                           and the game espansion was truncated after the first pass move.
 *                           Also this proved not to be a game changer ... around one hundred among two millions game position were affected.
 *
 *                           The evaluation function could check if the game position is a leaf, if yes the value is the disc difference.
 *                           In this scenario having end_game position into the data set just pollutes it.
 *
 *                           An option could be:
 *                           end_game           :: special case in ef(w) , it return the true value ( as it must be ! )
 *                                                 removed from the regressed data-set
 *                           pass               :: LMOCNT = 1
 *                                                 keept in the data set
 *                           legal_move_cnt > 0 :: standard case , LMOCNT := legal_move_cnt
 *                                                 keept in the data set
 *
 * @todo [2020-07-13 - done] Move the options of the main program from static declarations
 *                           to the main function. The reason is to avoid potential collisions,
 *                           and as a general rule it is a way to increase encapsulation.
 *
 *
 *
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
 * @copyright 2018, 2019, 2020 Roberto Corradini. All rights reserved.
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
#include "rglm_utils.h"
#include "linear_algebra.h"



/**
 * @cond
 */

/* Static constants. */


/* Static variables. */


/* Static functions. */
static void
print_error_and_stop (char *input_file_name,
                      int ret_code)
{
  fprintf(stderr, "rglm: format error reading file \"%s\", ret_code = %d\n", input_file_name, ret_code);
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
  uint64_t u64, u64_a, u64_b, v64, *u64p;
  int16_t i16;
  uint8_t u8;
  char **cpp;
  board_feature_id_t *bfip;
  board_pattern_id_t *bpip;
  char buf[512];
  rglmdf_position_summary_record_t *psrp;
  rglmdf_pattern_freq_summary_record_t *pfsrp;
  rglmdf_solved_and_classified_gp_record_t *scgprp;
  uint32_t *iarrayp;
  double *farrayp;
  uint64_t data_chunk_size;
  int ret_code;

  rglmdf_general_data_t data;
  rglmdf_general_data_init(&data);

  bool verbose = false;

  FILE *ofp = NULL;

  size_t re;
  int opt;
  int oindex = -1;

  mop_options_t options;
  int h_flag = false;
  int v_flag = false;
  int s_flag = false;
  int i_flag = false;
  char *i_arg = NULL;
  int o_flag = false;
  char *o_arg = NULL;
  int b_flag = false;
  char *b_arg = NULL;
  int A_flag = false;
  char *A_arg = NULL;
  int B_flag = false;
  char *B_arg = NULL;
  int P_flag = false;
  char *P_arg = NULL;
  int Q_flag = false;
  char *Q_arg = NULL;
  int T_flag = false;
  char *T_arg = NULL;
  int W_flag = false;
  char *W_arg = NULL;
  int R_flag = false;
  char *R_arg = NULL;
  int H_flag = false;
  char *H_arg = NULL;

  mop_options_long_t opt_list[] =
    {
     {"help",                 'h', MOP_NONE},
     {"verbose",              'v', MOP_NONE},
     {"solve",                's', MOP_NONE},
     {"input-file",           'i', MOP_REQUIRED},
     {"output-file",          'o', MOP_REQUIRED},
     {"rglm-par-output-file", 'b', MOP_REQUIRED},
     {"extract-ps-table",     'A', MOP_REQUIRED},
     {"extract-pfs-table",    'B', MOP_REQUIRED},
     {"extract-gp-table",     'P', MOP_REQUIRED},
     {"extract-gp-ptable",    'Q', MOP_REQUIRED},
     {"extract-gp-ttable",    'T', MOP_REQUIRED},
     {"extract-weights",      'W', MOP_REQUIRED},
     {"extract-residuals",    'R', MOP_REQUIRED},
     {"dump-hessian-matrix",  'H', MOP_REQUIRED},
     {0, 0, 0}
    };

  const char *documentation =
    "Usage:\n"
    "rglm [OPTION...] - Reversi Generalized Linear Model solver\n"
    "\n"
    "Options:\n"
    "  -h, --help                 Show help options\n"
    "  -v, --verbose              Verbose output\n"
    "  -s, --solve                Solve the Generalized Linear Model\n"
    "  -i, --input-file           Input file name - Mandatory\n"
    "  -o, --output-file          Output file name\n"
    "  -b, --rglm-par-output-file RGLM parameters values output file name\n"
    "  -A, --extract-ps-table     Dumps the position summary table in a CSV format\n"
    "  -B, --extract-pfs-table    Dumps the feature and pattern frequency summary table in a CSV format\n"
    "  -P, --extract-gp-table     Dumps the solved and classified game position table in a CSV format, with original pattern indexes\n"
    "  -Q, --extract-gp-ptable    Dumps the solved and classified game position table in a CSV format, with principal pattern indexes\n"
    "  -T, --extract-gp-ttable    Dumps the solved and classified game position table transformed to glm_variable_id in a CSV format\n"
    "  -W, --extract-weights      Dumps the weights, the optimized value assigned to the glm_variable_id keys in a CSV format\n"
    "  -R, --extract-residuals    Dumps the residuals of the minimization of the GLM model in a CSV format\n"
    "  -H, --dump-hessian-matrix  Dumps the unresolved Hessian matrix to a binary file and exits\n"
    "\n"
    "Description:\n"
    "The Reversi Generalized Linear Model solver is the main entry to a group of utilities dedicated to process a set of solved and classified game position retrieved from a binary imput file.\n"
    "\n"
    "Author:\n"
    "Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
    "\n"
    "Copyright (c) 2018, 2019, 2020 Roberto Corradini. All rights reserved.\n"
    "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
    "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
    ;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, opt_list, &oindex)) != -1) {
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
    case 'o':
      o_flag = true;
      o_arg = options.optarg;
      break;
    case 'b':
      b_flag = true;
      b_arg = options.optarg;
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
    case 'R':
      R_flag = true;
      R_arg = options.optarg;
      break;
    case 'H':
      H_flag = true;
      H_arg = options.optarg;
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

  /* Outputs verbose comments. */
  if (v_flag) verbose = true;

  /* Prints documentation and returns, when help option is active. */
  if (h_flag) {
    fprintf(stderr, "%s", documentation);
    return 0;
  }

  /* Checks command line options for consistency. */
  if (!i_flag) {
    fprintf(stderr, "Option -i, --input-file is mandatory.\n");
    return -3;
  }

  /* Checks command line options for consistency: H flag is exclusive. */
  if (H_arg) {
    if (o_flag || b_flag || A_flag || B_flag || P_flag || Q_flag || T_flag || W_flag || R_flag || s_flag ) {
      fprintf(stderr, "Option -H, --dump-hessian-matrix is not compatible with other selected flags.\n");
      return -4;
    }
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
  if (re != 1) print_error_and_stop(i_arg, -10);
  rglmdf_set_file_creation_time(&data, u64);
  if (verbose) {
    rglmdf_get_file_creation_time_as_string(&data, buf);
    fprintf(stdout, "Input file started to be written on (UTC) %s", buf);
  }

  /* Reads the batch_id_cnt, batch_ids input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -20);
  v64 = rglmdf_set_batch_id_cnt(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for batch_ids array.\n");
    return EXIT_FAILURE;
  }
  u64p = rglmdf_get_batch_ids(&data);
  re = fread(u64p, sizeof(uint64_t), u64, ifp);
  if (re != u64) print_error_and_stop(i_arg, -30);
  if (verbose) {
    rglmdf_batch_ids_to_text_stream(&data, stdout);
  }

  /* Reads the empty_count input field.*/
  re = fread(&u8, sizeof(uint8_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -40);
  rglmdf_set_empty_count(&data, u8);
  if (verbose) fprintf(stdout, "Selected empty_count value: %u\n", u8);

  /* Reads the position_status_cnt, position_statuses, position_status_buffer input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -50);
  v64 = rglmdf_set_position_status_cnt(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for position_statuses array.\n");
    return EXIT_FAILURE;
  }
  cpp = rglmdf_get_position_statuses(&data);
  re = fread(*cpp, RGLM_POSITION_STATUS_BUF_SIZE, u64, ifp);
  if (re != u64) print_error_and_stop(i_arg, -60);
  if (verbose) rglmdf_position_statuses_to_text_stream(&data, stdout);

  /* Reads the feature_cnt, features input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -70);
  v64 = rglmdf_set_feature_cnt(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for features array.\n");
    return EXIT_FAILURE;
  }
  bfip = rglmdf_get_features(&data);
  for (size_t i = 0; i < u64; i++) {
    re = fread(&i16, sizeof(int16_t), 1, ifp);
    if (re != 1) print_error_and_stop(i_arg, -72);
    bfip[i] = i16;
  }
  if (verbose) rglmdf_features_to_text_stream(&data, stdout);

  /* Reads the pattern_cnt, patterns input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -74);
  v64 = rglmdf_set_pattern_cnt(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for patterns array.\n");
    return EXIT_FAILURE;
  }
  bpip = rglmdf_get_patterns(&data);
  for (size_t i = 0; i < u64; i++) {
    re = fread(&i16, sizeof(int16_t), 1, ifp);
    if (re != 1) print_error_and_stop(i_arg, -80);
    bpip[i] = i16;
  }
  if (verbose) rglmdf_patterns_to_text_stream(&data, stdout);

  /* Reads the position summary table. */
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -90);
  v64 = rglmdf_set_position_summary_ntuples(&data, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for the records of position summary table.\n");
    return EXIT_FAILURE;
  }
  psrp = rglmdf_get_position_summary_records(&data);
  re = fread(psrp, sizeof(rglmdf_position_summary_record_t), u64, ifp);
  if (re != u64) print_error_and_stop(i_arg, -100);
  if (verbose) rglmdf_position_summary_cnt_to_text_stream(&data, stdout);

  /* Reads the pattern frequency summary table. */
  re = fread(&u64_a, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -110);
  re = fread(&u64_b, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -112);
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -114);
  v64 = rglmdf_set_pattern_freq_summary_ntuples(&data, u64_a, u64_b, u64);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for the records of pattern freq summary table.\n");
    return EXIT_FAILURE;
  }
  pfsrp = rglmdf_get_pattern_freq_summary_records(&data);
  re = fread(pfsrp, sizeof(rglmdf_pattern_freq_summary_record_t), u64, ifp);
  if (re != u64) print_error_and_stop(i_arg, -120);
  if (verbose) rglmdf_pattern_freq_summary_cnt_to_text_stream(&data, stdout);

  /* Creates the mapping betweeen (pattern_id, principal_index_value) --> glm_variable_id */
  rglmdf_build_reverse_map(&data);
  if (verbose) fprintf(stdout, "The reverse map \"(pattern_id, principal_index_value) --> glm_variable_id\" has been computed.\n");

  /* Reads the iarrai data type. */
  re = fread(&u8, sizeof(uint8_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -130);
  /* Read the number of record for the solved and classified game position table. */
  re = fread(&u64, sizeof(uint64_t), 1, ifp);
  if (re != 1) print_error_and_stop(i_arg, -132);
  v64 = rglmdf_set_positions_ntuples(&data, u64, u8);
  if (v64 != u64) {
    fprintf(stderr, "Unable to allocate memory for the positions table.\n");
    return EXIT_FAILURE;
  }
  /* Reads the sequence of chunk of records. Each chunk is organized as: chunk size n, n records, n irecords. */
  scgprp = rglmdf_get_positions_records(&data);
  farrayp = rglmdf_get_positions_farray(&data);
  iarrayp = rglmdf_get_positions_iarray(&data);
  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(&data);
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(&data);
  size_t n_record_read = 0;
  for (;;) {
    re = fread(&data_chunk_size, sizeof(uint64_t), 1, ifp);
    if (re != 1) print_error_and_stop(i_arg, -140);
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
    if (re != data_chunk_size) print_error_and_stop(i_arg, -150);
    if (nf != 0) {
      re = fread(farrayp, sizeof(double) * nf, data_chunk_size, ifp);
      if (re != data_chunk_size) print_error_and_stop(i_arg, -160);
    }
    if (ni != 0) {
      re = fread(iarrayp, sizeof(uint32_t) * ni, data_chunk_size, ifp);
      if (re != data_chunk_size) print_error_and_stop(i_arg, -162);
    }
    scgprp += data_chunk_size;
    farrayp += nf * data_chunk_size;
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
    rglmdf_fpfs_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Feature and pattern frequencies summary table dumped to CSV file: \"%s\".\n", B_arg);
  }

  /* If P flag is turned on, dumps the game position table to the output file. */
  if (P_arg) {
    if (data.positions.iarray_data_type == RGLMDF_IARRAY_IS_INDEX) {
      ofp = fopen(P_arg, "w");
      if (!ofp) {
        fprintf(stderr, "Unable to open output file: %s\n", P_arg);
        return EXIT_FAILURE;
      }
      rglmdf_gp_table_to_csv_file(&data, ofp);
      fclose(ofp);
      if (verbose) fprintf(stdout, "Game positions dumped to CSV file: \"%s\".\n", P_arg);
    } else {
      fprintf(stdout, "Game positions are classified with a format that doesn't allow to recall the pattern configuration index values.\n");
    }
  }

  /* Transforms the pattern index values to the principal ones. */
  if (data.positions.iarray_data_type == RGLMDF_IARRAY_IS_INDEX) {
    rglmdf_transform_piv_to_glm_variable_id(&data, true);
    if (verbose) fprintf(stdout, "Pattern index values have been translated to principal ones.\n");
  }

  /* If Q flag is turned on, dumps the game position transformed table to the output file. */
  if (Q_arg) {
    if (data.positions.iarray_data_type == RGLMDF_IARRAY_IS_PRINCIPAL_INDEX) {
      ofp = fopen(Q_arg, "w");
      if (!ofp) {
        fprintf(stderr, "Unable to open output file: %s\n", Q_arg);
        return EXIT_FAILURE;
      }
      rglmdf_gp_table_to_csv_file(&data, ofp);
      fclose(ofp);
      if (verbose) fprintf(stdout, "Game positions, with pattern indexes being remapped to principal values, dumped to CSV file: \"%s\".\n", Q_arg);
    } else {
      fprintf(stdout, "Game positions are classified with a format that doesn't allow (currently) to recall the principal pattern configuration index values.\n");
    }
  }

  /* Transforms the pattern principal index values to the corresponding global GLM variable id, in the game position table. */
  if (data.positions.iarray_data_type == RGLMDF_IARRAY_IS_PRINCIPAL_INDEX) {
    rglmdf_transform_piv_to_glm_variable_id(&data, false);
    if (verbose) fprintf(stdout, "Pattern principal index values have been translated to global GLM variable id.\n");
  }

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

  if (s_flag || H_flag) {

    /* Max number of iterations allowed to the Newton-Raphson algorithm. */
    size_t max_newton_iter;

    /* Stopwatch variables. */
    timespec_t time_0, time_1, delta_cpu_time, start_time, end_time, delta_time;

    /* All the Algorithm variables. */
    double *v, *w, *e, *de, *r, *minus_grad_f, **big_b;
    double effe, effe_last, delta_effe, lambda, grad_magnitude, g_abs_min, g_abs_max, r_magnitude, p_magnitude;
    double r_abs_min, r_abs_max, p_abs_min, p_abs_max;
    size_t r_abs_min_pos, r_abs_max_pos, g_abs_min_pos, g_abs_max_pos, p_abs_min_pos, p_abs_max_pos;

    double epsilon_on_gradient_modulus;

    bool evf_already_computed;
    bool cholesky_fact_ok;

    /* enne: number of parameters to be fitted. */
    const size_t enne = data.pattern_freq_summary.ntuples;

    /* emme: number of solved and classified game positions. */
    const size_t emme = data.positions.ntuples;

    /*
     * DPOTRF/DPOTRS - Cholesky factorization and solution parameters.
     * Are all constant.
     */
    char chol_upper = 'L'; // dpotrf follows the fortran convention of column major storage, so L translated in raw major mode means upper.
    int ret = 0;           // Function return value. 0 means NO ERROR.
    int enne_int = enne;   // The rank of the matrix need to be passed as a pointer to int.
    int nrhs = 1;          // Number of right hand side columns.
    int tc = 8;            // Thread count.

    /* v: transformd game value for each solved position. */
    v = lial_allocate_vector(emme);
    if (!v) abort();
    rglmut_gv_init(&data, emme, v);

    /* w: weigths.*/
    w = lial_allocate_vector(enne);
    if (!w) abort();
    for (size_t i = 0; i < enne; i++) w[i] = data.pattern_freq_summary.records[i].weight;

    /* e: evaluation function for the game positions.*/
    e = lial_allocate_vector(emme);
    if (!e) abort();

    /* de: derivative of the evaluation function for the game positions, de = e * (1 - e).*/
    de = lial_allocate_vector(emme);
    if (!de) abort();

    /* r: residual value for the game positions, r = e - v.*/
    r = lial_allocate_vector(emme);
    if (!r) abort();

    /* minus_grad_f: right hand side of the liner system solved at each iteration of the Newton-Raphson algorithm. */
    minus_grad_f = lial_allocate_vector(enne);
    if (!minus_grad_f) abort();

    /* big_b: square matrix generated by the matrix multiplication jacobian transposed * jacobian. */
    big_b = lial_allocate_square_matrix(enne);
    if (!big_b) abort();

    /* Computes the constant v vector. */
    rglmut_gv_init(&data, emme, v);

    /* lambda: scalar parameter used by the Levemberg-Marquardt algorith. */
    lambda = 1.e-9;

    /* Termination criteria. */
    epsilon_on_gradient_modulus = 1.0e-9;

    /* max_newton_iter: max number of iterations allowed to the Newton-Raphson algorithm. */
    max_newton_iter = 12;

    /* At the beginning of the first iteration the evaluation function is not already computed. */
    evf_already_computed = false;

    /* At the beginning of the first iteration the flag is set to true. */
    cholesky_fact_ok = true;

    if (verbose) {
      printf("Dumping factor for the diagonal of the Hessian matrix (Levemberg-Marquardt): lambda = %e\n", lambda);
      printf("Max number of Newton-Raphson algorithm iterations = %zu\n", max_newton_iter);
      printf("Termination criteria:\n");
      printf("   Epsilon on modulus of gradient: %e\n", epsilon_on_gradient_modulus);
    }

    for (size_t iter = 0; iter < max_newton_iter; iter++) {

      if (verbose) {
        printf("Iteration[%03zu]:\n", iter);
        if (!cholesky_fact_ok) printf("   New value assigned to the lambda parameter: lambda = %e\n", lambda);
      }

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_REALTIME, &start_time);
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      if (!evf_already_computed) {
        rglmut_evaluation_function_eval(&data, enne, w, emme, e);
        rglmut_evaluation_function_derivative_eval(emme, e, de);
        rglmut_residual_value_eval(emme, e, v, r);
        rglmut_minus_grad_f_eval(&data, minus_grad_f, enne, emme, r, de);
        effe = 0.0;
        for (size_t i = 0; i < emme; i++) effe += r[i]*r[i];
        effe *= 0.5;
        r_magnitude = lial_vector_magnitude(r, emme, &r_abs_min, &r_abs_min_pos, &r_abs_max, &r_abs_max_pos);
        grad_magnitude = lial_vector_magnitude(minus_grad_f, enne, &g_abs_min, &g_abs_min_pos, &g_abs_max, &g_abs_max_pos);
      }

      rgmlut_big_b_eval(&data, big_b, enne, emme, e, de, v);

      /* Increases the diagonal of the Hessian matrix. */
      for (size_t i = 0; i < enne; i++) big_b[i][i] += lambda;

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
      clock_gettime(CLOCK_REALTIME, &end_time);

      if (verbose) {
        printf("   Effe             = %30.18f\n", effe);
        printf("   Residual modulus = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", r_magnitude, r_abs_min, r_abs_min_pos, r_abs_max, r_abs_max_pos);
        printf("   Gradient modulus = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", grad_magnitude, g_abs_min, g_abs_min_pos, g_abs_max, g_abs_max_pos);
      }

      /* Computes the time taken, and updates the test cpu_time. */
      timespec_diff(&delta_time, &start_time, &end_time);
      timespec_diff(&delta_cpu_time, &time_0, &time_1);
      if (verbose) {
        printf("   Function, Gradient, Hessian, and Residual evaluation CPU time: ");
        printf("[%6lld.%9ld][%6lld.%9ld]\n",
               (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
               (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
      }

      /* Dumps the modified Hessian matrix and terminates if flag H is on. */
      if (H_arg) {
        /* Copies the upper triangle into the lower one. */
        for (size_t i = 1; i < enne; i++) {
          for (size_t j = 0; j < i; j++) {
            big_b[i][j] = big_b[j][i];
          }
        }
        int h_ret_code;
        lial_dump_matrix(big_b, enne, enne, H_arg, &h_ret_code);
        if (h_ret_code != 0) abort();
        if (verbose) fprintf(stdout, "Hessian matrix dumped to binary file: \"%s\".\n", H_arg);
        goto end_of_optimization_loop;
      }

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_REALTIME, &start_time);
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Factorizes the symmetrical Hessian matrix applying the Cholesky decomposition. */
      lial_dpotrf_blis(&chol_upper, &enne_int, *big_b, &enne_int, &ret, tc);
      if (ret != 0) {
        cholesky_fact_ok = false;
        lambda *= 10.0;
      } else {
        cholesky_fact_ok = true;
      }

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
      clock_gettime(CLOCK_REALTIME, &end_time);

      /* Computes the time taken, and updates the test cpu_time. */
      timespec_diff(&delta_time, &start_time, &end_time);
      timespec_diff(&delta_cpu_time, &time_0, &time_1);
      if (verbose) {
        printf("   Cholesky Factorization %s, CPU time:                           ", cholesky_fact_ok ? "ok" : "ko");
        printf("[%6lld.%9ld][%6lld.%9ld]\n",
               (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
               (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
      }
      if (!cholesky_fact_ok) continue;

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_REALTIME, &start_time);
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Solve the linear system by back substitution. */
      lial_dpotrs(&chol_upper, &enne_int, &nrhs, *big_b, &enne_int, minus_grad_f, &enne_int, &ret);
      if (ret != 0) {
        fprintf(stderr, "lial_dpotrs return code = %d, aborting ...", ret);
        abort();
      }

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
      clock_gettime(CLOCK_REALTIME, &end_time);

      /* Computes the time taken, and updates the test cpu_time. */
      timespec_diff(&delta_time, &start_time, &end_time);
      timespec_diff(&delta_cpu_time, &time_0, &time_1);
      if (verbose) {
        printf("   Cholesky Solution CPU time:                                    ");
        printf("[%6lld.%9ld][%6lld.%9ld]\n",
               (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
               (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
      }

      p_magnitude = lial_vector_magnitude(minus_grad_f, enne, &p_abs_min, &p_abs_min_pos, &p_abs_max, &p_abs_max_pos);
      if (verbose) printf("   Delta w modulus  = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", p_magnitude, p_abs_min, p_abs_min_pos, p_abs_max, p_abs_max_pos);

      /* Updates the vector of weights with the delta. */
      for (size_t i = 0; i < enne; i++) w[i] += minus_grad_f[i];

      /* After the update of w, re-computes e, de, r, and minus_grad_f. */
      rglmut_evaluation_function_eval(&data, enne, w, emme, e);
      rglmut_evaluation_function_derivative_eval(emme, e, de);
      rglmut_residual_value_eval(emme, e, v, r);
      rglmut_minus_grad_f_eval(&data, minus_grad_f, enne, emme, r, de);
      effe_last = effe;
      effe = 0.0;
      for (size_t i = 0; i < emme; i++) effe += r[i]*r[i];
      effe *= 0.5;
      delta_effe = effe_last - effe;

      r_magnitude = lial_vector_magnitude(r, emme, &r_abs_min, &r_abs_min_pos, &r_abs_max, &r_abs_max_pos);
      grad_magnitude = lial_vector_magnitude(minus_grad_f, enne, &g_abs_min, &g_abs_min_pos, &g_abs_max, &g_abs_max_pos);

      if (verbose) printf("   Delta Effe       = %30.18f\n", delta_effe);

      evf_already_computed = true;

      if (grad_magnitude <= epsilon_on_gradient_modulus) {
        break;
      }
    }

    if (verbose) {
      r_magnitude = lial_vector_magnitude(r, emme, &r_abs_min, &r_abs_min_pos, &r_abs_max, &r_abs_max_pos);
      grad_magnitude = lial_vector_magnitude(minus_grad_f, enne, &g_abs_min, &g_abs_min_pos, &g_abs_max, &g_abs_max_pos);
      if (grad_magnitude <= epsilon_on_gradient_modulus) {
        printf("Termination criteria reached:\n");
      } else {
        printf("Max number of iterations reached:\n");
      }
      printf("   Effe             = %30.18f\n", effe);
      printf("   Residual modulus = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", r_magnitude, r_abs_min, r_abs_min_pos, r_abs_max, r_abs_max_pos);
      printf("   Gradient modulus = %30.18f; abs min = [%24.18f,%8zu]; abs max = [%24.18f,%8zu]\n", grad_magnitude, g_abs_min, g_abs_min_pos, g_abs_max, g_abs_max_pos);
    }

  end_of_optimization_loop:
    ;

    /* Copies the optimized weighs into the general data structure. */
    for (size_t i = 0; i < enne; i++) data.pattern_freq_summary.records[i].weight = w[i];

    /* Copies residual and game value into the general data structure. */
    rglmut_evaluation_function_eval(&data, enne, w, emme, e);
    for (size_t i = 0; i < emme; i++) {
      const double gvt = rglmut_gv_scale(data.positions.records[i].game_value);
      data.positions.records[i].game_value_transformed = gvt;
      data.positions.records[i].evaluation_function = e[i];
      data.positions.records[i].residual = e[i] - gvt;
    }

    lial_free_matrix(big_b, enne);
    lial_free_vector(minus_grad_f);
    lial_free_vector(r);
    lial_free_vector(de);
    lial_free_vector(e);
    lial_free_vector(w);
    lial_free_vector(v);

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
    rglmdf_fpfs_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Optimized Weights table dumped to CSV file: \"%s\".\n", W_arg);
  }

  /* If R flag is turned on, dumps the game position table to the output file. */
  if (R_arg) {
    ofp = fopen(R_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", R_arg);
      return EXIT_FAILURE;
    }
    rglmdf_gp_table_to_csv_file(&data, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "Game positions dumped to CSV file: \"%s\".\n", R_arg);
  }

  /* Writes the binary output file. */
  if (o_arg) {
    ret_code = rglmdf_write_general_data_to_binary_file(&data, o_arg);
    if (ret_code == EXIT_SUCCESS) {
      if (verbose) fprintf(stdout, "Binary output file written to %s, computed SHA3-256 digest, written to file %s.sha3-256.\n", o_arg, o_arg);
    } else {
      fprintf(stderr, "Unable to write correctly binary output file: %s\n", o_arg);
      return ret_code;
    }
  }

  /* Writes the parameters (weights) binary file. */
  if (b_arg) {
    ret_code = rglmdf_write_rglm_weights_to_binary_file(&data, b_arg);
    if (ret_code == EXIT_SUCCESS) {
      if (verbose) fprintf(stdout, "RGLM parameters binary file written to %s\n", b_arg);
    } else {
      fprintf(stderr, "Unable to write correctly RGLM parameters binary file: %s\n", b_arg);
      return ret_code;
    }
  }

  /* Frees resources. */
  rglmdf_general_data_release(&data);

  return EXIT_SUCCESS;
}
