/**
 * @file
 *
 * @brief RGLM fit utility.
 * @details Computes the residual, gaps, errors, between the true game values and
 *          the evaluation function.
 *          Two files are loaded, one having the solved game positions, and a
 *          second one having the selected patterns and the weights for all
 *          the pattern configurations.
 *          Gaps are computed and logged to a CSV file.
 *
 * @par rglm_fit_utility.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2019, 2020 Roberto Corradini. All rights reserved.
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
#include <string.h>
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

static mop_options_t options;

static int h_flag = false;

static int v_flag = false;

static int p_flag = false;
static char *p_arg = NULL;

static int w_flag = false;
static char *w_arg = NULL;

static int R_flag = false;
static char *R_arg = NULL;

static mop_options_long_t opt_list[] =
  {
   {"help",                'h', MOP_NONE},
   {"verbose",             'v', MOP_NONE},
   {"game-positions-file", 'p', MOP_REQUIRED},
   {"weights-file",        'w', MOP_REQUIRED},
   {"gaps-output-file",    'g', MOP_REQUIRED},
   {"extract-residuals",   'R', MOP_REQUIRED},
   {0, 0, 0}
  };

static const char *documentation =
  "Usage:\n"
  "rglm_fit_utility [OPTION...] - Reversi Generalized Linear Model gaps calculator utility\n"
  "\n"
  "Options:\n"
  "  -h, --help                Show help options\n"
  "  -v, --verbose             Verbose output\n"
  "  -p, --game-positions-file Game positions input file name - Mandatory\n"
  "  -w, --weights-file        Pattern configurations weights input file name - Mandatory\n"
  "  -R, --extract-residuals   Dumps the residuals of the game positions computed by the evaluation function in a CSV format\n"
  "\n"
  "Description:\n"
  "The Reversi Generalized Linear Model fit utility computes the gaps between the true value of game positions and the outcome of the evaluation function.\n"
  "\n"
  "Author:\n"
  "Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2019, 2020 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;

/* Static functions. */
static void
print_error_and_stop (int ret_code,
                      char *file_name)
{
  fprintf(stderr, "rglm_fit_utility: format error %d reading file %s\n", ret_code, file_name);
  exit(ret_code);
}

static double
evaluation_function (size_t pattern_cnt,
                     board_pattern_id_t *board_pattern_ids,
                     board_pattern_index_t *indexes,
                     double **pattern_to_weight_index)
{
  double sum;
  const board_pattern_t *bpp;
  board_pattern_index_t index_value;

  sum = 0.0;

  for (size_t i = 0; i < pattern_cnt; i++) {
    bpp = &board_patterns[board_pattern_ids[i]];
    for (size_t k = 0; k < bpp->n_instances; k++) {
      index_value = *indexes++;
      sum += pattern_to_weight_index[board_pattern_ids[i]][index_value];
    }
  }

  return rglmut_logistic_function(sum);
}

/**
 * @endcond
 */



/**
 * @brief Main entry for the RGLM ( Reversi Generalized Linear Model ) fit utility program.
 */
int
main (int argc,
      char *argv[])
{
  FILE *pfp, *wfp, *ofp;

  size_t n, s, re;

  char buf[512];
  uint64_t u64;
  uint8_t u8;
  int16_t i16;

  time_t file_creation_time;
  struct tm file_creation_time_tm;

  size_t batch_id_cnt;
  uint64_t *batch_ids;
  uint8_t empty_count;
  size_t position_status_cnt;
  char *position_statuses_buf;
  char **position_statuses;
  size_t feature_cnt;
  size_t pattern_cnt;
  rglmdf_iarray_data_type_t iarray_type;
  size_t game_position_cnt;
  board_feature_id_t *board_feature_ids;
  board_pattern_id_t *board_pattern_ids;
  double *weights;
  double *pattern_to_weight_index[BOARD_PATTERN_COUNT];
  size_t data_chunk_size;
  size_t n_record_read = 0;
  rglmdf_solved_and_classified_gp_record_t *game_positions;
  rglmdf_solved_and_classified_gp_record_t *gpsp;

  bool verbose = false;

  int opt;
  int oindex = -1;

  ofp = NULL;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, opt_list, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
      break;
    case 'v':
      v_flag = true;
      break;
    case 'p':
      p_flag = true;
      p_arg = options.optarg;
      break;
    case 'w':
      w_flag = true;
      w_arg = options.optarg;
      break;
    case 'R':
      R_flag = true;
      R_arg = options.optarg;
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
  if (!p_arg) {
    fprintf(stderr, "Option -p, --game-positions-file is mandatory.\n");
    return -1;
  }
  if (!w_arg) {
    fprintf(stderr, "Option -w, --weights-file is mandatory.\n");
    return -2;
  }

  /* Checks size of types. */
  if (!rglmdf_verify_type_sizes()) {
    fprintf(stderr, "Data types read from, or written to, the binary file have a size that is not consistent with the original one.\n");
    return EXIT_FAILURE;
  }

  /* Opens the game positions binary file for reading. */
  pfp = fopen(p_arg, "r");
  if (!pfp) {
    fprintf(stderr, "Error: unable to open the game positions file %s\n", p_arg);
    return EXIT_FAILURE;
  }

  /* Reads the game positions file creation time field. */
  re = fread(&u64, sizeof(uint64_t), 1, pfp);
  if (re != 1) print_error_and_stop(-110, p_arg);
  if (verbose) {
    file_creation_time = (time_t) u64;
    gmtime_r(&file_creation_time, &file_creation_time_tm);
    asctime_r(&file_creation_time_tm, buf);
    fprintf(stdout, "Game Positions input file started to be written on (UTC) %s", buf);
  }

  /* Reads the batch_id_cnt, batch_ids input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, pfp);
  if (re != 1) print_error_and_stop(-120, p_arg);
  batch_id_cnt = u64;
  batch_ids = (uint64_t *) malloc(sizeof(uint64_t) * batch_id_cnt);
  if (!batch_ids) {
    fprintf(stderr, "Error: unable to allocate memory for the batch_ids array.\n");
    return EXIT_FAILURE;
  }
  re = fread(batch_ids, sizeof(uint64_t), batch_id_cnt, pfp);
  if (re != batch_id_cnt) print_error_and_stop(-130, p_arg);
  if (verbose) {
    fprintf(stdout, "Selected batch_id values: ");
    for (size_t i = 0; i < batch_id_cnt; i++ ) {
      fprintf(stdout, "%zu", batch_ids[i]);
      fprintf(stdout, "%s", (i < batch_id_cnt - 1) ? ", ": "\n");
    }
  }

  /* Reads the empty_count input field.*/
  re = fread(&u8, sizeof(uint8_t), 1, pfp);
  if (re != 1) print_error_and_stop(-140, p_arg);
  empty_count = u8;
  if (verbose) fprintf(stdout, "Selected empty_count value: %u\n", empty_count);

  /* Reads the position_status_cnt, position_statuses, position_status_buffer input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, pfp);
  if (re != 1) print_error_and_stop(-150, p_arg);
  position_status_cnt = u64;
  position_statuses_buf = (char *) malloc(sizeof(char) * RGLMDF_POSITION_STATUS_BUF_SIZE * position_status_cnt);
  position_statuses = (char **) malloc(sizeof(char *) * position_status_cnt);
  if (!position_statuses_buf || !position_statuses) {
    fprintf(stderr, "Error: unable to allocate memory for the position_statuses arrays.\n");
    return EXIT_FAILURE;
  }
  for (size_t i = 0; i < position_status_cnt; i++ )
    position_statuses[i] = position_statuses_buf + RGLMDF_POSITION_STATUS_BUF_SIZE * i;
  re = fread(position_statuses_buf, RGLMDF_POSITION_STATUS_BUF_SIZE, position_status_cnt, pfp);
  if (verbose) {
    fprintf(stdout, "Selected position_statuses values: ");
    for (size_t i = 0; i < position_status_cnt; i++ ) {
      fprintf(stdout, "%s", position_statuses[i]);
      fprintf(stdout, "%s", (i < position_status_cnt - 1) ? ", ": "\n");
    }
  }

  /* Reads the count of features. */
  re = fread(&u64, sizeof(uint64_t), 1, pfp);
  if (re != 1) print_error_and_stop(-160, p_arg);
  feature_cnt = u64;
  if (feature_cnt != 0) {
    fprintf(stderr, "feature_cnt = %zu\n", feature_cnt);
    fprintf(stderr, "Error: feature_cnt has to be zero, this file is not compatible with the option --game-positions.\n");
    return EXIT_FAILURE;
  }

  /* Reads the count of patterns. */
  re = fread(&u64, sizeof(uint64_t), 1, pfp);
  if (re != 1) print_error_and_stop(-164, p_arg);
  pattern_cnt = u64;
  if (pattern_cnt != 0) {
    fprintf(stderr, "pattern_cnt = %zu\n", pattern_cnt);
    fprintf(stderr, "Error: pattern_cnt has to be zero, this file is not compatible with the option --game-positions.\n");
    return EXIT_FAILURE;
  }

  /* Reads the iarrai data type. */
  re = fread(&u8, sizeof(uint8_t), 1, pfp);
  if (re != 1) print_error_and_stop(-170, p_arg);
  iarray_type = u8;
  if (iarray_type != RGLMDF_IARRAY_IS_MISSING) {
    fprintf(stderr, "iarray_type = %d\n", iarray_type);
    fprintf(stderr, "Error: iarray data type must be RGLMDF_IARRAY_IS_MISSING, this file is not compatible with the option --game-positions.\n");
    return EXIT_FAILURE;
  }

  /* Reads the count of game positions. */
  re = fread(&u64, sizeof(uint64_t), 1, pfp);
  if (re != 1) print_error_and_stop(-170, p_arg);
  game_position_cnt = u64;

  /* Reads game position records. */
  s = sizeof(rglmdf_solved_and_classified_gp_record_t) * game_position_cnt;
  game_positions = (rglmdf_solved_and_classified_gp_record_t *) malloc(s);
  if (!game_positions) {
    fprintf(stderr, "Error: unable to allocate memory for the game_positions array.\n");
    return EXIT_FAILURE;
  }
  memset(game_positions, 0, s);
  gpsp = game_positions;
  for (;;) {
    re = fread(&data_chunk_size, sizeof(uint64_t), 1, pfp);
    if (re != 1) print_error_and_stop(-180, p_arg);
    n_record_read += data_chunk_size;
    if (n_record_read > game_position_cnt) {
      fprintf(stderr, "Data chunks cumulated so far are more than expected.\n");
      return EXIT_FAILURE;
    }
    if (data_chunk_size == 0) {
      if (n_record_read != game_position_cnt) {
        fprintf(stderr, "Data chunks being read are less than expected.\n");
        fprintf(stderr, "Expected = %lu, number of record read = %zu\n", game_position_cnt, n_record_read);
        return EXIT_FAILURE;
      }
      break;
    }
    re = fread(gpsp, sizeof(rglmdf_solved_and_classified_gp_record_t), data_chunk_size, pfp);
    if (re != data_chunk_size) print_error_and_stop(-190, p_arg);
    gpsp += data_chunk_size;
  }
  if (verbose) fprintf(stdout, "Succesfully read %zu records of game positions.\n", game_position_cnt);

  fclose(pfp);

  /* Opens the weights binary file for reading. */
  wfp = fopen(w_arg, "r");
  if (!wfp) {
    fprintf(stderr, "Error: unable to open the weights file %s\n", w_arg);
    return EXIT_FAILURE;
  }

  /* Reads the weights file creation time field. */
  re = fread(&u64, sizeof(uint64_t), 1, wfp);
  if (re != 1) print_error_and_stop(-10, w_arg);
  if (verbose) {
    file_creation_time = (time_t) u64;
    gmtime_r(&file_creation_time, &file_creation_time_tm);
    asctime_r(&file_creation_time_tm, buf);
    fprintf(stdout, "Weights input file started to be written on (UTC) %s", buf);
  }

  /* Reads the empty_count input field.*/
  re = fread(&u8, sizeof(uint8_t), 1, wfp);
  if (re != 1) print_error_and_stop(-20, w_arg);
  empty_count = u8;
  if (verbose) fprintf(stdout, "Selected empty_count value: %u\n", empty_count);

  /* Reads the feature_cnt, features input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, wfp);
  if (re != 1) print_error_and_stop(-24, w_arg);
  feature_cnt = u64;
  board_feature_ids = (board_feature_id_t *) malloc(sizeof(board_feature_id_t) * feature_cnt);
  if (!board_feature_ids) {
    fprintf(stderr, "Error: unable to allocate memory for the board_feature_ids array.\n");
    return EXIT_FAILURE;
  }
  for (size_t i = 0; i < feature_cnt; i++) {
    re = fread(&i16, sizeof(int16_t), 1, wfp);
    if (re != 1) print_error_and_stop(-26, w_arg);
    board_feature_ids[i] = i16;
  }
  if (verbose) {
    fprintf(stdout, "Selected feature values: ");
    for (size_t i = 0; i < feature_cnt; i++) {
      fprintf(stdout, "%s", board_features[board_feature_ids[i]].name);
      fprintf(stdout, "%s", (i < feature_cnt - 1) ? ", ": "\n");
    }
  }

  /* Reads the pattern_cnt, patterns input fields.*/
  re = fread(&u64, sizeof(uint64_t), 1, wfp);
  if (re != 1) print_error_and_stop(-30, w_arg);
  pattern_cnt = u64;
  board_pattern_ids = (board_pattern_id_t *) malloc(sizeof(board_pattern_id_t) * pattern_cnt);
  if (!board_pattern_ids) {
    fprintf(stderr, "Error: unable to allocate memory for the board_pattern_ids array.\n");
    return EXIT_FAILURE;
  }
  for (size_t i = 0; i < pattern_cnt; i++) {
    re = fread(&i16, sizeof(int16_t), 1, wfp);
    if (re != 1) print_error_and_stop(-40, w_arg);
    board_pattern_ids[i] = i16;
  }
  if (verbose) {
    fprintf(stdout, "Selected pattern values: ");
    for (size_t i = 0; i < pattern_cnt; i++) {
      fprintf(stdout, "%s", board_patterns[board_pattern_ids[i]].name);
      fprintf(stdout, "%s", (i < pattern_cnt - 1) ? ", ": "\n");
    }
  }

  /* Initializes the pattern_to_weight_index array. */
  for (size_t i = 0; i < BOARD_PATTERN_COUNT; i++) {
    pattern_to_weight_index[i] = NULL;
  }

  /* Reads the weights. */
  n = 0;
  for (size_t i = 0; i < pattern_cnt; i++) {
    n += board_patterns[board_pattern_ids[i]].n_configurations;
  }
  weights = (double *) malloc(sizeof(double) * n);
  if (!weights) {
    fprintf(stderr, "Error: unable to allocate memory for the weights array.\n");
    return EXIT_FAILURE;
  }
  re = fread(weights, sizeof(double), n, wfp);
  n = 0;
  for (size_t i = 0; i < pattern_cnt; i++) {
    pattern_to_weight_index[board_pattern_ids[i]] = weights + n;
    n += board_patterns[board_pattern_ids[i]].n_configurations;
  }

  fclose(wfp);

  if (0) {
    size_t idx = 0;
    for (size_t i = 0; i < pattern_cnt; i++) {
      for (size_t j = 0; j < board_patterns[board_pattern_ids[i]].n_configurations; j++) {
        printf("[%3zu-%6s,%6zu,%8zu] = %0+11.8f\n", i, board_patterns[board_pattern_ids[i]].name, j, idx, weights[idx]);
        idx++;
      }
    }
  }

  board_t b, tr;
  board_pattern_rotated_t r;
  //GamePositionX gpx, gpx1;

  /* If R flag is turned on, dumps the game position table to the output file. */
  if (R_arg) {
    ofp = fopen(R_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", R_arg);
      return EXIT_FAILURE;
    }
    fprintf(ofp, "       I;    ROW_N;      GP_ID;                MOVER;             OPPONENT; GAME_VALUE; GAME_VALUE_TRANSFORMED; EVALUATION_FUNCTION;         RESIDUAL\n");
  }

  /**/
  for (size_t i = 0; i < game_position_cnt; i++) {
    gpsp = &game_positions[i];
    board_set_square_sets(&b, gpsp->mover, gpsp->opponent);

    /*
    gpx.blacks = board_get_mover_square_set(&b);
    gpx.whites = board_get_opponent_square_set(&b);
    gpx.player = BLACK_PLAYER;
    game_position_x_print(buf, &gpx);
    printf("\ngp_id = %ld\n\n%s\n", gpsp->gp_id, buf);
    */

    /* Computes rotated boards. */
    board_pattern_compute_rotated(&b, &r);

    /* Computes pattern indexes. */
    //
    const board_pattern_t *bpp;
    board_pattern_index_t index_value;
    board_pattern_index_t *indexp;
    board_pattern_index_t indexes[BOARD_PATTERN_MAX_N_INSTANCES * BOARD_PATTERN_COUNT];
    memset(indexes, 0, sizeof(board_pattern_index_t) * BOARD_PATTERN_MAX_N_INSTANCES * BOARD_PATTERN_COUNT);
    //gpx1.player = BLACK_PLAYER;
    indexp = indexes;
    for (size_t j = 0; j < pattern_cnt; j++) {
      bpp = &board_patterns[board_pattern_ids[j]];
      //printf("[%s] n_instances=%d\n", bpp->name, bpp->n_instances);
      for (size_t k = 0; k < bpp->n_instances; k++) {
        //printf("  instance=%zu\n", k);

        /*
        gpx1.blacks = r.board_array[k].square_sets[0];
        gpx1.whites = r.board_array[k].square_sets[1];
        game_position_x_print(buf, &gpx1);
        printf("\ngp_id = %ld\n\n%s\n", gpsp->gp_id, buf);
        */

        tr.square_sets[0] = bpp->pattern_pack_f(r.board_array[k].square_sets[0]);
        tr.square_sets[1] = bpp->pattern_pack_f(r.board_array[k].square_sets[1]);

        index_value = board_pattern_packed_to_index(&tr, bpp->n_squares);
        *indexp++ = index_value;
      }
    }
    gpsp->game_value_transformed = rglmut_gv_scale(gpsp->game_value);
    gpsp->evaluation_function = evaluation_function(pattern_cnt, board_pattern_ids, indexes, pattern_to_weight_index);
    gpsp->residual = gpsp->evaluation_function - gpsp->game_value_transformed;
    if (R_arg) {
      fprintf(ofp, "%8zu; %8zu; %10ld; %20ld; %20ld; %10d; %22.12f; %19.12f; %+16.12f\n",
              i, gpsp->row_n, gpsp->gp_id, gpsp->mover, gpsp->opponent, gpsp->game_value,
              gpsp->game_value_transformed, gpsp->evaluation_function, gpsp->residual);
    }
  }

  /* If R flag is turned on, closes the output file. */
  if (R_arg) {
    fclose(ofp);
    if (verbose) fprintf(stdout, "Game positions dumped to CSV file: \"%s\".\n", R_arg);
  }

  /* Frees resources. */
  free(weights);
  free(board_pattern_ids);
  free(game_positions);
  free(position_statuses);
  free(position_statuses_buf);
  free(batch_ids);

  return EXIT_SUCCESS;
}
