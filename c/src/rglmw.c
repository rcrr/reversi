/**
 * @file
 *
 * @brief RGLM weights program.
 * @details Computes the residual, gaps, errors, between the true game values and
 *          the evaluation function.
 *          Two files are loaded, one having the solved game positions, and a
 *          second one having the selected patterns and the weights for all
 *          the pattern configurations.
 *          Gaps are computed and logged to a CSV file.
 *
 * @par rglmw.c
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


/* Static functions. */

//static double
double
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
  mop_options_t options;

  int h_flag = false;

  int v_flag = false;

  int w_flag = false;
  char *w_arg = NULL;

  mop_options_long_t opt_list[] =
    {
     {"help",                'h', MOP_NONE},
     {"verbose",             'v', MOP_NONE},
     {"weights-file",        'w', MOP_REQUIRED},
     {0, 0, 0}
    };

  const char *documentation =
    "Usage:\n"
    "rglmw [OPTION...] - Reversi Generalized Linear Model Weights tools\n"
    "\n"
    "Options:\n"
    "  -h, --help                Show help options\n"
    "  -v, --verbose             Verbose output\n"
    "  -w, --weights-file        Pattern configurations weights input file name - Mandatory\n"
    "\n"
    "Description:\n"
    "The Reversi Generalized Linear Model Weights program computes the gaps between the true value of game positions and the outcome of the evaluation function.\n"
    "\n"
    "Author:\n"
    "Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
    "\n"
    "Copyright (c) 2019, 2020 Roberto Corradini. All rights reserved.\n"
    "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
    "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
    ;

  bool verbose = false;

  int opt;
  int oindex = -1;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, opt_list, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
      break;
    case 'v':
      v_flag = true;
      break;
    case 'w':
      w_flag = true;
      w_arg = options.optarg;
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
  if (!w_flag) {
    fprintf(stderr, "Option -w, --weights-file is mandatory.\n");
    return EXIT_FAILURE;
  }

  if (verbose) fprintf(stdout, "RGLMW ....\n");

  rglmdf_model_weights_t model_weights;
  rglmdf_model_weights_t *const mw = &model_weights;
  rglmdf_model_weights_init(mw);
  rglmdf_model_weights_read_from_binary_file(mw, w_arg, verbose);

  return EXIT_SUCCESS;
}

/**
 * @brief Main entry for the RGLM ( Reversi Generalized Linear Model ) fit utility program.
 */
int
old_main (int argc,
          char *argv[])
{

  /*

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

  board_t b, tr;
  board_pattern_rotated_t r;
  //GamePositionX gpx, gpx1;

  */


  /**/
  //for (size_t i = 0; i < game_position_cnt; i++) {
  //  gpsp = &game_positions[i];
  //  board_set_square_sets(&b, gpsp->mover, gpsp->opponent);

    /*
    gpx.blacks = board_get_mover_square_set(&b);
    gpx.whites = board_get_opponent_square_set(&b);
    gpx.player = BLACK_PLAYER;
    game_position_x_print(buf, &gpx);
    printf("\ngp_id = %ld\n\n%s\n", gpsp->gp_id, buf);
    */

    /* Computes rotated boards. */
  //  board_pattern_compute_rotated(&b, &r);

    /* Computes pattern indexes. */
    //
  //  const board_pattern_t *bpp;
  //  board_pattern_index_t index_value;
  //  board_pattern_index_t *indexp;
  //  board_pattern_index_t indexes[BOARD_PATTERN_MAX_N_INSTANCES * BOARD_PATTERN_COUNT];
  //  memset(indexes, 0, sizeof(board_pattern_index_t) * BOARD_PATTERN_MAX_N_INSTANCES * BOARD_PATTERN_COUNT);
    //gpx1.player = BLACK_PLAYER;
  //  indexp = indexes;
  //   for (size_t j = 0; j < pattern_cnt; j++) {
  //    bpp = &board_patterns[board_pattern_ids[j]];
      //printf("[%s] n_instances=%d\n", bpp->name, bpp->n_instances);
  //    for (size_t k = 0; k < bpp->n_instances; k++) {
        //printf("  instance=%zu\n", k);

        /*
        gpx1.blacks = r.board_array[k].square_sets[0];
        gpx1.whites = r.board_array[k].square_sets[1];
        game_position_x_print(buf, &gpx1);
        printf("\ngp_id = %ld\n\n%s\n", gpsp->gp_id, buf);
        */

  //      tr.square_sets[0] = bpp->pattern_pack_f(r.board_array[k].square_sets[0]);
  //      tr.square_sets[1] = bpp->pattern_pack_f(r.board_array[k].square_sets[1]);

  //      index_value = board_pattern_packed_to_index(&tr, bpp->n_squares);
  //      *indexp++ = index_value;
  //    }
  //  }
  //  gpsp->game_value_transformed = rglmut_gv_scale(gpsp->game_value);
  //  gpsp->evaluation_function = evaluation_function(pattern_cnt, board_pattern_ids, indexes, pattern_to_weight_index);
  //  gpsp->residual = gpsp->evaluation_function - gpsp->game_value_transformed;
  //}

  /* Frees resources. */
  /*
  free(weights);
  free(board_pattern_ids);
  free(game_positions);
  free(position_statuses);
  free(position_statuses_buf);
  free(batch_ids);
  */

  return EXIT_SUCCESS;
}
