/**
 * @file
 *
 * @brief RGLM weights program.
 * @details Computes the residuals, gaps, errors, between the true game values and
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
 * @copyright 2019, 2020, 2021, 2022 Roberto Corradini. All rights reserved.
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

/**
 * @endcond
 */



/**
 * @brief Main entry for the RGLMW ( Reversi Generalized Linear Model Weights ) utility program.
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

  int p_flag = false;
  char *p_arg = NULL;

  int f_flag = false;

  int W_flag = false;
  char *W_arg = NULL;

  int P_flag = false;
  char *P_arg = NULL;

  mop_options_long_t opt_list[] =
    {
     {"help",                'h', MOP_NONE},
     {"verbose",             'v', MOP_NONE},
     {"weights-file",        'w', MOP_REQUIRED},
     {"positions-file",      'p', MOP_REQUIRED},
     {"force-eval",          'f', MOP_NONE},
     {"extract-weights",     'W', MOP_REQUIRED},
     {"extract-positions",   'P', MOP_REQUIRED},
     {0, 0, 0}
    };

  const char *documentation =
    "Usage:\n"
    "rglmw [OPTION...] - Reversi Generalized Linear Model Weights tools\n"
    "\n"
    "Options:\n"
    "  -h, --help                Show help options\n"
    "  -v, --verbose             Verbose output\n"
    "  -w, --weights-file        RGLM model weights input file name - Mandatory\n"
    "  -p, --positions-file      RGLM general data input file name\n"
    "  -f, --force-eval          Force the evaluation regardless the different empty_count values\n"
    "  -W, --extract-weights     Extract the model weights table in a CSV format\n"
    "  -P, --extract-positions   Extract the game positions table in a CSV format\n"
    "\n"
    "Description:\n"
    "The Reversi Generalized Linear Model Weights program computes the gaps between the true value of game positions and the outcome of the evaluation function.\n"
    "When the weights file and the positions one have the same empty_count value (depth = 0) the evaluation happens directly.\n"
    "When the depth is negative the program terminates. When it is positive, a minimax algorithm is applied with PLY = depth.\n"
    "If the -f, --force-eval, option is active, the evaluation happens anyway without search, notwithstanding the empty_count mismatch.\n"
    "\n"
    "Author:\n"
    "Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
    "\n"
    "Copyright (c) 2019, 2020, 2021, 2022 Roberto Corradini. All rights reserved.\n"
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
    case 'p':
      p_flag = true;
      p_arg = options.optarg;
      break;
    case 'f':
      f_flag = true;
      break;
    case 'W':
      W_flag = true;
      W_arg = options.optarg;
      break;
    case 'P':
      P_flag = true;
      P_arg = options.optarg;
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

  if (P_flag && !p_flag) {
    fprintf(stderr, "Option -P, --extract-positions, requires that option -p, --positions-file, is selected.\n");
    return EXIT_FAILURE;
  }

  /* Checks size of types. */
  if (!rglmdf_verify_type_sizes()) {
    fprintf(stderr, "Data types read from, or written to, the binary file have a size that is not consistent with the original one.\n");
    return EXIT_FAILURE;
  }

  int ret_code = 0;

  /* Loads the RGLM model weights structure from the binary input data file. */
  rglmdf_model_weights_t model_weights;
  rglmdf_model_weights_t *const mw = &model_weights;
  rglmdf_model_weights_init(mw);
  ret_code = rglmdf_model_weights_read_from_binary_file(mw, w_arg, verbose, true);
  if (ret_code != EXIT_SUCCESS) {
    fprintf(stderr, "Error while reading the RGLM model weights binary input file.\n");
    return EXIT_FAILURE;
  }

  /* If W flag is turned on, dumps the model weights table to the CSV output file. */
  if (W_flag) {
    FILE *ofp = fopen(W_arg, "w");
    if (!ofp) {
      fprintf(stderr, "Unable to open output file: %s\n", W_arg);
      return EXIT_FAILURE;
    }
    rglmdf_model_weights_table_to_csv_file(mw, ofp);
    fclose(ofp);
    if (verbose) fprintf(stdout, "RGLM model weights table exported to CSV file: \"%s\".\n", W_arg);
  }

  if (p_flag) {

    /* Prepares the empty RGLM GENERAL DATA data structure, to host the game positions. */
    rglmdf_general_data_t general_data;
    rglmdf_general_data_t *const gd = &general_data;
    rglmdf_general_data_init(gd);

    /* Reads the game positions binary input file. */
    ret_code = rglmdf_read_general_data_from_binary_file(gd, p_arg, verbose);
    if (ret_code != EXIT_SUCCESS) {
      fprintf(stderr, "Unable to read properly the game positions binary input file: %s\n", p_arg);
      return EXIT_FAILURE;
    }

    /* If the game positions are a GENERAL format, transform it into a POSITION one. */
    const rglmdf_file_data_format_type_t format = rglmdf_get_format(gd);
    if (format == RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL) {
      rglmdf_transform_format_from_general_to_positions(gd);
      if (verbose) fprintf(stdout, "RGLM general data format transformed from GENERAL to POSITIONS.\n");
    }

    /* empty_count_w */
    const int empty_count_w = model_weights.empty_count;
    const int empty_count_p = rglmdf_get_empty_count(gd);
    const int eval_depth = empty_count_p - empty_count_w;
    printf("empty_count_w = %d\n", empty_count_w);
    printf("empty_count_p = %d\n", empty_count_p);
    printf("eval_depth    = %d\n", eval_depth);
    printf("f_flag        = %d\n", f_flag);
    double (*eval_gp_f) (const rglmdf_model_weights_t *, const board_t *);

    if (f_flag) {
      eval_gp_f = rglmut_eval_gp_using_model_weights;
    } else {
      if (eval_depth == 0) {
        eval_gp_f = rglmut_eval_gp_using_model_weights;
      } else if (eval_depth > 0) {
        eval_gp_f = rglmut_eval_gp_negascout;
      } else {
        fprintf(stderr, "Error, negative eval_depth value. eval_depth = : %d\n", eval_depth);
        return EXIT_FAILURE;
      }
    }

    /* npos is the count of game position that we have in the general data structure. */
    const size_t npos = rglmdf_get_positions_ntuples(gd);

    /* gp_records is the array of game position records. */
    rglmdf_solved_and_classified_gp_record_t *const gp_records = rglmdf_get_positions_records(gd);

    /* Loop on the array of game position to be evaluated. */
    for (size_t i = 0; i < npos; i++) {
      rglmdf_solved_and_classified_gp_record_t *const gp_record = &gp_records[i];

      board_t b;
      board_set_square_sets(&b, gp_record->mover, gp_record->opponent);

      /* Evaluate the position. */
      gp_record->evaluation_function = eval_gp_f(mw, &b);
      gp_record->residual = gp_record->game_value_transformed - gp_record->evaluation_function; // resiual := observed - predicted
    }

    /* If P flag is turned on, dumps the game position table to the output file. */
    if (P_flag) {
      FILE *ofp = fopen(P_arg, "w");
      if (!ofp) {
        fprintf(stderr, "Unable to open output file: %s\n", P_arg);
        return EXIT_FAILURE;
      }
      rglmdf_gp_table_to_csv_file(gd, ofp);
      fclose(ofp);
      if (verbose) fprintf(stdout, "Game positions dumped to CSV file: \"%s\".\n", P_arg);
    }

    rglmdf_general_data_release(gd);

  } // End of p_flag

  rglmdf_model_weights_release(mw);

  return EXIT_SUCCESS;
}
