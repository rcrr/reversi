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
 * @copyright 2018 Roberto Corradini. All rights reserved.
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

#include "main_option_parse.h"
#include "board_pattern.h"
#include "rglm_data_files.h"



/**
 * @cond
 */

/* Static constants. */



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int v_flag = false;

static int i_flag = false;
static char *i_arg = NULL;

static mop_options_long_t olist[] = {
  {"help",       'h', MOP_NONE},
  {"verbose",    'v', MOP_NONE},
  {"input-file", 'i', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "read_game_tree_log [OPTION...] - Loads a Game Tree Log dump file\n"
  "\n"
  "Options:\n"
  "  -h, --help       Show help options\n"
  "  -v, --verbose    Verbose output\n"
  "  -i, --input-file Input file name - Mandatory\n"
  "\n"
  "Description:\n"
  "To be completed ...\n"
  "\n"
  "Author:\n"
  "Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2018 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;

/**
 * @endcond
 */


static void
print_error_and_stop (int ret_code)
{
  fprintf(stderr, "rglm: format error reading file %s\n", i_arg);
  exit(ret_code);
}



/**
 * @brief Main entry for the RGLM ( Reversi Generalized Linear Model ) program.
 */
int
main (int argc, char *argv[])
{
  time_t current_time = (time_t) -1;
  char* c_time_string = NULL;
  size_t batch_id_cnt = 0;
  uint64_t *batch_ids = NULL;
  uint8_t empty_count = 0;
  size_t position_status_cnt = 0;
  char *position_status_buffer = NULL;
  char **position_statuses = NULL;
  size_t pattern_cnt = 0;
  board_pattern_id_t *patterns = NULL;

  regab_ext_cnt_pos_table_t position_summary;
  position_summary.ntuples = 0;
  position_summary.records = NULL;

  regab_ext_cnt_pattern_freq_table_t pattern_freq_summary;
  pattern_freq_summary.ntuples = 0;
  pattern_freq_summary.records = NULL;

  regab_ext_solved_and_classified_gp_table_t scgp_data;
  scgp_data.ntuples = 0;

  size_t solved_classified_gp_cnt = 0;

  bool verbose = false;

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
    case 'i':
      i_flag = true;
      i_arg = options.optarg;
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

  /* Opens the binary file for reading. */
  FILE *ifp = fopen(i_arg, "r");
  assert(ifp);

  re = fread(&current_time, sizeof(time_t), 1, ifp);
  if (re != 1) print_error_and_stop(-10);

  /* Convert to local time format. */
  c_time_string = ctime(&current_time);
  assert(c_time_string);

  /* ctime() has already added a terminating newline character. */
  if (verbose) fprintf(stdout, "Input file started to be written on %s", c_time_string);

  /* Reads the batch_id_cnt, batch_ids input fields.*/
  re = fread(&batch_id_cnt, sizeof(size_t), 1, ifp);
  if (re != 1) print_error_and_stop(-20);
  batch_ids = (uint64_t *) malloc(sizeof(uint64_t) * batch_id_cnt);
  if (!batch_ids) {
    fprintf(stderr, "Unable to allocate memory for batch_ids array.\n");
    return EXIT_FAILURE;
  }
  re = fread(batch_ids, sizeof(uint64_t), batch_id_cnt, ifp);
  if (re != batch_id_cnt) print_error_and_stop(-30);
  if (verbose) {
    fprintf(stdout, "Selected batch_id values: ");
    for (size_t i = 0; i < batch_id_cnt; i++ ) {
      fprintf(stdout, "%zu", batch_ids[i]);
      fprintf(stdout, "%s", (i < batch_id_cnt - 1) ? ", ": "\n");
    }
  }

  /* Reads the empty_count input field.*/
  re = fread(&empty_count, sizeof(uint8_t), 1, ifp);
  if (re != 1) print_error_and_stop(-40);
  if (verbose) fprintf(stdout, "Selected empty_count value: %u\n", empty_count);

  /* Reads the position_status_cnt, position_statuses, position_status_buffer input fields.*/
  re = fread(&position_status_cnt, sizeof(size_t), 1, ifp);
  if (re != 1) print_error_and_stop(-50);
  position_status_buffer = (char *) malloc(4 * position_status_cnt); // status has length 3, plus one for string termination.
  if (!position_status_buffer) {
    fprintf(stderr, "Unable to allocate memory for position_status_buffer array.\n");
    return EXIT_FAILURE;
  }
  re = fread(position_status_buffer, 4, position_status_cnt, ifp);
  if (re != position_status_cnt) print_error_and_stop(-60);
  position_statuses = (char **) malloc(sizeof(char *) * position_status_cnt);
  if (!position_statuses) {
    fprintf(stderr, "Unable to allocate memory for position_statuses array.\n");
    return EXIT_FAILURE;
  }
  for (size_t i = 0; i < position_status_cnt; i++ ) {
    position_statuses[i] = position_status_buffer + 4 * i;
  }
  if (verbose) {
    fprintf(stdout, "Selected position_statuses values: ");
    for (size_t i = 0; i < position_status_cnt; i++ ) {
      fprintf(stdout, "%s", position_statuses[i]);
      fprintf(stdout, "%s", (i < position_status_cnt - 1) ? ", ": "\n");
    }
  }

  /* Reads the pattern_cnt, patterns input fields.*/
  re = fread(&pattern_cnt, sizeof(size_t), 1, ifp);
  if (re != 1) print_error_and_stop(-70);
  patterns = (board_pattern_id_t *) malloc(sizeof(board_pattern_id_t) * pattern_cnt);
  if (!patterns) {
    fprintf(stderr, "Unable to allocate memory for patterns array.\n");
    return EXIT_FAILURE;
  }
  re = fread(patterns, sizeof(board_pattern_id_t), pattern_cnt, ifp);
  if (re != pattern_cnt) print_error_and_stop(-80);
  if (verbose) {
    fprintf(stdout, "Selected patterns values: ");
    for (size_t i = 0; i < pattern_cnt; i++ ) {
      fprintf(stdout, "%s", board_patterns[patterns[i]].name);
      fprintf(stdout, "%s", (i < pattern_cnt - 1) ? ", ": "\n");
    }
  }

  /* Reads the position summary table. */
  re = fread(&position_summary.ntuples, sizeof(size_t), 1, ifp);
  if (re != 1) print_error_and_stop(-90);
  position_summary.records = (regab_ext_cnt_pos_record_t *) malloc(sizeof(regab_ext_cnt_pos_record_t) * position_summary.ntuples);
  if (!position_summary.records) {
    fprintf(stderr, "Unable to allocate memory for position_summary.records array.\n");
    abort();
  }
  re = fread(position_summary.records, sizeof(regab_ext_cnt_pos_record_t), position_summary.ntuples, ifp);
  if (re != position_summary.ntuples) print_error_and_stop(-100);
  for (size_t i = 0; i < position_summary.ntuples; i++) solved_classified_gp_cnt += position_summary.records[i].classified_cnt;
  if (verbose) fprintf(stdout, "Position summary table has been read succesfully, solved_classified_gp_cnt: %zu\n", solved_classified_gp_cnt);

  /* Reads the pattern frequency summary table. */
  re = fread(&pattern_freq_summary.ntuples, sizeof(size_t), 1, ifp);
  if (re != 1) print_error_and_stop(-110);
  pattern_freq_summary.records = (regab_ext_cnt_pattern_freq_record_t *) malloc(sizeof(regab_ext_cnt_pattern_freq_record_t) * pattern_freq_summary.ntuples);
  if (!pattern_freq_summary.records) {
    fprintf(stderr, "Unable to allocate memory for pattern_freq_summary.records array.\n");
    abort();
  }
  re = fread(pattern_freq_summary.records, sizeof(regab_ext_cnt_pattern_freq_record_t), pattern_freq_summary.ntuples, ifp);
  if (re != pattern_freq_summary.ntuples) print_error_and_stop(-120);
  if (verbose) fprintf(stdout, "Pattern summary table has been read succesfully, records count: %zu\n", pattern_freq_summary.ntuples);

  //printf("glm_variable_id;pattern_id;principal_index_value;total_cnt;relative_frequency;theoretical_frequency\n");
  int pattern_id = -1;
  int64_t total_cnt = -1;
  double relative_frequency = -1.;
  double theoretical_probability = -1.;
  for (size_t i = 0; i < pattern_freq_summary.ntuples; i++) {
    regab_ext_cnt_pattern_freq_record_t *rec = &pattern_freq_summary.records[i];
    if (rec->pattern_id != pattern_id) {
      total_cnt = 0;
      relative_frequency = 0.;
      theoretical_probability = 0.;
      pattern_id = rec->pattern_id;
    }
    total_cnt += rec->total_cnt;
    relative_frequency += rec->relative_frequency;
    theoretical_probability += rec->theoretical_probability;
    //printf("%lld;%d;%d;%ld;%f;%f\n", rec->glm_variable_id, rec->pattern_id,rec->principal_index_value,rec->total_cnt,rec->relative_frequency,rec->theoretical_probability);
    if (verbose && (i + 1 == pattern_freq_summary.ntuples || (rec + 1)->pattern_id != pattern_id)) {
      const char *pattern_name = board_patterns[pattern_id].name;
      const int64_t gp_cnt = total_cnt / board_patterns[pattern_id].n_instances;
      printf("  Pattern id: %2d [%6s], total_cnt = %8ld, gp_cnt = %8ld, cumulated relative frequency = %1.4f, cumulated theoretical probability = %1.4f\n",
             pattern_id, pattern_name, total_cnt, gp_cnt, relative_frequency, theoretical_probability);
    }
  }
  // Checks could be added, the total_cnt has to be equal to the number of gp x ninstances ...

  /* Read the number of record for the solved and classified game position table. */
  re = fread(&scgp_data.ntuples, sizeof(size_t), 1, ifp);
  if (re != 1) print_error_and_stop(-130);
  printf("scgp_data.ntuples = %zu\n", scgp_data.ntuples);

  fclose(ifp);

  /* Frees resources. */
  free(pattern_freq_summary.records);
  free(position_summary.records);
  free(patterns);
  free(position_statuses);
  free(position_status_buffer);
  free(batch_ids);

  return EXIT_SUCCESS;
}
