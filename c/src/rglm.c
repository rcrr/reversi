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
main (int argc,
      char *argv[])
{
  uint64_t u64, v64, *u64p;
  int16_t i16;
  uint8_t u8;
  char **cpp;
  board_pattern_id_t *bpip;
  char buf[512];

  rglmdf_general_data_t data;
  rglmdf_general_data_init(&data);

  rglmdf_position_summary_table_t position_summary;
  position_summary.ntuples = 0;
  position_summary.records = NULL;

  rglmdf_pattern_freq_summary_table_t pattern_freq_summary;
  pattern_freq_summary.ntuples = 0;
  pattern_freq_summary.records = NULL;

  rglmdf_solved_and_classified_gp_table_t gps_data;
  gps_data.ntuples = 0;
  gps_data.records = NULL;

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
  re = fread(&position_summary.ntuples, sizeof(size_t), 1, ifp);
  if (re != 1) print_error_and_stop(-90);
  position_summary.records = (rglmdf_position_summary_record_t *) malloc(sizeof(rglmdf_position_summary_record_t) * position_summary.ntuples);
  if (!position_summary.records) {
    fprintf(stderr, "Unable to allocate memory for position_summary.records array.\n");
    abort();
  }
  re = fread(position_summary.records, sizeof(rglmdf_position_summary_record_t), position_summary.ntuples, ifp);
  if (re != position_summary.ntuples) print_error_and_stop(-100);
  for (size_t i = 0; i < position_summary.ntuples; i++) solved_classified_gp_cnt += position_summary.records[i].classified_cnt;
  if (verbose) fprintf(stdout, "Position summary table has been read succesfully, solved_classified_gp_cnt: %zu\n", solved_classified_gp_cnt);

  /* Reads the pattern frequency summary table. */
  re = fread(&pattern_freq_summary.ntuples, sizeof(size_t), 1, ifp);
  if (re != 1) print_error_and_stop(-110);
  pattern_freq_summary.records = (rglmdf_pattern_freq_summary_record_t *) malloc(sizeof(rglmdf_pattern_freq_summary_record_t) * pattern_freq_summary.ntuples);
  if (!pattern_freq_summary.records) {
    fprintf(stderr, "Unable to allocate memory for pattern_freq_summary.records array.\n");
    abort();
  }
  re = fread(pattern_freq_summary.records, sizeof(rglmdf_pattern_freq_summary_record_t), pattern_freq_summary.ntuples, ifp);
  if (re != pattern_freq_summary.ntuples) print_error_and_stop(-120);
  if (verbose) fprintf(stdout, "Pattern summary table has been read succesfully, records count: %zu\n", pattern_freq_summary.ntuples);

  //printf("glm_variable_id;pattern_id;principal_index_value;total_cnt;relative_frequency;theoretical_frequency\n");
  int pattern_id = -1;
  int64_t total_cnt = -1;
  double relative_frequency = -1.;
  double theoretical_probability = -1.;
  for (size_t i = 0; i < pattern_freq_summary.ntuples; i++) {
    rglmdf_pattern_freq_summary_record_t *rec = &pattern_freq_summary.records[i];
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
  re = fread(&gps_data.ntuples, sizeof(size_t), 1, ifp);
  if (re != 1) print_error_and_stop(-130);
  printf("gps_data.ntuples = %zu\n", gps_data.ntuples);
  gps_data.records = (rglmdf_solved_and_classified_gp_record_t *) malloc(sizeof(rglmdf_solved_and_classified_gp_record_t) * gps_data.ntuples);
  if (!gps_data.records) {
    fprintf(stderr, "Unable to allocate memory for gps_data.records array.\n");
    return EXIT_FAILURE;
  }
  /* Reads the sequence of chunk of records. Each chunk is organized as: chunk size n, n records, n irecords. */
  size_t data_chunk_size = 0;
  rglmdf_solved_and_classified_gp_record_t *r = gps_data.records;
  for(;;) {
    re = fread(&data_chunk_size, sizeof(size_t), 1, ifp);
    if (re != 1) print_error_and_stop(-140);
    printf("data_chunk_size=%zu\n", data_chunk_size);
    if (data_chunk_size == 0) break;
    re = fread(r, sizeof(rglmdf_solved_and_classified_gp_record_t), data_chunk_size, ifp);
    if (re != data_chunk_size) print_error_and_stop(-150);
    r += data_chunk_size;
  }

  for (size_t i = 0; i < gps_data.ntuples; i++) {
    //printf("%8zu, %8zu, %ld\n", i, gps_data.records[i].row_n, gps_data.records[i].gp_id);
  }


  fclose(ifp);

  /* Frees resources. */
  free(gps_data.records);
  free(pattern_freq_summary.records);
  free(position_summary.records);
  rglmdf_general_data_release(&data);

  return EXIT_SUCCESS;
}
