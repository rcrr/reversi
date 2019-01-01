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
  rglmdf_position_summary_record_t *psrp;
  rglmdf_pattern_freq_summary_record_t *pfsrp;
  rglmdf_solved_and_classified_gp_record_t *scgprp;
  uint32_t *iarrayp;
  uint64_t data_chunk_size;

  rglmdf_general_data_t data;
  rglmdf_general_data_init(&data);

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

  /* Populates irecords size. */


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

  printf("\n\n\n");
  printf("I;ROW_N;GP_ID;GAME_VALUE");
  for (size_t j = 0; j < ni; j++)
    printf(";I_%03zu", j);
  printf("\n");
  for (size_t i = 0; i < data.positions.ntuples; i++) {
    printf("%8zu; %8zu; %10ld; %3d",
           i, data.positions.records[i].row_n, data.positions.records[i].gp_id, data.positions.records[i].game_value);
    for (size_t j = 0; j < ni; j++) {
      printf(";%8u", data.positions.iarray[i * ni + j]);
    }
    printf("\n");
  }

  fclose(ifp);

  /* Frees resources. */
  rglmdf_general_data_release(&data);

  return EXIT_SUCCESS;
}
