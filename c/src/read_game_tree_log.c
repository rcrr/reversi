/**
 * @file
 *
 * @brief Reads a game tree log dat file.
 * @details This executable reads a file that contains the
 * game tree log binary dump, then executes the actions dictated by the flags given in the command line.
 *
 * @par read_game_tree_log.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2016, 2017 Roberto Corradini. All rights reserved.
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

#include "game_tree_logger.h"
#include "main_option_parse.h"
#include "game_tree_utils.h"



/**
 * @cond
 */

/* Static constants. */



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int f_flag = false;
static char *f_arg = NULL;

static mop_options_long_t olist[] = {
  {"help",       'h', MOP_NONE},
  {"input-file", 'f', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "read_game_tree_log [OPTION...] - Loads a Game Tree Log dump file\n"
  "\n"
  "Options:\n"
  "-h, --help           Show help options\n"
  "-f, --input-file     Input file name - Mandatory\n"
  "\n"
  "Description:\n"
  "Read Game Tree Log dump is a program that loads a binary dump file representation of a game tree log, and output it as a table.\n"
  "\n"
  "Author:\n"
  "Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2016, 2017 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;

/**
 * @endcond
 */


static void
print_error_and_stop (int ret_code)
{
  fprintf(stderr, "read_game_tree_log: format error reading file %s\n", f_arg);
  exit(ret_code);
}



/**
 * @brief Main entry for the Read Game Tree Log dump utility.
 */
int
main (int argc, char *argv[])
{
  gtl_log_data_h_t log_data_h_stack[GAME_TREE_MAX_DEPTH];
  gtl_log_data_t_t tail_record_structure;
  gtl_log_data_h_t *head_record = log_data_h_stack + 1; // Position zero is not logged.
  gtl_log_data_t_t *tail_record = &tail_record_structure;
  uint8_t rec_type = 0x00;
  size_t re;
  int opt;
  int oindex = -1;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, olist, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
      break;
    case 'f':
      f_flag = true;
      f_arg = options.optarg;
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

  /* Checks command line options for consistency. */
  if (!f_arg) {
    fprintf(stderr, "Option -f, --file is mandatory.\n");
    return -3;
  }

  board_module_init();

  /* Opens the binary file for reading. */
  FILE *fp = fopen(f_arg, "r");
  assert(fp);

  fprintf(stdout, "%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s\n",
          "SUB_RUN_ID",
          "CALL_ID",
          "HASH",
          "PARENT_HASH",
          "BLACKS",
          "WHITES",
          "PLAYER",
          "ALPHA",
          "BETA",
          "CALL_LEVEL",
          "EMPTY_COUNT",
          "IS_LEAF",
          "LEGAL_MOVE_COUNT",
          "LEGAL_MOVE_COUNT_ADJUSTED",
          "PARENT_MOVE",
          "T_CALL_CNT",
          "T_ALPHA",
          "T_BEST_MOVE",
          "T_SEARCHED_MOVE_CNT",
          "LEGAL_MOVE_ARRAY",
          "SEARCHED_MOVE_ARRAY");

  while ((re = fread(&rec_type, sizeof(rec_type), 1, fp))) {

    if (rec_type == gtl_rec_h) {
      re = fread(head_record, sizeof(gtl_log_data_h_t), 1, fp);
      if (re != 1) print_error_and_stop(-10);
      if (head_record->call_level != head_record - log_data_h_stack) print_error_and_stop(-13);
      head_record++;
    } else if (rec_type == gtl_rec_t) {
      re = fread(tail_record, sizeof(gtl_log_data_t_t), 1, fp);
      if (re != 1) print_error_and_stop(-11);
      head_record--;
      if (head_record->call_level != head_record - log_data_h_stack) print_error_and_stop(-14);
      if (head_record->call_level != tail_record->call_level) print_error_and_stop(-15);
      if (head_record->hash != tail_record->hash) print_error_and_stop(-16);

      fprintf(stdout, "%6d;%8" PRIu64 ";%+20" PRId64 ";%+20" PRId64 ";%+20" PRId64 ";%+20" PRId64 ";%1d;%+3d;%+3d;%2d;%2d;%c;%2d;%2d;%s;%8" PRIu64 ";%+3d;%s;%2d",
              head_record->sub_run_id,
              head_record->call_id,
              (int64_t) head_record->hash,
              (int64_t) head_record->parent_hash,
              (int64_t) head_record->blacks,
              (int64_t) head_record->whites,
              head_record->player,
              head_record->alpha,
              head_record->beta,
              head_record->call_level,
              head_record->empty_count,
              head_record->is_leaf ? 't' : 'f',
              head_record->legal_move_count,
              head_record->legal_move_count_adjusted,
              square_as_move_to_string(head_record->parent_move),
              tail_record->call_cnt,
              tail_record->alpha,
              square_as_move_to_string(tail_record->best_move),
              tail_record->searched_move_cnt);

      fprintf(stdout, ";{");
      for (int i = 0; i < head_record->legal_move_count; i++) {
        if (i != 0) fprintf(stdout, ",");
        fprintf(stdout, "%s", square_as_move_to_string(head_record->legal_move_array[i]));
      }
      fprintf(stdout, "};{");
      for (int i = 0; i < tail_record->searched_move_cnt; i++) {
        if (i != 0) fprintf(stdout, ",");
        fprintf(stdout, "%s", square_as_move_to_string(tail_record->searched_move_array[i]));
      }
      fprintf(stdout, "}\n");
    } else
      print_error_and_stop(-12);

  }

  fclose(fp);

  return 0;
}
