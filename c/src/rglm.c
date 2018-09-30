/**
 * @file
 *
 * @brief RGLM, Revrsi Generalized Linear Model.
 * @details Solves the ptoblem stated in the input file ...
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



/**
 * @cond
 */

/* Static constants. */



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int i_flag = false;
static char *i_arg = NULL;

static mop_options_long_t olist[] = {
  {"help",       'h', MOP_NONE},
  {"input-file", 'i', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "read_game_tree_log [OPTION...] - Loads a Game Tree Log dump file\n"
  "\n"
  "Options:\n"
  "-h, --help           Show help options\n"
  "-i, --input-file     Input file name - Mandatory\n"
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
 * @brief Main entry for the Read Game Tree Log dump utility.
 */
int
main (int argc, char *argv[])
{
  time_t current_time = (time_t) -1;
  char* c_time_string = NULL;

  size_t re;
  int opt;
  int oindex = -1;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, olist, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
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
  fprintf(stdout, "Input file started to be written on %s", c_time_string);

  fclose(ifp);

  return EXIT_SUCCESS;
}
