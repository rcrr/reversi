/**
 * @file
 *
 * @brief Reads a PVE dump dat file.
 * @details This executable reads a file that contains the
 * PVE dump, then executes the actions dictated by the flags given in the command line.
 *
 * @par read_pve_dump.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2015, 2017 Roberto Corradini. All rights reserved.
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
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include "file_utils.h"
#include "game_tree_utils.h"
#include "main_option_parse.h"



/**
 * @cond
 */

/* Static constants. */

static const mop_options_long_t olist[] = {
  {"help",              'h', MOP_NONE},
  {"input-file",        'f', MOP_REQUIRED},
  {"internals",         'i', MOP_REQUIRED},
  {"print-summary",     's', MOP_NONE},
  {"print-pv",          'p', MOP_NONE},
  {"print-pv-as-table", 't', MOP_NONE},
  {"check-invariant",   'c', MOP_NONE},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "  read_pve_dump [OPTION...] - Loads a PVE dump file\n"
  "\n"
  "Options:\n"
  "  -h, --help                  Show help options\n"
  "  -f, --input-file            Input file name     - Mandatory\n"
  "  -i, --internals             Print PVE internals - Switches in hex form\n"
  "  -s, --print-summary         Print summary       - Reads only the file header and exits\n"
  "  -p, --print-pv              Print pv            - Prints human readable PV\n"
  "  -t, --print-pv-as-table     Print pv as table   - Prints PV as a csv file ready for an SQL loader\n"
  "  -c, --check-invariant       Check PVE invariant - Stops execution if a violation is detected\n"
  "\n"
  "Description:\n"
  "  Read Principal Variation Environment dump is a program that load a binary dump file representation of a PVE.\n"
  "\n"
  "Details on Application Options:\n"
  "\n"
  "  -s, --print-summary\n"
  "\n"
  "  -i, --internals\n"
  "    Shows the internal PVE data selected by the switches turned on, the structure is read and then activated, this\n"
  "    last process translates all the memory references in the structure. The data shown is the memory view after translation.\n"
  "    The available sections are:\n"
  "    -00- PVE HEADER ... ... ... ... ... ... 0x0001, or ... 1\n"
  "    -01- PVE INDEX  ... ... ... ... ... ... 0x0002, or ... 2\n"
  "    -03- PVE PROPERTIES ... ... ... ... ... 0x0004, or ... 4\n"
  "    -03- PVE STRUCTURE HEADER   ... ... ... 0x0008, or ... 8\n"
  "    -04- PVE COMPUTED PROPERTIES    ... ... 0x0010, or .. 16\n"
  "    -05- PVE ACTIVE LINES   ... ... ... ... 0x0020, or .. 32\n"
  "    -06- PVE CELLS SEGMENTS ... ... ... ... 0x0040, or .. 64\n"
  "    -07- PVE SORTED CELLS SEGMENTS  ... ... 0x0080, or . 128\n"
  "    -08- PVE CELLS  ... ... ... ... ... ... 0x0100, or . 256\n"
  "    -09- PVE CELLS STACK    ... ... ... ... 0x0200, or . 512\n"
  "    -10- PVE LINES SEGMENTS ... ... ... ... 0x0400, or  1024\n"
  "    -11- PVE SORTED LINES SEGMENTS  ... ... 0x0800, or  2048\n"
  "    -12- PVE LINES  ... ... ... ... ... ... 0x1000, or  4096\n"
  "    -13- PVE LINES STACK    ... ... ... ... 0x2000, or  8192\n"
  "    -14- PVE ROOT GAME POSITION ... ... ... 0x4000, or 16384\n"
  "\n"
  "Author:\n"
  "   Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2015, 2017 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int f_flag = false;
static char *f_arg = NULL;

static int i_flag = false;
static char *i_arg = NULL;

static int s_flag = false;
static int p_flag = false;
static int t_flag = false;
static int c_flag = false;



/* Static functions. */

/**
 * @endcond
 */



/**
 * @brief Main entry for the PVE dump utility.
 */
int
main (int argc, char *argv[])
{
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
    case 'i':
      i_flag = true;
      i_arg = options.optarg;
      break;
    case 's':
      s_flag = true;
      break;
    case 'p':
      p_flag = true;
      break;
    case 't':
      t_flag = true;
      break;
    case 'c':
      c_flag = true;
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
  if (!f_flag) {
    fprintf(stderr, "Option -f, --file is mandatory.\n");
    return -3;
  } else {
    if (!fut_file_exists(f_arg)) {
      fprintf(stderr, "Argument for option -f: file %s does not exist.\n", f_arg);
    return -4;
    }
  }

  if (s_flag && (i_flag || c_flag)) {
    fprintf(stderr, "Option -s, --print-summary, is not compatible with options -i, or -c.\n");
    return -5;
  }

  /* Prints summary when the specific option is on. */
  if (s_flag) {
    pve_summary_from_binary_file_to_stream(f_arg, stdout);
    return 0;
  }

  /* Loads the full PVE data structure. */
  PVEnv *pve = pve_load_from_binary_file(f_arg);

  /* Runs a complete check on PVE invariant. */
  if (c_flag) {
    pve_error_code_t error_code = PVE_ERROR_CODE_OK;
    if (!pve_is_invariant_satisfied(pve, &error_code, 0xFFFFFFFFFFFFFFFF)) {
      printf("Running function pve_is_invariant_satisfied an error has been detected. Error code is: %d\n", error_code);
      free(pve);
      return -6;
    }
  }

  /* Prints selected internals depending on the specific option switches. */
  if (i_flag) {
    char *endptr;
    long int i_arg_to_int = strtol(i_arg, &endptr, 10);
    if (endptr - i_arg != strlen(i_arg)) {
      printf("Argument for option -i: %s is invalid.\n", i_arg);
      return -7;
    }
    if (i_arg_to_int < 0) {
      printf("Argument for option -i is %ld, it must be a non negative integer.\n", i_arg_to_int);
      return -8;
    }
    pve_internals_to_stream(pve, stdout, i_arg_to_int);
  }

  if (p_flag) {
    pve_transform_to_standard_form(pve);
    pve_line_with_variants_to_stream(pve, stdout);
  }

  if (t_flag) {
    board_module_init();
    pve_root_line_as_table_to_stream(pve, stdout);
  }

  free(pve);

  return 0;
}
