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
 * @copyright 2015 Roberto Corradini. All rights reserved.
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

#include "game_tree_utils.h"



/**
 * @cond
 */

/* Static constants. */

static const gchar *program_documentation_string =
  "Description:\n"
  "Read Principal Variation Environment dump is a program that load a binary dump file representation of a PVE.\n"
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
  "Copyright (c) 2015 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;



/* Static variables. */

static gchar    *input_file        = NULL;
static uint64_t  internals         = 0;
static gboolean  print_summary     = FALSE;
static gboolean  print_pv          = FALSE;
static gboolean  print_pv_as_table = FALSE;
static gboolean  check_invariant   = FALSE;

static const GOptionEntry entries[] =
  {
    { "input-file",        'f', 0, G_OPTION_ARG_FILENAME, &input_file,        "Input file name     - Mandatory", NULL },
    { "internals",         'i', 0, G_OPTION_ARG_INT64,    &internals,         "Print PVE internals - Switches in hex form", NULL },
    { "print-summary",     's', 0, G_OPTION_ARG_NONE,     &print_summary,     "Print summary       - Reads only the file header and exits", NULL },
    { "print-pv",          'p', 0, G_OPTION_ARG_NONE,     &print_pv,          "Print pv            - Prints human readable PV", NULL },
    { "print-pv-as-table", 't', 0, G_OPTION_ARG_NONE,     &print_pv_as_table, "Print pv as table   - Prints PV as a csv file ready for an SQL loader", NULL },
    { "check-invariant",   'c', 0, G_OPTION_ARG_NONE,     &check_invariant,   "Check PVE invariant - Stops execution if a violation is detected", NULL },
    { NULL }
  };

/**
 * @endcond
 */



/**
 * @brief Main entry for the PVE dump utility.
 */
int
main (int argc, char *argv[])
{

  /* GLib command line options and argument parsing. */
  GError *error = NULL;
  GOptionGroup *option_group = g_option_group_new("name", "description", "help_description", NULL, NULL);
  GOptionContext *context = g_option_context_new("- Loads a PVE dump file");
  g_option_context_add_main_entries(context, entries, NULL);
  g_option_context_add_group(context, option_group);
  g_option_context_set_description(context, program_documentation_string);
  if (!g_option_context_parse (context, &argc, &argv, &error)) {
    g_print("Option parsing failed: %s\n", error->message);
    return -1;
  }

  /* Checks command line options for consistency. */
  if (!input_file) {
    g_print("Option -f, --file is mandatory.\n");
    return -2;
  }
  if (print_summary && (internals || check_invariant)) {
    g_print("Option -s, --print-summary, is not compatible with options -i, or -c.\n");
    return -3;
  }

  /* Prints summary when the specific option is on. */
  if (print_summary) {
    pve_summary_from_binary_file_to_stream(input_file, stdout);
    return 0;
  }

  /* Loads the full PVE data structure. */
  PVEnv *pve = pve_load_from_binary_file(input_file);

  /* Runs a complete check on PVE invariant. */
  if (check_invariant) {
    pve_error_code_t error_code = PVE_ERROR_CODE_OK;
    if (!pve_is_invariant_satisfied(pve, &error_code, 0xFFFFFFFFFFFFFFFF)) {
      printf("Running function pve_is_invariant_satisfied an error has been detected. Error code is: %d\n", error_code);
      free(pve);
      return -4;
    }
  }

  /* Prints selected internals depending on the specific option switches. */
  if (internals) {
    pve_internals_to_stream(pve, stdout, internals);
    return 0;
  }

  if (print_pv) {
    pve_line_with_variants_to_stream(pve, (const PVCell **const) pve->root_line, stdout);
  }

  if (print_pv_as_table) {
    pve_root_line_as_table_to_stream(pve, stdout);
  }

  free(pve);

  return 0;
}
