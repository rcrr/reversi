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
  "Author:\n"
  "   Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2015 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;



/* Static variables. */

static gchar   *input_file    = NULL;

static const GOptionEntry entries[] =
  {
    { "file", 'f', 0, G_OPTION_ARG_FILENAME, &input_file, "Input file name - Mandatory", NULL },
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

  PVEnv *pve = pve_load_from_binary_file(input_file);

  switches_t shown_sections = 0x0000;
  shown_sections |= pve_internals_header_section;
  shown_sections |= pve_internals_index_section;
  shown_sections |= pve_internals_properties_section;
  shown_sections |= pve_internals_structure_section;
  shown_sections |= pve_internals_computed_properties_section;
  //shown_sections |= pve_internals_active_lines_section;
  shown_sections |= pve_internals_cells_segments_section;
  shown_sections |= pve_internals_sorted_cells_segments_section;
  shown_sections |= pve_internals_cells_section;
  shown_sections |= pve_internals_cells_stack_section;
  shown_sections |= pve_internals_lines_segments_section;
  shown_sections |= pve_internals_sorted_lines_segments_section;
  shown_sections |= pve_internals_lines_section;
  shown_sections |= pve_internals_lines_stack_section;
  shown_sections |= pve_internals_root_game_position;
  pve_internals_to_stream(pve, stdout, shown_sections);

  pve_error_code_t error_code = PVE_ERROR_CODE_OK;
  if (!pve_is_invariant_satisfied(pve, &error_code, 0xFF)) {
    printf("error_code=%d\n", error_code);
    return -3;
  }

  pve_line_with_variants_to_stream(pve, (const PVCell **const ) pve->root_line, stdout);

  free(pve);

  return 0;
}
