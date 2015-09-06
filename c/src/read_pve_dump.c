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
 * @brief Main entry for the PVE dump utility.
 */
int
main (int argc, char *argv[])
{
  const char *in_file_path = "pve_dump.dat";

  PVEnv *pve = pve_load_from_binary_file(in_file_path);

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
  //shown_sections |= pve_internals_cells_stack_section;
  //shown_sections |= pve_internals_lines_segments_section;
  //shown_sections |= pve_internals_sorted_lines_segments_section;
  //shown_sections |= pve_internals_lines_section;
  //shown_sections |= pve_internals_lines_stack_section;
  pve_internals_to_stream(pve, stdout, shown_sections);

  free(pve);

  return 0;
}
