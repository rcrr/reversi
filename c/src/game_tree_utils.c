/**
 * @file
 *
 * @brief Game tree utilities module implementation.
 *
 * @details Provides functions to support the game tree expansion.
 *
 *          The function pve_verify_consistency is very inefficient, but the check can
 *          be diseabled by a macro.
 *
 *          A consideration: should we introduce a typedef for PVLine and so get rid of
 *          the three star sin?
 *
 * @par game_tree_utils.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014, 2015 Roberto Corradini. All rights reserved.
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

#include <glib.h>

#include "game_tree_utils.h"
#include "sort_utils.h"



/**
 * @cond
 */

#define DISABLE_SLOW_ASSERT TRUE

/*
 * Prototypes for internal functions.
 */

static void
pve_double_lines_size (PVEnv *const pve);

static PVCell ***
pve_index_lines (const PVEnv *const pve);



/*
 * Internal variables and constants.
 */

/**
 * @endcond
 */



/**********************************************************/
/* Function implementations for the ExactSolution entity. */
/**********************************************************/

/**
 * @brief Exact solution structure constructor.
 *
 * @return a pointer to a new exact solution structure
 */
ExactSolution *
exact_solution_new (void)
{
  ExactSolution * es;
  static const size_t size_of_exact_solution = sizeof(ExactSolution);

  es = (ExactSolution*) malloc(size_of_exact_solution);
  g_assert(es);

  es->solved_game_position = NULL;
  es->outcome = invalid_outcome;
  for (int i = 0; i < PV_MAX_LENGTH; i++) {
    es->pv[i] = invalid_move;
  }
  es->pv_length = 0;
  es->final_board = NULL;
  es->node_count = 0;
  es->leaf_count = 0;

  return es;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #exact_solution_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] es the pointer to be deallocated
 */
void
exact_solution_free (ExactSolution *es)
{
  if (es) {
    if (es->solved_game_position) game_position_free(es->solved_game_position);
    if (es->final_board) board_free(es->final_board);
    free(es);
  }
}

/**
 * @brief Returns a formatted string describing the exact solution structure.
 *
 * @details The returned string has a dynamic extent set by a call to malloc.
 *          It must then properly garbage collected by a call to free when no more referenced.
 *
 * @invariant Parameter `es` must be not `NULL`.
 *            Invariants are guarded by assertions.
 *
 * @param [in] es a pointer to the exact solution structure
 * @return        a string being a representation of the structure
 */
char *
exact_solution_to_string (const ExactSolution *const es)
{
  g_assert(es);

  gchar *es_to_string;
  GString *tmp = g_string_sized_new(32);

  g_string_append_printf(tmp, "%s\n",
                         game_position_print(es->solved_game_position));
  g_string_append_printf(tmp, "[node_count=%" PRIu64 ", leaf_count=%" PRIu64 "]\n",
                         es->node_count,
                         es->leaf_count);
  g_string_append_printf(tmp, "Final outcome: best move=%s, position value=%d\n",
                         square_as_move_to_string(es->pv[0]),
                         es->outcome);

  if (es->pv_length != 0) {
    gchar *pv_to_s = square_as_move_array_to_string(es->pv, es->pv_length);
    g_string_append_printf(tmp, "PV: %s\n", pv_to_s);
    g_free(pv_to_s);
  }

  if (es->final_board) {
    gchar *b_to_s = board_print(es->final_board);
    g_string_append_printf(tmp, "\nFinal board configuration:\n%s\n", b_to_s);
    g_free(b_to_s);
  }

  es_to_string = tmp->str;
  g_string_free(tmp, FALSE);
  return es_to_string;
}

/**
 * @brief Computes the final board when the pv component is properly populated.
 *
 * @details Executes all the moves stored into the pv field up the the final
 *          board configuration, then stores it into the final_board field.
 *
 * @param [in] es a pointer to the exact solution structure
 */
void
exact_solution_compute_final_board (ExactSolution *const es)
{
  g_assert(es);

  int i;
  GamePosition *gp, *gp_next;
  for (i = 0, gp = game_position_clone(es->solved_game_position); i < es->pv_length; i++) {
    gp_next = game_position_make_move(gp, es->pv[i]);
    game_position_free(gp);
    gp = gp_next;
  }
  es->final_board = board_clone(gp->board);
  game_position_free(gp);
}



/**************************************************/
/* Function implementations for the PVEnv entity. */
/**************************************************/

/**
 * @brief PVEnv structure constructor.
 *
 * @details The sizing of the structure's components is done taking into account
 *          a worst case scenario, where every disc put on the board can cost two search
 *          levels, one for the move and one for a potential pass, plus two extra slots
 *          are reserved if the minimax algorithm check the leaf condition consuming two
 *          consecutive pass moves.
 *          These assumptions can be unrealistic, but are the only one that fit all the conditions.
 *
 *          Assertions check that the received pointers to the allocated
 *          structures are not `NULL`.
 *
 * @invariant Parameter `empty_count` cannot be negative.
 *            The invariant is guarded by an assertion.
 *
 * @param [in] empty_count the number of empty cells in the board, or the expected depth
 *                         for the search
 * @return                 a pointer to a new principal variation env structure
 */
PVEnv *
pve_new (const int empty_count)
{
  /*
   * Sixteen segments, having the first hosting 32 lines, leads to a maximum
   * of 1,048,576 lines.
   * The total memory required is 8,388,608 bytes for lines and the same for the stack,
   * leading to a total of 16 MBytes.
   */
  static const size_t lines_segments_size = 16;
  static const size_t lines_first_size = 4;

  /*
   * TBD
   */
  static const size_t cells_segments_size = 4;
  static const size_t cells_first_size = 8;

  g_assert(empty_count >= 0);
  //const int lines_size = (2 * (empty_count + 1) + 1) * 1000;
  const int cells_size = (((empty_count + 2) * ((empty_count + 2) + 1)) / 2) * 100;

  static const size_t size_of_pve   = sizeof(PVEnv);
  static const size_t size_of_pvc   = sizeof(PVCell);
  static const size_t size_of_pvcp  = sizeof(PVCell*);
  static const size_t size_of_pvcpp = sizeof(PVCell**);

  PVEnv *const pve = (PVEnv*) malloc(size_of_pve);
  g_assert(pve);

  /* cells --new-- */
  /* Prepares the cells segments. */
  pve->cells_segments_size = cells_segments_size;
  pve->cells_first_size = cells_first_size;
  pve->cells_segments = (PVCell **) malloc(size_of_pvcp * cells_segments_size);
  g_assert(pve->cells_segments);
  for (int i = 0; i < cells_segments_size; i++) {
    *(pve->cells_segments + i) = NULL;
  }
  pve->cells_segments_head = pve->cells_segments;

  /* Prepares the first segment of cells. */
  pve->cells_size = cells_first_size;
  PVCell *cells = (PVCell *) malloc(cells_first_size * size_of_pvc);
  g_assert(cells);
  *(pve->cells_segments_head) = cells;
  pve->cells_segments_head++;
  for (int i = 0; i < cells_first_size; i++) {
    (cells + i)->move = invalid_move;
    (cells + i)->is_active = FALSE;
    (cells + i)->next = NULL;
    (cells + i)->variant = NULL;
  }

  /* Creates the cells stack and load it with the cells held in the first segment. */
  /*
  pve->cells_stack = (PVCell **) malloc(cells_first_size * size_of_pvcp);
  g_assert(pve->cells_stack);
  for (int i = 0; i < cells_first_size; i++) {
    *(pve->cells_stack + i) = cells + i;
  }
  pve->cells_stack_head = pve->cells_stack;
  */
  /* cells --new-- */

  /* cells --old-- */
  pve->cells_size = cells_size;
  pve->cells = (PVCell*)  malloc(cells_size * size_of_pvc);

  pve->cells_stack = (PVCell**) malloc(cells_size * size_of_pvcp);
  pve->cells_stack_head = pve->cells_stack;

  for (int i = 0; i < cells_size; i++) {
    (pve->cells + i)->move = invalid_move;
    (pve->cells + i)->is_active = FALSE;
    (pve->cells + i)->next = NULL;
    (pve->cells + i)->variant = NULL;
    *(pve->cells_stack + i) = pve->cells + i;
  }
  /* cells --old-- */

  /* Prepares the lines segments. */
  pve->lines_segments_size = lines_segments_size;
  pve->lines_first_size = lines_first_size;
  pve->lines_segments = (PVCell ***) malloc(size_of_pvcpp * lines_segments_size);
  g_assert(pve->lines_segments);
  for (int i = 0; i < lines_segments_size; i++) {
    *(pve->lines_segments + i) = NULL;
  }
  pve->lines_segments_head = pve->lines_segments;

  /* Prepares the first segment of lines. */
  pve->lines_size = lines_first_size;
  PVCell **lines = (PVCell **) malloc(lines_first_size * size_of_pvcp);
  g_assert(lines);
  *(pve->lines_segments_head) = lines;
  pve->lines_segments_head++;
  for (int i = 0; i < lines_first_size; i++) {
    *(lines + i) = NULL;
  }

  /* Prepares the sorted lines segments and the sorted sizes. */
  pve->lines_segments_sizes = (size_t *) malloc(sizeof(size_t) * lines_segments_size);
  pve->lines_segments_sorted = (PVCell ***) malloc(size_of_pvcpp * lines_segments_size);
  for (int i = 0; i < lines_segments_size; i++) {
    *(pve->lines_segments_sizes + i) = 0;
    *(pve->lines_segments_sorted + i) = NULL;
  }
  *(pve->lines_segments_sizes + 0) = lines_first_size;
  *(pve->lines_segments_sorted + 0) = lines;

  /* Creates the lines stack and load it with the lines held in the first segment. */
  pve->lines_stack = (PVCell ***) malloc(lines_first_size * size_of_pvcpp);
  g_assert(pve->lines_stack);
  for (int i = 0; i < lines_first_size; i++) {
    *(pve->lines_stack + i) = lines + i;
  }
  pve->lines_stack_head = pve->lines_stack;

  /*
  char *s = pve_internals_to_string(pve);
  printf("\n\n%s\n\n", s);
  fflush(stdout);
  free(s);
  */
  return pve;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #pve_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] pve the pointer to be deallocated
 */
void
pve_free (PVEnv *pve)
{
  if (pve) {

    /* Frees cells_segments. */
    int cells_segments_size = pve->cells_segments_head - pve->cells_segments;
    g_assert(cells_segments_size >= 0 && cells_segments_size < pve->cells_segments_size);
    while (cells_segments_size) {
      pve->cells_segments_head--;
      g_free(*(pve->cells_segments_head));
      cells_segments_size--;
    }
    g_free(pve->cells_segments);

    /* Frees lines_segments. */
    int lines_segments_size = pve->lines_segments_head - pve->lines_segments;
    g_assert(lines_segments_size >= 0 && lines_segments_size < pve->lines_segments_size);
    while (lines_segments_size) {
      pve->lines_segments_head--;
      g_free(*(pve->lines_segments_head));
      lines_segments_size--;
    }
    g_free(pve->lines_segments);
    g_free(pve->lines_segments_sizes);
    g_free(pve->lines_segments_sorted);

    g_free(pve->cells);
    g_free(pve->cells_stack);
    //g_free(pve->lines);
    g_free(pve->lines_stack);
    g_free(pve);
  }
}

/**
 * @brief Verifies that the pve structure is consistent.
 *
 * @details If `pve` parameter is `NULL` the function return `FALSE` with error_code `-1`.
 *          When no error is found return value is `TRUE` and the variables pointed by
 *          parameters `error_code` and `error_message` are unchanged.
 *
 * @param [in]  pve           a pointer to the principal variation environment
 * @param [out] error_code    a pointer to error code
 * @param [out] error_message a pointer to error message
 * @return                    true if everithing is ok
 */
gboolean
pve_verify_consistency (const PVEnv *const pve,
                        int *const error_code,
                        gchar **const error_message)
{
  gboolean ret = TRUE;

  if (!pve) {
    *error_code = -1;
    *error_message = "Parameter pve is NULL.";
    return FALSE;
  }

  /*
   * Tests that the head pointers are within the proper bounds.
   */
  const int lines_in_use_count = pve->lines_stack_head - pve->lines_stack;
  const int cells_in_use_count = pve->cells_stack_head - pve->cells_stack;
  if (!(cells_in_use_count >= 0)) {
    *error_code = 1;
    *error_message = "The count for cells in use is negative, pointer cells_stack_head is smaller than cells_stack.";
    return FALSE;
  }
  if (!(cells_in_use_count < pve->cells_size)) {
    *error_code = 2;
    *error_message = "The count for cells in use excedes allocated size, pointer cells_stack_head is larger than cells_stack + cells_size.";
    return FALSE;
  }
  if (!(lines_in_use_count >= 0)) {
    *error_code = 3;
    *error_message = "The count for lines in use is negative, pointer lines_stack_head is smaller than lines_stack.";
    return FALSE;
  }
  if (!(lines_in_use_count < pve->lines_size)) {
    *error_code = 4;
    *error_message = "The count for lines in use excedes allocated size, pointer lines_stack_head is larger than lines_stack + lines_size.";
    return FALSE;
  }

  /*
   * Tests that free lines and free cells are unique has to be added.
   */

  /*
   * Tests that active lines and active cells are unique has to be added.
   */

  /*
   * Tests that all active lines have active cells, and then that the
   * active lines and cells count are consistent with their stack pointers.
   */

  return ret;
}

/**
 * @brief Prints the `pve` internals into the returning string.
 *
 * @details The returned string is structured into ten sections:
 *          - The index of sections
 *          - A list of properties
 *          - A header with the fields of the pve structure
 *          - A list of computed properties
 *          - A detail of active lines
 *          - A csv table for the cells segments
 *          - A csv table for the stack of pointers to cells
 *          - A csv table for the lines
 *          - A csv table for the stack of pointers to lines
 *
 * @param [in] pve  a pointer to the principal variation environment
 * @return          a string reporting the pve internals
 */
char *
pve_internals_to_string (const PVEnv *const pve)
{
  g_assert(pve);

  gchar *pve_to_string;
  GString *tmp = g_string_sized_new(2048);

  g_string_append_printf(tmp, "# PVE INDEX OF SECTIONS\n");
  g_string_append_printf(tmp, " -00- PVE PROPERTIES\n");
  g_string_append_printf(tmp, " -01- PVE STRUCTURE HEADER\n");
  g_string_append_printf(tmp, " -02- PVE COMPUTED PROPERTIES\n");
  g_string_append_printf(tmp, " -03- PVE ACTIVE LINES\n");
  g_string_append_printf(tmp, " -04- PVE CELLS SEGMENTS\n");
  g_string_append_printf(tmp, " -05- PVE CELLS\n");
  g_string_append_printf(tmp, " -06- PVE CELLS STACK\n");
  g_string_append_printf(tmp, " -07- PVE LINES SEGMENTS\n");
  g_string_append_printf(tmp, " -08- PVE SORTED LINES SEGMENTS\n");
  g_string_append_printf(tmp, " -09- PVE LINES\n");
  g_string_append_printf(tmp, " -10- PVE LINES STACK\n");
  g_string_append_printf(tmp, "\n");

  static const size_t size_of_pve   = sizeof(PVEnv);
  static const size_t size_of_pvc   = sizeof(PVCell);
  static const size_t size_of_pvcp  = sizeof(PVCell*);
  static const size_t size_of_pvcpp = sizeof(PVCell**);
  g_string_append_printf(tmp, "# PVE PROPERTIES\n");
  g_string_append_printf(tmp, "pve address:                 %20p  --  Address of the PVEnv structure.\n", (void *) pve);
  g_string_append_printf(tmp, "size of pve:                 %20zu  --  Bytes used by a PVEnv structure.\n", size_of_pve);
  g_string_append_printf(tmp, "size of pv cell:             %20zu  --  Bytes used by a PVCell structure.\n", size_of_pvc);
  g_string_append_printf(tmp, "size of pv cell pointer:     %20zu  --  Bytes used by a PVCell pointer, or a line.\n", size_of_pvcp);
  g_string_append_printf(tmp, "size of line pointer:        %20zu  --  Bytes used by a line pointer.\n", size_of_pvcpp);
  g_string_append_printf(tmp, "\n");

  g_string_append_printf(tmp, "# PVE STRUCTURE HEADER\n");
  g_string_append_printf(tmp, "cells_size:                  %20zu  --  Count of cells contained by the cells segments.\n", pve->cells_size);
  g_string_append_printf(tmp, "cells_segments_size:         %20zu  --  Count of cells segments.\n", pve->cells_segments_size);
  g_string_append_printf(tmp, "cells_first_size:            %20zu  --  Number of cells contained by the first segment.\n", pve->cells_first_size);
  g_string_append_printf(tmp, "cells_segments:              %20p  --  Segments are pointers to array of cells.\n", (void *) pve->cells_segments);
  g_string_append_printf(tmp, "cells_segments_head:         %20p  --  Next cells segment to be used.\n", (void *) pve->cells_segments_head);
  g_string_append_printf(tmp, "cells_stack:                 %20p  --  Array of pointers used to manage the cells.\n", (void *) pve->cells_stack);
  g_string_append_printf(tmp, "cells_stack_head:            %20p  --  Next, free to be assigned, pointer in the cells stack.\n", (void *) pve->cells_stack_head);
  g_string_append_printf(tmp, "lines_size:                  %20zu  --  Count of lines contained by the lines segments.\n", pve->lines_size);
  g_string_append_printf(tmp, "lines_segments_size:         %20zu  --  Count of lines segments.\n", pve->lines_segments_size);
  g_string_append_printf(tmp, "lines_first_size:            %20zu  --  Number of lines contained by the first segment.\n", pve->lines_first_size);
  g_string_append_printf(tmp, "lines_segments:              %20p  --  Segments are pointers to array of lines.\n", (void *) pve->lines_segments);
  g_string_append_printf(tmp, "lines_segments_head:         %20p  --  Next lines segment to be used.\n", (void *) pve->lines_segments_head);
  g_string_append_printf(tmp, "lines_stack:                 %20p  --  Array of pointers used to manage the lines.\n", (void *) pve->lines_stack);
  g_string_append_printf(tmp, "lines_stack_head:            %20p  --  Next, free to be assigned, pointer in the lines stack.\n", (void *) pve->lines_stack_head);
  g_string_append_printf(tmp, "\n");

  const long long int lines_in_use_count = pve->lines_stack_head - pve->lines_stack;
  const long long int cells_in_use_count = pve->cells_stack_head - pve->cells_stack;
  const long long int lines_segments_in_use_count = pve->lines_segments_head - pve->lines_segments;
  const long long int cells_segments_in_use_count = pve->cells_segments_head - pve->cells_segments;
  const size_t lines_max_size = (1ULL << (pve->lines_segments_size - 1)) * pve->lines_first_size;
  const size_t cells_max_size = (1ULL << (pve->cells_segments_size - 1)) * pve->cells_first_size;
  const size_t lines_actual_max_size = (1ULL << (lines_segments_in_use_count - 1)) * pve->lines_first_size;
  const size_t cells_actual_max_size = (1ULL << (cells_segments_in_use_count - 1)) * pve->cells_first_size;
  const size_t pve_max_allowed_mem_consum =
    size_of_pve +
    size_of_pvcp * pve->cells_segments_size +
    size_of_pvcpp * pve->lines_segments_size +
    (size_of_pvc + size_of_pvcp) * cells_max_size +
    (size_of_pvcp + size_of_pvcpp) * lines_max_size;
  const size_t pve_current_mem_consum =
    size_of_pve +
    size_of_pvcp * pve->cells_segments_size +
    size_of_pvcpp * pve->lines_segments_size +
    (size_of_pvc + size_of_pvcp) * cells_actual_max_size +
    (size_of_pvcp + size_of_pvcpp) * lines_actual_max_size;
  g_string_append_printf(tmp, "# PVE COMPUTED PROPERTIES\n");
  g_string_append_printf(tmp, "cells_segments_in_use_count: %20lld  --  Cells segments being allocated.\n", cells_segments_in_use_count);
  g_string_append_printf(tmp, "cells_in_use_count:          %20lld  --  Cells actively assigned to lines.\n", cells_in_use_count);
  g_string_append_printf(tmp, "cells_actual_max_size:       %20zu  --  Actual maximum number of cells without allocating new segments.\n", cells_actual_max_size);
  g_string_append_printf(tmp, "cells_max_size:              %20zu  --  Overall maximum number of cells.\n", cells_max_size);
  g_string_append_printf(tmp, "lines_segments_in_use_count: %20lld  --  Lines segments being allocated.\n", lines_segments_in_use_count);
  g_string_append_printf(tmp, "lines_in_use_count:          %20lld  --  Active lines.\n", lines_in_use_count);
  g_string_append_printf(tmp, "lines_actual_max_size:       %20zu  --  Actual maximum number of lines without allocating new segments.\n", lines_actual_max_size);
  g_string_append_printf(tmp, "lines_max_size:              %20zu  --  Overall maximum number of lines.\n", lines_max_size);
  g_string_append_printf(tmp, "pve_max_allowed_mem_consum   %20zu  --  PV environment max allowed memory consumption.\n", pve_max_allowed_mem_consum);
  g_string_append_printf(tmp, "pve_current_mem_consum       %20zu  --  PV environment current memory consumption.\n", pve_current_mem_consum);
  g_string_append_printf(tmp, "\n");

  g_string_append_printf(tmp, "# PVE ACTIVE LINES\n");
  PVCell ***lines_index = pve_index_lines(pve);
  for (int i = 0; i < lines_in_use_count; i++) {
    PVCell **line = *(lines_index + i);
    gchar *pve_root_line_to_s = pve_line_print_internals(pve, (const PVCell **) line);
    g_string_append_printf(tmp, "line_internals: %s\n", pve_root_line_to_s);
    g_free(pve_root_line_to_s);
  }
  free(lines_index);
  g_string_append_printf(tmp, "\n");

  g_string_append_printf(tmp, "# PVE LINES SEGMENTS\n");
  g_string_append_printf(tmp, "ORDINAL;             ADDRESS;           POINTS_TO\n");
  for (int i = 0; i < pve->lines_segments_size; i++) {
    g_string_append_printf(tmp, "%7d;%20p;%20p\n",
                           i,
                           (void *) (pve->lines_segments + i),
                           (void *) *(pve->lines_segments + i));
  }
  g_string_append_printf(tmp, "\n");

  g_string_append_printf(tmp, "# PVE SORTED LINES SEGMENTS\n");
  g_string_append_printf(tmp, "ORDINAL;             ADDRESS;           POINTS_TO;     SIZE\n");
  for (int i = 0; i < pve->lines_segments_size; i++) {
    g_string_append_printf(tmp, "%7d;%20p;%20p;%9zu\n",
                           i,
                           (void *) (pve->lines_segments_sorted + i),
                           (void *) *(pve->lines_segments_sorted + i),
                           *(pve->lines_segments_sizes + i));
  }
  g_string_append_printf(tmp, "\n");

  size_t segment_size = pve->lines_first_size;
  size_t segment_size_incr = 0;
  g_string_append_printf(tmp, "# PVE LINES\n");
  g_string_append_printf(tmp, "SEGMENT; ORDINAL;             ADDRESS;           POINTS_TO\n");
  for (int i = 0; i < lines_segments_in_use_count; i++, segment_size += segment_size_incr, segment_size_incr = segment_size) {
    for (int j = 0; j < segment_size; j++) {
      g_string_append_printf(tmp, "%7d;%8d;%20p;%20p\n",
                             i,
                             j,
                             (void *) (*(pve->lines_segments + i) + j),
                             (void *) *(*(pve->lines_segments + i) + j));
    }
  }
  g_string_append_printf(tmp, "\n");

  g_string_append_printf(tmp, "# PVE LINES STACK\n");
  g_string_append_printf(tmp, "ORDINAL;             ADDRESS;           POINTS_TO\n");
  for (int i = 0; i < pve->lines_size; i++) {
    g_string_append_printf(tmp, "%7d;%20p;%20p\n",
                           i,
                           (void *) (pve->lines_stack + i),
                           (void *) *(pve->lines_stack + i));
  }
  g_string_append_printf(tmp, "\n");

  g_string_append_printf(tmp, "# PVE CELLS\n");
  g_string_append_printf(tmp, "ORDINAL;       ADDRESS; MOVE; IS_ACTIVE;          NEXT;       VARIANT\n");
  for (int i = 0; i < pve->cells_size; i++) {
    g_string_append_printf(tmp, "%7d;%14p;   %s;%10d;%14p;%14p\n",
                           i,
                           (void *) (pve->cells + i),
                           square_as_move_to_string((pve->cells + i)->move),
                           (pve->cells + i)->is_active,
                           (void *) (pve->cells + i)->next,
                           (void *) (pve->cells + i)->variant);
  }
  g_string_append_printf(tmp, "\n");

  g_string_append_printf(tmp, "# PVE CELLS STACK\n");
  g_string_append_printf(tmp, "ORDINAL;       ADDRESS;     POINTS_TO\n");
  for (int i = 0; i < pve->cells_size; i++) {
    g_string_append_printf(tmp, "%7d;%14p;%14p\n",
                           i,
                           (void *) (pve->cells_stack + i),
                           (void *) *(pve->cells_stack + i));
  }

  pve_to_string = tmp->str;
  g_string_free(tmp, FALSE);
  return pve_to_string;
}

/**
 * @brief Returns a free line pointer.
 *
 * @details The environment is modified because the line stack fill pointer,
 *          lines_stack_head, is incremented.
 *
 * @param [in,out] pve a pointer to the principal variation environment
 * @return             a pointer to the next free line
 */
PVCell **
pve_line_create (PVEnv *pve)
{
  if (!DISABLE_SLOW_ASSERT) g_assert(pve_verify_consistency(pve, NULL, NULL));
  PVCell **line_p = *(pve->lines_stack_head);
  *(line_p) = NULL;
  pve->lines_stack_head++;
  //g_assert(pve->lines_stack_head - pve->lines_stack < pve->lines_size); //ZZZ
  if (pve->lines_stack_head - pve->lines_stack == pve->lines_size) pve_double_lines_size(pve);
  return line_p;
}

/**
 * @brief Adds the `move` to the given `line`.
 *
 * @details The function inserts a new cell at the front of the linked list of cells.
 *          A cell is retrieved from the stack, the stack fill pointer is then decremented.
 *          The cell is filled with the move and the previous first cell as next.
 *          The line is updated referring to the new added cell.
 *
 *          The `line` must be active, it is an error to call the function on free lines.
 *
 * @param [in,out] pve  a pointer to the principal variation environment
 * @param [in,out] line the line to be updated
 * @param [in]     move the move value to add to the line
 */
void
pve_line_add_move (PVEnv *pve,
                   PVCell **line,
                   Square move)
{
  if (!DISABLE_SLOW_ASSERT) g_assert(pve_verify_consistency(pve, NULL, NULL));
  PVCell *added_cell = *(pve->cells_stack_head);
  pve->cells_stack_head++;
  g_assert(pve->cells_stack_head - pve->cells_stack < pve->cells_size);
  added_cell->move = move;
  added_cell->is_active = TRUE;
  added_cell->next = *line;
  *line = added_cell;
}

void
pve_line_add_variant (PVEnv *pve,
                      PVCell **line,
                      PVCell **line_variant)
{
  g_assert(line_variant);
  PVCell **tmp_line = (*line)->variant;
  (*line)->variant = line_variant;
  (*line_variant)->variant = tmp_line;
}

/**
 * @brief Deletes the `line`.
 *
 * @details Traverses the linked list of cells and returns them to the cell stack.
 *          For each cell traverse recurvively all the variants.
 *          Finally returns the line to the line stack.
 *
 * @param [in,out] pve  a pointer to the principal variation environment
 * @param [in,out] line the line to be deleted
 */
void
pve_line_delete (PVEnv *pve,
                 PVCell **line)
{
  if (!DISABLE_SLOW_ASSERT) g_assert(pve_verify_consistency(pve, NULL, NULL));
  PVCell *cell = *line; // A poiter to the first cell, or null if the line is empty.
  while (cell) {
    PVCell **v_line = cell->variant;
    if (v_line) pve_line_delete(pve, v_line);
    pve->cells_stack_head--;
    *(pve->cells_stack_head) = cell;
    cell->is_active = FALSE;
    cell->variant = NULL;
    cell = cell->next;
  }
  pve->lines_stack_head--;
  *(pve->lines_stack_head) = line;
}

/**
 * @brief Prints the `line` internals into the returning string.
 *
 * @param [in] pve  a pointer to the principal variation environment
 * @param [in] line the line to be printed
 * @return          a string reporting the line internals
 */
char *
pve_line_print_internals (const PVEnv *const pve,
                          const PVCell **const line)
{
  gchar *line_to_string;
  GString *tmp = g_string_sized_new(32);

  g_string_append_printf(tmp, "line_address=%p, first_cell=%p", (void *) line, (void *) *line);
  if (*line) g_string_append_printf(tmp, ", chain: ");
  for (const PVCell *c = *line; c != NULL; c = c->next) {
    g_string_append_printf(tmp, "(c=%p, m=%s, n=%p, v=%p)",
                           (void *) c,
                           square_as_move_to_string(c->move),
                           (void *) c->next,
                           (void *) c->variant);
  }

  line_to_string = tmp->str;
  g_string_free(tmp, FALSE);
  return line_to_string;
}

/**
 * @brief Prints the `line` into the returning string.
 *
 * @param [in] pve  a pointer to the principal variation environment
 * @param [in] line the line to be printed
 * @return          a string describing the sequence of moves held by the line
 */
char *
pve_line_to_string (const PVEnv *const pve,
                    const PVCell **const line)
{
  gchar *line_to_string;
  GString *tmp = g_string_sized_new(16);

  for (const PVCell *c = *line; c != NULL; c = c->next) {
    g_string_append_printf(tmp, "%s", square_as_move_to_string(c->move));
    if (c->next) g_string_append_printf(tmp, " ");
  }

  line_to_string = tmp->str;
  g_string_free(tmp, FALSE);
  return line_to_string;
}

/**
 * @brief Prints the `line` with variants into the returning string.
 *
 * @param [in] pve  a pointer to the principal variation environment
 * @param [in] line the line to be printed
 * @return          a string describing the sequence of moves held by the line
 */
char *
pve_line_with_variants_to_string (const PVEnv *const pve,
                                  const PVCell **const line)
{
  gchar *line_to_string;
  GString *tmp = g_string_sized_new(256);

  int branches[128];
  int holes[128];
  PVCell **lines[128];

  int idx = 0;
  holes[idx] = 0;
  lines[idx] = (PVCell **) line;

 print_line:
  branches[idx] = 0;
  int ind = 0;
  for (int i = 0; i <= idx; i++) {
    ind += holes[i];
  }
  for (int i = 0; i < ind; i++) {
    g_string_append_printf(tmp, "    ");
  }
  for (const PVCell *c = *lines[idx]; c != NULL; c = c->next) {
    g_string_append_printf(tmp, "%s", square_as_move_to_string(c->move));
    if (c->variant) {
      branches[idx]++;
      g_string_append_printf(tmp, ".");
      if (c->next) g_string_append_printf(tmp, " ");
    } else {
      if (c->next) g_string_append_printf(tmp, "  ");
    }
  }
 variants:
  if (branches[idx] > 0) {
    g_string_append_printf(tmp, "\n");
    int branch_count = branches[idx];
    int hole_count = 0;
    const PVCell *c = *lines[idx];
    for (;;) {
      if (c->variant) branch_count--;
      if (branch_count == 0) break;
      hole_count++;
      c = c->next;
    }
    branches[idx]--;
    idx++;
    holes[idx] = hole_count;
    lines[idx] = c->variant;
    goto print_line;
  } else {
    if (idx != 0) {
      idx--;
      goto variants;
    }
  }

  line_to_string = tmp->str;
  g_string_free(tmp, FALSE);
  return line_to_string;
}

/**
 * @brief Copies the pve line into the exact solution structure.
 *
 * @details Exact solution `es` must have an empty pv field. The assumption is
 *          checked assuring that the pv_length field is equal to zero.
 *          The assumption is guarded by an assertion.
 *
 * @param [in]     pve  a pointer to the principal variation environment
 * @param [in]     line the line to be copied
 * @param [in,out] es   the exact solution to be updated
 */
void
pve_line_copy_to_exact_solution (const PVEnv *const pve,
                                 const PVCell **const line,
                                 ExactSolution *const es)
{
  g_assert(es->pv_length == 0);
  for (const PVCell *c = *line; c != NULL; c = c->next) {
    es->pv[(es->pv_length)++] = c->move;
  }
}



/*******************************************************/
/* Function implementations for the SearchNode entity. */
/*******************************************************/

/**
 * @brief Search node structure constructor.
 *
 * @param [in] move  the move field
 * @param [in] value the value field
 * @return           a pointer to a new search node structure
 */
SearchNode *
search_node_new (const Square move, const int value)
{
  SearchNode * sn;
  static const size_t size_of_search_node = sizeof(SearchNode);

  sn = (SearchNode*) malloc(size_of_search_node);
  g_assert(sn);

  sn->move = move;
  sn->value = value;

  return sn;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #search_node_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] sn the pointer to be deallocated
 */
void
search_node_free (SearchNode *sn)
{
  free(sn);
}

/**
 * @brief Negate the value of the node.
 *
 * @param sn the search node to be negated
 * @return   a new node having the negated value
 */
SearchNode *
search_node_negated (SearchNode *sn)
{
  SearchNode *result;
  result = search_node_new(sn->move, -sn->value);
  search_node_free(sn);
  return result;
}



/**********************************************************/
/* Function implementations for the GameTreeStack entity. */
/**********************************************************/

/**
 * @brief GameTreeStack structure constructor.
 *
 * @return a pointer to a new game tree stack structure
 */
GameTreeStack *
game_tree_stack_new (void)
{
  GameTreeStack* stack;
  static const size_t size_of_stack = sizeof(GameTreeStack);

  stack = (GameTreeStack*) malloc(size_of_stack);
  g_assert(stack);

  return stack;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #game_tree_stack_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] stack the pointer to be deallocated
 */
void
game_tree_stack_free (GameTreeStack *stack)
{
  free(stack);
}

/**
 * @brief Initializes the stack structure.
 */
void
game_tree_stack_init (const GamePosition *const root,
                      GameTreeStack* const stack)
{
  NodeInfo* ground_node_info = &stack->nodes[0];
  game_position_x_copy_from_gp(root, &ground_node_info->gpx);
  ground_node_info->gpx.player = player_opponent(ground_node_info->gpx.player);
  ground_node_info->hash = game_position_x_hash(&ground_node_info->gpx);
  ground_node_info->move_set = 0ULL;
  ground_node_info->move_count = 0;
  ground_node_info->head_of_legal_move_list = &stack->legal_move_stack[0];
  ground_node_info->best_move = invalid_move;
  ground_node_info->alpha = out_of_range_defeat_score;
  ground_node_info->beta = out_of_range_defeat_score;

  NodeInfo* first_node_info  = &stack->nodes[1];
  game_position_x_copy_from_gp(root, &first_node_info->gpx);
  first_node_info->head_of_legal_move_list = &stack->legal_move_stack[0];
  first_node_info->alpha = worst_score;
  first_node_info->beta = best_score;

  stack->fill_index = 1;
}

/**
 * @brief Computes the legal move list given the set.
 *
 * @param [in]  legal_move_set    the set of legal moves
 * @param [out] current_node_info the node info updated with the compuetd list of legal moves
 * @param [out] next_node_info    the node info updated with the new head_of_legal_move_list poiter
 */
void
legal_move_list_from_set (const SquareSet legal_move_set,
                          NodeInfo* const current_node_info,
                          NodeInfo* const next_node_info)
{
  uint8_t *move_ptr = current_node_info->head_of_legal_move_list;
  SquareSet remaining_moves = legal_move_set;
  current_node_info->move_count = 0;
  while (remaining_moves) {
    const uint8_t move = bit_works_bitscanLS1B_64(remaining_moves);
    *move_ptr = move;
    move_ptr++;
    current_node_info->move_count++;
    remaining_moves ^= 1ULL << move;
  }
  next_node_info->head_of_legal_move_list = move_ptr;
  return;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

/**
 * @brief Doubles the line count in the PV environment.
 *
 * @param [in,out] pve the principal variation environment pointer
 */
static void
pve_double_lines_size (PVEnv *const pve)
{
  g_assert(pve);

  /* Doubleing lines_size can occur only if the stack is fully used. */
  g_assert(pve->lines_size == pve->lines_stack_head - pve->lines_stack);

  /* The number of lines segments cannot grows further the limit. */
  size_t lines_segments_used = pve->lines_segments_head - pve->lines_segments;
  g_assert(pve->lines_segments_size > lines_segments_used);

  static const size_t size_of_pvcp  = sizeof(PVCell *);
  static const size_t size_of_pvcpp = sizeof(PVCell **);

  const size_t actual_lines_size = pve->lines_size;
  const size_t lines_size_extension = pve->lines_size;

  /* Prepares the extension segment of lines. */
  PVCell **lines_extension = (PVCell **) malloc(lines_size_extension * size_of_pvcp);
  g_assert(lines_extension);
  *(pve->lines_segments_head) = lines_extension;
  pve->lines_segments_head++;
  lines_segments_used++;
  pve->lines_size += lines_size_extension;
  for (int i = 0; i < lines_size_extension; i++) {
    *(lines_extension + i) = NULL;
  }

  /* Creates the new lines stack and load it with the lines held in the extension segment. */
  PVCell ***new_lines_stack = (PVCell ***) malloc(pve->lines_size * size_of_pvcpp);
  g_assert(new_lines_stack);
  free(pve->lines_stack);
  pve->lines_stack = new_lines_stack;
  for (int i = 0; i < actual_lines_size; i++) {
    *(pve->lines_stack + i) = NULL;
  }
  for (int i = 0; i < lines_size_extension; i++) {
    *(pve->lines_stack + actual_lines_size + i) = lines_extension + i;
  }
  pve->lines_stack_head = pve->lines_stack + actual_lines_size;

  /* Re-compute the sorted lines segments array, and respective sizes. */
  for (size_t i = 0; i < lines_segments_used; i++) {
    *(pve->lines_segments_sorted + i) = *(pve->lines_segments + i);
  }
  sort_utils_insertionsort_asc_p ((void **) pve->lines_segments_sorted, lines_segments_used);
  for (size_t i = 0; i < lines_segments_used; i++) {
    PVCell **segment = *(pve->lines_segments_sorted + i);
    size_t segment_size = pve->lines_first_size;
    size_t segment_size_incr = 0;
    for (size_t j = 0; j < lines_segments_used; j++, segment_size += segment_size_incr, segment_size_incr = segment_size) {
      if (segment == *(pve->lines_segments + j)) break;
    }
    *(pve->lines_segments_sizes + i) = segment_size;
  }

  /*
  char *s = pve_internals_to_string(pve);
  printf("\n\n%s\n\n", s);
  fflush(stdout);
  free(s);
  */
}

/**
 * @brief Returns a fresh sorted array of poiters to lines.
 *
 * @details The returned array has the same size of the lines_stack.
 *          The first part of the array is filled with the used lines,
 *          the second part is holding the free lines.
 *          The two sub-arrays are sorted in their natural order.
 *
 *          The caller has to free the array when no more used.
 *
 * @param [in,out] pve the principal variation environment pointer
 * @retur              an index of the lines_stack
 */
static PVCell ***
pve_index_lines (const PVEnv *const pve)
{
  static const size_t size_of_pvcpp = sizeof(PVCell **);

  PVCell ***index = (PVCell ***) malloc(pve->lines_size * size_of_pvcpp);
  g_assert(index);

  /* Here the index preparation. */
  const size_t lines_in_use_count = pve->lines_stack_head - pve->lines_stack;
  const size_t lines_not_in_use_count = pve->lines_size - lines_in_use_count;
  for (size_t i = 0; i < lines_in_use_count; i++) {
    *(index + i) = NULL;
  }
  for (size_t i = lines_in_use_count; i < pve->lines_size; i++) {
    *(index + i) = *(pve->lines_stack + i);
  }
  PVCell ***lines_not_in_use_head = index +  lines_in_use_count;
  sort_utils_insertionsort_asc_p ((void **) lines_not_in_use_head, lines_not_in_use_count);
  const int lines_segments_in_use_count = pve->lines_segments_head - pve->lines_segments;
  PVCell ***used_lines_stack_p = index;
  PVCell ***free_lines_stack_p = index + lines_in_use_count;
  for (int i = 0; i < lines_segments_in_use_count; i++) {
    size_t segment_size = *(pve->lines_segments_sizes + i);
    for (int j = 0; j < segment_size; j++) {
      PVCell **line = *(pve->lines_segments_sorted + i) + j;
      if (line < *free_lines_stack_p) {
        *used_lines_stack_p++ = line;
      } else {
        free_lines_stack_p++;
      }
    }
  }

  if (0) {
    printf("ORDINAL;             ADDRESS;           POINTS_TO\n");
    for (size_t i = 0; i < pve->lines_size; i++) {
      printf("%7zu;%20p;%20p\n",
             i,
             (void *) (index + i),
             (void *) *(index + i));
    }
  }

  return index;
}

/**
 * @endcond
 */
