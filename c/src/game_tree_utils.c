/**
 * @file
 *
 * @brief Game tree utilities module implementation.
 *
 * @details Provides functions to support the game tree expansion.
 *
 *          The function pve_is_invariant_satisfied is quite inefficient, but the check can
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

#define PVE_VERIFY_INVARIANT FALSE
#define PVE_VERIFY_INVARIANT_MASK 0xFFFF
#define pve_verify_invariant(chk_mask)                                  \
  if (PVE_VERIFY_INVARIANT) do {                                        \
      pve_error_code_t error_code = 0;                                  \
      pve_is_invariant_satisfied(pve, &error_code, chk_mask);           \
      g_assert(!error_code);                                            \
    } while (0);

/*
 * Prototypes for internal functions.
 */

static void
pve_double_lines_size (PVEnv *const pve);

static void
pve_double_cells_size (PVEnv *const pve);

static void
pve_sort_lines_in_place (PVEnv *const pve);

static void
pve_state_unset_lines_stack_sorted (PVEnv *const pve);



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
 * @return a pointer to a new principal variation env structure
 */
PVEnv *
pve_new (void)
{
  /*
   * Sixteen segments, having the first hosting 32 lines, leads to a maximum
   * of 1,048,576 lines.
   * The total memory required is 8,388,608 bytes for lines and the same for the stack,
   * leading to a total of 16 MBytes.
   */
  static const size_t lines_segments_size = PVE_LINES_SEGMENTS_SIZE;
  static const size_t lines_first_size = PVE_LINES_FIRST_SIZE;

  /*
   * TBD
   */
  static const size_t cells_segments_size = 28;
  static const size_t cells_first_size = 8;

  static const size_t size_of_pve   = sizeof(PVEnv);
  static const size_t size_of_pvc   = sizeof(PVCell);
  static const size_t size_of_pvcp  = sizeof(PVCell*);
  static const size_t size_of_pvcpp = sizeof(PVCell**);

  PVEnv *const pve = (PVEnv*) malloc(size_of_pve);
  g_assert(pve);

  /* Sets the state switches. */
  pve->state = 0x0000000000000000;

  /* Sets to zero the max number of cells, and lines ever used. */
  pve->cells_max_usage = 0;
  pve->lines_max_usage = 0;

  /* Prepares the cells segments. */
  pve->cells_segments_size = cells_segments_size;
  pve->cells_first_size = cells_first_size;
  pve->cells_segments = (PVCell **) malloc(size_of_pvcp * cells_segments_size);
  g_assert(pve->cells_segments);
  for (size_t i = 0; i < cells_segments_size; i++) {
    *(pve->cells_segments + i) = NULL;
  }
  pve->cells_segments_head = pve->cells_segments;

  /* Prepares the first segment of cells. */
  pve->cells_size = cells_first_size;
  PVCell *cells = (PVCell *) malloc(cells_first_size * size_of_pvc);
  g_assert(cells);
  *(pve->cells_segments_head) = cells;
  pve->cells_segments_head++;
  for (size_t i = 0; i < cells_first_size; i++) {
    (cells + i)->move = invalid_move;
    (cells + i)->is_active = FALSE;
    (cells + i)->next = NULL;
    (cells + i)->variant = NULL;
  }

  /* Prepares the sorted cells segments and the sorted sizes. */
  pve->cells_segments_sorted_sizes = (size_t *) malloc(sizeof(size_t) * cells_segments_size);
  pve->cells_segments_sorted = (PVCell **) malloc(size_of_pvcp * cells_segments_size);
  for (size_t i = 0; i < cells_segments_size; i++) {
    *(pve->cells_segments_sorted_sizes + i) = 0;
    *(pve->cells_segments_sorted + i) = NULL;
  }
  *(pve->cells_segments_sorted_sizes + 0) = cells_first_size;
  *(pve->cells_segments_sorted + 0) = cells;

  /* Creates the cells stack and load it with the cells held in the first segment. */
  pve->cells_stack = (PVCell **) malloc(cells_first_size * size_of_pvcp);
  g_assert(pve->cells_stack);
  for (size_t i = 0; i < cells_first_size; i++) {
    *(pve->cells_stack + i) = cells + i;
  }
  pve->cells_stack_head = pve->cells_stack;

  /* Prepares the lines segments. */
  pve->lines_segments_size = lines_segments_size;
  pve->lines_first_size = lines_first_size;
  pve->lines_segments = (PVCell ***) malloc(size_of_pvcpp * lines_segments_size);
  g_assert(pve->lines_segments);
  for (size_t i = 0; i < lines_segments_size; i++) {
    *(pve->lines_segments + i) = NULL;
  }
  pve->lines_segments_head = pve->lines_segments;

  /* Prepares the first segment of lines. */
  pve->lines_size = lines_first_size;
  PVCell **lines = (PVCell **) malloc(lines_first_size * size_of_pvcp);
  g_assert(lines);
  *(pve->lines_segments_head) = lines;
  pve->lines_segments_head++;
  for (size_t i = 0; i < lines_first_size; i++) {
    *(lines + i) = NULL;
  }

  /* Prepares the sorted lines segments and the sorted sizes. */
  pve->lines_segments_sorted_sizes = (size_t *) malloc(sizeof(size_t) * lines_segments_size);
  pve->lines_segments_sorted = (PVCell ***) malloc(size_of_pvcpp * lines_segments_size);
  for (size_t i = 0; i < lines_segments_size; i++) {
    *(pve->lines_segments_sorted_sizes + i) = 0;
    *(pve->lines_segments_sorted + i) = NULL;
  }
  *(pve->lines_segments_sorted_sizes + 0) = lines_first_size;
  *(pve->lines_segments_sorted + 0) = lines;

  /* Creates the lines stack and load it with the lines held in the first segment. */
  pve->lines_stack = (PVCell ***) malloc(lines_first_size * size_of_pvcpp);
  g_assert(pve->lines_stack);
  for (size_t i = 0; i < lines_first_size; i++) {
    *(pve->lines_stack + i) = lines + i;
  }
  pve->lines_stack_head = pve->lines_stack;

  /* Sets the statistical counters to zero. */
  pve->line_create_count = 0;
  pve->line_delete_count = 0;
  pve->line_add_move_count = 0;
  pve->line_release_cell_count = 0;

  g_assert(pve_is_invariant_satisfied(pve, NULL, 0xFF));

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

    free(pve->cells_stack);

    /* Frees cells_segments. */
    size_t cells_segments_size = pve->cells_segments_head - pve->cells_segments;
    while (cells_segments_size) {
      pve->cells_segments_head--;
      free(*(pve->cells_segments_head));
      cells_segments_size--;
    }
    free(pve->cells_segments);
    free(pve->cells_segments_sorted_sizes);
    free(pve->cells_segments_sorted);

    free(pve->lines_stack);

    /* Frees lines_segments. */
    size_t lines_segments_size = pve->lines_segments_head - pve->lines_segments;
    while (lines_segments_size) {
      pve->lines_segments_head--;
      free(*(pve->lines_segments_head));
      lines_segments_size--;
    }
    free(pve->lines_segments);
    free(pve->lines_segments_sorted_sizes);
    free(pve->lines_segments_sorted);

    free(pve);
  }
}

/**
 * @brief Verifies that the PVE invariant is satisfied.
 *
 * @details Invariants verified are:
 *          - Lines stack must have NULL values from the
 *
 * Tests that free lines and free cells are unique has to be added.
 * Tests that active lines and active cells are unique has to be added.
 * Tests that all active lines have active cells, and then that the
 *   active lines and cells count are consistent with their stack pointers.
 *
 *
 * @param [in]  pve                a pointer to the principal variation environment
 * @param [out] error_code         a pointer to the error code
 * @param [in]  checked_invariants switches that turn checks on
 * @return                         true when checked invariants are satisfied, otherwise false
 */
bool
pve_is_invariant_satisfied (const PVEnv *const pve,
                            pve_error_code_t *const error_code,
                            const switches_t checked_invariants)
{
  if (pve->lines_segments_size != PVE_LINES_SEGMENTS_SIZE) {
    if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_SIZE_IS_INCORRECT;
    return FALSE;
  }
  if (pve->lines_first_size != PVE_LINES_FIRST_SIZE) {
    if (error_code) *error_code = PVE_ERROR_CODE_LINES_FIRST_SIZE_IS_INCORRECT;
    return FALSE;
  }
  if (!pve->lines_segments_head) {
    if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_HEAD_IS_NULL;
    return FALSE;
  }
  if (!pve->lines_segments) {
    if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_IS_NULL;
    return FALSE;
  }
  const ptrdiff_t active_lines_segments_count = pve->lines_segments_head - pve->lines_segments;
  if (active_lines_segments_count < 0) {
    if (error_code) *error_code = PVE_ERROR_CODE_LINES_SEGMENTS_HEADS_PRECEDES_ARRAY_INDEX_0;
    return FALSE;
  }
  if (active_lines_segments_count > pve->lines_segments_size) {
    if (error_code) *error_code = PVE_ERROR_CODE_ACTIVE_LINES_SEGMENTS_COUNT_EXCEEDS_BOUND;
    return FALSE;
  }
  const size_t expected_lines_size = (size_t) (active_lines_segments_count == 0
                                               ? 0 : (1ULL << (active_lines_segments_count - 1)) * PVE_LINES_FIRST_SIZE);
  if (expected_lines_size != pve->lines_size) {
    if (error_code) *error_code = 7;
    return FALSE;
  }

  for (size_t i = 0; i < active_lines_segments_count; i++) {
    const PVCell ***ls = (const PVCell ***) *(pve->lines_segments + i);
    if (!ls) {
      if (error_code) *error_code = 8;
      return FALSE;
    }
  }

  for (size_t i = active_lines_segments_count; i < pve->lines_segments_size; i++) {
    const PVCell ***ls = (const PVCell ***) *(pve->lines_segments + i);
    if (ls) {
      if (error_code) *error_code = 9;
      return FALSE;
    }
  }

  for (size_t i = 0; i < active_lines_segments_count; i++) {
    const PVCell ***ls = (const PVCell ***) *(pve->lines_segments_sorted + i);
    if (!ls) {
      if (error_code) *error_code = 10;
      return FALSE;
    }
  }

  for (size_t i = active_lines_segments_count; i < pve->lines_segments_size; i++) {
    const PVCell ***ls = (const PVCell ***) *(pve->lines_segments_sorted + i);
    if (ls) {
      if (error_code) *error_code = 11;
      return FALSE;
    }
  }

  size_t cumulated_lines_size_from_sorted_segments = 0;
  bool ls_p0_found = FALSE;
  bool ls_p1_found = FALSE;
  const PVCell ***ls_position_0 = (const PVCell ***) *(pve->lines_segments + 0);
  const PVCell ***ls_position_1 = (const PVCell ***) *(pve->lines_segments + 1);
  for (size_t i = 0; i < active_lines_segments_count; i++) {
    const size_t segment_size = (size_t) *(pve->lines_segments_sorted_sizes + i);
    cumulated_lines_size_from_sorted_segments += segment_size;
    const PVCell ***ls = (const PVCell ***) *(pve->lines_segments_sorted + i);
    const size_t ls_index = bit_works_bitscanMS1B_64((size_t) (segment_size / PVE_LINES_FIRST_SIZE)) + 1;
    if (ls_index > active_lines_segments_count) {
      if (error_code) *error_code = 12;
      return FALSE;
    }
    if (ls_index == 1) {
      if (!ls_p0_found && ls == ls_position_0) {
        ls_p0_found = TRUE;
        goto milestone;
      }
      if (!ls_p1_found && ls == ls_position_1) {
        ls_p1_found = TRUE;
        goto milestone;
      }
      if (error_code) *error_code = 13;
      return FALSE;
    } else {
      const PVCell ***ls_unsorted = (const PVCell ***) *(pve->lines_segments + ls_index);
      if (ls != ls_unsorted) {
        if (error_code) *error_code = 14;
        return FALSE;
      }
    }
  milestone:
    if (i > 0) {
      const PVCell ***ls_prec = (const PVCell ***) *(pve->lines_segments_sorted + i - 1);
      if (ls <= ls_prec) {
        if (error_code) *error_code = 15;
        return FALSE;
      }
    }
  }
  if (expected_lines_size != cumulated_lines_size_from_sorted_segments) {
    if (error_code) *error_code = 16;
    return FALSE;
  }
  if (active_lines_segments_count == 1 && !ls_p0_found) {
    if (error_code) *error_code = 17;
    return FALSE;
  }
  if (active_lines_segments_count > 1 && !ls_p0_found && !ls_p1_found) {
    if (error_code) *error_code = 18;
    return FALSE;
  }

  for (size_t i = active_lines_segments_count; i < pve->lines_segments_size; i++) {
    const size_t segment_size = (size_t) *(pve->lines_segments_sorted_sizes + i);
    if (segment_size != 0) {
      if (error_code) *error_code = 19;
      return FALSE;
    }
  }

  /*
   * Lines stack invariants checks.
   */

  /* Lines stack pointers are in the proper range. */
  if (!pve->lines_stack_head) {
    if (error_code) *error_code = 1001;
    return FALSE;
  }
  if (!pve->lines_stack) {
    if (error_code) *error_code = 1002;
    return FALSE;
  }
  const size_t used_lines_count = pve->lines_stack_head - pve->lines_stack;
  if (used_lines_count < 0) {
    if (error_code) *error_code = 1003;
    return FALSE;
  }
  if (used_lines_count > pve->lines_size) {
    if (error_code) *error_code = 1004;
    return FALSE;
  }

  /* Verifies that the line reference is contained into a segment. */
  if (pve->state & pve_state_lines_stack_sorted) { // Lines stack is sorted.
    PVCell ***lines_stack_cursor_prev;
    PVCell ***lines_stack_cursor;
    for (lines_stack_cursor = pve->lines_stack, lines_stack_cursor_prev = NULL;
         lines_stack_cursor < pve->lines_stack_head;
         lines_stack_cursor++) {
      if (*lines_stack_cursor == NULL) {
        if (error_code) *error_code = 1005;
        printf("lines_stack_cursor=%p\n", (void *) lines_stack_cursor);
        return FALSE;
      } else {
        if (lines_stack_cursor_prev && *lines_stack_cursor <= *lines_stack_cursor_prev) {
          if (error_code) *error_code = 1006;
          return FALSE;
        }
        lines_stack_cursor_prev = lines_stack_cursor;
      }
    }
    for (lines_stack_cursor = pve->lines_stack_head, lines_stack_cursor_prev = NULL;
         lines_stack_cursor < pve->lines_stack + pve->lines_size;
         lines_stack_cursor++) {
      if (*lines_stack_cursor == NULL) {
        if (error_code) *error_code = 1007;
        printf("lines_stack_cursor=%p\n", (void *) lines_stack_cursor);
        return FALSE;
      } else {
        if (lines_stack_cursor_prev && *lines_stack_cursor <= *lines_stack_cursor_prev) {
          if (error_code) *error_code = 1008;
          return FALSE;
        }
        lines_stack_cursor_prev = lines_stack_cursor;
      }
    }
  } else { // Lines stack is not sorted.
    for (size_t i = 0; i < used_lines_count; i++) {
      const PVCell **line = (const PVCell **) *(pve->lines_stack + i);
      if (line) {
        if (error_code) *error_code = 1009;
        return FALSE;
      }
    }

    for (size_t i = used_lines_count; i < pve->lines_size; i++) {
      const PVCell **line = (const PVCell **) *(pve->lines_stack + i);
      if (!line) {
        if (error_code) *error_code = 1010;
        return FALSE;
      }
      // Moving on lines_segments is linear, a bisection approach would be better, but a lot more sophisticated .....
      for (size_t lsi = active_lines_segments_count - 1; ; lsi--) { // lsi: lines_segment_index
        const PVCell **first_line_in_segment = (const PVCell **) *(pve->lines_segments_sorted + lsi);
        if (line >= first_line_in_segment) {
          if (line - first_line_in_segment < *(pve->lines_segments_sorted_sizes + lsi)) {
            break;
          } else {
            if (error_code) *error_code = 1011;
            return FALSE;
          }
        }
        if (lsi == 0) {
          if (error_code) *error_code = 1012;
          return FALSE;
        }
      }
    }
  }

  if (pve->line_create_count - pve->line_delete_count != used_lines_count) {
    if (error_code) *error_code = 1100;
    return FALSE;
  }

  const size_t used_cells_count = pve->cells_stack_head - pve->cells_stack;

  if (pve->line_add_move_count - pve->line_release_cell_count != used_cells_count) {
    if (error_code) *error_code = 1101;
    return FALSE;
  }

  return TRUE;
}

/**
 * @brief Prints the `pve` internals into the given `stream`.
 *
 * @details The text is structured into an header and ten sections:
 *          - The index of sections
 *          - A list of properties
 *          - A header with the fields of the pve structure
 *          - A list of computed properties
 *          - A detail of active lines
 *          - A csv table reporting cells segments
 *          - A csv table reporting sorted cells segments
 *          - A csv table reporting cells
 *          - A csv table reporting the stack of pointers to cells
 *          - A csv table reporting lines segments
 *          - A csv table reporting sorted lines segments
 *          - A csv table reporting lines
 *          - A csv table reporting the stack of pointers to lines
 *
 *          The block on active lines makes a call to the function `pve_sort_lines_in_place`
 *          that sort, and so modify, the lines stack.
 *
 *          Sections are turned on and off according to the `shown_sections` parameter.
 *          The header file defines this list of constants, taht document the relationship
 *          between the switches and the sections:
 *          - #pve_internals_header_section                `0x0001`
 *          - #pve_internals_index_section                 `0x0002`
 *          - #pve_internals_properties_section            `0x0004`
 *          - #pve_internals_structure_section             `0x0008`
 *          - #pve_internals_computed_properties_section   `0x0010`
 *          - #pve_internals_active_lines_section          `0x0020`
 *          - #pve_internals_cells_segments_section        `0x0040`
 *          - #pve_internals_sorted_cells_segments_section `0x0080`
 *          - #pve_internals_cells_section                 `0x0100`
 *          - #pve_internals_cells_stack_section           `0x0200`
 *          - #pve_internals_lines_segments_section        `0x0400`
 *          - #pve_internals_sorted_lines_segments_section `0x0800`
 *          - #pve_internals_lines_section                 `0x1000`
 *          - #pve_internals_lines_stack_section           `0x2000`
 *
 *
 * @param [in] pve            a pointer to the principal variation environment
 * @param [in] stream         the file handler destination of the report of the pve internals
 * @param [in] shown_sections switches that turn on/off the sections
 */
void
pve_internals_to_stream (PVEnv *const pve,
                         FILE *const stream,
                         const switches_t shown_sections)
{
  g_assert(pve);

  static const size_t size_of_pve   = sizeof(PVEnv);
  static const size_t size_of_pvc   = sizeof(PVCell);
  static const size_t size_of_pvcp  = sizeof(PVCell*);
  static const size_t size_of_pvcpp = sizeof(PVCell**);

  if (shown_sections & pve_internals_header_section) {
    time_t current_time = (time_t) -1;
    char* c_time_string = NULL;

    /* Obtain current time as seconds elapsed since the Epoch. */
    current_time = time(NULL);
    g_assert(current_time != ((time_t) -1));

    /* Convert to local time format. */
    c_time_string = ctime(&current_time);
    g_assert(c_time_string);

    fprintf(stream, "# PVE HEADER\n");

    /* ctime() has already added a terminating newline character. */
    fprintf(stream, "Current time is %s", c_time_string);

    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_index_section) {
    fprintf(stream, "# PVE INDEX OF SECTIONS\n");
    fprintf(stream, " -00- PVE PROPERTIES\n");
    fprintf(stream, " -01- PVE STRUCTURE HEADER\n");
    fprintf(stream, " -02- PVE COMPUTED PROPERTIES\n");
    fprintf(stream, " -03- PVE ACTIVE LINES\n");
    fprintf(stream, " -04- PVE CELLS SEGMENTS\n");
    fprintf(stream, " -05- PVE SORTED CELLS SEGMENTS\n");
    fprintf(stream, " -06- PVE CELLS\n");
    fprintf(stream, " -07- PVE CELLS STACK\n");
    fprintf(stream, " -08- PVE LINES SEGMENTS\n");
    fprintf(stream, " -09- PVE SORTED LINES SEGMENTS\n");
    fprintf(stream, " -10- PVE LINES\n");
    fprintf(stream, " -11- PVE LINES STACK\n");
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_properties_section) {
    fprintf(stream, "# PVE PROPERTIES\n");
    fprintf(stream, "pve address:                 %20p  --  Address of the PVEnv structure.\n", (void *) pve);
    fprintf(stream, "size of pve:                 %20zu  --  Bytes used by a PVEnv structure.\n", size_of_pve);
    fprintf(stream, "size of pv cell:             %20zu  --  Bytes used by a PVCell structure.\n", size_of_pvc);
    fprintf(stream, "size of pv cell pointer:     %20zu  --  Bytes used by a PVCell pointer, or a line.\n", size_of_pvcp);
    fprintf(stream, "size of line pointer:        %20zu  --  Bytes used by a line pointer.\n", size_of_pvcpp);
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_structure_section) {
    fprintf(stream, "# PVE STRUCTURE HEADER\n");
    fprintf(stream, "cells_size:                  %20zu  --  Count of cells contained by the cells segments.\n", pve->cells_size);
    fprintf(stream, "cells_segments_size:         %20zu  --  Count of cells segments.\n", pve->cells_segments_size);
    fprintf(stream, "cells_first_size:            %20zu  --  Number of cells contained by the first segment.\n", pve->cells_first_size);
    fprintf(stream, "cells_segments:              %20p  --  Segments are pointers to array of cells.\n", (void *) pve->cells_segments);
    fprintf(stream, "cells_segments_head:         %20p  --  Next cells segment to be used.\n", (void *) pve->cells_segments_head);
    fprintf(stream, "cells_segments_sorted_sizes: %20p  --  Array of sizes of cells segments in the sorted order.\n", (void *) pve->cells_segments_sorted_sizes);
    fprintf(stream, "cells_segments_sorted:       %20p  --  Sorted cells segments, by means of the natural order of the memory adress.\n", (void *) pve->cells_segments_sorted);
    fprintf(stream, "cells_stack:                 %20p  --  Array of pointers used to manage the cells.\n", (void *) pve->cells_stack);
    fprintf(stream, "cells_stack_head:            %20p  --  Next, free to be assigned, pointer in the cells stack.\n", (void *) pve->cells_stack_head);
    fprintf(stream, "cells_max_usage:             %20zu  --  The maximum number of cells in use.\n", pve->cells_max_usage);
    fprintf(stream, "lines_size:                  %20zu  --  Count of lines contained by the lines segments.\n", pve->lines_size);
    fprintf(stream, "lines_segments_size:         %20zu  --  Count of lines segments.\n", pve->lines_segments_size);
    fprintf(stream, "lines_first_size:            %20zu  --  Number of lines contained by the first segment.\n", pve->lines_first_size);
    fprintf(stream, "lines_segments:              %20p  --  Segments are pointers to array of lines.\n", (void *) pve->lines_segments);
    fprintf(stream, "lines_segments_head:         %20p  --  Next lines segment to be used.\n", (void *) pve->lines_segments_head);
    fprintf(stream, "lines_segments_sorted_sizes: %20p  --  Array of sizes of lines segments in the sorted order.\n", (void *) pve->lines_segments_sorted_sizes);
    fprintf(stream, "lines_segments_sorted:       %20p  --  Sorted lines segments, by means of the natural order of the memory adress.\n", (void *) pve->lines_segments_sorted);
    fprintf(stream, "lines_stack:                 %20p  --  Array of pointers used to manage the lines.\n", (void *) pve->lines_stack);
    fprintf(stream, "lines_stack_head:            %20p  --  Next, free to be assigned, pointer in the lines stack.\n", (void *) pve->lines_stack_head);
    fprintf(stream, "lines_max_usage:             %20zu  --  The maximum number of lines in use.\n", pve->lines_max_usage);
    fprintf(stream, "line_create_count            %20zu  --  The number of calls to the function pve_line_create().\n", pve->line_create_count);
    fprintf(stream, "line_delete_count            %20zu  --  The number of calls to the function pve_line_delete().\n", pve->line_delete_count);
    fprintf(stream, "line_add_move_count          %20zu  --  The number of calls to the function pve_line_add_move().\n", pve->line_add_move_count);
    fprintf(stream, "line_release_cell_count      %20zu  --  The number of times a cell is released in the pve_line_delete() function.\n", pve->line_release_cell_count);
    fprintf(stream, "\n");
  }

  g_assert(pve->cells_segments_head >= pve->cells_segments);
  g_assert(pve->lines_segments_head >= pve->lines_segments);
  g_assert(pve->cells_stack_head >= pve->cells_stack);
  g_assert(pve->lines_stack_head >= pve->lines_stack);
  const size_t lines_in_use_count = pve->lines_stack_head - pve->lines_stack;
  const size_t cells_in_use_count = pve->cells_stack_head - pve->cells_stack;
  const size_t lines_segments_in_use_count = pve->lines_segments_head - pve->lines_segments;
  const size_t cells_segments_in_use_count = pve->cells_segments_head - pve->cells_segments;
  const size_t lines_max_size = (1ULL << (pve->lines_segments_size - 1)) * pve->lines_first_size;
  const size_t cells_max_size = (1ULL << (pve->cells_segments_size - 1)) * pve->cells_first_size;
  const size_t lines_actual_max_size = (1ULL << (lines_segments_in_use_count - 1)) * pve->lines_first_size;
  const size_t cells_actual_max_size = (1ULL << (cells_segments_in_use_count - 1)) * pve->cells_first_size;
  const size_t pve_max_allowed_mem_consum =
    size_of_pve +
    size_of_pvcp * pve->cells_segments_size +
    size_of_pvcpp * pve->lines_segments_size +
    (sizeof(size_t) + size_of_pvcp) * pve->cells_segments_size +
    (sizeof(size_t) + size_of_pvcpp) * pve->lines_segments_size +
    (size_of_pvc + size_of_pvcp) * cells_max_size +
    (size_of_pvcp + size_of_pvcpp) * lines_max_size;
  const size_t pve_current_mem_consum =
    size_of_pve +
    size_of_pvcp * pve->cells_segments_size +
    size_of_pvcpp * pve->lines_segments_size +
    (sizeof(size_t) + size_of_pvcp) * pve->cells_segments_size +
    (sizeof(size_t) + size_of_pvcpp) * pve->lines_segments_size +
    (size_of_pvc + size_of_pvcp) * cells_actual_max_size +
    (size_of_pvcp + size_of_pvcpp) * lines_actual_max_size;
  if (shown_sections & pve_internals_computed_properties_section) {
    fprintf(stream, "# PVE COMPUTED PROPERTIES\n");
    fprintf(stream, "cells_segments_in_use_count: %20zu  --  Cells segments being allocated.\n", cells_segments_in_use_count);
    fprintf(stream, "cells_in_use_count:          %20zu  --  Cells actively assigned to lines.\n", cells_in_use_count);
    fprintf(stream, "cells_actual_max_size:       %20zu  --  Actual maximum number of cells without allocating new segments.\n", cells_actual_max_size);
    fprintf(stream, "cells_max_size:              %20zu  --  Overall maximum number of cells.\n", cells_max_size);
    fprintf(stream, "lines_segments_in_use_count: %20zu  --  Lines segments being allocated.\n", lines_segments_in_use_count);
    fprintf(stream, "lines_in_use_count:          %20zu  --  Active lines.\n", lines_in_use_count);
    fprintf(stream, "lines_actual_max_size:       %20zu  --  Actual maximum number of lines without allocating new segments.\n", lines_actual_max_size);
    fprintf(stream, "lines_max_size:              %20zu  --  Overall maximum number of lines.\n", lines_max_size);
    fprintf(stream, "pve_max_allowed_mem_consum   %20zu  --  PV environment max allowed memory consumption.\n", pve_max_allowed_mem_consum);
    fprintf(stream, "pve_current_mem_consum       %20zu  --  PV environment current memory consumption.\n", pve_current_mem_consum);
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_active_lines_section) {
    fprintf(stream, "# PVE ACTIVE LINES\n");
    pve_sort_lines_in_place(pve);
    for (size_t i = 0; i < lines_in_use_count; i++) {
      const PVCell **line = (const PVCell **) *(pve->lines_stack + i);
      if (!line) fprintf(stream, "line=%zu, lines_in_use_count=%zu\n", i, lines_in_use_count);
      fflush(stream);
      g_assert(line);
      fprintf(stream, "line_internals: ");
      pve_line_internals_to_stream(pve, line, stream);
      fprintf(stream, "\n");
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_cells_segments_section) {
    fprintf(stream, "# PVE CELLS SEGMENTS\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO\n");
    for (size_t i = 0; i < pve->cells_segments_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p\n",
              i,
              (void *) (pve->cells_segments + i),
              (void *) *(pve->cells_segments + i));
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_sorted_cells_segments_section) {
    fprintf(stream, "# PVE SORTED CELLS SEGMENTS\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO;     SIZE\n");
    for (size_t i = 0; i < pve->cells_segments_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p;%9zu\n",
              i,
              (void *) (pve->cells_segments_sorted + i),
              (void *) *(pve->cells_segments_sorted + i),
              *(pve->cells_segments_sorted_sizes + i));
    }
    fprintf(stream, "\n");
  }

  size_t cells_segment_size = pve->cells_first_size;
  size_t cells_segment_size_incr = 0;
  if (shown_sections & pve_internals_cells_section) {
    fprintf(stream, "# PVE CELLS\n");
    fprintf(stream, "SEGMENT; ORDINAL;             ADDRESS; MOVE; IS_ACTIVE;                NEXT;             VARIANT\n");
    for (size_t i = 0; i < cells_segments_in_use_count; i++, cells_segment_size += cells_segment_size_incr, cells_segment_size_incr = cells_segment_size) {
      for (size_t j = 0; j < cells_segment_size; j++) {
        PVCell *cell = (PVCell *) (*(pve->cells_segments + i) + j);
        fprintf(stream, "%7zu;%8zu;%20p;%5s;%10d;%20p;%20p\n",
                i,
                j,
                (void *) cell,
                square_as_move_to_string(cell->move),
                cell->is_active,
                (void *) cell->next,
                (void *) cell->variant);
      }
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_cells_stack_section) {
    fprintf(stream, "# PVE CELLS STACK\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO\n");
    for (size_t i = 0; i < pve->cells_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p\n",
              i,
              (void *) (pve->cells_stack + i),
              (void *) *(pve->cells_stack + i));
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_lines_segments_section) {
    fprintf(stream, "# PVE LINES SEGMENTS\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO;      SIZE\n");
    for (size_t i = 0; i < pve->lines_segments_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p;%10zu\n",
              i,
              (void *) (pve->lines_segments + i),
              (void *) *(pve->lines_segments + i),
              (size_t) ((i == 0) ? 1 : (1ULL << (i - 1))) * pve->lines_first_size);
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_sorted_lines_segments_section) {
    fprintf(stream, "# PVE SORTED LINES SEGMENTS\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO;      SIZE\n");
    for (size_t i = 0; i < pve->lines_segments_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p;%10zu\n",
              i,
              (void *) (pve->lines_segments_sorted + i),
              (void *) *(pve->lines_segments_sorted + i),
              *(pve->lines_segments_sorted_sizes + i));
    }
    fprintf(stream, "\n");
  }

  size_t lines_segment_size = pve->lines_first_size;
  size_t lines_segment_size_incr = 0;
  if (shown_sections & pve_internals_lines_section) {
    fprintf(stream, "# PVE LINES\n");
    fprintf(stream, "SEGMENT; ORDINAL;             ADDRESS;           POINTS_TO\n");
    for (size_t i = 0; i < lines_segments_in_use_count; i++, lines_segment_size += lines_segment_size_incr, lines_segment_size_incr = lines_segment_size) {
      for (size_t j = 0; j < lines_segment_size; j++) {
        fprintf(stream, "%7zu;%8zu;%20p;%20p\n",
                i,
                j,
                (void *) (*(pve->lines_segments + i) + j),
                (void *) *(*(pve->lines_segments + i) + j));
      }
    }
    fprintf(stream, "\n");
  }

  if (shown_sections & pve_internals_lines_stack_section) {
    fprintf(stream, "# PVE LINES STACK\n");
    fprintf(stream, "ORDINAL;             ADDRESS;           POINTS_TO\n");
    for (size_t i = 0; i < pve->lines_size; i++) {
      fprintf(stream, "%7zu;%20p;%20p\n",
              i,
              (void *) (pve->lines_stack + i),
              (void *) *(pve->lines_stack + i));
    }
    fprintf(stream, "\n");
  }
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
  pve_verify_invariant(PVE_VERIFY_INVARIANT_MASK);
  pve_state_unset_lines_stack_sorted(pve);
  pve->line_create_count++;
  PVCell **line_p = *(pve->lines_stack_head);
  *(line_p) = NULL;
  *(pve->lines_stack_head) = NULL; /* Set to NULL the stack cell. */
  pve->lines_stack_head++;
  if (pve->lines_stack_head - pve->lines_stack > pve->lines_max_usage) pve->lines_max_usage++;
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
  pve_verify_invariant(PVE_VERIFY_INVARIANT_MASK);
  pve->line_add_move_count++;
  PVCell *added_cell = *(pve->cells_stack_head);
  pve->cells_stack_head++;
  if (pve->cells_stack_head - pve->cells_stack > pve->cells_max_usage) pve->cells_max_usage++;
  if (pve->cells_stack_head - pve->cells_stack == pve->cells_size) pve_double_cells_size(pve);
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
  pve_verify_invariant(PVE_VERIFY_INVARIANT_MASK);
  pve_state_unset_lines_stack_sorted(pve);
  pve->line_delete_count++;
  PVCell *cell = *line; // A poiter to the first cell, or null if the line is empty.
  while (cell) {
    PVCell **v_line = cell->variant;
    if (v_line) pve_line_delete(pve, v_line);
    pve->cells_stack_head--;
    *(pve->cells_stack_head) = cell;
    cell->is_active = FALSE;
    cell->variant = NULL;
    pve->line_release_cell_count++;
    cell = cell->next;
  }
  pve->lines_stack_head--;
  *(pve->lines_stack_head) = line;
}

/**
 * @brief Prints the `line` internals into the given `stream`.
 *
 * @param [in] pve    a pointer to the principal variation environment
 * @param [in] line   the line to be printed
 * @param [in] stream the stream collecting the output
 */
void
pve_line_internals_to_stream (const PVEnv *const pve,
                              const PVCell **const line,
                              FILE *const stream)
{
  if (line) {
    fprintf(stream, "line_address=%p, first_cell=%p", (void *) line, (void *) *line);
    if (*line) fprintf(stream, ", chain: ");
    for (const PVCell *c = *line; c != NULL; c = c->next) {
      fprintf(stream, "(c=%p, m=%s, n=%p, v=%p)",
              (void *) c,
              square_as_move_to_string(c->move),
              (void *) c->next,
              (void *) c->variant);
    }
  } else {
    fprintf(stream, "Line is NULL, it shouldn't.\n");
  }
}

/**
 * @brief Prints the `line` with variants into the returning string.
 *
 * @param [in] pve  a pointer to the principal variation environment
 * @param [in] line the line to be printed
 * @param [in] stream the stream collecting the output
 */
void
pve_line_with_variants_to_stream (const PVEnv *const pve,
                                  const PVCell **const line,
                                  FILE *const stream)
{
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
    fprintf(stream, "    ");
  }
  for (const PVCell *c = *lines[idx]; c != NULL; c = c->next) {
    fprintf(stream, "%s", square_as_move_to_string(c->move));
    if (c->variant) {
      branches[idx]++;
      fprintf(stream, ".");
      if (c->next) fprintf(stream, " ");
    } else {
      if (c->next) fprintf(stream, "  ");
    }
  }
 variants:
  if (branches[idx] > 0) {
    fprintf(stream, "\n");
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
 * @brief Doubles the cell count in the PV environment.
 *
 * @param [in,out] pve the principal variation environment pointer
 */
static void
pve_double_cells_size (PVEnv *const pve)
{
  g_assert(pve);

  /* Doubleing cells_size can occur only if the stack is fully used. */
  g_assert(pve->cells_size == pve->cells_stack_head - pve->cells_stack);

  /* The number of cells segments cannot grow further than the limit. */
  size_t cells_segments_used = pve->cells_segments_head - pve->cells_segments;
  g_assert(pve->cells_segments_size > cells_segments_used);

  static const size_t size_of_pvc  = sizeof(PVCell);
  static const size_t size_of_pvcp  = sizeof(PVCell *);

  const size_t actual_cells_size = pve->cells_size;
  const size_t cells_size_extension = pve->cells_size;

  /* Prepares the extension segment of cells. */
  PVCell *cells_extension = (PVCell *) malloc(cells_size_extension * size_of_pvc);
  g_assert(cells_extension);
  *(pve->cells_segments_head) = cells_extension;
  pve->cells_segments_head++;
  cells_segments_used++;
  pve->cells_size += cells_size_extension;
  for (size_t i = 0; i < cells_size_extension; i++) {
    (cells_extension + i)->move = invalid_move;
    (cells_extension + i)->is_active = FALSE;
    (cells_extension + i)->next = NULL;
    (cells_extension + i)->variant = NULL;
  }

  /* Creates the new cells stack and load it with the cells held in the extension segment. */
  PVCell **new_cells_stack = (PVCell **) malloc(pve->cells_size * size_of_pvcp);
  g_assert(new_cells_stack);
  free(pve->cells_stack);
  pve->cells_stack = new_cells_stack;
  for (size_t i = 0; i < actual_cells_size; i++) {
    *(pve->cells_stack + i) = NULL;
  }
  for (size_t i = 0; i < cells_size_extension; i++) {
    *(pve->cells_stack + actual_cells_size + i) = cells_extension + i;
  }
  pve->cells_stack_head = pve->cells_stack + actual_cells_size;

  /* Re-compute the sorted cells segments array, and respective sizes. */
  for (size_t i = 0; i < cells_segments_used; i++) {
    *(pve->cells_segments_sorted + i) = *(pve->cells_segments + i);
  }
  sort_utils_insertionsort_asc_p ((void **) pve->cells_segments_sorted, cells_segments_used);
  for (size_t i = 0; i < cells_segments_used; i++) {
    PVCell *segment = *(pve->cells_segments_sorted + i);
    size_t segment_size = pve->cells_first_size;
    size_t segment_size_incr = 0;
    for (size_t j = 0; j < cells_segments_used; j++, segment_size += segment_size_incr, segment_size_incr = segment_size) {
      if (segment == *(pve->cells_segments + j)) break;
    }
    *(pve->cells_segments_sorted_sizes + i) = segment_size;
  }
}

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

  /* The number of lines segments cannot grow further than the limit. */
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
  for (size_t i = 0; i < lines_size_extension; i++) {
    *(lines_extension + i) = NULL;
  }

  /* Creates the new lines stack and load it with the lines held in the extension segment. */
  PVCell ***new_lines_stack = (PVCell ***) malloc(pve->lines_size * size_of_pvcpp);
  g_assert(new_lines_stack);
  free(pve->lines_stack);
  pve->lines_stack = new_lines_stack;
  for (size_t i = 0; i < actual_lines_size; i++) {
    *(pve->lines_stack + i) = NULL;
  }
  for (size_t i = 0; i < lines_size_extension; i++) {
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
    *(pve->lines_segments_sorted_sizes + i) = segment_size;
  }
}

/**
 * @brief Sorts the stack of lines in natural order.
 *
 * @details The lines_stack array is sorted in place in two parts.
 *          The first part of the array is filled with the used lines,
 *          the second part is holding the free lines.
 *          The two sub-arrays are sorted in their natural order.
 *
 * @param [in,out] pve the principal variation environment pointer
 */
static void
pve_sort_lines_in_place (PVEnv *const pve)
{
  PVCell ***index = pve->lines_stack;

  /* Here the index preparation. */
  const size_t lines_in_use_count = pve->lines_stack_head - pve->lines_stack;
  const size_t lines_not_in_use_count = pve->lines_size - lines_in_use_count;

  PVCell ***lines_not_in_use_head = index +  lines_in_use_count;
  sort_utils_insertionsort_asc_p ((void **) lines_not_in_use_head, lines_not_in_use_count);

  PVCell ***free_lines_stack_cursor = pve->lines_stack_head;
  PVCell ***used_lines_stack_cursor = pve->lines_stack;
  size_t lines_segments_count = pve->lines_segments_head - pve->lines_segments;
  for (size_t lines_segments_sorted_index = 0;
       lines_segments_sorted_index < lines_segments_count;
       lines_segments_sorted_index++) {
    PVCell **first_line_in_segment = *(pve->lines_segments_sorted + lines_segments_sorted_index);
    PVCell **segment_boundary = first_line_in_segment + *(pve->lines_segments_sorted_sizes + lines_segments_sorted_index);
    for (PVCell **line = first_line_in_segment; line < segment_boundary; line++) {
      if (line == *free_lines_stack_cursor) {
        free_lines_stack_cursor++;
      } else {
        *used_lines_stack_cursor++ = line;
      }
    }
  }

  pve->state |= pve_state_lines_stack_sorted;
}

void
pve_state_unset_lines_stack_sorted (PVEnv *const pve)
{
  if (pve->state & pve_state_lines_stack_sorted) { // Lines stack is sorted.
    for (PVCell ***lines_stack_cursor = pve->lines_stack_head - 1; lines_stack_cursor >= pve->lines_stack; lines_stack_cursor--) {
      *lines_stack_cursor = NULL;
    }
    pve->state &= ~pve_state_lines_stack_sorted;
    return;
  } else {
    return;
  }
}

/**
 * @endcond
 */
