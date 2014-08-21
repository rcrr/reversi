/**
 * @file
 *
 * @todo pve_print has to:
 *       - Be renamed into pve_to_string.
 *       - Be documented.
 *       - Has to allocate a GString and return it.
 *       - Has to receive constant arguments!
 *       - Should be designed avoiding repeating constant string each line. Having section each one more like a csv.
 * 
 * @todo pve_verify_consistency is very inefficient.
 *
 * @todo Should we introduce a typedef for PVLine and so get rid of the three star sin?
 *
 * @brief Game tree utilities module implementation.
 * @details Provides functions to support the game tree expansion.
 *
 * @par game_tree_utils.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014 Roberto Corradini. All rights reserved.
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

#include <glib.h>

#include "game_tree_utils.h"



/*
 * Prototypes for internal functions.
 */

static gboolean
pve_is_cell_free (const PVEnv *const pve,
                  const PVCell *const cell);

static gboolean
pve_is_cell_active (const PVEnv *const pve,
                    const PVCell *const cell);

static gboolean
pve_is_line_free (const PVEnv *const pve,
                  PVCell **line);

static gboolean
pve_is_line_active (const PVEnv *const pve,
                    PVCell **line);



/*
 * Internal variables and constants.
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
 * @brief Exact solution structure destructor.
 *
 * @invariant Parameter `es` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] es the pointer to be deallocated
 * @return        always the NULL pointer
 */
ExactSolution *
exact_solution_free (ExactSolution *es)
{
  g_assert(es);

  if (es->solved_game_position) game_position_free(es->solved_game_position);
  if (es->final_board) board_free(es->final_board);

  free(es);
  es = NULL;

  return es;
}

/**
 * @brief Returns a formatted string describing the exact solution structure.
 *
 * @todo If computed the final board should be added.
 *
 * The returned string has a dynamic extent set by a call to malloc. It must then properly
 * garbage collected by a call to free when no more referenced.
 *
 * @invariant Parameter `es` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] es a pointer to the exact solution structure
 * @return        a string being a representation of the structure
 */
gchar *
exact_solution_to_string (const ExactSolution * const es)
{
  g_assert(es);

  gchar *es_to_string;
  GString *tmp = g_string_sized_new(32);

  g_string_append_printf(tmp, "%s\n",
                         game_position_print(es->solved_game_position));
  g_string_append_printf(tmp, "[node_count=%llu, leaf_count=%llu]\n",
                         es->node_count, 
                         es->leaf_count);
  g_string_append_printf(tmp, "Final outcome: best move=%s, position value=%d\n",
                         square_as_move_to_string2(es->pv[0]),
                         es->outcome);

  if (es->pv_length != 0) {
    gchar *pv_to_s = exact_solution_pv_to_string(es);
    g_string_append_printf(tmp, "PV: %s\n", pv_to_s);
    g_free(pv_to_s);
  }

  es_to_string = tmp->str;
  g_string_free(tmp, FALSE);
  return es_to_string;
}

/**
 * @brief Prints the principal variation line into the returning string.
 *
 * @param [in] es a pointer to the exact solution structure
 * @return        a string being a representation of the pv line
 */
gchar *
exact_solution_pv_to_string (const ExactSolution *const es)
{
  gchar *pv_to_string;
  GString *tmp = g_string_sized_new(16);

  for (int i = 0; i < es->pv_length; i++) {
    g_string_append_printf(tmp, "%s", square_as_move_to_string2(es->pv[i]));
    if (i != es->pv_length) g_string_append_printf(tmp, " ");
  }

  pv_to_string = tmp->str;
  g_string_free(tmp, FALSE);
  return pv_to_string;
}



/**************************************************/
/* Function implementations for the PVEnv entity. */ 
/**************************************************/

/**
 * @brief PVEnv structure constructor.
 *
 * @details The sizing of the structure's components is done taking into account
 * a worst case scenario, where every disc put on the board can cost two search
 * levels, one for the move and one for a potential pass, plus two extra slots
 * are reserved if the minimax algorithm check the leaf condition consuming two
 * consecutive pass moves.
 * These assumptions can be unrealistic, but are the only one that fit all the conditions.
 *
 * Assertions check that the received pointers to the allocated
 * structures are not `NULL`.
 *
 * @invariant Parameter `empty_count` cannot be negative. 
 * The invariant is guarded by an assertion.
 *
 * @param [in] empty_count the number of empty cells in the board, or the expected depth
 *                         for the search 
 * @return                 a pointer to a new principal variation env structure
 */
PVEnv *
pve_new (const int empty_count)
{
  g_assert(empty_count >= 0);
  const int lines_size = 2 * (empty_count + 1) + 1;
  const int cells_size = ((empty_count + 2) * ((empty_count + 2) + 1)) / 2;
  
  static const size_t size_of_pve   = sizeof(PVEnv);
  static const size_t size_of_pvc   = sizeof(PVCell);
  static const size_t size_of_pvcp  = sizeof(PVCell*);
  static const size_t size_of_pvcpp = sizeof(PVCell**);

  PVEnv *const pve = (PVEnv*) malloc(size_of_pve);
  g_assert(pve);

  pve->cells_size = cells_size;
  pve->cells = (PVCell*)  malloc(cells_size * size_of_pvc);

  pve->cells_stack = (PVCell**) malloc(cells_size * size_of_pvcp);
  pve->cells_stack_head = pve->cells_stack;

  for (int i = 0; i < cells_size; i++) {
    (pve->cells + i)->move = invalid_move; 
    (pve->cells + i)->next = NULL;
    *(pve->cells_stack + i) = pve->cells + i;
  }
  
  pve->lines_size = lines_size;
  pve->lines = (PVCell**) malloc(lines_size * size_of_pvcp);
  pve->lines_stack = (PVCell***) malloc(lines_size * size_of_pvcpp);

  for (int i = 0; i < lines_size; i++) {
    *(pve->lines + i) = NULL;
    *(pve->lines_stack + i) = pve->lines + i;
  }
  pve->lines_stack_head = pve->lines_stack;
  
  return pve;
}

/**
 * @brief PVEnv structure destructor.
 *
 * @invariant Parameter `pve` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in,out] pve the pointer to be deallocated
 * @return             always the NULL pointer
 */
PVEnv *
pve_free (PVEnv *pve)
{
  g_assert(pve_verify_consistency(pve));

  g_free(pve->cells);
  g_free(pve->cells_stack);
  g_free(pve->lines);
  g_free(pve->lines_stack);
  g_free(pve);
  pve = NULL;

  return pve;
}

/**
 * @brief Verifies that the pve structure is consistent.
 *
 * @param [in] pve a pointer to the principal variation environment
 * @return         true if everithing is ok
 */
gboolean
pve_verify_consistency (const PVEnv *const pve)
{
  gboolean ret = TRUE;
  int err_code = 0;
  if (!pve) return FALSE;

  /*
   * Tests that the head pointers are within the proper bounds.
   */
  const int lines_in_use_count = pve->lines_stack_head - pve->lines_stack;
  const int cells_in_use_count = pve->cells_stack_head - pve->cells_stack;
  if (!(lines_in_use_count >= 0) ||
      !(lines_in_use_count < pve->lines_size) ||
      !(cells_in_use_count >= 0) ||
      !(cells_in_use_count < pve->cells_size)) {
    ret = FALSE;
    err_code = 1;
    goto end;
  }

  /*
   * Tests that free lines and free cells are unique has to be added.
   */

  /*
   * Tests that active lines and active cells are unique has to be added.
   */

  /*
   * Tests that all active lines have active cells, and then that the
   * acrive lines and cells count are consistent with their stack pointers.
   */
  int active_cell_count = 0;
  int active_line_count = 0;
  for (int i = 0; i < pve->lines_size; i++) {
    PVCell **line = pve->lines + i;
    if (pve_is_line_active(pve, line)) {
      active_line_count++;
      for (const PVCell *c = *line; c != NULL; c = c->next) {
        if (!pve_is_cell_active(pve, c)) {
          ret = FALSE;
          err_code = 2;
          goto end;
        } else {
          active_cell_count++;
        }
      }
    }
  }
  if (active_line_count != lines_in_use_count) {
    ret = FALSE;
    err_code = 3;
    goto end;
  }
  if (active_cell_count != cells_in_use_count) {
    ret = FALSE;
    err_code = 4;
    goto end;
  }
  ;
 end:
  if (!ret) {
    printf("pve_verify_consistency: error code %d.\n", err_code);
    printf("pve_verify_consistency: active_line_count=%d, active_cell_count=%d\n", active_line_count, active_cell_count);
  }
  return ret;
}

/**
 * @brief Prints the `pve` internals into the returning string.
 *
 * @param [in] pve  a pointer to the principal variation environment
 * @return          a string reporting the pve internals
 */
gchar *
pve_internals_to_string (const PVEnv *const pve)
{
  g_assert(pve);

  gchar *pve_to_string;
  GString *tmp = g_string_sized_new(1024);

  g_string_append_printf(tmp, "# PVE STRUCTURE HEADER\n");
  g_string_append_printf(tmp, "pve address: %p\n", (void*) pve);
  g_string_append_printf(tmp, "pve cells_size: %d\n", pve->cells_size);
  g_string_append_printf(tmp, "pve lines_size: %d\n", pve->lines_size);
  g_string_append_printf(tmp, "pve cells_stack_head points_to: %p\n", (void*) pve->cells_stack_head);
  g_string_append_printf(tmp, "pve lines_stack_head points_to: %p\n", (void*) pve->lines_stack_head);
  const int line_in_use_count = pve->lines_stack_head - pve->lines_stack;
  const int cell_in_use_count = pve->cells_stack_head - pve->cells_stack;
  g_string_append_printf(tmp, "pve line_in_use_count: %d\npve cell_in_use_count: %d\n", line_in_use_count, cell_in_use_count);
  for (int i = 0; i < pve->lines_size; i++) {
    PVCell **line = pve->lines + i;
    if (pve_is_line_active(pve, line)) {
      gchar *pve_root_line_to_s = pve_line_print_internals(pve, (const PVCell**) line);
      g_string_append_printf(tmp, "line_internals: %s\n", pve_root_line_to_s);
      g_free(pve_root_line_to_s);
    }
  }
  g_string_append_printf(tmp, "\n# PVE CELLS\n");
  g_string_append_printf(tmp, "ordinal;address;move;next\n");
  for (int i = 0; i < pve->cells_size; i++) {
    g_string_append_printf(tmp, "%4d;%p;%s;%p\n",
                           i, (void*) (pve->cells + i), square_as_move_to_string2((pve->cells + i)->move), (void*) (pve->cells + i)->next);
  }
  g_string_append_printf(tmp, "\n# PVE CELLS STACK\n");
  g_string_append_printf(tmp, "ordinal;address;points_to\n");
  for (int i = 0; i < pve->cells_size; i++) {
    g_string_append_printf(tmp, "%4d;%p;%p\n",
                           i, (void*) (pve->cells_stack + i),  (void*) (*pve->cells_stack + i));
  }
  g_string_append_printf(tmp, "\n# PVE LINES\n");
  g_string_append_printf(tmp, "ordinal;address;points_to\n");
  for (int i = 0; i < pve->lines_size; i++) {
    g_string_append_printf(tmp, "%2d;%p;%p\n",
                           i, (void*) (pve->lines + i),  (void*) *(pve->lines + i));
  }
  g_string_append_printf(tmp, "\n# PVE LINES STACK\n");
  g_string_append_printf(tmp, "ordinal;address;points_to\n");
  for (int i = 0; i < pve->lines_size; i++) {
    g_string_append_printf(tmp, "%2d;%p;%p\n",
                           i, (void*) (pve->lines_stack + i),  (void*) *(pve->lines_stack + i));
  }

  pve_to_string = tmp->str;
  g_string_free(tmp, FALSE);
  return pve_to_string;
}

/**
 * @brief Returns a free line pointer.
 *
 * @details The environment is modified because the line stack fill pointer,
 * lines_stack_head, is incremented. 
 *
 * @param [in,out] pve a pointer to the principal variation environment
 * @return             a pointer to the next free line
 */
PVCell **
pve_line_create (PVEnv *pve)
{
  g_assert(pve_verify_consistency(pve));
  PVCell **line_p = *(pve->lines_stack_head);
  *(line_p) = NULL;
  pve->lines_stack_head++;
  return line_p;
}

/**
 * @brief Adds the `move` to the given `line`.
 *
 * @details The function inserts a new cell at the front of the linked list of cells.
 * A cell is retrieved from the stack, the stack fill pointer is then decremented.
 * The cell is filled with the move and the previous first cell as next.
 * The line is updated referring to the new added cell.
 *
 * The `line` must be active, it is an error to call the function on free lines.
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
  g_assert(pve_verify_consistency(pve));
  PVCell *added_cell = *(pve->cells_stack_head);
  pve->cells_stack_head++;
  added_cell->move = move;
  added_cell->next = *line;
  *line = added_cell;
}

/**
 * @brief Deletes the `line` and returns cells to the cell stack.
 *
 * @param [in,out] pve  a pointer to the principal variation environment
 * @param [in,out] line the line to be deleted
 */
void
pve_line_delete (PVEnv *pve,
                 PVCell **line)
{
  g_assert(pve_verify_consistency(pve));
  PVCell *cell = *line; // A poiter to the first cell, or null if the line is empty.
  while (cell) {
    pve->cells_stack_head--;
    *(pve->cells_stack_head) = cell;
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
gchar *
pve_line_print_internals (const PVEnv *const pve,
                          const PVCell **const line)
{
  gchar *line_to_string;
  GString *tmp = g_string_sized_new(32);

  g_string_append_printf(tmp, "line_address=%p, first_cell=%p", (void *) line, (void *) *line);
  if (*line) g_string_append_printf(tmp, ", chain: ");
  for (const PVCell *c = *line; c != NULL; c = c->next) {
    g_string_append_printf(tmp, "(c=%p, m=%s, n=%p)", (void *) c, square_as_move_to_string2(c->move), (void *) c->next);
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
gchar *
pve_line_to_string (const PVEnv *const pve,
                    const PVCell **const line)
{
  gchar *line_to_string;
  GString *tmp = g_string_sized_new(16);

  for (const PVCell *c = *line; c != NULL; c = c->next) {
    g_string_append_printf(tmp, "%s", square_as_move_to_string2(c->move));
    if (c->next) g_string_append_printf(tmp, " ");
  }

  line_to_string = tmp->str;
  g_string_free(tmp, FALSE);
  return line_to_string;
}

/**
 * @brief Copies the pve line into the exact solution structure.
 *
 * Exact solution `es` must have an empty pv field. The assumption is
 * checked assuring that the pv_length field is equal to zero.
 * The assumption is guarded by an assertion.  
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
 * @brief Search node structure destructor.
 *
 * @invariant Parameter `sn` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] sn the pointer to be deallocated
 * @return        always the NULL pointer
 */
SearchNode *
search_node_free (SearchNode *sn)
{
  g_assert(sn);

  free(sn);
  sn = NULL;

  return sn;
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



/*
 * Internal functions.
 */

static gboolean
pve_is_cell_free (const PVEnv *const pve,
                  const PVCell *const cell)
{
  const int cell_in_use_count = pve->cells_stack_head - pve->cells_stack;
  const int cell_free_count = pve->cells_size - cell_in_use_count;
  for (int i = 0; i < cell_free_count; i++) {
    if (cell == *(pve->cells_stack_head + i)) return TRUE;
  }
  return FALSE;
}

static gboolean
pve_is_cell_active (const PVEnv *const pve,
                    const PVCell *const cell)
{
  return !pve_is_cell_free(pve, cell);
}

static gboolean
pve_is_line_free (const PVEnv *const pve,
                  PVCell **line)
{
  const int line_in_use_count = pve->lines_stack_head - pve->lines_stack;
  const int line_free_count = pve->lines_size - line_in_use_count;
  for (int i = 0; i < line_free_count; i++) {
    if (line == *(pve->lines_stack_head + i)) return TRUE;
  }
  return FALSE;
}

static gboolean
pve_is_line_active (const PVEnv *const pve,
                    PVCell **line)
{
  return !pve_is_line_free(pve, line);
}
