/**
 * @file
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

static void
pve_print_cells (PVCell cells[],
                 int cells_size);

static void
pve_print_stack (PVCell *stack[],
                 int cells_size);

static void
pve_print_lines (PVCell *lines[],
                 int lines_size);

static void
pve_assert (PVEnv *pve);

static gboolean
pve_is_cell_free (const PVCell *const cell,
                  PVCell **stack_head,
                  PVCell **stack_bottom,
                  int stack_size);

static gboolean
pve_is_cell_active (const PVCell *const cell,
                    PVCell **stack_head,
                    PVCell **stack_bottom,
                    int stack_size);



/*
 * Internal variables and constants.
 */



/**************************************************/
/* Function implementations for the PVEnv entity. */ 
/**************************************************/

/**
 * @brief PVEnv structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * principal variation line structure is not `NULL`.
 *
 * @invariant Parameter `cells_size` cannot be negative. 
 * The invariant is guarded by an assertion.
 *
 * @param [in] cells_size the number of the cells available in the env structure
 * @param [in] lines_size the number of the lines available in the env structure
 * @return                a pointer to a new principal variation env structure
 */
PVEnv *
pve_new (const int cells_size,
         const int lines_size)
{
  g_assert(cells_size >= 0);
  g_assert(lines_size >= 0);

  static const size_t size_of_pve  = sizeof(PVEnv);
  static const size_t size_of_pvc  = sizeof(PVCell);
  static const size_t size_of_pvcp = sizeof(PVCell*);

  PVEnv *const pve = (PVEnv*) malloc(size_of_pve);
  g_assert(pve);

  pve->cells_size = cells_size;
  pve->cells = (PVCell*)  malloc(cells_size * size_of_pvc);

  pve->stack = (PVCell**) malloc(cells_size * size_of_pvcp);
  pve->stack_head = pve->stack;

  for (int i = 0; i < cells_size; i++) {
    (pve->cells + i)->move = (Square) -2; 
    (pve->cells + i)->next = NULL;
    *(pve->stack + i) = pve->cells + i;
  }
  
  pve->lines_size = lines_size;
  pve->lines = (PVCell**) malloc(lines_size * size_of_pvcp);
  for (int i = 0; i < lines_size; i++) {
    *(pve->lines + i) = NULL;
  }
  pve->lines_head = pve->lines;
  
  return pve;
}

/**
 * @brief PVEnv structure destructor.
 *
 * @invariant Parameter `pvl` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] pve the pointer to be deallocated
 * @return         always the NULL pointer
 */
PVEnv *
pve_free (PVEnv *pve)
{
  pve_assert(pve);

  g_free(pve->cells);
  g_free(pve->stack);
  g_free(pve->lines);
  g_free(pve);
  pve = NULL;

  return pve;
}

void
pve_print (PVEnv *pve)
{
  pve_assert(pve);
  printf("pve address: %p\n", (void*) pve);
  printf("pve cells_size: %d\n", pve->cells_size);
  printf("pve lines_size: %d\n", pve->lines_size);
  printf("pve stack_head: points_to=%p\n", (void*) pve->stack_head);
  printf("pve lines_head: points_to=%p\n", (void*) pve->lines_head);
  pve_print_cells(pve->cells, pve->cells_size);
  pve_print_stack(pve->stack, pve->cells_size);
  pve_print_lines(pve->lines, pve->lines_size);
}

/**
 * @brief Returns a free line pointer
 *
 * @param [in] pve a pointer to the principal variatin line structure
 * @return         a pointer to the next free pointers in the lines array
 */
PVCell **
pvl_create_line (PVEnv *pve)
{
  pve_assert(pve);
  // debug using ./build/bin/endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified-9 -s es
  printf("pvl_create_line: pve->lines_head=%p\n", (void *) pve->lines_head);
  return pve->lines_head++;
}

/**
 * @brief Adds the `move` to the given `line`.
 *
 * @param [in] pve  a pointer to the principal variatin line structure
 * @param [in] line reference to the pointer to the head of the current line
 * @param [in] move the move to add to the line
 */
void
pvl_add_move (PVEnv *pve,
              PVCell **line,
              Square move)
{
  pve_assert(pve);
  // This is the suspect! When we add a move the line is managed properly? mmmmm first add all the proper printf!!!!!
  PVCell *added_cell = *(pve->stack_head);
  pve->stack_head++;
  added_cell->move = move;
  added_cell->next = *line;
  *line = added_cell;
  printf("pvl_add_move: line=%p, added_cell->move=%s, added_cell->next=%p, added_cell=%p\n",
         (void *) line, square_to_string2(move), (void *) added_cell->next, (void *) added_cell);
}

/**
 * @brief Deletes the `line` and returns cells to the stack.
 *
 * @param [in] pve  a pointer to the principal variatin line sructure
 * @param [in] line a pointer to the head of the line to be deleted
 */
void
pvl_delete_line (PVEnv *pve,
                 PVCell **line)
{
  pve_assert(pve);
  PVCell *current = *line;
  printf("  pvl_delete_line: line=%p\n", (void *) line);
  while (current) {
    printf("  pvl_delete_line: current=%p, current->move=%s, current->next=%p\n", (void *) current, square_to_string2(current->move), (void *) current->next);
    pve->stack_head--;
    //printf("  pvl_delete_line: pve->stack_head=%p, *(pve->stack_head)=%p\n", (void *) pve->stack_head, (void *) *(pve->stack_head));
    *(pve->stack_head) = current;
    //printf("  pvl_delete_line: *(pve->stack_head)=%p\n", (void *) *(pve->stack_head));
    current = current->next;
  }
  pve->lines_head--;
  // it was an error *(pve->lines_head) = *line;
  printf("pvl_delete_line: pve->lines_head=%p, *(pve->lines_head)=%p, *line=%p\n", (void *) pve->lines_head, (void *) *(pve->lines_head), (void* ) *line);
  *line = NULL;
}

void
pvl_copy_line (PVEnv *pve,
               PVCell **from_line,
               PVCell **to_line)
{
  pve_assert(pve);
  PVCell *previous = *to_line;
  for (const PVCell *c = *from_line; c != NULL; c = c->next) {
    PVCell *target_cell = *(pve->stack_head);
    pve->stack_head++;
    target_cell->move = c->move;
    target_cell->next = NULL;
    previous = target_cell;
    previous = previous->next;
  }
}

/*
 * Internal functions.
 */

static void
pve_print_cells (PVCell cells[],
                 int cells_size)
{
  for (int i = 0; i < cells_size; i++) {
    printf("cells[%2d]: move=%d, next=%p, address=%p\n", i, cells[i].move, (void*) cells[i].next, (void*) &cells[i]);
  }
}

static void
pve_print_stack (PVCell *stack[],
                 int cells_size)
{
  for (int i = 0; i < cells_size; i++) {
    printf("stack[%2d]: points_to=%p, address=%p\n", i, (void*) stack[i], (void*) &(stack[i]));
  }
}

static void
pve_print_lines (PVCell *lines[],
                 int lines_size)
{
  for (int i = 0; i < lines_size; i++) {
    printf("lines[%2d]: points_to=%p, address=%p\n", i, (void*) lines[i], (void*) &(lines[i]));
  }
}

static void
pve_assert (PVEnv *pve)
{
  g_assert(pve);
  const int lines_in_use_count = pve->lines_head - pve->lines;
  const int stack_in_use_count = pve->stack_head - pve->stack;
  g_assert(lines_in_use_count >= 0);
  g_assert(lines_in_use_count < pve->lines_size);
  g_assert(stack_in_use_count >= 0);
  g_assert(stack_in_use_count < pve->cells_size);
  for (int i = 0; i < lines_in_use_count; i++) {
    const PVCell *const cell = *(pve->lines + i);
    printf("pve_assert: line_address=%p, cell=%p", (void *) (pve->lines + i), (void *) cell);
    for (const PVCell *c = cell; c != NULL; c = c->next) {
      printf("(c=%p, m=%s, n=%p)", (void *) c, square_to_string2(c->move), (void *) c->next);
      g_assert(pve_is_cell_active(c, pve->stack_head, pve->stack, pve->cells_size));
    }
    printf("\n");
  }
}


static gboolean
pve_is_cell_free (const PVCell *const cell,
                  PVCell **stack_head,
                  PVCell **stack_bottom,
                  int stack_size)
{
  const int stack_in_use_count = stack_head - stack_bottom;
  g_assert(stack_in_use_count >= 0);
  const int stack_free_count = stack_size - stack_in_use_count;
  g_assert(stack_free_count >= 0);
  for (int i = 0; i < stack_free_count; i++) {
    if (cell == *(stack_head + i)) return TRUE;
  }
  /*
  printf("  pve_is_cell_free: cell=%p, stack_in_use_count=%d, stack_head=%p, stack_bottom=%p, stack_size=%d, stack_free_count=%d\n",
         (void *) cell, stack_in_use_count, (void *) stack_head, (void *) stack_bottom, stack_size, stack_free_count);
  for (int i = 0; i < stack_free_count; i++) {
    printf("  pve_is_cell_free: i=%d, (stack_head + i)=%p, *(stack_headg + i)=%p\n",
           i, (void *) (stack_head + i), (void *) *(stack_head + i));
  }
  */
  return FALSE;
}


static gboolean
pve_is_cell_active (const PVCell *const cell,
                    PVCell **stack_head,
                    PVCell **stack_bottom,
                    int stack_size)
{
  const gboolean result = !pve_is_cell_free(cell, stack_head, stack_bottom, stack_size);
  if (!result) {
    const int stack_in_use_count = stack_head - stack_bottom;
    const int stack_free_count = stack_size - stack_in_use_count;
    printf("  pve_is_cell_active: cell=%p, stack_head=%p, stack_bottom=%p, stack_size=%d, stack_in_use_count=%d, stack_free_count=%d\n",
           (void *) cell, (void *) stack_head, (void *) stack_bottom, stack_size, stack_in_use_count, stack_free_count);
    for (int i = 0; i < stack_free_count; i++) {
      printf("  pve_is_cell_active, free_cells: i=%3d, (stack_head + i)=%p, *(stack_head + i)=%p\n",
             i, (void *) (stack_head + i), (void *) *(stack_head + i));
    }
  }
  return result;
}
