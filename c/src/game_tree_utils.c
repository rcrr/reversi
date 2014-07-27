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
pvl_print_cells (PrincipalVariationCell cells[], int size);

static void
pvl_print_stack (PrincipalVariationCell *stack[], int size);



/*
 * Internal variables and constants.
 */



/*******************************************************************/
/* Function implementations for the PrincipalVariationLine entity. */ 
/*******************************************************************/

/**
 * @brief PrincipalVariationLine structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * principal variation line structure is not `NULL`.
 *
 * @invariant Parameter `size` cannot be negative. 
 * The invariant is guarded by an assertion.
 *
 * @param [in] size the size of the cell stack
 * @return     a pointer to a new principal variation line structure
 */
PrincipalVariationLine *
pvl_new (const int size)
{
  g_assert(size >= 0);

  PrincipalVariationLine *pvl;
  static const size_t size_of_pvl = sizeof(PrincipalVariationLine);

  pvl = (PrincipalVariationLine*) malloc(size_of_pvl);
  g_assert(pvl);

  pvl->size = size;
  pvl->head = NULL;
  pvl->cells = (PrincipalVariationCell*)  malloc(size * sizeof(PrincipalVariationCell));
  pvl->stack = (PrincipalVariationCell**) malloc(size * sizeof(PrincipalVariationCell*));

  for (int i = 0; i < size; i++) {
    (pvl->cells + i)->move = (Square) -2; 
    (pvl->cells + i)->next = NULL;
    *(pvl->stack + i) = pvl->cells + i;
 }
  
  return pvl;
}

/**
 * @brief PrincipalVariationLine structure destructor.
 *
 * @invariant Parameter `pvl` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] pvl the pointer to be deallocated
 * @return         always the NULL pointer
 */
PrincipalVariationLine *
pvl_free (PrincipalVariationLine *pvl)
{
  g_assert(pvl);

  g_free(pvl->cells);
  g_free(pvl->stack);
  g_free(pvl);
  pvl = NULL;

  return pvl;
}

void
pvl_print (PrincipalVariationLine *pvl)
{
  printf("pvl address: %p\n", (void*) pvl);
  printf("pvl size: %d\n", pvl->size);
  pvl_print_cells(pvl->cells, pvl->size);
  pvl_print_stack(pvl->stack, pvl->size);
}


/*
 * Internal functions.
 */

static void
pvl_print_cells (PrincipalVariationCell cells[], int size)
{
  for (int i = 0; i < size; i++) {
    printf("cells[%d]: move=%d, next=%p, address=%p\n", i, cells[i].move, (void*) cells[i].next, (void*) &cells[i]);
  }
}

static void
pvl_print_stack (PrincipalVariationCell *stack[], int size)
{
  for (int i = 0; i < size; i++) {
    printf("stack[%d]: points_to=%p, address=%p\n", i, (void*) stack[i], (void*) &(stack[i]));
  }
}
