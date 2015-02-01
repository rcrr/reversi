/**
 * @file
 *
 * @brief Linked list module implementation.
 * @details This module defines a linked list implementation.
 *
 * @par linked_list.c
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
#include <assert.h>

#include "linked_list.h"



/****************************************************/
/* Function implementations for the llist_t entity. */
/****************************************************/

/**
 * @brief Linked list structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * linked list structure is not `NULL`.
 *
 * @return a pointer to a new linked list structure
 */
llist_t *
llist_new (void)
{
  llist_t *l;
  static const size_t size_of_t = sizeof(llist_t);
  l = (llist_t *) malloc(size_of_t);
  assert(l);
  return l;
}

/**
 * @brief Deallocates the memory previously allocated by a call to #llist_new.
 *
 * @details If a null pointer is passed as argument, no action occurs.
 *
 * @param [in,out] l the pointer to be deallocated
 */
void
llist_free (l)
     llist_t *l;
{
  free(l);
}

/**
 * @brief Adds a new element at the beginning of the linked list.
 *
 * @details A new element is created, having as data the parameter d, and
 *          as next the first element of the list, then it becomes the actual
 *          first element of the list.
 *
 * An assertion checks that the received pointer to the
 * linked list structure is not `NULL`.
 *
 * @param [in,out] l the linked list
 * @param [in]     d the data referenced by the added element
 */
void
llist_add (l, d)
     llist_t *const l;
     void *const d;
{
  assert(l);
  llist_elm_t *const e = llist_elm_new();
  e->data = d;
  e->next = l->fe;
  l->fe = e;
}


/********************************************************/
/* Function implementations for the llist_elm_t entity. */
/********************************************************/

/**
 * @brief Element structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * element is not `NULL`.
 *
 * @return a pointer to a new element structure
 */
llist_elm_t *
llist_elm_new (void)
{
  llist_elm_t *e;
  static const size_t size_of_t = sizeof(llist_elm_t);
  e = (llist_elm_t *) malloc(size_of_t);
  assert(e);
  return e;
  ;
}

void
llist_elm_free (llist_elm_t *e)
{
  free(e);
}
