/**
 * @file
 *
 * @brief Linked list module definitions.
 * @details This module defines the #llist_t entity.
 *
 * @par linked_list.h
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

#ifndef LINKED_LIST_H
#define LINKED_LIST_H



/**********************************************/
/* Type declarations.                         */
/**********************************************/

/**
 * @brief A linked list element.
 */
typedef struct llist_elm_t_ {
  void *data;                   /**< @brief The payload of this element. */
  struct llist_elm_t_ *next;    /**< @brief The next element in the list. */
} llist_elm_t;

/**
 * @brief A linked list.
 */
typedef struct {
  llist_elm_t *fe;              /**< @brief The first element in the list. */
  size_t length;                /**< @brief The number of elements in the list. */
} llist_t;



/***************************************************/
/* Function prototypes for the llist_elm_t entity. */
/***************************************************/

extern llist_elm_t *
llist_elm_new (void);

extern void
llist_elm_free (llist_elm_t *e);

extern void
llist_remove (llist_t *const l,
              void *const d);

extern void
llist_foreach (llist_t *const l,
               void (* fn) (void *const elm_data,
                            void *const aux_data),
               void *const aux_data);

extern llist_elm_t *
llist_find (llist_t *const l,
            void *const d,
            size_t *i);



/***********************************************/
/* Function prototypes for the llist_t entity. */
/***********************************************/

extern llist_t *
llist_new (void);

extern void
llist_free (llist_t *l);

extern void
llist_add (llist_t *const l,
           void *const d);



#endif /* LINKED_LIST_H */
