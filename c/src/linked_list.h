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



/**
 * @brief Compare function signature.
 *
 * @details The implementer has to ensure that when the value refernced by `a`
 *          is equal to the one addressed by `b`, than zero is returned.
 *          When the value identified by `a` is greater than the one pointed by `b`,
 *          than the return value has to be a positive integer, and otherwise a negative one.
 *
 *          Pseudocode:
 * @code
 *  if (*a > *b) return +1;
 *  else if (*a == *b) return 0;
 *  else return -1;
 * @endcode
 *
 * @param [in] a a pointer to the first value
 * @param [in] b a pointer to the second value
 * @return       result of comparing the values pointed by `a` and `b`
 */
typedef int
(*llist_compare_f) (const void *const a,
                    const void *const b);


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
  llist_elm_t *head;            /**< @brief The first element in the list. */
  size_t length;                /**< @brief The number of elements in the list. */
  llist_compare_f cmp;          /**< @brief Compare function for element data type. */
} llist_t;



/***********************************************/
/* Function prototypes for the llist_t entity. */
/***********************************************/

extern llist_t *
llist_new (llist_compare_f cmp);

extern void
llist_free (llist_t *l);

extern void
llist_add (llist_t *const l,
           void *const d);
extern void
llist_remove (llist_t *const l,
              void *const d);

extern void
llist_foreach (llist_t *const l,
               void (* fn) (void *const elm_data,
                            void *const aux_data),
               void *const aux_data);

extern llist_elm_t *
llist_nth (llist_t *const l,
           const size_t n);

extern void *
llist_nth_data (llist_t *const l,
                const size_t n);

extern llist_elm_t *
llist_find (llist_t *const l,
            void *const d,
            size_t *i);

extern void
llist_insert_at_position (llist_t *const l,
                          void *const d,
                          const size_t i);

extern void
llist_insert_after_elm (llist_t *const l,
                        llist_elm_t *const p,
                        void *const d);

extern void
llist_insert_before_elm (llist_t *const l,
                         llist_elm_t *const n,
                         void *const d);

extern llist_elm_t *
llist_last_elm (llist_t *const l);

extern void
llist_concat (llist_t *const la,
              llist_t *const lb);

extern void
llist_reverse (llist_t *const l);

extern void
llist_insertion_sort (llist_t *const l);

extern void
llist_adv_insertion_sort (llist_t *const l);

extern void
llist_merge_sort (llist_t *const l);



/***************************************************/
/* Function prototypes for the llist_elm_t entity. */
/***************************************************/

extern llist_elm_t *
llist_elm_new (void);

extern void
llist_elm_free (llist_elm_t *e);



#endif /* LINKED_LIST_H */
