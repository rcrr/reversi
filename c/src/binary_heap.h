/**
 * @file
 *
 * @brief Binary Heap module definitions.
 * @details This module defines utilities based on the binary heap concept like:
 * max priority queue, min priority queue, and heap sort.
 *
 * @par binary_heap.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2021 Roberto Corradini. All rights reserved.
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

#ifndef BINARY_HEAP_H
#define BINARY_HEAP_H

#include <stdbool.h>

/* This should go into item.h header file. */
typedef void * item_t;



/*
 * To be added, maybe:
 *
 * resize
 * remove
 * remove_all
 * rebuild_pq
 * insert_all
 * compute_item_priority // this is tricky and requires a further function pointer ....
 * sort
 * invert
 * get_array_and_destroy
 */



typedef int
(*bihp_priority_f) (item_t a);

typedef void
(*bihp_print_item_f) (item_t a,
                      FILE *f);

typedef void
(*bihp_item_call_back_on_swap_f) (item_t a,
                                int index);

/**
 * @enum bihp_pq_type_t
 * @brief Priority queue type.
 *
 * Priority queues have to be defined either maximum or minimum.
 * When of max type the `peek() / pull()` operations return the element
 * having the highest priority. On the contrary when of min type the
 * returned element has the lowest priority.
 */
typedef enum
  {
   BIHP_PQ_TYPE_MAX,      /**< Maximum priority queue. */
   BIHP_PQ_TYPE_MIN,      /**< Minimum priority queue. */
   BIHP_PQ_TYPE_INVALID   /**< Invalid priority queue. */
  } bihp_pq_type_t;

typedef struct bihp_pq_s * bihp_pq_t;

typedef struct bihp_pq_s const * bihp_const_pq_t;

extern bihp_pq_t
bihp_pq_create (bihp_pq_type_t t,
                size_t array_size,
                bihp_priority_f pf,
                bihp_print_item_f pif,
                bihp_item_call_back_on_swap_f icbf);

extern void
bihp_pq_destroy (bihp_pq_t q);

extern bihp_pq_type_t
bihp_pq_type (const bihp_const_pq_t q);

extern size_t
bihp_pq_heap_size (const bihp_const_pq_t q);

extern size_t
bihp_pq_array_size (const bihp_const_pq_t q);

extern bool
bihp_pq_is_empty (const bihp_const_pq_t q);

extern void
bihp_pq_clear (bihp_pq_t q);

extern void
bihp_pq_print (bihp_const_pq_t q,
               FILE *f);

extern item_t
bihp_pq_peek (bihp_const_pq_t q);

extern void
bihp_pq_insert (bihp_pq_t q,
                item_t item);

extern item_t
bihp_pq_pull (bihp_pq_t q);

extern bool
bihp_pq_has_heap_property (bihp_const_pq_t q,
                           long long int *offending_parent_index);

extern void
bihp_pq_update_priority (bihp_pq_t q,
                         int index);

extern int
bihp_pq_find_index (bihp_pq_t q,
                    item_t item);

#endif /* BINARY_HEAP_H */
