/**
 * @file
 *
 * @brief Binary Peap module definitions.
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
typedef const void * item_t;

typedef struct bihp_max_priority_queue_s * bihp_max_priority_queue_t;

typedef struct bihp_max_priority_queue_s const * bihp_const_max_priority_queue_t;

typedef int
(*bihp_priority_function) (item_t a);

typedef void
(*bihp_print_item_function) (item_t a,
                             FILE *f);

extern bihp_max_priority_queue_t
bihp_maxpq_create (size_t array_size,
                   bihp_priority_function pf,
                   bihp_print_item_function pif);

extern void
bihp_maxpq_destroy (bihp_max_priority_queue_t q);

extern void
bihp_maxpq_print (bihp_const_max_priority_queue_t q,
                  FILE *f);

extern item_t
bihp_maxpq_maximum (bihp_const_max_priority_queue_t q);

extern void
bihp_maxpq_insert (bihp_max_priority_queue_t q,
                   item_t item);

extern item_t
bihp_maxpq_extract_max (bihp_max_priority_queue_t q);

extern bool
bihp_maxpq_has_max_heap_property (bihp_const_max_priority_queue_t q);

/*
 * To be added:
 *
 * resize
 * heap_size
 * array_size
 * is_empty
 * rebuild_maxpq
 * insert_bulk
 * compute_item_priority
 * sort
 * invert
 * get_array_and_destroy
 *
 * The same interface for _minpq_
 */


#endif /* BINARY_HEAP_H */
