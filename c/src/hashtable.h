/**
 * @file
 *
 * @brief Hashtable module definitions.
 * @details This module defines utilities based on the binary heap concept like:
 * max priority queue, min priority queue, and heap sort.
 *
 * @par hashtable.h
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

#ifndef HASHTABLE_H
#define HASHTABLE_H

#include <stdbool.h>

/* This should go into item.h header file. */
typedef void * item_t;

#define T htab_t

typedef struct htab_s *T;

extern T
htab_new (int hint,
          int cmp (const void *x, const void *y),
          unsigned hash (const void *key));

extern void
htab_free (T *t);

extern size_t
htab_length (T t);

extern size_t
htab_size (T t);

extern void *
htab_put (T t,
          const void *key,
          void *value);

extern void *
htab_get (T t,
          const void *key);

extern void *
htab_remove (T t,
             const void *key);

extern void
htab_map (T t,
          void apply(const void *key, void **value, void *cl),
          void *cl);

extern void **
htab_to_array (T t,
               void *end);

extern void
htab_bucket_filling_stats (T t,
                           size_t *stats,
                           size_t stats_size);

#undef T

#endif /* HASHTABLE_H */
