/**
 * @file
 *
 * @brief Block memory allocator module definitions.
 *
 * @details This module defines the #bma_t entity.
 *
 * @par memory_manager.h
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

#ifndef MEMORY_MANAGER_H
#define MEMORY_MANAGER_H



/**********************************************/
/* Type declarations.                         */
/**********************************************/

/**
 * @brief Memory allocator.
 *
 * @details This structure defines two function pointers that are used to allocate and free memory.
 *          The two functions have the definition consistent with the ANSI C `malloc` and `free`
 *          functions as defined in <stdlib.h>.
 */
typedef struct mem_allocator {
  void *(*libavl_malloc) (struct mem_allocator *alloc, size_t size);
  void (*libavl_free) (struct mem_allocator *alloc, void *ptr);
} mem_allocator_t;

/* Default memory allocator. */
extern mem_allocator_t rb_allocator_default;

extern void *rb_malloc (mem_allocator_t *, size_t);
extern void rb_free (mem_allocator_t *, void *);



#endif /* MEMORY_MANAGER_H */
