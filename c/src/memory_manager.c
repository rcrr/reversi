/**
 * @file
 *
 * @brief Block memory allocator module.
 *
 * @details Memory management by mean of block allocation is a technique designed to
 *          have the flexibility of dynamic allocation togheter with a reduced speed penalty.
 *
 *          All the symbols exported by the module adopt the `bma_` prefix.
 *
 * @par memory_manager.c
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

#include "memory_manager.h"



/**
 * @brief Default memory allocator.
 *
 * @details It is defined as a variable, but must not be modified.
 *          It is a constant structure.
 *          It is a front end for ANSI C `malloc` and `free` functions.
 */
mem_allocator_t mem_allocator_default = { mem_basic_malloc,
                                          mem_basic_free };



/**
 * @brief Allocates `size` bytes of space using `malloc`.
 *
 * @details Returns a `NULL` pointer if allocation fails.
 *
 * @param [in] alloc a pointer to the #mem_allocator structure
 * @param [in] size  number of bytes to allocate
 * @return           a pointer to the allocated memory
 */
void *
mem_basic_malloc (mem_allocator_t *alloc,
                  size_t size)
{
  assert (alloc != NULL && size > 0);
  return malloc (size);
}

/**
 * @brief Frees the memory space pointed to by `block`.
 *
 * @details The memory released must have been returned by a previous call to mem_basic_malloc().
 *          Otherwise, or if mem_basic_free() has already been called before on `block`, undefined
 *          behavior occurs.
 *
 * @param [in]     alloc a pointer to the #mem_allocator structure
 * @param [in,out] block the pointer to the freed memory
 */
void
mem_basic_free (mem_allocator_t *alloc,
                void *block)
{
  assert (alloc != NULL && block != NULL);
  free (block);
}
