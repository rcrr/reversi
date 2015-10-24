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


/* Allocates |size| bytes of space using |malloc()|.
   Returns a null pointer if allocation fails. */
void *
rb_malloc (mem_allocator_t *allocator, size_t size)
{
  assert (allocator != NULL && size > 0);
  return malloc (size);
}

/* Frees |block|. */
void
rb_free (mem_allocator_t *allocator, void *block)
{
  assert (allocator != NULL && block != NULL);
  free (block);
}

/* Default memory allocator that uses |malloc()| and |free()|. */
mem_allocator_t rb_allocator_default = { rb_malloc, rb_free };
