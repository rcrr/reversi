/**
 * @file
 *
 * @brief Memory manager module definitions.
 *
 * @details This module defines the #mem_allocator structure.
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



/* Forward declaration for the allocator structure */
typedef struct mem_allocator mem_allocator_t;



/*******************/
/* Function types. */
/*******************/

/**
 * @brief Allocates `size` bytes and returns a pointer to the allocated memory.
 *
 * @details The returned memory is not initialized. If size is `0`, then the function
 *          returns either `NULL`, or a unique pointer value that can later be successfully
 *          passed to mem_free_f().
 *
 * @param [in] alloc a pointer to the #mem_allocator structure
 * @param [in] size  number of bytes to allocate
 * @return           a pointer to the allocated memory
 */
typedef void *
mem_malloc_f (mem_allocator_t *alloc,
              size_t size);

/**
 * @brief Frees the memory space pointed to by `block`.
 *
 * @details The memory released must have been returned by a previous call to mem_malloc_f(),
 *          and the same `mem_allocator_t` object.
 *          Otherwise, or if mem_free_f() has already been called before on `block`, undefined
 *          behavior occurs. If `block` is `NULL`, no operation is performed.
 *
 * @param [in]     alloc a pointer to the #mem_allocator structure
 * @param [in,out] block the pointer to the freed memory
 */
typedef void
mem_free_f (mem_allocator_t *alloc,
            void *block);

/**********************************************/
/* Type declarations.                         */
/**********************************************/

/**
 * @brief Memory allocator.
 *
 * @details This structure defines two function pointers that are used to allocate and free memory.
 */
struct mem_allocator {
  mem_malloc_f *malloc;   /**< @brief Memory allocation function pointer. */
  mem_free_f   *free;     /**< @brief Memory de-allocation function pointer. */
};



/**********************************************/
/* Global constants.                          */
/**********************************************/

/*
 * Default memory allocator that uses malloc() and free().
 * It is a variable, but must be considered as a constant.
 */
extern mem_allocator_t mem_allocator_default;



/****************************************************/
/* Function prototypes for the allocator structure. */
/****************************************************/

extern void *
mem_basic_malloc (mem_allocator_t *alloc,
                  size_t size);

extern void
mem_basic_free (mem_allocator_t *alloc,
                void *block);



#endif /* MEMORY_MANAGER_H */
