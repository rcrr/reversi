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



/*************************/
/* Forward declarations. */
/*************************/

/**
 * @struct mem_allocator_t
 * @brief Memory allocator.
 *
 * @details
 * Forward declaration for the mem_allocator structure
 */
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
 * @param [in] alloc a pointer to the mem_allocator_t structure
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
 * @param [in]     alloc a pointer to the #mem_allocator_t structure
 * @param [in,out] block the pointer to the freed memory
 */
typedef void
mem_free_f (mem_allocator_t *alloc,
            void *block);



/**************************************************/
/* Type declarations for the allocator structure. */
/**************************************************/

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



/**********************************************************/
/* Function prototypes for the basic allocator structure. */
/**********************************************************/

extern void *
mem_basic_malloc (mem_allocator_t *alloc,
                  size_t size);

extern void
mem_basic_free (mem_allocator_t *alloc,
                void *block);



/*
 *
 * Debug memory tracker.
 *
 */

/************************************************************************/
/* Type declarations for the debugging memory tracker allocator family. */
/************************************************************************/

/**
 * @enum mem_dbg_policy_t
 * @brief Memory tracking policy.
 *
 * @details
 * `MEM_DBG_TRACK` and `MEM_DBG_NO_TRACK` should be self-explanatory.<br>
 *    `MEM_DBG_FAIL_COUNT` takes an argument specifying after how many allocations
 *    further allocations should always fail.<br>
 *    `MEM_DBG_FAIL_PERCENT` takes an argument specifying an integer percentage of
 *    allocations to randomly fail.<br>
 *
 * `MEM_DBG_SUBALLOC` causes small blocks to be carved out of larger ones allocated with `malloc()`.<br>
 *    This is a good idea for two reasons: `malloc()` can be slow and `malloc()` can waste a lot of
 *    space dealing with the small blocks.<br>
 *    Suballocation cannot be implemented in an entirely
 *    portable way because of alignment issues, but the test program here requires the user
 *    to specify the alignment needed, and its use is optional anyhow.
 */
typedef enum {
  MEM_DBG_TRACK,         /**< Track allocation for leak detection. */
  MEM_DBG_NO_TRACK,      /**< No leak detection. */
  MEM_DBG_FAIL_COUNT,    /**< Fail allocations after a while. */
  MEM_DBG_FAIL_PERCENT,  /**< Fail allocations randomly. */
  MEM_DBG_SUBALLOC       /**< Suballocate from larger blocks. */
} mem_dbg_policy_t;

/**
 * @brief A memory block.
 *
 * @details
 * The memory manager keeps track of allocated blocks using mem_dbg_block_t.
 *
 * The next member of mem_dbg_block_t is used to keep a linked list of all the currently allocated blocks.<br>
 *    Searching this list is inefficient, but there are at least two reasons to do it this way,
 *    instead of using a more efficient data structure, such as a binary tree.<br>
 *    First, this code is for testing binary tree routines—using a binary tree data structure to do it is a strange idea!<br>
 *    Second, the `ISO C` standard says that, with few exceptions, using the relational operators
 *    `(<, <=, >, >=)` to compare pointers that do not point inside the same array produces
 *    undefined behavior, but allows use of the equality operators `(==, !=)` for a larger class of
 *    pointers.
 */
typedef struct mem_dbg_block {
  struct mem_dbg_block *next;   /**< @brief Next in linked list. */
  int idx;                      /**< @brief Allocation order index number. */
  size_t size;                  /**< @brief Size in bytes. */
  size_t used;                  /**< @brief `MEM_DBG_SUBALLOC`: amount used so far. */
  void *content;                /**< @brief Allocated region. */
} mem_dbg_block_t;

/**
 * @enum mem_dbg_arg_index_t
 * @brief Indexes into `arg[]` within mem_dbg_allocator_t.
 *
 * @details Used when calling mem_dbg_allocator_new().
 */
typedef enum {
  MEM_DBG_COUNT = 0,         /**< `MEM_DBG_FAIL_COUNT`: Remaining successful allocations. */
  MEM_DBG_PERCENT = 0,       /**< `MEM_DBG_FAIL_PERCENT`: Failure percentage. */
  MEM_DBG_BLOCK_SIZE = 0,    /**< `MEM_DBG_SUBALLOC`: Size of block to suballocate. */
  MEM_DBG_ALIGN = 1          /**< `MEM_DBG_SUBALLOC`: Alignment of suballocated blocks. */
} mem_dbg_arg_index_t;

/**
 * @brief Memory tracking allocator.
 *
 * @details
 * A data structure to keep track of settings and a list of blocks.
 */
typedef struct mt_allocator {
  mem_allocator_t allocator;         /**< @brief Allocator. Must be first member. */
  /* Settings. */
  mem_dbg_policy_t policy;           /**< @brief Allocation policy. */
  int args[2];                       /**< @brief Policy arguments. */
  int verbosity;                     /**< @brief Message verbosity level. */
  /* Current state. */
  mem_dbg_block_t *head;             /**< @brief Head of block list. */
  mem_dbg_block_t *tail;             /**< @brief Tail of block list. */
  int alloc_idx;                     /**< @brief Number of allocations so far. */
  int block_cnt;                     /**< @brief Number of still-allocated blocks. */
} mem_dbg_allocator_t;



/*******************************************************************/
/* Function prototypes for the memory tracker allocator structure. */
/*******************************************************************/

extern mem_dbg_allocator_t *
mem_dbg_allocator_new (mem_dbg_policy_t policy,
                      int args[2],
                      int verbosity);

extern void
mem_dbg_allocator_free (mem_dbg_allocator_t *mt);

extern mem_allocator_t *
mem_dbg_allocator (mem_dbg_allocator_t *mt);



/*
 *
 * Object memory tracker.
 *
 */

/*********************************************************/
/* Type declarations for the object allocator structure. */
/*********************************************************/

/**
 * @brief Memory object allocator.
 *
 * @details This allocator provides block allocation for specific structures (objects).
 */
typedef struct mem_obj_allocator {
  mem_allocator_t allocator;         /**< @brief Allocator. Must be first member. */
  /* Settings. */
  size_t object_size;                /**< @brief The object size in bytes. */
  size_t segment_size;               /**< @brief The segment size in bytes. */
  size_t stack_size;                 /**< @brief The stack size in bytes. */
} mem_obj_allocator_t;

typedef struct mem_obj_segment {
  struct mem_obj_segment *next;      /**< @brief Next in linked list. */
  void *content;                     /**< @brief Allocated region. */
} mem_obj_segment_t;



/*******************************************************************/
/* Function prototypes for the memory tracker allocator structure. */
/*******************************************************************/

/*
 * In order to have a stack size that is not as big as the allocated size, when
 * we free "too much" we have to free a block.
 * Freeing blocks requires to compact the data.
 * Compacting the data has two requirements:
 * - data (objects) are not referenced outside the module.
 * - references to objects moved from one block to another has to be updated as well ...
 *
 * Taking as example the red-black tree, if we exchange a node we have to update the parent,
 * and invalidate traversers.
 * Updating the parent is at no cost having the child to parent link, otherwise we have to
 * search for it.
 *
 * This is an open point ....
 */

extern mem_obj_allocator_t *
mem_obj_allocator_new (const size_t object_size,
                       const size_t objects_x_segment,
                       const size_t segments_in_stack);

extern void
mem_obj_allocator_free (mem_obj_allocator_t *a);

extern mem_allocator_t *
mem_obj_allocator (mem_obj_allocator_t *a);



#endif /* MEMORY_MANAGER_H */
