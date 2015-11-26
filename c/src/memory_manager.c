/**
 * @file
 *
 * @brief Block memory allocator module.
 *
 * @details Memory management by mean of block allocation is a technique designed to
 *          have the flexibility of dynamic allocation togheter with a reduced speed penalty.
 *
 *          All the symbols exported by the module adopt the `mem_` prefix.
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
  assert(alloc != NULL && size > 0);
  return malloc(size);
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
  assert(alloc != NULL && block != NULL);
  free(block);
}



/*
 * Block memory allocator.
 *
 */

typedef struct {
  size_t          o_size;                /**< @brief Object size as number of bytes consumed. */
  size_t          block_size;            /**< @brief Block size. */
  void          **blocks;                /**< @brief Linked list?. */
} mem_block_alloc_t;

void *
mem_block_malloc (mem_allocator_t *alloc,
                  size_t size)
{
  assert(alloc != NULL && size > 0);
  return malloc(size);
}

void
mem_block_free (mem_allocator_t *alloc,
                void *block)
{
  assert(alloc != NULL && block != NULL);
  free(block);
}

/* **** **** **** **** */


/*
 * Memory tracking policy.
 *
 * MT_TRACK and MT_NO_TRACK should be self-explanatory.
 * MT_FAIL_COUNT takes an
 * argument specifying after how many allocations further allocations should always fail.
 * MT_FAIL_PERCENT takes an argument specifying an integer percentage of allocations to
 * randomly fail.
 *
 * MT_SUBALLOC causes small blocks to be carved out of larger ones allocated with malloc().
 * This is a good idea for two reasons: malloc() can be slow and malloc() can waste a lot of
 * space dealing with the small blocks that Libavl uses for its node. Suballocation cannot be
 * implemented in an entirely portable way because of alignment issues, but the test program
 * here requires the user to specify the alignment needed, and its use is optional anyhow.
 */
enum mt_policy {
  MT_TRACK,         /* Track allocation for leak detection. */
  MT_NO_TRACK,      /* No leak detection. */
  MT_FAIL_COUNT,    /* Fail allocations after a while. */
  MT_FAIL_PERCENT,  /* Fail allocations randomly. */
  MT_SUBALLOC       /* Suballocate from larger blocks. */
};

/*
 * A memory block.
 *
 * The memory manager keeps track of allocated blocks using struct block.
 *
 * The next member of struct block is used to keep a linked list of all the currently allocated
 * blocks. Searching this list is inefficient, but there are at least two reasons to do it this way,
 * instead of using a more efficient data structure, such as a binary tree. First, this code is for
 * testing binary tree routines—using a binary tree data structure to do it is a strange idea!
 * Second, the ISO C standard says that, with few exceptions, using the relational operators
 * (<, <=, >, >=) to compare pointers that do not point inside the same array produces
 * undefined behavior, but allows use of the equality operators (==, !=) for a larger class of
 * pointers.
 *
 */
struct block {
  struct block *next;   /* Next in linked list. */
  int idx;              /* Allocation order index number. */
  size_t size;          /* Size in bytes. */
  size_t used;          /* MT SUBALLOC: amount used so far. */
  void *content;        /* Allocated region. */
};

/* Indexes into arg[] within struct mt_allocator. */
enum mt_arg_index {
  MT_COUNT = 0,         /* MT_FAIL_COUNT: Remaining successful allocations. */
  MT_PERCENT = 0,       /* MT_FAIL_PERCENT: Failure percentage. */
  MT_BLOCK_SIZE = 0,    /* MT_SUBALLOC: Size of block to suballocate. */
  MT_ALIGN = 1          /* MT_SUBALLOC: Alignment of suballocated blocks. */
};

/* Memory tracking allocator. */
struct mt_allocator {
  struct mem_allocator allocator;    /* Allocator. Must be first member. */
  /* Settings. */
  enum mt_policy policy;             /* Allocation policy. */
  int arg[2];                        /* Policy arguments. */
  int verbosity;                     /* Message verbosity level. */
  /* Current state. */
  struct block *head, *tail;         /* Head and tail of block list. */
  int alloc_idx;                     /* Number of allocations so far. */
  int block_cnt;                     /* Number of still-allocated blocks. */
};

/* pp 109 in PDF ... */
