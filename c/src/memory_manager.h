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

/*
 * Memory tracking policy.
 *
 * MEM_MT_TRACK and MEM_MT_NO_TRACK should be self-explanatory.
 * MEM_MT_FAIL_COUNT takes an
 * argument specifying after how many allocations further allocations should always fail.
 * MEM_MT_FAIL_PERCENT takes an argument specifying an integer percentage of allocations to
 * randomly fail.
 *
 * MEM_MT_SUBALLOC causes small blocks to be carved out of larger ones allocated with malloc().
 * This is a good idea for two reasons: malloc() can be slow and malloc() can waste a lot of
 * space dealing with the small blocks that Libavl uses for its node. Suballocation cannot be
 * implemented in an entirely portable way because of alignment issues, but the test program
 * here requires the user to specify the alignment needed, and its use is optional anyhow.
 */
typedef enum {
  MEM_MT_TRACK,         /* Track allocation for leak detection. */
  MEM_MT_NO_TRACK,      /* No leak detection. */
  MEM_MT_FAIL_COUNT,    /* Fail allocations after a while. */
  MEM_MT_FAIL_PERCENT,  /* Fail allocations randomly. */
  MEM_MT_SUBALLOC       /* Suballocate from larger blocks. */
} mem_mt_policy_t;

/*
 * A memory block.
 *
 * The memory manager keeps track of allocated blocks using mem_mt_block_t.
 *
 * The next member of mem_mt_block_t is used to keep a linked list of all the currently allocated
 * blocks. Searching this list is inefficient, but there are at least two reasons to do it this way,
 * instead of using a more efficient data structure, such as a binary tree. First, this code is for
 * testing binary tree routines—using a binary tree data structure to do it is a strange idea!
 * Second, the ISO C standard says that, with few exceptions, using the relational operators
 * (<, <=, >, >=) to compare pointers that do not point inside the same array produces
 * undefined behavior, but allows use of the equality operators (==, !=) for a larger class of
 * pointers.
 *
 */
typedef struct mem_mt_block {
  struct mem_mt_block *next;   /* Next in linked list. */
  int idx;                     /* Allocation order index number. */
  size_t size;                 /* Size in bytes. */
  size_t used;                 /* MEM_MT_SUBALLOC: amount used so far. */
  void *content;               /* Allocated region. */
} mem_mt_block_t;

/* Indexes into arg[] within mem_mt_allocator_t. */
typedef enum {
  MEM_MT_COUNT = 0,         /* MEM_MT_FAIL_COUNT: Remaining successful allocations. */
  MEM_MT_PERCENT = 0,       /* MEM_MT_FAIL_PERCENT: Failure percentage. */
  MEM_MT_BLOCK_SIZE = 0,    /* MEM_MT_SUBALLOC: Size of block to suballocate. */
  MEM_MT_ALIGN = 1          /* MEM_MT_SUBALLOC: Alignment of suballocated blocks. */
} mem_mt_arg_index_t;

/* Memory tracking allocator. */
typedef struct mt_allocator {
  mem_allocator_t allocator;         /* Allocator. Must be first member. */
  /* Settings. */
  mem_mt_policy_t policy;            /* Allocation policy. */
  int arg[2];                        /* Policy arguments. */
  int verbosity;                     /* Message verbosity level. */
  /* Current state. */
  mem_mt_block_t *head, *tail;       /* Head and tail of block list. */
  int alloc_idx;                     /* Number of allocations so far. */
  int block_cnt;                     /* Number of still-allocated blocks. */
} mem_mt_allocator_t;



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



/*******************************************************************/
/* Function prototypes for the memory tracker allocator structure. */
/*******************************************************************/

extern mem_mt_allocator_t *
mem_mt_allocator_new (mem_mt_policy_t policy,
                      int arg[2],
                      int verbosity);

extern void
mem_mt_allocator_free (mem_mt_allocator_t *mt);

extern mem_allocator_t *
mem_mt_allocator (mem_mt_allocator_t *mt);



#endif /* MEMORY_MANAGER_H */
