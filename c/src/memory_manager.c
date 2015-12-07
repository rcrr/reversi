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
#include <stdarg.h>


#include "memory_manager.h"

/* Module name. */
static const char *pgm_name = "memory_manager";


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
 * Memory tracker mem_dbg_allocator_t implementation.
 * This memory tracker is used for debugging.
 */

/* Static functions. */
static void *mem_dbg_allocate (mem_allocator_t *allocator, size_t size);
static void mem_dbg_deallocate (mem_allocator_t *allocator, void *block);
static void *mem_dbg_malloc (size_t size);
static void mem_dbg_fail (const char *message, ...);

/**
 * @brief Returns a newly created memory debug allocator.
 *
 * @details Initializes the memory manager for use with allocation policy `policy`
 *          and policy arguments `arg[]`, at verbosity level `verbosity`,
 *          where `0` is a “normal” value.
 *
 * @param [in] policy    type of allocator
 * @param [in] arg       addictional arguments
 * @param [in] verbosity verbosity level
 * @return               a poiter to the new allocator
 */
mem_dbg_allocator_t *
mem_dbg_allocator_new (mem_dbg_policy_t policy,
                       int args[2],
                       int verbosity)
{
  mem_dbg_allocator_t *mt = mem_dbg_malloc(sizeof(mem_dbg_allocator_t));
  mt->allocator.malloc = mem_dbg_allocate;
  mt->allocator.free = mem_dbg_deallocate;
  mt->policy = policy;
  mt->args[0] = args[0];
  mt->args[1] = args[1];
  mt->verbosity = verbosity;
  mt->head = mt->tail = NULL;
  mt->alloc_idx = 0;
  mt->block_cnt = 0;
  return mt;
}

/**
 * @brief Frees and destroys memory tracker mt,
 *        reporting any memory leaks.
 *
 * @param [in] mt the memory tracker
 */
void
mem_dbg_allocator_free (mem_dbg_allocator_t *mt)
{
  assert(mt != NULL);
  if (mt->block_cnt == 0) {
    if (mt->policy != MEM_DBG_NO_TRACK && mt->verbosity >= 1)
      printf(" No memory leaks.\n");
  } else {
    mem_dbg_block_t *iter, *next;
    if (mt->policy != MEM_DBG_SUBALLOC) printf (" Memory leaks detected:\n");
    for (iter = mt->head; iter != NULL; iter = next) {
      if (mt->policy != MEM_DBG_SUBALLOC)
        printf("block #%d: %lu bytes\n", iter->idx , (unsigned long) iter->size);
      next = iter->next;
      free(iter->content);
      free(iter);
    }
  }
  free(mt);
}

/**
 * @brief Returns the mem_allocator_t field associated with `mt`.
 *
 * @param [in] mt the memory tracker
 * @return        the associated allocator
 */
mem_allocator_t *
mem_dbg_allocator (mem_dbg_allocator_t *mt)
{
  return &mt->allocator;
}

/*
 * Creates a new mem_dbg_block_t containing size bytes of content and returns
 * a pointer to content.
 */
static void *
mem_dbg_block_new (mem_dbg_allocator_t *mt,
                   size_t size)
{
  mem_dbg_block_t *new;
  /* Allocate and initialize new mem_dbg_block_t. */
  new = mem_dbg_malloc(sizeof(mem_dbg_block_t));
  new->next = NULL;
  new->idx = mt->alloc_idx++;
  new->size = size;
  new->used = 0;
  new->content = mem_dbg_malloc(size);
  /* Add block to linked list. */
  if (mt->head == NULL)
    mt->head = new;
  else mt->tail->next = new;
  mt->tail = new;
  /* Alert user. */
  if (mt->verbosity >= 3)
    printf ("   block #%d: allocated %lu bytes\n", new->idx, (unsigned long) size);
  /* Finish up and return. */
  mt->block_cnt++;
  return new->content;
}

/* Prints a message about a rejected allocation if appropriate. */
static void
mem_reject_request (mem_dbg_allocator_t *mt,
                    size_t size)
{
  if (mt->verbosity >= 2)
    printf ("    block #%d: rejected request for %lu bytes\n", mt->alloc_idx++, (unsigned long) size);
}

/* Allocates and returns a block of size bytes. */
static void *
mem_dbg_allocate (mem_allocator_t *allocator,
                  size_t size)
{
  mem_dbg_allocator_t *mt = (mem_dbg_allocator_t *) allocator;
  /* Special case. */
  if (size == 0)
    return NULL;

  switch (mt->policy) {

  case MEM_DBG_TRACK: return mem_dbg_block_new(mt, size);

  case MEM_DBG_NO_TRACK: return mem_dbg_malloc(size);

  case MEM_DBG_FAIL_COUNT:
    if (mt->args[MEM_DBG_COUNT] == 0) {
      mem_reject_request(mt, size);
      return NULL;
    }
    mt->args[MEM_DBG_COUNT]--;
    return mem_dbg_block_new(mt, size);

  case MEM_DBG_FAIL_PERCENT:
    if (rand () / (RAND_MAX / 100 + 1) < mt->args[MEM_DBG_PERCENT]) {
      mem_reject_request(mt, size);
      return NULL;
    }
    else return mem_dbg_block_new(mt, size);

  case MEM_DBG_SUBALLOC:
    if (mt->tail == NULL || mt->tail->used + size > (size_t) mt->args[MEM_DBG_BLOCK_SIZE])
      mem_dbg_block_new(mt, mt->args[MEM_DBG_BLOCK_SIZE]);
    if (mt->tail->used + size <= (size_t) mt->args[MEM_DBG_BLOCK_SIZE]) {
      void *p = (char *) mt->tail->content + mt->tail->used;
      size = ((size + mt->args[MEM_DBG_ALIGN] - 1) / mt->args[MEM_DBG_ALIGN] * mt->args[MEM_DBG_ALIGN]);
      mt->tail->used += size;
      if (mt->verbosity >= 3)
        printf("    block #%d: suballocated %lu bytes\n", mt->tail->idx, (unsigned long) size);
      return p;
    }
    else mem_dbg_fail("blocksize %lu too small for %lubyte allocation", (unsigned long) mt->tail->size, (unsigned long) size);

  default: assert(0);
  }

}

/*
 * Releases block previously returned by mt_allocate().
 */
static void
mem_dbg_deallocate (mem_allocator_t *allocator,
                    void *block)
{
  mem_dbg_allocator_t *mt = (mem_dbg_allocator_t *) allocator;
  mem_dbg_block_t *iter, *prev;
  /* Special cases. */
  if (block == NULL || mt->policy == MEM_DBG_NO_TRACK) {
    free(block);
    return;
  }
  if (mt->policy == MEM_DBG_SUBALLOC)
    return;
  /* Search for block within the list of allocated blocks. */
  for (prev = NULL, iter = mt->head; iter; prev = iter, iter = iter->next) {
    if (iter->content == block ) {
      /* Block found. Remove it from the list. */
      mem_dbg_block_t *next = iter->next;
      if (prev == NULL)
        mt->head = next;
      else prev->next = next;
      if (next == NULL) mt->tail = prev;
      /* Alert user. */
      if (mt->verbosity >= 4)
        printf ("    block #%d: freed %lu bytes\n", iter->idx, (unsigned long) iter->size);
      /* Free block. */
      free(iter->content);
      free(iter);
      /* Finish up and return. */
      mt->block_cnt--;
      return;
    }
  }
  /* Block not in list. */
  printf ("    attempt to free unknown block %p (already freed?)\n", block);
}

/*
 * Allocates and returns a pointer to size bytes of memory.
 * Aborts if allocation fails.
 */
static void *
mem_dbg_malloc (size_t size)
{
  void *block = malloc(size);
  if (block == NULL && size != 0)
    mem_dbg_fail("out of memory");
  return block;
}

/*
 * Prints message on stderr, which is formatted as for printf(),
 * and terminates the program unsuccessfully.
 */
static void
mem_dbg_fail (const char *message, ...)
{
  va_list args;
  fprintf(stderr, "%s: ", pgm_name);
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);
  putchar('\n');
  exit(EXIT_FAILURE);
}

/*
 * End of emory tracker mem_dbg_allocator_t implementation.
 */
