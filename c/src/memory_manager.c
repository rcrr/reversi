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

const char *pgm_name = "memory_manager";


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

static void *mt_allocate (struct mem_allocator *, size_t);
static void mt_free (struct mem_allocator *, void *);
static void *xmalloc (size_t size);
static void fail (const char *message, ...);

/* Initializes the memory manager for use with allocation policy policy and policy arguments arg[],
 * at verbosity level verbosity, where 0 is a “normal” value.
 */
struct mt_allocator *
mt_create (enum mt_policy policy,
           int arg[2],
           int verbosity)
{
  struct mt_allocator *mt = xmalloc (sizeof *mt);
  mt->allocator.malloc = mt_allocate;
  mt->allocator.free = mt_free;
  mt->policy = policy;
  mt->arg[0] = arg[0];
  mt->arg[1] = arg[1];
  mt->verbosity = verbosity;
  mt->head = mt->tail = NULL;
  mt->alloc_idx = 0;
  mt->block_cnt = 0;
  return mt;
}

/* Frees and destroys memory tracker mt, reporting any memory leaks. */
void
mt_destroy (struct mt_allocator *mt)
{
  assert(mt != NULL);
  if (mt->block_cnt == 0) {
    if (mt->policy != MT_NO_TRACK && mt->verbosity >= 1)
      printf(" No memory leaks.\n");
  } else {
    struct block *iter , *next;
    if (mt->policy != MT_SUBALLOC) printf (" Memory leaks detected:\n");
    for (iter = mt->head; iter != NULL; iter = next) {
      if (mt->policy != MT_SUBALLOC)
        printf("block #%d: %lu bytes\n", iter->idx , (unsigned long) iter->size);
      next = iter->next;
      free(iter->content);
      free(iter);
    }
  }
  free(mt);
}

/* Returns the struct libavl allocator associated with mt. */
void *
mt_allocator (struct mt_allocator *mt)
{
  return &mt->allocator;
}

/* Creates a new struct block containing size bytes of content and returns a pointer to content. */
static void *
new_block (struct mt_allocator *mt,
           size_t size) {
  struct block *new;
  /* Allocate and initialize new struct block. */
       new = xmalloc(sizeof *new);
       new->next = NULL;
       new->idx = mt->alloc_idx++;
       new->size = size;
       new->used = 0;
       new->content = xmalloc(size);
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
reject_request (struct mt_allocator *mt,
                size_t size)
{
  if (mt->verbosity >= 2)
    printf ("    block #%d: rejected request for %lu bytes\n", mt->alloc_idx++, (unsigned long) size);
}

/* Allocates and returns a block of size bytes. */
static void *
mt_allocate (struct mem_allocator *allocator,
             size_t size)
{
  struct mt_allocator *mt = (struct mt_allocator *) allocator;
  /* Special case. */
  if (size == 0)
    return NULL;

  switch (mt->policy) {

  case MT_TRACK: return new_block(mt, size);

  case MT_NO_TRACK: return xmalloc(size);

  case MT_FAIL_COUNT:
    if (mt->arg[MT_COUNT] == 0) {
      reject_request(mt, size);
      return NULL;
    }
    mt->arg[MT_COUNT]--;
    return new_block(mt, size);

  case MT_FAIL_PERCENT:
    if (rand () / (RAND_MAX / 100 + 1) < mt->arg[MT_PERCENT]) {
      reject_request(mt, size);
      return NULL;
    }
    else return new_block(mt, size);

  case MT_SUBALLOC:
    if (mt->tail == NULL || mt->tail->used + size > (size_t) mt->arg[MT_BLOCK_SIZE])
      new_block(mt, mt->arg[MT_BLOCK_SIZE]);
    if (mt->tail->used + size <= (size_t) mt->arg[MT_BLOCK_SIZE]) {
      void *p = (char *) mt->tail->content + mt->tail->used;
      size = ((size + mt->arg[MT_ALIGN] - 1) / mt->arg[MT_ALIGN] * mt->arg[MT_ALIGN]);
      mt->tail->used += size;
      if (mt->verbosity >= 3)
        printf("    block #%d: suballocated %lu bytes\n", mt->tail->idx, (unsigned long) size);
      return p;
    }
    else fail("blocksize %lu too small for %lubyte allocation", (unsigned long) mt->tail->size, (unsigned long) size);
  default: assert(0);
  }
}

/* Releases block previously returned by mt allocate(). */
static void
mt_free (struct mem_allocator *allocator,
         void *block )
{
  struct mt_allocator *mt = (struct mt_allocator *) allocator;
  struct block *iter, *prev;
  /* Special cases. */
  if (block == NULL || mt->policy == MT_NO_TRACK) {
    free(block);
    return;
  }
  if (mt->policy == MT_SUBALLOC)
    return;
  /* Search for block within the list of allocated blocks. */
  for (prev = NULL, iter = mt->head; iter; prev = iter, iter = iter->next) {
    if (iter->content == block ) {
      /* Block found. Remove it from the list. */
      struct block *next = iter->next;
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

/* Allocates and returns a pointer to size bytes of memory. Aborts if allocation fails. */
static void *
xmalloc (size_t size)
{
  void *block = malloc(size);
  if (block == NULL && size != 0)
    fail("out of memory");
  return block;
}

/* Prints message on stderr , which is formatted as for printf (), and terminates the program unsuccessfully. */
static void
fail (const char *message, ...)
{
  va_list args;
  fprintf(stderr, "%s: ", pgm_name);
  va_start(args, message);
  vfprintf(stderr, message, args);
  va_end(args);
  putchar('\n');
  exit(EXIT_FAILURE);
}
