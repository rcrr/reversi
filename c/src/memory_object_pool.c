/**
 * @file
 *
 * @brief Memory Object Pool module implementation.
 *
 * @details This module defines an implementation for a memory pool of objects of the same size.
 *
 * @par memory_object_pool.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2017 Roberto Corradini. All rights reserved.
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <errno.h>

#include "memory_object_pool.h"

typedef struct mopool_mseg_s {
  size_t                nslots;
  void                 *addr;
  struct mopool_mseg_s *next_mseg;
} mopool_mseg_t;

struct mopool_s {
  size_t                nobjs;             /**< @brief Size of the pool in terms of object count. */
  size_t                obj_size;          /**< @brief Space consumed in the pool by one object. */
  size_t                slot_size;         /**< @brief Space consumed in the pool by one object. */
  size_t                nfree;             /**< @brief Count of the free slots. */
  void                 *freelist;          /**< @brief Head of the linked list of free slots. */
  mopool_ext_policy_t   policy;            /**< @brief The policy that governs extensions. */
  size_t                nobjs_initial;
  size_t                nobjs_extension;
  size_t                nobjs_limit;       /**< @brief Define the limit size when the extension policy establishes a cap. */
  struct mopool_mseg_s *head_of_mseg_list; /**< @brief A reference to the list of memory segments. */
};

mopool_mseg_t *
mopool_add_memory_segment (mopool_t *mop,
                           size_t nslots)
{
  mopool_mseg_t *segment = malloc(sizeof(mopool_mseg_t));
  if (!segment) return NULL;
  segment->nslots = nslots;
  segment->next_mseg = mop->head_of_mseg_list;
  segment->addr = malloc(nslots * mop->slot_size);
  if (!segment->addr) {
    free(segment);
    return NULL;
  }

  void **slot_cursor = (void **) segment->addr;
  for (size_t j = 0; j < nslots - 1; j++) {
    void *slot = (char *) slot_cursor + mop->slot_size;
    *slot_cursor = slot;
    slot_cursor = slot;
  }
  *slot_cursor = mop->freelist;
  mop->freelist = segment->addr;

  mop->nobjs += nslots;
  mop->nfree += nslots;

  return segment;
}

size_t mopool_get_nobjs (mopool_t *mop) { return mop->nobjs; }
size_t mopool_get_obj_size (mopool_t *mop) { return mop->obj_size; }
size_t mopool_get_slot_size (mopool_t *mop) { return mop->slot_size; }
size_t mopool_get_nfree (mopool_t *mop) { return mop->nfree; }
mopool_ext_policy_t mopool_get_policy (mopool_t *mop) { return mop->policy; }
size_t mopool_get_nobjs_initial (mopool_t *mop) { return mop->nobjs_initial; }
size_t mopool_get_nobjs_extension (mopool_t *mop) { return mop->nobjs_extension; }
size_t mopool_get_nobjs_limit (mopool_t *mop) { return mop->nobjs_limit; }


extern bool
mopool_check_consistency (mopool_t *mop)
{
  return true;
}

mopool_t *
mopool_create (size_t obj_size,
               mopool_ext_policy_t policy,
               size_t nobjs_initial,
               size_t nobjs_extension,
               size_t nobjs_limit)
{
  const size_t s = sizeof(void *);

  mopool_t *mop = NULL;

  if (nobjs_initial == 0 || obj_size == 0) return NULL;

  mop = malloc(sizeof(mopool_t));
  if (!mop) return NULL;

  mop->nobjs = 0;
  mop->obj_size = obj_size;
  mop->slot_size = (((obj_size > s) ? obj_size : s) + s - 1) / s * s;
  mop->nfree = 0;
  mop->freelist = NULL;
  mop->policy = policy;
  mop->nobjs_initial = nobjs_initial;
  mop->nobjs_extension = nobjs_extension;
  mop->nobjs_limit = nobjs_limit;
  mop->head_of_mseg_list = NULL;

  mop->head_of_mseg_list = mopool_add_memory_segment(mop, nobjs_initial);
  if (!mop->head_of_mseg_list) {
    free(mop);
    return NULL;
  }

  return mop;
}

void
mopool_destroy (mopool_t *mop)
{
  assert(mop);

  mopool_mseg_t *segment = mop->head_of_mseg_list;
  while (segment) {
    mopool_mseg_t *next = segment->next_mseg;
    free(segment->addr);
    free(segment);
    segment = next;
  }
  free(mop);
}

void *
mopool_malloc (mopool_t *mop)
{
  assert(mop);

  if (mop->nfree == 0) {
    switch (mop->policy) {
    case MOPOOL_EXT_POLICY_FIXED:
      goto memory_not_available;
    case MOPOOL_EXT_POLICY_LIMITED:
      if (mop->nobjs + mop->nobjs_extension > mop->nobjs_limit)
        goto memory_not_available;
    case MOPOOL_EXT_POLICY_UNLIMITED:
      if (mop->nobjs_extension == 0) goto memory_not_available;
      mopool_mseg_t * extension = mopool_add_memory_segment(mop, mop->nobjs_extension);
      if (!extension) goto memory_not_available;
      mop->head_of_mseg_list = extension;
      break;
    default:
      fprintf(stderr, "Undefined memory extension policy, aborting.\n");
      abort();
    }
  }

  void *ptr = mop->freelist;
  mop->freelist = *(void **) ptr;
  mop->nfree--;
  return ptr;

 memory_not_available:
  errno = ENOMEM;
  return NULL;
}

void
mopool_free (mopool_t *mop,
             void *ptr)
{
  assert(mop);
  if (!ptr) return;

  assert(mop->nfree <= mop->nobjs);

  *(void **)ptr = mop->freelist;
  mop->freelist = ptr;
  mop->nfree++;
}
