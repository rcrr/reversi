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
#include <alloca.h>
#include <stdint.h>

#include "memory_object_pool.h"
#include "sort_utils.h"

typedef struct mopool_mseg_s {
  size_t                nslots;            // Number of slots in the allocated array
  void                 *addr;              // Reference to the allocated space for the array of slots
  struct mopool_mseg_s *next_mseg;         // Reference to the next entry in the linked list of segments
} mopool_mseg_t;

typedef struct mseg_info_s {
  mopool_mseg_t *seg;                      // The segment memory address
  size_t         pos;                      // Position in the list of segments
  size_t         nslots;                   // Number of slots
  void          *i_addr;                   // First address in the segment
  void          *e_addr;                   // First address after the end of the segment
  size_t         nallocated;               // Allocated object count
} mseg_info_t;

struct mopool_s {
  size_t                nobjs;             // Size of the pool in terms of object count.
  size_t                obj_size;          // Space consumed in the pool by one object.
  size_t                slot_size;         // Space consumed in the pool by one object.
  size_t                nfree;             // Count of the free slots.
  void                 *free_slot_list;    // Head of the linked list of free slots.
  mopool_ext_policy_t   policy;            // The policy that governs extensions.
  size_t                nobjs_initial;     //
  size_t                nobjs_extension;   //
  size_t                nobjs_limit;       // Define the limit size when the extension policy establishes a cap.
  mopool_mseg_t        *head_of_mseg_list; // A reference to the list of memory segments.
  size_t                nsegments;         // Count of memory segments.
};



/* Private functions. */

/*
 * Allocates and adds to the pool an incremental segment of memory.
 */
static mopool_mseg_t *
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
  mop->nsegments++;

  void **slot_cursor = (void **) segment->addr;
  for (size_t j = 0; j < nslots - 1; j++) {
    void *slot = (char *) slot_cursor + mop->slot_size;
    *slot_cursor = slot;
    slot_cursor = slot;
  }
  *slot_cursor = mop->free_slot_list;
  mop->free_slot_list = segment->addr;

  mop->nobjs += nslots;
  mop->nfree += nslots;

  return segment;
}

/*
 * Compares two mseg_inf_t structures looking at the segment initial address.
 */
static int
mopool_seg_info_cmp (const void *const a,
                     const void *const b)
{
  void *x = ((mseg_info_t *) a)->i_addr;
  void *y = ((mseg_info_t *) b)->i_addr;
  return (x > y) - (x < y);
}



/* Public functions. */

/*
 * Functions that get field values from the mopool_t structure.
 */
size_t mopool_get_nobjs (mopool_t *mop) { return mop->nobjs; }
size_t mopool_get_obj_size (mopool_t *mop) { return mop->obj_size; }
size_t mopool_get_slot_size (mopool_t *mop) { return mop->slot_size; }
size_t mopool_get_nfree (mopool_t *mop) { return mop->nfree; }
mopool_ext_policy_t mopool_get_policy (mopool_t *mop) { return mop->policy; }
size_t mopool_get_nobjs_initial (mopool_t *mop) { return mop->nobjs_initial; }
size_t mopool_get_nobjs_extension (mopool_t *mop) { return mop->nobjs_extension; }
size_t mopool_get_nobjs_limit (mopool_t *mop) { return mop->nobjs_limit; }

bool
mopool_check_consistency (mopool_t *mop)
{
  size_t i, m, l, r, mseg_count, free_slot_count, allocated_slot_count;
  mopool_mseg_t *seg;
  void * fslot;
  mseg_info_t *msia, *msi;

  if (!mop) return false;

  msi = msia = alloca(sizeof(mseg_info_t) * mop->nsegments);
  mseg_count = 0;
  seg = mop->head_of_mseg_list;
  while (seg) {
    msi->seg = seg;
    msi->pos = mseg_count;
    msi->nslots = seg->nslots;
    msi->i_addr = seg->addr;
    msi->e_addr = (char *) seg->addr + seg->nslots * mop->slot_size;
    msi->nallocated = seg->nslots;
    mseg_count++;
    seg = seg->next_mseg;
    msi++;
  }
  if (mop->nsegments != mseg_count) return false;

  // Sorts the msia (slot info array)
  sort_utils_binarysort(msia, mseg_count, sizeof(mseg_info_t), mopool_seg_info_cmp);

  free_slot_count = 0;
  fslot = mop->free_slot_list;
  while (fslot) {
    free_slot_count++;

    // Binary search: spots the segment that holds the slot.
    msi = msia;
    m = 0;
    l = 0;
    r = mseg_count - 1;
    while (l <= r) {
      m = l + (r - l) / 2;
      msi = msia + m;
      if (msi->i_addr == fslot) break;
      if (msi->i_addr < fslot) l = m + 1;
      else r = m - 1;
    }
    if (msi->e_addr <= fslot) return false; // the slot must come first then the end boundary of the segment.
    msi->nallocated--;

    fslot = *(void **) fslot; // next free slot ...
  }
  if (mop->nfree != free_slot_count) return false;

  allocated_slot_count = 0;
  for (i = 0; i < mseg_count; i++) {
    msi = &msia[i];
    if (msi->nallocated > msi->nslots) return false;
    allocated_slot_count += msi->nallocated;
  }
  if (allocated_slot_count + free_slot_count != mop->nobjs) return false;

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
  mop->free_slot_list = NULL;
  mop->policy = policy;
  mop->nobjs_initial = nobjs_initial;
  mop->nobjs_extension = nobjs_extension;
  mop->nobjs_limit = nobjs_limit;
  mop->head_of_mseg_list = NULL;
  mop->nsegments = 0;

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

  void *ptr = mop->free_slot_list;
  mop->free_slot_list = *(void **) ptr;
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

  *(void **)ptr = mop->free_slot_list;
  mop->free_slot_list = ptr;
  mop->nfree++;
}
