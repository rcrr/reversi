/**
 * @file
 *
 * @brief Binary Heap module implementation.
 *
 * @par binary_heap.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2021 Roberto Corradini. All rights reserved.
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

#include "binary_heap.h"



typedef long long int bihp_index_t;

typedef bool
(*bihp_compare_priority) (const bihp_const_pq_t q,
                          const bihp_index_t index_a,
                          const bihp_index_t index_b);

struct bihp_pq_s {
  bihp_pq_type_t t;                  /* priority queue type. */
  bihp_priority_f pf;                /* priority function. */
  bihp_print_item_f pif;             /* print item function. */
  size_t array_size;                 /* max number of item, it is the size of the a and p arrays. */
  size_t heap_size;                  /* number of items in the binary heap. */
  item_t *a;                         /* array of pointers to items. */
  int *p;                            /* array of priorities. */
  bihp_compare_priority higher;      /* true when index_a has higher priority than index_b. */
  bihp_compare_priority lower;       /* true when index_a has lower priority than index_b. */
};

static void
bihp_swap (const bihp_pq_t q,
           const bihp_index_t index_a,
           const bihp_index_t index_b)
{
  item_t t;
  int it;

  item_t *const a = &q->a[index_a];
  item_t *const b = &q->a[index_b];
  t = *a;
  *a = *b;
  *b = t;

  it = q->p[index_a];
  q->p[index_a] = q->p[index_b];
  q->p[index_b] = it;
}

/*
 * The call is not safe.
 *
 * index must be greater than 0
 * index must be lesser than hash_size
 *
 * index belongs to (0..hash_size)
 */
static bihp_index_t
bihp_parent (const bihp_index_t index)
{
  return (index - 1) / 2;
}

static bihp_index_t
bihp_left_child (const bihp_index_t index)
{
  return 2 * index + 1;
}

static bihp_index_t
bihp_right_child (const bihp_index_t index)
{
  return 2 * index + 2;
}

static int
bihp_compare (const bihp_const_pq_t q,
              const bihp_index_t index_a,
              const bihp_index_t index_b)
{
  return q->p[index_a] - q->p[index_b];
}

static bool
bihp_cmp_gt (const bihp_const_pq_t q,
             const bihp_index_t index_a,
             const bihp_index_t index_b)
{
  return bihp_compare(q, index_a, index_b) > 0;
}

static bool
bihp_cmp_lt (const bihp_const_pq_t q,
             const bihp_index_t index_a,
             const bihp_index_t index_b)
{
  return bihp_compare(q, index_a, index_b) < 0;
}

static void
bihp_bubble_up (const bihp_pq_t q,
                const bihp_index_t index)
{
  bihp_index_t i = index;
  bihp_index_t p = bihp_parent(index);
  while (i > 0 && q->higher(q, i, p)) {
    bihp_swap(q, i, p);
    i = p;
    p = bihp_parent(i);
  }
}

static void
bihp_trickle_down (const bihp_pq_t q,
                   const bihp_index_t index)
{
  bihp_index_t i = index;

  do {
    bihp_index_t j = -1;
    const bihp_index_t l = bihp_left_child(i);
    const bihp_index_t r = bihp_right_child(i);
    if (r < q->heap_size && q->higher(q, r, i)) {
      if (q->higher(q, l, r)) {
        j = l;
      } else {
        j = r;
      }
    } else {
      if (l < q->heap_size && q->higher(q, l, i)) {
        j = l;
      }
    }
    if (j >= 0) bihp_swap(q, i, j);
    i = j;
  } while (i >= 0);
}

bihp_pq_t
bihp_pq_create (bihp_pq_type_t t,
                size_t array_size,
                bihp_priority_f pf,
                bihp_print_item_f pif)
{
  bihp_pq_t q;
  item_t *a;
  int *p;

  if (!(t == BIHP_PQ_TYPE_MAX || t == BIHP_PQ_TYPE_MIN)) return NULL;
  if (array_size == 0) return NULL;
  if (pf == NULL) return NULL;

  q = (bihp_pq_t) malloc(sizeof(struct bihp_pq_s));
  if (!q) {
    return NULL;
  }
  a = (item_t *) malloc(array_size * sizeof(item_t));
  if (!a) {
    free(q);
    return NULL;
  }
  p = (int *) malloc(array_size * sizeof(int));
  if (!p) {
    free(q);
    free(a);
    return NULL;
  }

  for (size_t i = 0; i < array_size; i++) {
    a[i] = NULL;
    p[i] = 0;
  }

  q->t = t;
  q->pf = pf;
  q->pif = pif;
  q->array_size = array_size;
  q->heap_size = 0;
  q->a = a;
  q->p = p;

  if (q->t == BIHP_PQ_TYPE_MAX) {
    q->higher = bihp_cmp_gt;
    q->lower = bihp_cmp_lt;
  } else {
    q->higher = bihp_cmp_lt;
    q->lower = bihp_cmp_gt;
  }

  return q;
}

void
bihp_pq_destroy (bihp_pq_t q)
{
  assert(q);

  free(q->p);
  free(q->a);
  free(q);
}

bihp_pq_type_t
bihp_pq_type (const bihp_const_pq_t q)
{
  assert(q);
  return q->t;
}

size_t
bihp_pq_heap_size (const bihp_const_pq_t q)
{
  assert(q);
  return q->heap_size;
}

size_t
bihp_pq_array_size (const bihp_const_pq_t q)
{
  assert(q);
  return q->array_size;
}

bool
bihp_pq_is_empty (const bihp_const_pq_t q)
{
  assert(q);
  return q->heap_size == 0;
}

extern void
bihp_pq_clear (bihp_pq_t q)
{
  assert(q);
  q->heap_size = 0;
}

void
bihp_pq_print (bihp_const_pq_t q,
               FILE *f)
{
  assert(q);

  if (!f) return;

  fprintf(f, "Priority Queue <%p>:\n", (void *) q);
  fprintf(f, "pf         = <%p>\n", (void *)(size_t) q->pf);
  fprintf(f, "pif        = <%p>\n", (void *)(size_t) q->pif);
  fprintf(f, "array_size = %zd\n", q->array_size);
  fprintf(f, "heap_size  = %zd\n", q->heap_size);
  if (q->pif) {
    fprintf(f, "items:\n");
    for (size_t i = 0; i < q->heap_size; i++) {
      fprintf(f, "  %06zu pri->%+8d: ", i, q->p[i]);
      q->pif(q->a[i], f);
      fprintf(f, "\n");
    }
  }
}

item_t
bihp_pq_peek (bihp_const_pq_t q)
{
  assert(q);
  if (q->heap_size == 0) return NULL;
  return q->a[0];
}

void
bihp_pq_insert (bihp_pq_t q,
                item_t item) {
  assert(q);
  assert(q->heap_size < q->array_size); // We could add a resize procedure ...

  q->a[q->heap_size] = item;
  q->p[q->heap_size] = q->pf(item);
  q->heap_size++;
  bihp_bubble_up(q, q->heap_size - 1);
}

item_t
bihp_pq_pull (bihp_pq_t q)
{
  assert(q);

  if (q->heap_size == 0) return NULL;

  item_t t = q->a[0];

  q->heap_size--;
  q->a[0] = q->a[q->heap_size];
  q->p[0] = q->p[q->heap_size];
  bihp_trickle_down(q, 0);

  // eventually resize ...

  return t;
}

bool
bihp_pq_has_heap_property (bihp_const_pq_t q,
                           long long int *offending_parent_index)
{
  assert(q);

  const bihp_index_t first_node_not_having_children = q->heap_size / 2;
  const bihp_index_t last_node_having_children = first_node_not_having_children - 1;
  if (last_node_having_children < 0) return true;

  for (bihp_index_t i = 0; i < last_node_having_children; i++) {
    if ( q->lower(q, i, bihp_left_child(i)) ||
         q->lower(q, i, bihp_right_child(i)) ) {
      if (offending_parent_index) *offending_parent_index = i;
      return false;
    }
  }

  if ( q->lower(q, last_node_having_children, bihp_left_child(last_node_having_children)) ||
       ( q->heap_size % 2 == 1 &&
         q->lower(q, last_node_having_children, bihp_right_child(last_node_having_children)) )) {
    if (offending_parent_index) *offending_parent_index = last_node_having_children;
    return false;
  }

  return true;
}
