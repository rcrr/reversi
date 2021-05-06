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

struct bihp_max_priority_queue_s {
  bihp_priority_function pf;
  bihp_print_item_function pif;
  size_t array_size;
  size_t heap_size;
  item_t *a;
};

static void
bihp_swap (bihp_max_priority_queue_t q,
           size_t index_a,
           size_t index_b)
{
  item_t *a = &q->a[index_a];
  item_t *b = &q->a[index_b];
  item_t t;
  t = *a;
  *a = *b;
  *b = t;
}

/*
 * The call is not safe.
 *
 * index must be greater than 0
 * index must be lesser than hash_size
 *
 * index belongs to (0..hash_size)
 */
static size_t
bihp_parent (size_t index)
{
  return (index - 1) / 2;
}

static size_t
bihp_left_child (size_t index)
{
  return 2 * index + 1;
}

static size_t
bihp_right_child (size_t index)
{
  return 2 * index + 2;
}

static int
bihp_compare (bihp_const_max_priority_queue_t q,
              size_t index_a,
              size_t index_b)
{
  int priority_a = q->pf(q->a[index_a]);
  int priority_b = q->pf(q->a[index_b]);
  return priority_a - priority_b;
}

static void
bihp_bubble_up (bihp_max_priority_queue_t q,
                size_t index)
{
  size_t i = index;
  size_t p = bihp_parent(index);
  while (i > 0 && bihp_compare(q, i, p) > 0) {
    bihp_swap(q, i, p);
    i = p;
    p = bihp_parent(i);
  }
}

static void
bihp_trickle_down (bihp_max_priority_queue_t q,
                   size_t index)
{
  long long int i = index;

  do {
    long long int j = -1;
    long long int l = bihp_left_child(i);
    long long int r = bihp_right_child(i);
    if (r < q->heap_size && bihp_compare(q, r, i) > 0) {
      if (bihp_compare(q, l, r) > 0) {
        j = l;
      } else {
        j = r;
      }
    } else {
      if (l < q->heap_size && bihp_compare(q, l, i) > 0) {
        j = l;
      }
    }
    if (j >= 0) bihp_swap(q, i, j);
    i = j;
  } while (i >= 0);
}


bihp_max_priority_queue_t
bihp_maxpq_create (size_t array_size,
                   bihp_priority_function pf,
                   bihp_print_item_function pif)
{
  bihp_max_priority_queue_t q;
  item_t *a;

  if (array_size == 0) return NULL;
  if (pf == NULL) return NULL;

  q = (bihp_max_priority_queue_t) malloc(sizeof(struct bihp_max_priority_queue_s));
  if (!q) {
    return NULL;
  }
  a = (item_t *) malloc(array_size * sizeof(item_t));
  if (!a) {
    free(q);
    return NULL;
  }

  for (size_t i = 0; i < array_size; i++)
    a[i] = NULL;

  q->pf = pf;
  q->pif = pif;
  q->array_size = array_size;
  q->heap_size = 0;
  q->a = a;

  return q;
}

void
bihp_maxpq_destroy (bihp_max_priority_queue_t q)
{
  assert(q);

  free(q->a);
  free(q);
}

void
bihp_maxpq_print (bihp_const_max_priority_queue_t q,
                  FILE *f)
{
  assert(q);

  if (!f) return;

  fprintf(f, "Max Priority Queue <%p>:\n", (void *) q);
  fprintf(f, "pf         = <%p>\n", (void *)(size_t) q->pf);
  fprintf(f, "pif        = <%p>\n", (void *)(size_t) q->pif);
  fprintf(f, "array_size = %zd\n", q->array_size);
  fprintf(f, "heap_size  = %zd\n", q->heap_size);
  if (q->pif) {
    fprintf(f, "items:\n");
    for (size_t i = 0; i < q->heap_size; i++) {
      fprintf(f, "  %06zu: ", i);
      q->pif(q->a[i], f);
      fprintf(f, "\n");
    }
  }
}

item_t
bihp_maxpq_maximum (bihp_const_max_priority_queue_t q)
{
  assert(q);
  if (q->heap_size == 0) return NULL;
  return q->a[0];
}

void
bihp_maxpq_insert (bihp_max_priority_queue_t q,
                   item_t item) {
  assert(q);
  assert(q->heap_size < q->array_size); // We could add a resize procedure ...

  q->a[q->heap_size++] = item;
  bihp_bubble_up(q, q->heap_size - 1);
}

item_t
bihp_maxpq_extract_max (bihp_max_priority_queue_t q)
{
  assert(q);

  if (q->heap_size == 0) return NULL;

  item_t t = q->a[0];
  q->a[0] = q->a[--q->heap_size];
  bihp_trickle_down(q, 0);

  // eventually resize ...

  return t;
}

bool
bihp_maxpq_has_max_heap_property (bihp_const_max_priority_queue_t q)
{
  assert(q);

  bool lb = true;
  bool rb = true;

  bool max_heap_property;
  size_t i;

  const size_t first_node_not_having_children = q->heap_size / 2;

  for (i = 0; i < first_node_not_having_children; i++) {
    const long long int l = bihp_left_child(i);
    const long long int r = bihp_right_child(i);

    if (l < q->heap_size && bihp_compare(q, i, l) < 0) lb = false;
    if (r < q->heap_size && bihp_compare(q, i, r) < 0) rb = false;

    max_heap_property = lb && rb;
    if (!max_heap_property) break;
  }

  return max_heap_property;
}
