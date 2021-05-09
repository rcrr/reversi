/**
 * @file
 *
 * @brief Binary heap module unit test suite.
 * @details Collects tests and helper methods for the binary heap module.
 *
 * @par ut_binary_heap.c
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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "unit_test.h"
#include "binary_heap.h"



typedef struct aux_item_s {
  int value;
} * aux_item_t;

typedef struct aux_item_s const * const_aux_item_t;

/* This struct has to be the same defined by binary_heap.c We keep a
 * second copy to be able to violate the ADT opaque interface so to
 * properly craft faulty structures and check them with a call
 * bihp_pq_has_heap_property()
 */
struct bihp_pq_s {
  bihp_pq_type_t t;                  /* priority queue type. */
  bihp_priority_f pf;                /* priority function. */
  bihp_print_item_f pif;             /* print item function. */
  size_t array_size;                 /* max number of item, it is the size of the a and p arrays. */
  size_t heap_size;                  /* number of items in the binary heap. */
  item_t *a;                         /* array of pointers to items. */
  int *p;                            /* array of priorities. */
};


/*
 * Auxiliary functions.
 */

int
aux_priority (item_t a)
{
  const_aux_item_t x = a;
  return x->value;
}

void
aux_print_item (item_t a,
                FILE *f)
{
  const_aux_item_t x = a;
  fprintf(f, "%p, %d", (void *) x, x->value);
}



/*
 * Test functions.
 */

static void
aux_priority_t (ut_test_t *const t)
{
  aux_item_t x;
  struct aux_item_s xs;
  x = &xs;

  x->value = 0;
  ut_assert(t, aux_priority(x) == 0);

  x->value = 10;
  ut_assert(t, aux_priority(x) == 10);
}

static void
bihp_pq_create_t (ut_test_t *const t)
{
  bihp_pq_t q;
  size_t array_size = 10;
  bihp_priority_f pf = NULL;
  bihp_print_item_f pif = NULL;

  q = bihp_pq_create(BIHP_PQ_TYPE_MAX, array_size, pf, pif);
  ut_assert(t, q == NULL);

  pf = aux_priority;

  q = bihp_pq_create(BIHP_PQ_TYPE_INVALID, array_size, pf, pif);
  ut_assert(t, q == NULL);

  q = bihp_pq_create(BIHP_PQ_TYPE_MAX, array_size, pf, pif);
  ut_assert(t, q);

  const size_t hs = bihp_pq_heap_size(q);
  ut_assert(t, hs == 0);

  const size_t as = bihp_pq_array_size(q);
  ut_assert(t, as == array_size);

  const bool is_empty = bihp_pq_is_empty(q);
  ut_assert(t, is_empty == true);

  bihp_pq_destroy(q);
  q = NULL;
}

static void
bihp_pq_insert_t (ut_test_t *const t)
{
  const bool verbose = ut_run_time_is_verbose(t);

  bihp_pq_t q;
  size_t array_size = 10;
  bihp_priority_f pf = aux_priority;
  bihp_print_item_f pif = aux_print_item;

  struct aux_item_s xs;
  aux_item_t x = &xs;
  x->value = 7;

  struct aux_item_s ys;
  aux_item_t y = &ys;
  y->value = 3;

  struct aux_item_s zs;
  aux_item_t z = &zs;
  z->value = 5;

  q = bihp_pq_create(BIHP_PQ_TYPE_MAX, array_size, pf, pif);
  ut_assert(t, q);

  bihp_pq_insert(q, x);

  if (verbose) {
    printf("\n");
    bihp_pq_print(q, stdout);
  }

  bihp_pq_insert(q, y);

  if (verbose) {
    printf("\n");
    bihp_pq_print(q, stdout);
  }

  bihp_pq_insert(q, z);

  if (verbose) {
    printf("\n");
    bihp_pq_print(q, stdout);
  }

  bihp_pq_destroy(q);
  q = NULL;
}

static void
bihp_pq_peek_t (ut_test_t *const t)
{
  static const int max_heap_size = 13;

  const bool verbose = ut_run_time_is_verbose(t);

  bihp_pq_t q;
  size_t array_size = max_heap_size;
  bihp_priority_f pf = aux_priority;
  bihp_print_item_f pif = aux_print_item;

  struct aux_item_s xs[max_heap_size];
  for (int i = 0; i < max_heap_size; i++) {
    xs[i].value = i;
  }

  q = bihp_pq_create(BIHP_PQ_TYPE_MAX, array_size, pf, pif);
  ut_assert(t, q);

  for (int i = 0; i < max_heap_size; i++) {
    bihp_pq_insert(q, &xs[i]);

    const item_t max = bihp_pq_peek(q);
    const int max_value = pf(max);
    ut_assert(t, i == max_value);

    const size_t hs = bihp_pq_heap_size(q);
    ut_assert(t, hs == i + 1);

    const size_t as = bihp_pq_array_size(q);
    ut_assert(t, as == max_heap_size);

    const bool is_empty = bihp_pq_is_empty(q);
    ut_assert(t, is_empty == false);

    const bool has_max_heap_property = bihp_pq_has_heap_property(q, NULL);
    ut_assert(t, has_max_heap_property);
  }

  if (verbose) {
    printf("\n");
    bihp_pq_print(q, stdout);
  }

  bihp_pq_destroy(q);
  q = NULL;
}

static void
bihp_pq_pull_max_t (ut_test_t *const t)
{
  static const int heap_size = 17;

  const bool verbose = ut_run_time_is_verbose(t);

  bihp_pq_t q;
  size_t array_size = heap_size;
  bihp_priority_f pf = aux_priority;
  bihp_print_item_f pif = aux_print_item;

  struct aux_item_s xs[heap_size];
  for (int i = 0; i < heap_size; i++) {
    xs[i].value = i;
  }

  q = bihp_pq_create(BIHP_PQ_TYPE_MAX, array_size, pf, pif);
  ut_assert(t, q);

  for (int i = 0; i < heap_size; i++) {
    bihp_pq_insert(q, &xs[i]);

    item_t max = bihp_pq_peek(q);
    int max_value = pf(max);
    ut_assert(t, i == max_value);

    bool has_max_heap_property = bihp_pq_has_heap_property(q, NULL);
    ut_assert(t, has_max_heap_property);
  }

  for (int i = heap_size - 1; i >= 0; i--) {
    item_t max = bihp_pq_pull(q);
    int max_value = pf(max);
    ut_assert(t, i == max_value);

    bool has_max_heap_property = bihp_pq_has_heap_property(q, NULL);
    ut_assert(t, has_max_heap_property);
  }

  const bool is_empty = bihp_pq_is_empty(q);
  ut_assert(t, is_empty == true);

  if (verbose) {
    printf("\n");
    bihp_pq_print(q, stdout);
  }

  bihp_pq_destroy(q);
  q = NULL;
}

static void
bihp_pq_pull_min_t (ut_test_t *const t)
{
  static const int heap_size = 17;

  const bool verbose = ut_run_time_is_verbose(t);

  bihp_pq_t q;
  size_t array_size = heap_size;
  bihp_priority_f pf = aux_priority;
  bihp_print_item_f pif = aux_print_item;

  struct aux_item_s xs[heap_size];
  for (int i = 0; i < heap_size; i++) {
    xs[i].value = i;
  }

  q = bihp_pq_create(BIHP_PQ_TYPE_MIN, array_size, pf, pif);
  ut_assert(t, q);

  for (int i = heap_size - 1; i >= 0; i--) {
    bihp_pq_insert(q, &xs[i]);

    item_t min = bihp_pq_peek(q);
    int min_value = pf(min);
    ut_assert(t, i == min_value);

    bool has_min_heap_property = bihp_pq_has_heap_property(q, NULL);
    ut_assert(t, has_min_heap_property);
  }

  for (int i = 0; i < heap_size; i++) {
    item_t min = bihp_pq_pull(q);
    int min_value = pf(min);
    ut_assert(t, i == min_value);

    bool has_min_heap_property = bihp_pq_has_heap_property(q, NULL);
    ut_assert(t, has_min_heap_property);
  }

  const bool is_empty = bihp_pq_is_empty(q);
  ut_assert(t, is_empty == true);

  if (verbose) {
    printf("\n");
    bihp_pq_print(q, stdout);
  }

  bihp_pq_destroy(q);
  q = NULL;
}

static void
bihp_pq_has_heap_property_max_t (ut_test_t *const t)
{
  const bool verbose = ut_run_time_is_verbose(t);

  bihp_pq_t q;
  size_t array_size = 10;
  bihp_priority_f pf = aux_priority;
  bihp_print_item_f pif = aux_print_item;
  long long int offending_parent_index;

  struct aux_item_s xs;
  aux_item_t x = &xs;
  x->value = 7;

  struct aux_item_s ys;
  aux_item_t y = &ys;
  y->value = 3;

  struct aux_item_s us;
  aux_item_t u = &us;
  u->value = 5;

  struct aux_item_s zs;
  aux_item_t z = &zs;
  z->value = 1;

  q = bihp_pq_create(BIHP_PQ_TYPE_MAX, array_size, pf, pif);
  ut_assert(t, q);
  ut_assert(t, bihp_pq_type(q) == BIHP_PQ_TYPE_MAX);
  ut_assert(t, bihp_pq_heap_size(q) == 0);
  ut_assert(t, bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));

  bihp_pq_insert(q, x);
  ut_assert(t, bihp_pq_heap_size(q) == 1);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));
  bihp_pq_insert(q, y);
  ut_assert(t, bihp_pq_heap_size(q) == 2);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));
  bihp_pq_insert(q, u);
  ut_assert(t, bihp_pq_heap_size(q) == 3);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));
  bihp_pq_insert(q, z);
  ut_assert(t, bihp_pq_heap_size(q) == 4);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));

  bihp_pq_clear(q);
  ut_assert(t, bihp_pq_heap_size(q) == 0);
  ut_assert(t, bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));

  bihp_pq_insert(q, x);
  bihp_pq_insert(q, x);
  bihp_pq_insert(q, x);
  ut_assert(t, bihp_pq_heap_size(q) == 3);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));

  bihp_pq_clear(q);
  ut_assert(t, bihp_pq_heap_size(q) == 0);
  ut_assert(t, bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));

  bihp_pq_insert(q, x);
  bihp_pq_insert(q, u);
  bihp_pq_insert(q, y);
  bihp_pq_insert(q, z);
  offending_parent_index = -1;
  ut_assert(t, bihp_pq_heap_size(q) == 4);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, &offending_parent_index));
  ut_assert(t, offending_parent_index == -1);

  q->p[1] = 9;
  offending_parent_index = -1;
  ut_assert(t, !bihp_pq_has_heap_property(q, &offending_parent_index));
  ut_assert(t, offending_parent_index == 0);

  bihp_pq_clear(q);
  bihp_pq_insert(q, x);
  bihp_pq_insert(q, u);
  bihp_pq_insert(q, y);
  bihp_pq_insert(q, z);
  q->p[0] = 2;
  offending_parent_index = -1;
  ut_assert(t, !bihp_pq_has_heap_property(q, &offending_parent_index));
  ut_assert(t, offending_parent_index == 0);

  bihp_pq_clear(q);
  bihp_pq_insert(q, x);
  bihp_pq_insert(q, u);
  bihp_pq_insert(q, y);
  bihp_pq_insert(q, z);
  q->p[2] = 9;
  offending_parent_index = -1;
  ut_assert(t, !bihp_pq_has_heap_property(q, &offending_parent_index));
  ut_assert(t, offending_parent_index == 0);

  bihp_pq_clear(q);
  bihp_pq_insert(q, x);
  bihp_pq_insert(q, u);
  bihp_pq_insert(q, y);
  bihp_pq_insert(q, z);
  q->p[3] = 6;
  offending_parent_index = -1;
  ut_assert(t, !bihp_pq_has_heap_property(q, &offending_parent_index));
  ut_assert(t, offending_parent_index == 1);

  bihp_pq_clear(q);
  bihp_pq_insert(q, x);
  bihp_pq_insert(q, u);
  bihp_pq_insert(q, y);
  bihp_pq_insert(q, z);
  bihp_pq_insert(q, x);
  q->p[4] = 8;
  offending_parent_index = -1;
  ut_assert(t, !bihp_pq_has_heap_property(q, &offending_parent_index));
  ut_assert(t, offending_parent_index == 1);

  if (verbose) {
    printf("\n");
    bihp_pq_print(q, stdout);
  }

  bihp_pq_destroy(q);
  q = NULL;
}

static void
bihp_pq_has_heap_property_min_t (ut_test_t *const t)
{
  const bool verbose = ut_run_time_is_verbose(t);

  bihp_pq_t q;
  size_t array_size = 10;
  bihp_priority_f pf = aux_priority;
  bihp_print_item_f pif = aux_print_item;
  long long int offending_parent_index;

  struct aux_item_s xs;
  aux_item_t x = &xs;
  x->value = 7;

  struct aux_item_s ys;
  aux_item_t y = &ys;
  y->value = 3;

  struct aux_item_s us;
  aux_item_t u = &us;
  u->value = 5;

  struct aux_item_s zs;
  aux_item_t z = &zs;
  z->value = 1;

  q = bihp_pq_create(BIHP_PQ_TYPE_MIN, array_size, pf, pif);
  ut_assert(t, q);
  ut_assert(t, bihp_pq_type(q) == BIHP_PQ_TYPE_MIN);
  ut_assert(t, bihp_pq_heap_size(q) == 0);
  ut_assert(t, bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));

  bihp_pq_insert(q, x);
  ut_assert(t, bihp_pq_heap_size(q) == 1);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));
  bihp_pq_insert(q, y);
  ut_assert(t, bihp_pq_heap_size(q) == 2);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));
  bihp_pq_insert(q, u);
  ut_assert(t, bihp_pq_heap_size(q) == 3);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));
  bihp_pq_insert(q, z);
  ut_assert(t, bihp_pq_heap_size(q) == 4);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, NULL));
  bihp_pq_insert(q, y);
  offending_parent_index = -1;
  ut_assert(t, bihp_pq_heap_size(q) == 5);
  ut_assert(t, !bihp_pq_is_empty(q));
  ut_assert(t, bihp_pq_has_heap_property(q, &offending_parent_index));
  ut_assert(t, offending_parent_index == -1);

  q->p[4] = 2;
  offending_parent_index = -1;
  ut_assert(t, !bihp_pq_has_heap_property(q, &offending_parent_index));
  ut_assert(t, offending_parent_index == 1);

  if (verbose) {
    printf("\n");
    bihp_pq_print(q, stdout);
  }

  bihp_pq_destroy(q);
  q = NULL;
}


/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_prog_arg_config_t config;
  ut_init(&config, &argc, &argv);

  ut_suite_t *const s = ut_suite_new(&config, "binary_heap");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "aux_priority", aux_priority_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_pq_create", bihp_pq_create_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_pq_insert", bihp_pq_insert_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_pq_peek", bihp_pq_peek_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_pq_pull_max", bihp_pq_pull_max_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_pq_pull_min", bihp_pq_pull_min_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_pq_has_heap_property_max", bihp_pq_has_heap_property_max_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_pq_has_heap_property_min", bihp_pq_has_heap_property_min_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
