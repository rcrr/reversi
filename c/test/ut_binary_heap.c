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
bihp_maxpq_create_t (ut_test_t *const t)
{
  bihp_max_priority_queue_t q;
  size_t array_size = 10;
  bihp_priority_function pf = NULL;
  bihp_print_item_function pif = NULL;

  q = bihp_maxpq_create(array_size, pf, pif);
  ut_assert(t, q == NULL);

  pf = aux_priority;

  q = bihp_maxpq_create(array_size, pf, pif);
  ut_assert(t, q);

  bihp_maxpq_destroy(q);
  q = NULL;
}

static void
bihp_maxpq_insert_t (ut_test_t *const t)
{
  const bool verbose = ut_run_time_is_verbose(t);

  bihp_max_priority_queue_t q;
  size_t array_size = 10;
  bihp_priority_function pf = aux_priority;
  bihp_print_item_function pif = aux_print_item;

  struct aux_item_s xs;
  aux_item_t x = &xs;
  x->value = 7;

  struct aux_item_s ys;
  aux_item_t y = &ys;
  y->value = 3;

  struct aux_item_s zs;
  aux_item_t z = &zs;
  z->value = 5;

  q = bihp_maxpq_create(array_size, pf, pif);
  ut_assert(t, q);

  bihp_maxpq_insert(q, x);

  if (verbose) {
    printf("\n");
    bihp_maxpq_print(q, stdout);
  }

  bihp_maxpq_insert(q, y);

  if (verbose) {
    printf("\n");
    bihp_maxpq_print(q, stdout);
  }

  bihp_maxpq_insert(q, z);

  if (verbose) {
    printf("\n");
    bihp_maxpq_print(q, stdout);
  }

  bihp_maxpq_destroy(q);
  q = NULL;
}

static void
bihp_maxpq_maximum_t (ut_test_t *const t)
{
  static const int heap_size = 13;

  const bool verbose = ut_run_time_is_verbose(t);

  bihp_max_priority_queue_t q;
  size_t array_size = heap_size;
  bihp_priority_function pf = aux_priority;
  bihp_print_item_function pif = aux_print_item;

  struct aux_item_s xs[heap_size];
  for (int i = 0; i < heap_size; i++) {
    xs[i].value = i;
  }

  q = bihp_maxpq_create(array_size, pf, pif);
  ut_assert(t, q);

  for (int i = 0; i < heap_size; i++) {
    bihp_maxpq_insert(q, &xs[i]);

    item_t max = bihp_maxpq_maximum(q);
    int max_value = pf(max);
    ut_assert(t, i == max_value);
  }

  if (verbose) {
    printf("\n");
    bihp_maxpq_print(q, stdout);
  }

  bool has_max_heap_property = bihp_maxpq_has_max_heap_property(q);
  ut_assert(t, has_max_heap_property);

  bihp_maxpq_destroy(q);
  q = NULL;
}

static void
bihp_maxpq_extract_max_t (ut_test_t *const t)
{
  static const int heap_size = 17;

  const bool verbose = ut_run_time_is_verbose(t);

  bihp_max_priority_queue_t q;
  size_t array_size = heap_size;
  bihp_priority_function pf = aux_priority;
  bihp_print_item_function pif = aux_print_item;

  struct aux_item_s xs[heap_size];
  for (int i = 0; i < heap_size; i++) {
    xs[i].value = i;
  }

  q = bihp_maxpq_create(array_size, pf, pif);
  ut_assert(t, q);

  for (int i = 0; i < heap_size; i++) {
    bihp_maxpq_insert(q, &xs[i]);

    item_t max = bihp_maxpq_maximum(q);
    int max_value = pf(max);
    ut_assert(t, i == max_value);

    bool has_max_heap_property = bihp_maxpq_has_max_heap_property(q);
    ut_assert(t, has_max_heap_property);
  }

  for (int i = heap_size - 1; i >= 0; i--) {
    item_t max = bihp_maxpq_extract_max(q);
    int max_value = pf(max);
    ut_assert(t, i == max_value);

    bool has_max_heap_property = bihp_maxpq_has_max_heap_property(q);
    ut_assert(t, has_max_heap_property);
  }

  if (verbose) {
    printf("\n");
    bihp_maxpq_print(q, stdout);
  }

  bihp_maxpq_destroy(q);
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

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_maxpq_create", bihp_maxpq_create_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_maxpq_insert", bihp_maxpq_insert_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_maxpq_maximum", bihp_maxpq_maximum_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bihp_maxpq_extract_max", bihp_maxpq_extract_max_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
