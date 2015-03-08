/**
 * @file
 *
 * @brief Linked list utils unit test suite.
 * @details Collects tests and helper methods for the linked list module.
 *
 * @par llist_test.c
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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "unit_test.h"
#include "linked_list.h"



/*
 * Auxiliary functions.
 */

void
aux_print_elements (d, aux)
     void *const d;
     void *const aux;
{
  int *ip = (int *) d;
  printf("%d\n", *ip);
}

void
aux_add_elements (d, aux)
     void *const d;
     void *const aux;
{
  assert(d);
  assert(aux);
  int *value = (int *) d;
  int *sum = (int *) aux;
  *sum += *value;
}

void
aux_count_elements (d, aux)
     void *const d;
     void *const aux;
{
  assert(aux);
  int *count = (int *) aux;
  (*count)++;
}

int
aux_int_cmp (const void *const a,
             const void *const b)
{
  const int *const x = (const int *const) a;
  const int *const y = (const int *const) b;
  return (*x > *y) - (*x < *y);
}



/*
 * Test functions.
 */

static void
llist_new_free_test (ut_test_t *const t)
{
  llist_t *l = llist_new(NULL);
  ut_assert(t, l != NULL);
  llist_free(l);
}

static void
llist_add_remove_foreach_test (ut_test_t *const t)
{
  int data[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};

  /* Iterating on the empty list. */
  {
    llist_t *l = llist_new(NULL);
    ut_assert(t, l != NULL);
    int sum = 0;
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 0);
    llist_free(l);
  }

  /* Adding one element and iterating on it. */
  {
    llist_t *l = llist_new(NULL);
    ut_assert(t, l != NULL);
    int sum = 0;
    llist_add(l, &data[7]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 7);
    llist_free(l);
  }

  /* Adding, iterating, removing, and adding again ... */
  {
    int sum;
    llist_t *l = llist_new(NULL);
    ut_assert(t, l != NULL);

    sum = 0;
    llist_add(l, &data[0]);
    llist_add(l, &data[1]);
    llist_add(l, &data[2]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 3);

    sum = 0;
    llist_remove(l, NULL);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 3);

    sum = 0;
    llist_remove(l, &data[0]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 3);

    sum = 0;
    llist_remove(l, &data[2]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 1);

    sum = 0;
    llist_add(l, &data[7]);
    llist_add(l, &data[8]);
    llist_add(l, &data[9]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 25);

    sum = 0;
    llist_remove(l, &data[8]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 17);

    llist_free(l);
  }

  /* Adding and removing the same element. */
  {
    int sum;
    llist_t *l = llist_new(NULL);
    ut_assert(t, l != NULL);

    sum = 0;
    llist_add(l, &data[7]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 7);

    sum = 0;
    llist_add(l, &data[7]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 14);

    sum = 0;
    llist_add(l, &data[7]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 21);

    sum = 0;
    llist_remove(l, &data[7]);
    llist_foreach(l, aux_add_elements, &sum);
    ut_assert(t, sum == 14);

    llist_free(l);
  }
}

static void
llist_length_test (ut_test_t *const t)
{
  int count;
  llist_t *l = llist_new(NULL);
  ut_assert(t, l != NULL);
  ut_assert(t, l->length == 0);

  count = 0;
  llist_foreach(l, aux_count_elements, &count);
  ut_assert(t, count == 0);

  count = 0;
  llist_add(l, NULL);
  llist_foreach(l, aux_count_elements, &count);
  ut_assert(t, count == 1);
  ut_assert(t, l->length == count);

  count = 0;
  llist_add(l, NULL);
  llist_add(l, NULL);
  llist_foreach(l, aux_count_elements, &count);
  ut_assert(t, count == 3);
  ut_assert(t, l->length == count);

  count = 0;
  llist_remove(l, NULL);
  llist_foreach(l, aux_count_elements, &count);
  ut_assert(t, count == 2);
  ut_assert(t, l->length == count);

  llist_free(l);
}

static void
llist_find_test (ut_test_t *const t)
{
  int data[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  int data_size = 10;

  llist_t *l = llist_new(NULL);
  ut_assert(t, l != NULL);
  for (int i = 0; i < data_size; i++) {
    llist_add(l, &data[i]);
  }

  llist_elm_t *e = llist_find(l, NULL, NULL);
  ut_assert(t, e == NULL);

  for (int i = 0; i < data_size; i++) {
    size_t index;
    llist_elm_t *e = llist_find(l, &data[i], &index);
    ut_assert(t, e != NULL);
    if (e) ut_assert(t, i == *((int *)e->data));
    if (e) ut_assert(t, index == (data_size -i -1));
  }

  llist_free(l);
}

static void
llist_nth_test (ut_test_t *const t)
{
  int data[] = {9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
  int data_size = 10;

  llist_t *l = llist_new(NULL);
  ut_assert(t, l != NULL);
  for (int i = 0; i < data_size; i++) {
    llist_add(l, &data[i]);
  }

  for (int i = 0; i < data_size; i++) {
    const llist_elm_t *const e = llist_nth(l, i);
    const int *data = (int *) e->data;
    ut_assert(t, i == *data);
  }

  llist_free(l);
}

static void
llist_nth_data_test (ut_test_t *const t)
{
  int data[] = {9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
  int data_size = 10;

  llist_t *l = llist_new(NULL);
  ut_assert(t, l != NULL);
  for (int i = 0; i < data_size; i++) {
    llist_add(l, &data[i]);
  }

  for (int i = 0; i < data_size; i++) {
    const int *data = (int *) llist_nth_data(l, i);
    ut_assert(t, i == *data);
  }

  llist_free(l);
}

static void
llist_insert_at_position_test (ut_test_t *const t)
{
  int data[] = {9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
  int data_size = 10;
  int v = 99;

  for (int p = 0; p < data_size + 1; p++) {
    llist_t *l = llist_new(NULL);
    ut_assert(t, l != NULL);
    for (int i = 0; i < data_size; i++) {
      llist_add(l, &data[i]);
    }
    ut_assert(t, l->length == data_size);
    llist_insert_at_position(l, &v, p);
    const int *inserted = (int *) llist_nth_data(l, p);
    ut_assert(t, v == *inserted);
    llist_free(l);
  }
}

static void
llist_insert_after_elm_test (ut_test_t *const t)
{
  int data[] = {9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
  int data_size = 10;
  int v = 99;

  for (int p = 0; p < data_size; p++) {
    llist_t *l = llist_new(NULL);
    ut_assert(t, l != NULL);
    for (int i = 0; i < data_size; i++) {
      llist_add(l, &data[i]);
    }
    ut_assert(t, l->length == data_size);
    llist_elm_t *const e = llist_nth(l, p);
    llist_insert_after_elm(l, e, &v);
    const int *inserted = (int *) llist_nth_data(l, p + 1);
    ut_assert(t, v == *inserted);
    ut_assert(t, l->length == data_size + 1);
    size_t index;
    llist_find(l, &v, &index);
    ut_assert(t, index == p + 1);
    llist_free(l);
  }
}

static void
llist_insert_before_elm_test (ut_test_t *const t)
{
  int data[] = {9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
  int data_size = 10;
  int v = 99;

  for (int p = 0; p < data_size; p++) {
    llist_t *l = llist_new(NULL);
    ut_assert(t, l != NULL);
    for (int i = 0; i < data_size; i++) {
      llist_add(l, &data[i]);
    }
    ut_assert(t, l->length == data_size);
    llist_elm_t *const e = llist_nth(l, p);
    llist_insert_before_elm(l, e, &v);
    const int *inserted = (int *) llist_nth_data(l, p);
    ut_assert(t, v == *inserted);
    ut_assert(t, l->length == data_size + 1);
    size_t index;
    llist_find(l, &v, &index);
    ut_assert(t, index == p);
    llist_free(l);
  }
}

static void
llist_last_elm_test (ut_test_t *const t)
{
  int data[] = {0, 1, 2, 3};
  int data_size = 4;

  llist_t *l = llist_new(NULL);
  ut_assert(t, l != NULL);
  ut_assert(t, llist_last_elm(l) == NULL);
  for (int i = 0; i < data_size; i++) {
    llist_add(l, &data[i]);
    ut_assert(t, llist_last_elm(l) == llist_nth(l, i));
    llist_elm_t *expected_last = llist_find(l, &data[0], NULL);
    ut_assert(t, llist_last_elm(l) == expected_last);
  }

  llist_free(l);
}

static void
llist_reverse_test (ut_test_t *const t)
{
  int data[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  int data_size = 10;


  for (int j = 0; j < data_size + 1; j++) {
    llist_t *l = llist_new(NULL);
    ut_assert(t, l != NULL);
    for (int i = 0; i < j; i++) {
      llist_add(l, &data[i]);
    }

    llist_reverse(l);

    int i;
    llist_elm_t *e;
    for (e = l->head, i = 0; e; e = e->next, i++) {
      int element_value = *(int *) e->data;
      ut_assert(t, i == element_value);
      ut_assert(t, data[i] == element_value);
    }

    llist_free(l);
  }
}

static void
llist_insertion_sort_test (ut_test_t *const t)
{
  int data[] = {4, 2, 6, 1, 3, 0, 8, 9, 7, 5};
  int data_size = 10;

  llist_t *l = llist_new(aux_int_cmp);
  ut_assert(t, l != NULL);
  for (int i = 0; i < data_size; i++) {
    llist_add(l, &data[i]);
  }

  llist_insertion_sort(l);

  for (int i = 0; i < data_size; i++) {
    ut_assert(t, i == *(int *)llist_nth_data(l, i));
  }

  llist_free(l);
}

static void
llist_adv_insertion_sort_test (ut_test_t *const t)
{
  int data[] = {4, 2, 6, 1, 3, 0, 8, 9, 7, 5};
  //int data[] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  //int data[] = {9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
  //int data[] = {9, 8, 7, 6, 1, 4, 3, 2, 5, 0};
  int data_size = 10;

  llist_t *l = llist_new(aux_int_cmp);
  ut_assert(t, l != NULL);
  for (int i = 0; i < data_size; i++) {
    llist_add(l, &data[i]);
  }

  llist_adv_insertion_sort(l);

  for (int i = 0; i < data_size; i++) {
    ut_assert(t, i == *(int *)llist_nth_data(l, i));
  }

  llist_free(l);
}

static void
llist_merge_sort_test (ut_test_t *const t)
{
  int data_size = 31;
  int data[] = {4, 25, 10,  1, 12, 18,  8, 21,
                7, 16,  6,  3, 13, 14, 11, 15,
                5, 17,  0, 19, 20,  9, 23, 22,
                24, 2, 29, 30, 27, 26, 28};

  llist_t *l = llist_new(aux_int_cmp);
  ut_assert(t, l != NULL);
  for (int i = 0; i < data_size; i++) {
    llist_add(l, &data[i]);
  }

  llist_merge_sort(l);
  llist_foreach(l, aux_print_elements, NULL);
  printf("\n");

  /*
  for (int i = 0; i < data_size; i++) {
    ut_assert(t, i == *(int *)llist_nth_data(l, i));
  }
  */

  llist_free(l);
}


/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_suite_t *const s = ut_suite_new();

  ut_suite_add_simple_test(s, "llist_new_free", llist_new_free_test);
  ut_suite_add_simple_test(s, "llist_add_remove_foreach", llist_add_remove_foreach_test);
  ut_suite_add_simple_test(s, "llist_length", llist_length_test);
  ut_suite_add_simple_test(s, "llist_find", llist_find_test);
  ut_suite_add_simple_test(s, "llist_nth", llist_nth_test);
  ut_suite_add_simple_test(s, "llist_nth_data", llist_nth_data_test);
  ut_suite_add_simple_test(s, "llist_insert_at_position", llist_insert_at_position_test);
  ut_suite_add_simple_test(s, "llist_insert_after_elm", llist_insert_after_elm_test);
  ut_suite_add_simple_test(s, "llist_insert_before_elm", llist_insert_before_elm_test);
  ut_suite_add_simple_test(s, "llist_last_elm", llist_last_elm_test);
  ut_suite_add_simple_test(s, "llist_reverse", llist_reverse_test);
  ut_suite_add_simple_test(s, "llist_insertion_sort", llist_insertion_sort_test);
  ut_suite_add_simple_test(s, "llist_adv_insertion_sort", llist_adv_insertion_sort_test);
  ut_suite_add_simple_test(s, "llist_merge_sort", llist_merge_sort_test);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
