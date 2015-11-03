/**
 * @file
 *
 * @brief Red black tree unit test suite.
 * @details Collects tests and helper methods for the red-black tree module.
 *
 * @par red_black_tree_test.c
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

#include <glib.h>

#include "red_black_tree.h"
#include "random.h"



/**
 * @brief To be documented.
 */
const int test_array[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };



/* Test function prototypes. */

static void creation_and_destruction_test (void);
static void probe_test (void);
static void copy_test (void);
static void copy_test (void);
static void insert_replace_and_find_test (void);
static void delete_test (void);
static void volume_test (void);
static void random_key_volume_test (void);
static void traverser_basic_test (void);
static void traverser_find_and_copy_test (void);



/* Helper function prototypes. */

static int
compare_int (const void *item_a,
             const void *item_b,
             void *param);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/red_black_tree/creation_and_destruction_test", creation_and_destruction_test);
  g_test_add_func("/red_black_tree/probe_test", probe_test);
  g_test_add_func("/red_black_tree/copy_test", copy_test);
  g_test_add_func("/red_black_tree/insert_replace_and_find_test", insert_replace_and_find_test);
  g_test_add_func("/red_black_tree/delete_test", delete_test);
  g_test_add_func("/red_black_tree/volume_test", volume_test);
  g_test_add_func("/red_black_tree/random_key_volume_test", random_key_volume_test);

  g_test_add_func("/red_black_tree/traverser_basic_test", traverser_basic_test);
  g_test_add_func("/red_black_tree/traverser_find_and_copy_test", traverser_find_and_copy_test);

  return g_test_run();
}



/*
 * Test functions for the table structure.
 */

static void
creation_and_destruction_test (void)
{
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  g_assert(table);

  size_t count = rbt_count(table);
  g_assert(count == 0);

  rbt_destroy(table, NULL);
}

static void
probe_test (void)
{
  /* Test data set is composed by an array of ten integers: [0..9]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  g_assert(table);

  /* Count has to be zero. */
  g_assert(rbt_count(table) == 0);

  /* Inserts the [0..9] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    int *item = &data[i];
    int **item_ref = (int **) rbt_probe(table, item);
    g_assert(rbt_count(table) == i + 1);             /* Table count has to be equal to the number of inserted elements. */
    g_assert(*item_ref != NULL);                     /* Item pointer has to be not null. */
    g_assert(*item_ref == &data[i]);                 /* Item pointer has to reference the appropriate array element. */
    g_assert(**item_ref == i);                       /* Item (**item_ref) has to be equal to the loop counter. */
  }

  /* Probes the table again with the same data set. Nothing has to happen. */
  for (size_t i = 0; i < data_size; i++) {
    int *item = &data[i];
    int **item_ref = (int **) rbt_probe(table, item);
    g_assert(rbt_count(table) == data_size);         /* Table count has to stay constat at data_size. */
    g_assert(*item_ref != NULL);
    g_assert(*item_ref == &data[i]);
    g_assert(**item_ref == i);
  }

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
copy_test (void)
{
  /* Test data set is composed by an array of ten integers: [0..9]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  g_assert(table);

  /* Inserts the [0..9] set of elements in the table. */
  for (size_t i = 0; i < data_size; i++) {
    int *item = &data[i];
    rbt_probe(table, item);
  }

  /* We must have data_size elements in the table. */
  g_assert(rbt_count(table) == data_size);

  /* Copies table into a new tree. */
  rbt_item_copy_f *copy = NULL;
  rbt_item_destroy_f *destroy = NULL;
  mem_allocator_t *alloc = NULL;
  rbt_table_t *copied_table = rbt_copy(table, copy, destroy, alloc);

  /* Frees the table. */
  rbt_destroy(table, NULL);

  /* We must have data_size elements in the table. */
  g_assert(rbt_count(copied_table) == data_size);

  /* Frees the table. */
  rbt_destroy(copied_table, NULL);
}

static void
insert_replace_and_find_test (void)
{
  /* We need data with key and content to properly test the replace use case. */
  struct element {
    int key;
    int content;
  };

  /* Values assigned to data. */
  const int value_a = 0;
  const int value_b = 1;

  /* Test data set is composed by an array of ten element structure: [{0,0}..{9,0}]. */
  struct element data_a[] = { {0, value_a},
                              {1, value_a},
                              {2, value_a},
                              {3, value_a},
                              {4, value_a},
                              {5, value_a},
                              {6, value_a},
                              {7, value_a},
                              {8, value_a},
                              {9, value_a} };

  /* Data size is dynamically computed. */
  const size_t data_size = sizeof(data_a) / sizeof(data_a[0]);

  /* A second array set is prepared, having the same size, and same keys, but different content. */
  struct element data_b[data_size];
  for (size_t i = 0; i < data_size; i++) {
    data_b[i].key = data_a[i].key;
    data_b[i].content = value_b;
  }

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  g_assert(table);

  /* Inserts the data_a set of elements in the table. */
  for (size_t i = 0; i < data_size; i++) {
    struct element *e = &data_a[i];
    struct element *e_ref = (struct element *) rbt_insert(table, e);
    g_assert(rbt_count(table) == i + 1);         /* Table count has to be equal to the number of inserted elements. */
    g_assert(e_ref == NULL);                     /* Item pointer has to be null when insertion succeeds. */
  }

  /* We must have data_size elements in the table now. */
  g_assert(rbt_count(table) == data_size);

  /* Inserts the data_b set of elements in the table. Nothing has to happen. */
  for (size_t i = 0; i < data_size; i++) {
    struct element *e = &data_b[i];
    struct element *e_ref = (struct element *) rbt_insert(table, e);
    g_assert(rbt_count(table) == data_size);     /* Table count has to stay constant. */
    g_assert(e_ref != NULL);                     /* Item pointer has to be not null. */
    g_assert(e_ref == &data_a[i]);               /* Item pointer has to reference the appropriate array element. */
    g_assert(e_ref->key == i);                   /* Item's key has to be equal to the loop counter. */
    g_assert(e_ref->content == value_a);         /* Item's content has to be equal to value_a. */
  }

  /* We must have still data_size elements in the table. */
  g_assert(rbt_count(table) == data_size);

  /* Replaces elements using data_b set. All elements has to be replaced. */
  for (size_t i = 0; i < data_size; i++) {
    struct element *e = &data_b[i];
    struct element *e_ref = (struct element *) rbt_replace(table, e);
    g_assert(rbt_count(table) == data_size);     /* Table count has to stay constant. */
    g_assert(e_ref != NULL);                     /* Item pointer has to be not null. */
    g_assert(e_ref == &data_a[i]);               /* Item pointer has to reference the appropriate array element. */
    g_assert(e_ref->key == i);                   /* Item's key has to be equal to the loop counter. */
    g_assert(e_ref->content == value_a);         /* Item's content has to be equal to value_a. */
  }

  /* We must have still data_size elements in the table. */
  g_assert(rbt_count(table) == data_size);

  /* Searches all the ten elements, they must be found, and the contant has to be equal to value_b. */
  for (size_t i = 0; i < data_size; i++) {
    struct element *e = (struct element *) rbt_find(table, &i);
    g_assert(e != NULL);
    g_assert(e == &data_b[i]);
    g_assert(e->key == i);
    g_assert(e->content == value_b);
  }

  /* Searches for a missing key, result has to be null. */
  int missing_key = -1;
  struct element *missing = rbt_find(table, &missing_key);
  g_assert(missing == NULL);

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
delete_test (void)
{
  /* Test data set is composed by an array of 48 integers: [0..47]. */
  int data[] = {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  g_assert(table);

  /* Count has to be zero. */
  g_assert(rbt_count(table) == 0);

  /* Inserts the [0..9] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    int *item = &data[i];
    int **item_ref = (int **) rbt_probe(table, item);
    g_assert(rbt_count(table) == i + 1);             /* Table count has to be equal to the number of inserted elements. */
    g_assert(*item_ref != NULL);                     /* Item pointer has to be not null. */
    g_assert(*item_ref == &data[i]);                 /* Item pointer has to reference the appropriate array element. */
    g_assert(**item_ref == i);                       /* Item (**item_ref) has to be equal to the loop counter. */
  }

  /* Deletes all elements one by one. */
  for (size_t i = 0; i < data_size; i++) {
    int *e = &data[i];
    int *e_ref = (int *) rbt_delete(table, e);
    g_assert(rbt_count(table) == data_size - (i + 1));         /* Table count has to stay constat at data_size. */
    g_assert(*e_ref == *e);
    for (size_t j = 0; j <= i; j++) {
      int *e1 = (int *) rbt_find(table, &j);
      g_assert(e1 == NULL);
    }
    for (size_t j = i + 1; j < data_size; j++) {
      int *e1 = (int *) rbt_find(table, &j);
      g_assert(e1 != NULL);
      g_assert(*e1 == j);
    }
  }

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
volume_test (void)
{
  int *data;
  mem_allocator_t *alloc = &mem_allocator_default;

  const size_t max_size = 1024;

  for (size_t k = 1; k < max_size; k++) {

    data = alloc->malloc(alloc, sizeof(int) * k);
    for (size_t i = 0; i < k; i++) {
      data[i] = i;
    }

    rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
    g_assert(table);

    g_assert(rbt_count(table) == 0);

    /* Inserts elements sequentially. */
    for (size_t i = 0; i < k; i++) {
      int *e = &data[i];
      int **e_ref = (int **) rbt_probe(table, e);
      g_assert(rbt_count(table) == i + 1);         /* Table count has to be equal to the number of inserted elements. */
      g_assert(*e_ref != NULL);                    /* Item pointer has to be not null. */
      g_assert(*e_ref == &data[i]);                /* Item pointer has to reference the appropriate array element. */
      g_assert(**e_ref == i);                      /* Item (**e_ref) has to be equal to the loop counter. */
    }

    /* We must have k elements in the table. */
    g_assert(rbt_count(table) == k);

    /* Searches for each element and checks it exists in the table. */
    for (size_t i = 0; i < k; i++) {
      int *e = (int *) rbt_find(table, &i);
      g_assert(e != NULL);
      g_assert(*e == i);
    }

    /* Deletes odd element and checks it exists in the table. */
    for (size_t i = 0; i < (k + 1) / 2; i++) {
      int i2 = i * 2;
      int *e = (int *) rbt_delete(table, &i2);
      g_assert(e != NULL);
      g_assert(*e == i2);
    }

    /* Final count has to be half of the number of inserted elements. */
    g_assert(rbt_count(table) == (size_t)(k / 2));

    rbt_destroy(table, NULL);

    alloc->free(alloc, data);
  }

}

static void
random_key_volume_test (void)
{
  /* Structure element for data array has key and val. */
  struct element {
    int key;
    int val;
  };

  /* Data size. */
  const size_t data_size = 2048;

  /* Number of insertions. */
  const size_t insertion_count = 4096;

  /* Checksums. */
  size_t element_count_checksum = 0;
  size_t insertion_count_checksum = 0;

  /* Data is initialized having key equal to the element index into the array, and val equal to zero. */
  struct element data[data_size];
  for (size_t i = 0; i < data_size; i++) {
    data[i].key = i;
    data[i].val = 0;
  }

  /* Sets up the RNG. */
  const unsigned int seed = 20150801;
  RandomNumberGenerator *rng = rng_new(seed);
  g_assert(rng);

  /* Creates the table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  g_assert(table);

  /* Populates the table with random selection within the set. Element's val field is incremented on each probe. */
  for (size_t k = 0; k < insertion_count; k++) {
    const unsigned long index = rng_random_choice_from_finite_set(rng, data_size);
    struct element **e = (struct element **) rbt_probe(table, &data[index]);
    (*e)->val++;
  }

  /* Checks that the tree has the probed elements, and doesn't have the skipped ones. */
  for (size_t i = 0; i < data_size; i++) {
    const int key = i;
    const int count = data[i].val;
    insertion_count_checksum += count;
    if (count) element_count_checksum++;
    struct element *e = (struct element *) rbt_find(table, &key);
    if (e) g_assert(count > 0);
    else g_assert(count == 0);
  }
  g_assert(insertion_count_checksum == insertion_count);
  g_assert(element_count_checksum == rbt_count(table));
  g_assert(element_count_checksum == 1776);      /* Depends on RNG, seed, data_size, and insertion_count. */

  /* Frees resources. */
  rbt_destroy(table, NULL);
  rng_free(rng);
}



/*
 * Test functions for the traverser structure.
 */

static void
traverser_basic_test (void)

{
  rbt_traverser_t traverser;
  rbt_traverser_t *t = &traverser;
  int *e;
  int counter;

  /* Test data set is composed by an array of ten integers: [0..9]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  g_assert(table);

  /* Inserts the [0..9] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    rbt_probe(table, &data[i]);
  }

  /* Traverses the table clockwise starting from the null element. */
  counter = 0;
  rbt_t_init(t, table);
  while ((e = rbt_t_next(t))) {
    g_assert(*e == counter++);
    g_assert(rbt_t_cur(t) == e);
  }
  g_assert(counter == data_size);
  g_assert(rbt_t_cur(t) == NULL);

  /* Traverses the table counterclockwise starting from the null element. */
  counter = data_size;
  rbt_t_init(t, table);
  while ((e = rbt_t_prev(t))) {
    g_assert(*e == --counter);
    g_assert(rbt_t_cur(t) == e);
  }
  g_assert(counter == 0);
  g_assert(rbt_t_cur(t) == NULL);

  /* Traverses the table clockwise starting from the first element. */
  for (e = rbt_t_first(t, table), counter = 0; e; e = rbt_t_next(t), counter++) {
    g_assert(*e == counter);
    g_assert(rbt_t_cur(t) == e);
  }
  g_assert(counter == data_size);
  g_assert(rbt_t_cur(t) == NULL);

  /* Traverses the table counterclockwise starting from the last element. */
  for (e = rbt_t_last(t, table), counter = data_size - 1; e; e = rbt_t_prev(t), counter--) {
    g_assert(*e == counter);
    g_assert(rbt_t_cur(t) == e);
  }
  g_assert(counter == -1);
  g_assert(rbt_t_cur(t) == NULL);

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
traverser_find_and_copy_test (void)

{
  rbt_traverser_t traverser_1, traverser_2;
  rbt_traverser_t *t1 = &traverser_1;
  rbt_traverser_t *t2 = &traverser_2;
  int *e;

  /* Test data set is composed by an array of ten integers: [0..16]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  g_assert(table);

  /* Inserts the [0..9] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    rbt_probe(table, &data[i]);
  }

  for (int key = 0; key < data_size; key ++) {
    e = (int *) rbt_t_find(t1, table, &key);
    g_assert(*e == key);

    e = rbt_t_copy(t2, t1);
    g_assert(*e == key);

    int *next = (int *) rbt_t_next(t1);
    if (key == data_size - 1) g_assert(!next);
    else g_assert(*next == key + 1);

    int *prev = (int *) rbt_t_prev(t2);
    if (key == 0) g_assert(!prev);
    else g_assert(*prev == key - 1);
  }

  /* Frees the table. */
  rbt_destroy(table, NULL);
}


/*
 * Internal functions.
 */

static int
compare_int (const void *item_a,
             const void *item_b,
             void *param)
{
  assert(item_a);
  assert(item_b);
  const int *a = (int *) item_a;
  const int *b = (int *) item_b;
  return (*a > *b) - (*a < *b);
}
