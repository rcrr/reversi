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



/**
 * @brief To be documented.
 */
const int test_array[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };



/* Test function prototypes. */

static void dummy_test (void);
static void creation_and_destruction_test (void);
static void probe_test (void);
static void copy_test (void);


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

  g_test_add_func("/red_black_tree/dummy", dummy_test);
  g_test_add_func("/red_black_tree/creation_and_destruction_test", creation_and_destruction_test);
  g_test_add_func("/red_black_tree/probe_test", probe_test);
  g_test_add_func("/red_black_tree/copy_test", copy_test);

  return g_test_run();
}



/*
 * Test functions.
 */

static void
dummy_test (void)
{
  g_assert(TRUE);
}

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
  const size_t data_size = 10;

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
