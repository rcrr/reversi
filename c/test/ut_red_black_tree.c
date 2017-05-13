/**
 * @file
 *
 * @brief Red black tree unit test suite.
 * @details Collects tests and helper methods for the red black tree module.
 *
 * @par ut_red_black_tree.c
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
#include <limits.h>
#include <assert.h>

#include "unit_test.h"

#include "red_black_tree.h"



/*
 * Auxiliary functions.
 */

/*
 * Compare function for integers.
 * It is used by the tree.
 */
static int
compare_int (const void *item_a,
             const void *item_b,
             void *param)
{
  assert(item_a);
  assert(item_b);
  const int a = *(int *) item_a;
  const int b = *(int *) item_b;
  return (a > b) - (a < b);
}

/*
 * Compare function for integers.
 * Argument param is a pointer to an unsigned long long integer
 * that is incremented each call.
 */
static int
compare_int_and_increment_param (const void *item_a,
                                 const void *item_b,
                                 void *param)
{
  assert(item_a);
  assert(item_b);
  assert(param);
  unsigned long long int *cnt_p = (unsigned long long int *) param;
  const int a = *(int *) item_a;
  const int b = *(int *) item_b;
  (*cnt_p)++;
  return (a > b) - (a < b);
}

/*
 * Argument param is a pointer to an unsigned long long integer
 * that is incremented each call.
 */
static void
on_item_destroy_increment_param (void *item,
                                 void *param)
{
  assert(item);
  assert(param);
  unsigned long long int *cnt_p = (unsigned long long int *) param;
  (*cnt_p)++;
}

/*
 * Examines the binary tree rooted at node.
 * Zeroes *okay if an error occurs.
 * Otherwise, does not modify *okay.
 * Sets *count to the number of nodes in that tree, including node itself if node != NULL.
 * Sets *bh to the tree's black-height.
 * All the nodes in the tree are verified to be at least min but no greater than max.
 */
static void
recurse_verify_tree (rbt_node_t *node,
                     int *okay,
                     size_t *count,
                     int min,
                     int max,
                     int *bh)
{
  int d;                /* Value of this node's data. */
  size_t subcount[2];   /* Number of nodes in subtrees. */
  int subbh[2];         /* Black-heights of subtrees. */

  if (node == NULL) {
    *count = 0;
    *bh = 0;
    return;
  }
  d = *(int *) node->data;

  if (min > max) {
    printf(" Parents of node %d constrain it to empty range %d...%d.\n", d, min, max);
    *okay = 0;
  }
  else if (d < min || d > max) {
    printf(" Node %d is not in range %d...%d implied by its parents.\n", d, min, max);
    *okay = 0;
  }

  recurse_verify_tree(node->links[0], okay, &subcount[0], min, d - 1, &subbh[0]);
  recurse_verify_tree(node->links[1], okay, &subcount[1], d + 1, max, &subbh[1]);
  *count = 1 + subcount[0] + subcount[1];
  *bh = (node->color == RBT_BLACK) + subbh[0];

  if (node->color != RBT_RED && node->color != RBT_BLACK) {
    printf(" Node %d is neither red nor black (%d).\n", d, node->color);
    *okay = 0;
  }

  /* Verifies compliance with rule 1. */
  if (node->color == RBT_RED) {
    if (node->links[0] != NULL && node->links[0]->color == RBT_RED) {
      printf(" Red node %d has red left child %d\n", d, *(int *) node->links[0]->data);
      *okay = 0;
    }

    if (node->links[1] != NULL && node->links[1]->color == RBT_RED) {
      printf(" Red node %d has red right child %d\n", d, *(int *) node->links[1]->data);
      *okay = 0;
    }
  }

  /* Verifies compliance with rule 2. */
  if (subbh[0] != subbh[1]) {
    printf(" Node %d has two different black-heights: left bh=%d, right bh=%d\n", d, subbh[0], subbh[1]);
    *okay = 0;
  }
}

/*
 * Checks that tree is well-formed and verifies that the values in array[] are actually in tree.
 * There must be n elements in array[] and tree.
 * Returns nonzero only if no errors detected.
 */
static int
verify_tree (rbt_table_t *tree,
             int array[],
             size_t n)
{
  int okay = 1;

  /* Checks tree's rbt_count against that supplied. */
  if (rbt_count(tree) != n) {
    printf(" Tree count is %lu, but should be %lu.\n", (unsigned long) rbt_count(tree), (unsigned long) n);
    okay = 0;
  }

  if (okay) {
    if (tree->root != NULL && tree->root->color != RBT_BLACK) {
      printf(" Tree's root is not black.\n");
      okay = 0;
    }
  }

  if (okay) {
    /* Recursively verifies tree structure. */
    size_t count;
    int bh;

    recurse_verify_tree(tree->root, &okay, &count, 0, INT_MAX, &bh);
    if (count != n) {
      printf(" Tree has %lu nodes, but should have %lu.\n", (unsigned long) count, (unsigned long) n);
      okay = 0;
    }
  }

  if (okay) {
    /* Checks that all the values in array[] are in tree. */
    size_t i;

    for (i = 0; i < n; i++)
      if (rbt_find(tree, &array[i]) == NULL) {
        printf(" Tree does not contain expected value %d.\n", array[i]);
        okay = 0;
      }
  }

  if (okay) {
    /* Checks that rbt_t_first() and rbt_t_next() work properly. */
    rbt_traverser_t trav;
    size_t i;
    int prev = -1;
    int *item;

    for (i = 0, item = rbt_t_first(&trav, tree); i < 2 * n && item != NULL;
         i++, item = rbt_t_next(&trav)) {
      if (*item <= prev) {
        printf(" Tree out of order: %d follows %d in traversal\n", *item, prev);
        okay = 0;
      }

      prev = *item;
    }

    if (i != n) {
      printf(" Tree should have %lu items, but has %lu in traversal\n", (unsigned long) n, (unsigned long) i);
      okay = 0;
    }
  }

  if (okay) {
    /* Checks that rbt_t_last() and rbt_t_prev() work properly. */
    rbt_traverser_t trav;
    size_t i;
    int next = INT_MAX;
    int *item;

    for (i = 0, item = rbt_t_last(&trav, tree); i < 2 * n && item != NULL; i++, item = rbt_t_prev(&trav)) {
      if (*item >= next) {
        printf(" Tree out of order: %d precedes %d in traversal\n", *item, next);
        okay = 0;
      }

      next = *item;
    }

    if (i != n) {
      printf (" Tree should have %lu items, but has %lu in reverse\n", (unsigned long) n, (unsigned long) i);
      okay = 0;
    }
  }

  if (okay) {
    /* Checks that rbt_t_init() works properly. */
    rbt_traverser_t init, first, last;
    int *cur, *prev, *next;

    rbt_t_init (&init, tree);
    rbt_t_first (&first, tree);
    rbt_t_last (&last, tree);

    cur = rbt_t_cur(&init);
    if (cur != NULL) {
      printf(" Inited traverser should be null, but is actually %d.\n", *cur);
      okay = 0;
    }

    next = rbt_t_next(&init);
    if (next != rbt_t_cur(&first)) {
      printf(" Next after null should be %d, but is actually %d.\n", *(int *) rbt_t_cur(&first), *next);
      okay = 0;
    }
    rbt_t_prev(&init);

    prev = rbt_t_prev(&init);
    if (prev != rbt_t_cur(&last)) {
      printf(" Previous before null should be %d, but is actually %d.\n", *(int *) rbt_t_cur(&last), *prev);
      okay = 0;
    }
    rbt_t_next(&init);
  }

  return okay;
}

/*
 * Argument param is a pointer to an unsigned long long integer
 * that is incremented each call.
 */
static void *
on_item_copy_increment_param (void *item,
                              void *param)
{
  assert(item);
  assert(param);
  unsigned long long int *cnt_p = (unsigned long long int *) param;
  (*cnt_p)++;
  return item;
}

/*
 * Compares binary trees rooted at a and b, making sure that they are identical.
 */
static int
compare_trees (rbt_node_t *a,
               rbt_node_t *b,
               rbt_item_compare_f *compare,
               void *param)
{
  int okay;

  if (a == NULL || b == NULL) {
    assert(a == NULL && b == NULL);
    return 1;
  }

  if (compare(a->data, b->data, param) != 0
      || ((a->links[0] != NULL) != (b->links[0] != NULL))
      || ((a->links[1] != NULL) != (b->links[1] != NULL))
      || a->color != b->color) {
    printf("Copied nodes differ.\n");
    return 0;
  }

  okay = 1;
  if (a->links[0] != NULL) okay &= compare_trees(a->links[0], b->links[0], compare, param);
  if (a->links[1] != NULL) okay &= compare_trees(a->links[1], b->links[1], compare, param);
  return okay;
}



/*
 * Test functions.
 */

static void
creation_and_destruction_t (ut_test_t *const t)
{
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  ut_assert(t, table != NULL);

  size_t count = rbt_count(table);
  ut_assert(t, count == 0);

  ut_assert(t, verify_tree(table, NULL, 0));

  rbt_destroy(table, NULL);
}

static void
probe_t (ut_test_t *const t)
{
  /* Test data set is composed by an array of ten integers: [0..9]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  ut_assert(t, table != NULL);

  /* Count has to be zero. */
  ut_assert(t, rbt_count(table) == 0);

  /* Inserts the [0..9] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    int *item = &data[i];
    int **item_ref = (int **) rbt_probe(table, item);
    ut_assert(t, rbt_count(table) == i + 1);             /* Table count has to be equal to the number of inserted elements. */
    ut_assert(t, *item_ref != NULL);                     /* Item pointer has to be not null. */
    ut_assert(t, *item_ref == &data[i]);                 /* Item pointer has to reference the appropriate array element. */
    ut_assert(t, **item_ref == i);                       /* Item (**item_ref) has to be equal to the loop counter. */
    ut_assert(t, verify_tree(table, data, i + 1));       /* Runs the verify_tree procedure on the growing table. */
  }

  /* Probes the table again with the same data set. Nothing has to happen. */
  for (size_t i = 0; i < data_size; i++) {
    int *item = &data[i];
    int **item_ref = (int **) rbt_probe(table, item);
    ut_assert(t, rbt_count(table) == data_size);         /* Table count has to stay constat at data_size. */
    ut_assert(t, *item_ref != NULL);
    ut_assert(t, *item_ref == &data[i]);
    ut_assert(t, **item_ref == i);
  }

  /* Finally the tree must be consistent. */
  ut_assert(t, verify_tree(table, data, data_size));

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
item_compare_f_t (ut_test_t *const t)
{
  /* Tests that the data set is composed by an array of ten integers: [0..9]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /*
   * Parameter passed to the table initialization.
   * The function compare_int_and_increment_param increment the parameter at each call.
   */
  unsigned long long int cnt = 0;
  const unsigned long long int expected_comparison_cnt = 27;

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int_and_increment_param, &cnt, NULL);
  ut_assert(t, table != NULL);

  /* Count has to be zero. */
  ut_assert(t, rbt_count(table) == 0);

  /* Inserts the [0..9] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    unsigned long long int tmp = cnt;
    int *item = &data[i];
    int **item_ref = (int **) rbt_probe(table, item);
    ut_assert(t, rbt_count(table) == i + 1);             /* Table count has to be equal to the number of inserted elements. */
    ut_assert(t, *item_ref != NULL);                     /* Item pointer has to be not null. */
    ut_assert(t, *item_ref == &data[i]);                 /* Item pointer has to reference the appropriate array element. */
    ut_assert(t, **item_ref == i);                       /* Item (**item_ref) has to be equal to the loop counter. */
    ut_assert(t, cnt >= tmp);
  }

  /* Frees the table. */
  rbt_destroy(table, NULL);

  /* Final comparison count check. */
  ut_assert(t, cnt == expected_comparison_cnt);
}

static void
item_destroy_f_t (ut_test_t *const t)
{
  /* Test data set is composed by an array of ten integers: [0..9]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /*
   * Parameter passed to the table initialization.
   * The function compare_int_and_increment_param increment the parameter at each call.
   */
  unsigned long long int cnt = 0;
  const unsigned long long int expected_destroy_cnt = data_size;

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, &cnt, NULL);
  ut_assert(t, table != NULL);

  /* Count has to be zero. */
  ut_assert(t, rbt_count(table) == 0);

  /* Inserts the [0..9] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    int *item = &data[i];
    int **item_ref = (int **) rbt_probe(table, item);
    ut_assert(t, rbt_count(table) == i + 1);             /* Table count has to be equal to the number of inserted elements. */
    ut_assert(t, *item_ref != NULL);                     /* Item pointer has to be not null. */
    ut_assert(t, *item_ref == &data[i]);                 /* Item pointer has to reference the appropriate array element. */
    ut_assert(t, **item_ref == i);                       /* Item (**item_ref) has to be equal to the loop counter. */
  }

  /* Frees the table. */
  rbt_destroy(table, on_item_destroy_increment_param);

  /* Final comparison count check. */
  ut_assert(t, cnt == expected_destroy_cnt);
}

static void
copy_t (ut_test_t *const t)
{
  /* Test data set is composed by an array of ten integers: [0..9]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /* Shared field passed as param to the rbt_item_copy_f function. */
  unsigned long long int copy_cnt = 0;

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, &copy_cnt, NULL);
  ut_assert(t, table != NULL);

  /* Inserts the [0..9] set of elements in the table. */
  for (size_t i = 0; i < data_size; i++) {
    int *item = &data[i];
    rbt_probe(table, item);
  }

  /* We must have data_size elements in the table. */
  ut_assert(t, rbt_count(table) == data_size);

  /* Copies table into a new tree. */
  rbt_item_copy_f *copy = on_item_copy_increment_param;
  rbt_item_destroy_f *destroy = NULL;
  mem_allocator_t *alloc = NULL;
  rbt_table_t *copied_table = rbt_copy(table, copy, destroy, alloc);

  /* Function on_item_copy_increment_param has to be called data_size times. */
  ut_assert(t, copy_cnt == data_size);

  /* Compares trees node by node. */
  ut_assert(t, compare_trees(table->root, copied_table->root, compare_int, NULL));

  /* Frees the table. */
  rbt_destroy(table, NULL);

  /* We must have data_size elements in the table. */
  ut_assert(t, rbt_count(copied_table) == data_size);

  /* The copied tree must be consistent, and complete. */
  ut_assert(t, verify_tree(copied_table, data, data_size));

  /* Frees the table. */
  rbt_destroy(copied_table, NULL);
}

static void
insert_replace_and_find_t (ut_test_t *const t)
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
  ut_assert(t, table != NULL);

  /* Inserts the data_a set of elements in the table. */
  for (size_t i = 0; i < data_size; i++) {
    struct element *e = &data_a[i];
    struct element *e_ref = (struct element *) rbt_insert(table, e);
    ut_assert(t, rbt_count(table) == i + 1);         /* Table count has to be equal to the number of inserted elements. */
    ut_assert(t, e_ref == NULL);                     /* Item pointer has to be null when insertion succeeds. */
  }

  /* We must have data_size elements in the table now. */
  ut_assert(t, rbt_count(table) == data_size);

  /* Inserts the data_b set of elements in the table. Nothing has to happen. */
  for (size_t i = 0; i < data_size; i++) {
    struct element *e = &data_b[i];
    struct element *e_ref = (struct element *) rbt_insert(table, e);
    ut_assert(t, rbt_count(table) == data_size);     /* Table count has to stay constant. */
    ut_assert(t, e_ref != NULL);                     /* Item pointer has to be not null. */
    ut_assert(t, e_ref == &data_a[i]);               /* Item pointer has to reference the appropriate array element. */
    ut_assert(t, e_ref->key == i);                   /* Item's key has to be equal to the loop counter. */
    ut_assert(t, e_ref->content == value_a);         /* Item's content has to be equal to value_a. */
  }

  /* We must have still data_size elements in the table. */
  ut_assert(t, rbt_count(table) == data_size);

  /* Replaces elements using data_b set. All elements has to be replaced. */
  for (size_t i = 0; i < data_size; i++) {
    struct element *e = &data_b[i];
    struct element *e_ref = (struct element *) rbt_replace(table, e);
    ut_assert(t, rbt_count(table) == data_size);     /* Table count has to stay constant. */
    ut_assert(t, e_ref != NULL);                     /* Item pointer has to be not null. */
    ut_assert(t, e_ref == &data_a[i]);               /* Item pointer has to reference the appropriate array element. */
    ut_assert(t, e_ref->key == i);                   /* Item's key has to be equal to the loop counter. */
    ut_assert(t, e_ref->content == value_a);         /* Item's content has to be equal to value_a. */
  }

  /* We must have still data_size elements in the table. */
  ut_assert(t, rbt_count(table) == data_size);

  /* Searches all the ten elements, they must be found, and the contant has to be equal to value_b. */
  for (size_t i = 0; i < data_size; i++) {
    struct element *e = (struct element *) rbt_find(table, &i);
    ut_assert(t, e != NULL);
    ut_assert(t, e == &data_b[i]);
    ut_assert(t, e->key == i);
    ut_assert(t, e->content == value_b);
  }

  /* Searches for a missing key, result has to be null. */
  int missing_key = -1;
  struct element *missing = rbt_find(table, &missing_key);
  ut_assert(t, missing == NULL);

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
delete_t (ut_test_t *const t)
{
  /* Test data set is composed by an array of 48 integers: [0..47]. */
  int data[] = {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  ut_assert(t, table != NULL);

  /* Count has to be zero. */
  ut_assert(t, rbt_count(table) == 0);

  /* Inserts the [0..47] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    int *item = &data[i];
    int **item_ref = (int **) rbt_probe(table, item);
    ut_assert(t, rbt_count(table) == i + 1);             /* Table count has to be equal to the number of inserted elements. */
    ut_assert(t, *item_ref != NULL);                     /* Item pointer has to be not null. */
    ut_assert(t, *item_ref == &data[i]);                 /* Item pointer has to reference the appropriate array element. */
    ut_assert(t, **item_ref == i);                       /* Item (**item_ref) has to be equal to the loop counter. */
  }

  /* Deletes all elements one by one. */
  for (size_t i = 0; i < data_size; i++) {
    int *e = &data[i];
    int *e_ref = (int *) rbt_delete(table, e);
    ut_assert(t, rbt_count(table) == data_size - (i + 1));         /* Table count has to decrease at each step. */
    ut_assert(t, *e_ref == *e);
    for (size_t j = 0; j <= i; j++) {
      int *e1 = (int *) rbt_find(table, &j);
      ut_assert(t, e1 == NULL);
    }
    for (size_t j = i + 1; j < data_size; j++) {
      int *e1 = (int *) rbt_find(table, &j);
      ut_assert(t, e1 != NULL);
      ut_assert(t, *e1 == j);
    }
    ut_assert(t, verify_tree(table, &data[i + 1], data_size - (i + 1)));
  }

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
volume_t (ut_test_t *const t)
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
    ut_assert(t, table != NULL);

    ut_assert(t, rbt_count(table) == 0);

    /* Inserts elements sequentially. */
    for (size_t i = 0; i < k; i++) {
      int *e = &data[i];
      int **e_ref = (int **) rbt_probe(table, e);
      ut_assert(t, rbt_count(table) == i + 1);         /* Table count has to be equal to the number of inserted elements. */
      ut_assert(t, *e_ref != NULL);                    /* Item pointer has to be not null. */
      ut_assert(t, *e_ref == &data[i]);                /* Item pointer has to reference the appropriate array element. */
      ut_assert(t, **e_ref == i);                      /* Item (**e_ref) has to be equal to the loop counter. */
    }

    /* We must have k elements in the table. */
    ut_assert(t, rbt_count(table) == k);

    /* Final table has to be complete and consistent. */
    ut_assert(t, verify_tree(table, data, k));

    /* Searches for each element and checks it exists in the table. */
    for (size_t i = 0; i < k; i++) {
      int *e = (int *) rbt_find(table, &i);
      ut_assert(t, e != NULL);
      ut_assert(t, *e == i);
    }

    /* Deletes odd element and checks it exists in the table. */
    for (size_t i = 0; i < (k + 1) / 2; i++) {
      int i2 = i * 2;
      int *e = (int *) rbt_delete(table, &i2);
      ut_assert(t, e != NULL);
      ut_assert(t, *e == i2);
    }

    /* Final count has to be half of the number of inserted elements. */
    ut_assert(t, rbt_count(table) == (size_t)(k / 2));

    rbt_destroy(table, NULL);

    alloc->free(alloc, data);
  }

}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_init(&argc, &argv);

  ut_suite_t *const s = ut_suite_new("red_black_tree");

  ut_suite_add_simple_test(s, "creation_and_destruction", creation_and_destruction_t);
  ut_suite_add_simple_test(s, "probe", probe_t);
  ut_suite_add_simple_test(s, "item_compare_f", item_compare_f_t);
  ut_suite_add_simple_test(s, "item_destroy_f", item_destroy_f_t);
  ut_suite_add_simple_test(s, "copy", copy_t);
  ut_suite_add_simple_test(s, "insert_replace_and_find", insert_replace_and_find_t);
  ut_suite_add_simple_test(s, "delete", delete_t);
  ut_suite_add_simple_test(s, "volume", volume_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
