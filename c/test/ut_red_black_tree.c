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
 * @copyright 2015, 2017 Roberto Corradini. All rights reserved.
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
#include <time.h>
#include <unistd.h>
#include <assert.h>
#include <string.h>

#include "unit_test.h"

#include "red_black_tree.h"
#include "prng.h"
#include "sort_utils.h"



/* Time spec type definition. See sys/time.h for more details. */
typedef struct timespec timespec_t;



/*
 * Static variables.
 */

/* Directory used to save the performance log file. */
static char *output_perf_log_dir = NULL;



/*
 * Auxiliary functions.
 */

/*
 * it is used by the main function.
 */
static void
local_parse_args (int *argc_p,
                  char ***argv_p)
{
  int argc = *argc_p;
  char **argv = *argv_p;

  /* Parses known args. */
  for (int i = 1; i < argc; i++) {
    if (strcmp("--output-perf-log-dir", argv[i]) == 0) {
      if (i + 1 < argc) {
        argv[i++] = NULL;
        output_perf_log_dir = argv[i];
      } else {
        fprintf(stderr, "%s: missing PATH value after --output-perf-log-dir flag.\n", argv[0]);
        exit(EXIT_FAILURE);
      }
      argv[i] = NULL;
    }
  }
}

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
 * Compare function for pointers to integer.
 * It is used for sorting the array that compares to the tree's traverser.
 */
static int
sort_utils_element_cmp (const void *const a,
                        const void *const b)
{
  const int **const x = (const int **const) a;
  const int **const y = (const int **const) b;
  return (**x > **y) - (**x < **y);
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
 * Allocates and returns an array of shuffled integers.
 */
static int *
prepare_data_array (const size_t len,
                    const int seed)
{
  int *a = (int *) malloc(len * sizeof(int));
  assert(a);

  for (size_t i = 0; i < len; i++) {
    a[i] = i;
  }

  prng_mt19937_t *prng = prng_mt19937_new();
  assert(prng);
  prng_mt19937_init_by_seed(prng, seed);
  prng_mt19937_shuffle_array_int(prng, a, len);
  prng_mt19937_free(prng);

  return a;
}

/*
 * The struct timespec structure represents an elapsed time. It is declared in time.h and has the following members:
 *
 * - time_t     tv_sec    This represents the number of whole seconds of elapsed time.
 * - long int   tv_nsec   This is the rest of the elapsed time (a fraction of a second),
 *                        represented as the number of nanoseconds. It is always less than one billion.
 *
 * The structure is also defined as:
 *
 *    typedef struct timespec timespec_t;
 *
 *
 * A way of obtaining the timespec structure value is:
 *
 *    timespec_t time_0;
 *    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);
 *
 *
 * The function timespec_diff works as follow:
 *
 *    Subtracts the timespec_t values `start` and `end`,
 *    storing the result in `result`.
 *    Return 1 if the difference is negative, otherwise 0.
 */
static int
timespec_diff (timespec_t *const result,
               const timespec_t *const start,
               const timespec_t *const end)
{
  assert(result);
  assert(start);
  assert(end);
  if ((end->tv_sec - start->tv_sec) < 0) return 1;
  if ((end->tv_sec - start->tv_sec) == 0 &&
      (end->tv_nsec - start->tv_nsec) < 0) return 1;

  if ((end->tv_nsec - start->tv_nsec) < 0) {
    result->tv_sec = end->tv_sec - start->tv_sec - 1;
    result->tv_nsec = 1000000000 + end->tv_nsec - start->tv_nsec;
  } else {
    result->tv_sec = end->tv_sec - start->tv_sec;
    result->tv_nsec = end->tv_nsec - start->tv_nsec;
  }
  return 0;
}



/*
 * Test functions.
 */



/*
 * Test functions for the table structure.
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

static void
random_key_volume_t (ut_test_t *const t)
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
  prng_mt19937_t *prng = prng_mt19937_new();
  ut_assert(t, prng != NULL);
  prng_mt19937_init_by_seed(prng, seed);

  /* Creates the table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  ut_assert(t, table != NULL);

  /* Populates the table with random selection within the set. Element's val field is incremented on each probe. */
  for (size_t k = 0; k < insertion_count; k++) {
    const unsigned long index = prng_mt19937_random_choice_from_finite_set(prng, data_size);
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
    if (e) ut_assert(t, count > 0);
    else ut_assert(t, count == 0);
  }
  ut_assert(t, insertion_count_checksum == insertion_count);
  ut_assert(t, element_count_checksum == rbt_count(table));
  ut_assert(t, element_count_checksum == 1767);      /* Depends on PRNG, seed, data_size, and insertion_count. */

  /* Frees resources. */
  rbt_destroy(table, NULL);
  prng_mt19937_free(prng);
}



/*
 * Test functions for the traverser structure.
 */

static void
traverser_basic_t (ut_test_t *const t)

{
  rbt_traverser_t traverser;
  rbt_traverser_t *tr = &traverser;
  int *e;
  int counter;

  /* Test data set is composed by an array of ten integers: [0..9]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  ut_assert(t, table != NULL);

  /* Inserts the [0..9] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    rbt_probe(table, &data[i]);
  }

  /* Traverses the table clockwise starting from the null element. */
  counter = 0;
  rbt_t_init(tr, table);
  while ((e = rbt_t_next(tr))) {
    ut_assert(t, *e == counter++);
    ut_assert(t, rbt_t_cur(tr) == e);
  }
  ut_assert(t, counter == data_size);
  ut_assert(t, rbt_t_cur(tr) == NULL);

  /* Traverses the table counterclockwise starting from the null element. */
  counter = data_size;
  rbt_t_init(tr, table);
  while ((e = rbt_t_prev(tr))) {
    ut_assert(t, *e == --counter);
    ut_assert(t, rbt_t_cur(tr) == e);
  }
  ut_assert(t, counter == 0);
  ut_assert(t, rbt_t_cur(tr) == NULL);

  /* Traverses the table clockwise starting from the first element. */
  for (e = rbt_t_first(tr, table), counter = 0; e; e = rbt_t_next(tr), counter++) {
    ut_assert(t, *e == counter);
    ut_assert(t, rbt_t_cur(tr) == e);
  }
  ut_assert(t, counter == data_size);
  ut_assert(t, rbt_t_cur(tr) == NULL);

  /* Traverses the table counterclockwise starting from the last element. */
  for (e = rbt_t_last(tr, table), counter = data_size - 1; e; e = rbt_t_prev(tr), counter--) {
    ut_assert(t, *e == counter);
    ut_assert(t, rbt_t_cur(tr) == e);
  }
  ut_assert(t, counter == -1);
  ut_assert(t, rbt_t_cur(tr) == NULL);

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
traverser_find_and_copy_t (ut_test_t *const t)

{
  rbt_traverser_t traverser_1, traverser_2;
  rbt_traverser_t *tr1 = &traverser_1;
  rbt_traverser_t *tr2 = &traverser_2;
  int *e;

  /* Test data set is composed by an array of ten integers: [0..16]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  ut_assert(t, table != NULL);

  /* Inserts the [0..9] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    rbt_probe(table, &data[i]);
  }

  for (int key = 0; key < data_size; key ++) {
    e = (int *) rbt_t_find(tr1, table, &key);
    ut_assert(t, *e == key);

    e = rbt_t_copy(tr2, tr1);
    ut_assert(t, *e == key);

    int *next = (int *) rbt_t_next(tr1);
    if (key == data_size - 1) ut_assert(t, next == NULL);
    else ut_assert(t, *next == key + 1);

    int *prev = (int *) rbt_t_prev(tr2);
    if (key == 0) ut_assert(t, prev == NULL);
    else ut_assert(t, *prev == key - 1);
  }

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
traverser_insert_t (ut_test_t *const t)
{
  rbt_traverser_t traverser;
  rbt_traverser_t *tr = &traverser;
  int *e;

  /* Test data set is composed by an array of ten integers: [0..16]. */
  int data[] = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16 };
  const size_t data_size = sizeof(data) / sizeof(data[0]);
  const size_t missing_element = 13;
  int *prev = &data[12];

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  ut_assert(t, table != NULL);

  /* Inserts the [0..9] set of elements in the table in sequential order. */
  for (size_t i = 0; i < data_size; i++) {
    if (i != 13) rbt_probe(table, &data[i]);
  }

  /* Table must have one element missing. */
  ut_assert(t, rbt_count(table) == data_size - 1);

  /* Searches for the 12th element and verifies that the next is the 14th. */
  e = (int *) rbt_t_find(tr, table, prev);
  ut_assert(t, e && *e == 12);
  e = rbt_t_next(tr);
  ut_assert(t, e && *e == 14);

  /* Inserts the missing elements and verifies it. */
  e = (int *) rbt_t_insert(tr, table, &data[missing_element]);
  ut_assert(t, e != NULL);
  ut_assert(t, *e == 13);
  ut_assert(t, rbt_count(table) == data_size);

  /* Searches for the 12th element and verifies that the next is the 13th. */
  e = (int *) rbt_t_find(tr, table, prev);
  ut_assert(t, e && *e == 12);
  e = rbt_t_next(tr);
  ut_assert(t, e && *e == 13);

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
traverser_replace_t (ut_test_t *const t)
{
  rbt_traverser_t traverser;
  rbt_traverser_t *tr = &traverser;

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
    rbt_insert(table, e);
  }

  size_t index = 3;
  struct element *e = &data_a[index];
  struct element *r = &data_b[index];
  ut_assert(t, e->key == r->key);

  e = (struct element *) rbt_t_find(tr, table, e);
  ut_assert(t, e && e->key == index && e->content == value_a);

  rbt_t_replace(tr, r);
  e = (struct element *) rbt_t_next(tr);
  ut_assert(t, e && e->key == index + 1 && e->content == value_a);
  e = (struct element *) rbt_t_prev(tr);
  ut_assert(t, e && e->key == index && e->content == value_b);

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
traverser_on_changing_table_t (ut_test_t *const t)
{
  rbt_traverser_t traverser;
  rbt_traverser_t *tr = &traverser;
  int *e;

  /* Test data set is composed by an array of 32 integers: [0..31]. */
  int data[] = {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
                 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31 };

  /* Set 0. */
  size_t keys_0[] = { 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30 };
  const size_t keys_0_size = sizeof(keys_0) / sizeof(keys_0[0]);

  /* Set 1. */
  size_t keys_1[] = { 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30 };
  const size_t keys_1_size = sizeof(keys_1) / sizeof(keys_1[0]);

  /* Set 2. */
  size_t keys_2[] = { 20, 21, 22, 23, 24, 25, 26, 27 };
  const size_t keys_2_size = sizeof(keys_2) / sizeof(keys_2[0]);

  /* Creates the new empty table. */
  rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
  ut_assert(t, table != NULL);

  /* Inserts the data set of elements identified by keys_0 in the table. */
  for (size_t i = 0; i < keys_0_size; i++) {
    e = &data[keys_0[i]];
    int **e_ptr = (int **) rbt_probe(table, e);
    ut_assert(t, e_ptr != NULL);
  }
  ut_assert(t, rbt_count(table) == keys_0_size);

  /* The last element has to be 30. */
  e = (int *) rbt_t_last(tr, table);
  ut_assert(t, e && *e == 30);

  /* Inserts the data set of elements identified by keys_1 in the table. */
  for (size_t i = 0; i < keys_1_size; i++) {
    e = &data[keys_1[i]];
    int **e_ptr = (int **) rbt_probe(table, e);
    ut_assert(t, e_ptr != NULL);
  }
  ut_assert(t, rbt_count(table) == 21);

  /* Going back from element 30 we shoud find element 28. */
  e = (int *) rbt_t_prev(tr);
  ut_assert(t, e && *e == 28);

  /* Removes the data set of elements identified by keys_2 from the table. */
  for (size_t i = 0; i < keys_2_size; i++) {
    e = &data[keys_2[i]];
    rbt_delete(table, e);
  }
  ut_assert(t, rbt_count(table) == 15);

  /* Going back from element 28 we shoud find element 18. */
  e = (int *) rbt_t_prev(tr);
  ut_assert(t, e && *e == 18);

  /* Frees the table. */
  rbt_destroy(table, NULL);
}

static void
performance_t (ut_test_t *const t)
{
  const size_t initial_len = 1000;
  const size_t step_len = 4000;
  const size_t steps = 100;
  const size_t delta = 10;
  const size_t repeats = 13;
  assert(initial_len > delta * (repeats + 1));

  const int initial_seed = 1898;
  const int seed_increment = 37;
  const char *const out_perf_log_file_name = "rbt_performance_a_log.csv";

  int *data;
  timespec_t time_0, time_1, time_diff;
  FILE *fp;
  char *op_type;
  size_t op_initial_count, op_final_count;
  char ltime_to_s[64];
  time_t ltime;
  int ret;

  /* Opens the log file. */
  char fname[512];
  int access_check;
  if (output_perf_log_dir) {
    access_check = access(output_perf_log_dir, W_OK);
    snprintf(fname, sizeof(fname), "%s/%s", output_perf_log_dir, out_perf_log_file_name);
  } else {
    access_check = access(".", W_OK);
    snprintf(fname, sizeof(fname), "%s", out_perf_log_file_name);
  }
  if (access_check != 0) {
    fprintf(stderr, "Directory doesn't exist, or access denied for file: %s\n", fname);
    exit(1);
  }
  fp = fopen(fname, "w");
  ut_assert(t, fp != NULL);
  fprintf(fp, "%s;%s;%s;%s;%s;%s;%s\n",
          "LTIME",
          "OP_TYPE",
          "OP_SIZE",
          "OP_INITIAL_COUNT",
          "OP_FINAL_COUNT",
          "CPUTIME_SEC",
          "CPUTIME_NSEC");

  for (size_t k = 0; k < steps; k++) {
    size_t len = initial_len + k * step_len;
    size_t tlen = len + (repeats * delta);
    int seed = initial_seed + k * seed_increment;

    /* Prepares the array of integers [0..tlen], then shuffled. */
    data = prepare_data_array(tlen, seed);

    /* Creates the new empty table. */
    rbt_table_t *table = rbt_create(compare_int, NULL, NULL);
    ut_assert(t, table != NULL);

    /* Operation 1: populate the table. */
    op_type = "rnd_populate";
    op_initial_count = rbt_count(table);

    /* Takes initial time for operation. */
    ltime = time(NULL);
    strftime(ltime_to_s, sizeof(ltime_to_s), "%Y%m%d-%H:%M:%S-%Z", localtime(&ltime));

    /* Starts the stop-watch. */
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

    /* Inserts the data set of elements in the table. */
    for (size_t i = 0; i < len; i++) {
      rbt_probe(table, &data[i]);
    }

    /* Stops the stop-watch. */
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

    /* Verifies that the size of the table is equal to the count of inserted elements. */
    op_final_count = rbt_count(table);
    ut_assert(t, op_final_count == len);

    /* Computes the time taken. */
    ret = timespec_diff(&time_diff, &time_0, &time_1);
    ut_assert(t, ret == 0);

    fprintf(fp, "%s;%s;%zu;%zu;%zu;%ld;%ld\n",
            ltime_to_s,
            op_type,
            len,
            op_initial_count,
            op_final_count,
            time_diff.tv_sec,
            time_diff.tv_nsec);

    /* End of operation 1. */


    /* Repeats insert and delete sequences a number of times equal to repeats constant. */
    for (size_t j = 0; j < repeats; j++) {

      /* Operation 2: inserts delta new elements in table. */
      op_type = "rnd_insert_new_elm";
      op_initial_count = rbt_count(table);

      /* Takes initial time for operation. */
      ltime = time(NULL);
      strftime(ltime_to_s, 64, "%Y%m%d-%H:%M:%S-%Z", localtime(&ltime));

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Inserts the data set of elements in the table. */
      for (size_t i = 0; i < delta; i++) {
        rbt_probe(table, &data[len + delta * j + i]);
      }

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

      /* Verifies that the size of the table is equal to the count of inserted elements. */
      op_final_count = rbt_count(table);
      ut_assert(t, op_final_count == len + delta);

      /* Computes the time taken. */
      ret = timespec_diff(&time_diff, &time_0, &time_1);
      ut_assert(t, ret == 0);

      fprintf(fp, "%s;%s;%zu;%zu;%zu;%ld;%ld\n",
              ltime_to_s,
              op_type,
              delta,
              op_initial_count,
              op_final_count,
              time_diff.tv_sec,
              time_diff.tv_nsec);

      /* End of operation 2. */


      /* Operation 3: deletes delta elements from table. */
      op_type = "rnd_remove_existing_elm";
      op_initial_count = rbt_count(table);

      /* Takes initial time for operation. */
      ltime = time(NULL);
      strftime(ltime_to_s, 64, "%Y%m%d-%H:%M:%S-%Z", localtime(&ltime));

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Deletes the data set of elements from the table. */
      for (size_t i = 0; i < delta; i++) {
        rbt_delete(table, &data[delta * j + i]);
      }

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

      /* Verifies that the size of the table is equal to the count of inserted elements. */
      op_final_count = rbt_count(table);
      ut_assert(t, op_final_count == len);

      /* Computes the time taken. */
      ret = timespec_diff(&time_diff, &time_0, &time_1);
      ut_assert(t, ret == 0);

      fprintf(fp, "%s;%s;%zu;%zu;%zu;%ld;%ld\n",
              ltime_to_s,
              op_type,
              delta,
              op_initial_count,
              op_final_count,
              time_diff.tv_sec,
              time_diff.tv_nsec);

      /* End of operation 3. */


      /* Operation 4a: searches for existing elements. */
      op_type = "find_existing_a_elm";
      op_initial_count = rbt_count(table);

      /* Takes initial time for operation. */
      ltime = time(NULL);
      strftime(ltime_to_s, 64, "%Y%m%d-%H:%M:%S-%Z", localtime(&ltime));

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Searches for the first group of elements. */
      for (size_t i = 0; i < delta; i++) {
        int *e = (int *) rbt_find(table, &data[delta * (j + 1) + i]);
        ut_assert(t, e != NULL);
      }

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

      /* Verifies that the size of the table is equal to the count of inserted elements. */
      op_final_count = rbt_count(table);
      ut_assert(t, op_final_count == len);

      /* Computes the time taken. */
      ret = timespec_diff(&time_diff, &time_0, &time_1);
      ut_assert(t, ret == 0);

      fprintf(fp, "%s;%s;%zu;%zu;%zu;%ld;%ld\n",
              ltime_to_s,
              op_type,
              delta,
              op_initial_count,
              op_final_count,
              time_diff.tv_sec,
              time_diff.tv_nsec);

      /* End of operation 4a. */


      /* Operation 4z: searches for existing elements. */
      op_type = "find_existing_z_elm";
      op_initial_count = rbt_count(table);

      /* Takes initial time for operation. */
      ltime = time(NULL);
      strftime(ltime_to_s, 64, "%Y%m%d-%H:%M:%S-%Z", localtime(&ltime));

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Searches for the first group of elements. */
      for (size_t i = 0; i < delta; i++) {
        int *e = (int *) rbt_find(table, &data[len - delta * j + i]);
        ut_assert(t, e != NULL);
      }

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

      /* Verifies that the size of the table is equal to the count of inserted elements. */
      op_final_count = rbt_count(table);
      ut_assert(t, op_final_count == len);

      /* Computes the time taken. */
      ret = timespec_diff(&time_diff, &time_0, &time_1);
      ut_assert(t, ret == 0);

      fprintf(fp, "%s;%s;%zu;%zu;%zu;%ld;%ld\n",
              ltime_to_s,
              op_type,
              delta,
              op_initial_count,
              op_final_count,
              time_diff.tv_sec,
              time_diff.tv_nsec);

      /* End of operation 4z. */


      /* Operation 5: searches for missing elements. */
      op_type = "find_missing_elm";
      op_initial_count = rbt_count(table);

      /* Takes initial time for operation. */
      ltime = time(NULL);
      strftime(ltime_to_s, 64, "%Y%m%d-%H:%M:%S-%Z", localtime(&ltime));

      /* Starts the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

      /* Searches for the elements just deleted. */
      for (size_t i = 0; i < delta; i++) {
        int *e = (int *) rbt_find(table, &data[delta * j + i]);
        ut_assert(t, e == NULL);
      }

      /* Stops the stop-watch. */
      clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

      /* Verifies that the size of the table is equal to the count of inserted elements. */
      op_final_count = rbt_count(table);
      ut_assert(t, op_final_count == len);

      /* Computes the time taken. */
      ret = timespec_diff(&time_diff, &time_0, &time_1);
      ut_assert(t, ret == 0);

      fprintf(fp, "%s;%s;%zu;%zu;%zu;%ld;%ld\n",
              ltime_to_s,
              op_type,
              delta,
              op_initial_count,
              op_final_count,
              time_diff.tv_sec,
              time_diff.tv_nsec);

      /* End of operation 5. */


    } /* End of repeat cicles. */


    /*
     * Operation 6:
     * - Prepares an index of keys pointing to the portion of data contained in table.
     * - Sorts the keys.
     * - Verifies that the traverser provides the same sequence of pointers prepared in
     *   the sorted index.
     */
    op_type = "traverse_table";
    op_initial_count = rbt_count(table);

    int **index = (int **) malloc(len * sizeof(int *));
    ut_assert(t, index != NULL);
    for (size_t j = 0; j < len; j++) {
      index[j] = &data[delta * repeats + j];
    }
    sort_utils_heapsort(index, len, sizeof(int *), sort_utils_element_cmp);
    rbt_traverser_t trav;

    /* Takes initial time for operation. */
    ltime = time(NULL);
    strftime(ltime_to_s, 64, "%Y%m%d-%H:%M:%S-%Z", localtime(&ltime));

    /* Starts the stop-watch. */
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

    /* Traverses the table. */
    int *e = (int *) rbt_t_first(&trav, table);
    size_t j = 0;
    while (e) {
      ut_assert(t, *e == *index[j]);
      e = (int *) rbt_t_next(&trav);
      j++;
    }

    /* Stops the stop-watch. */
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);

    /* Verifies that the size of the table is equal to the count of inserted elements. */
    op_final_count = rbt_count(table);
    ut_assert(t, op_final_count == len);

    /* Computes the time taken. */
    ret = timespec_diff(&time_diff, &time_0, &time_1);
    ut_assert(t, ret == 0);

    fprintf(fp, "%s;%s;%zu;%zu;%zu;%ld;%ld\n",
            ltime_to_s,
            op_type,
            len,
            op_initial_count,
            op_final_count,
            time_diff.tv_sec,
            time_diff.tv_nsec);

    free(index);

    /* End of operation 6. */


    /* Frees the table. */
    rbt_destroy(table, NULL);

    /* Frees the data array. */
    free(data);

  } /* End of steps loop. */

  /* Closes the log file. */
  int fclose_ret = fclose(fp);
  ut_assert(t, fclose_ret == 0);
}



/*
 * Test functions for advanced memory managers applyed to the table structure.
 */

static void
creation_and_destruction_mem_dbg_t (ut_test_t *const t)
{
  int verbosity = 0;
  int arg[2] = {0, 0};

  mem_dbg_allocator_t *mt = mem_dbg_allocator_new(MEM_DBG_TRACK, arg, verbosity);
  mem_allocator_t * alloc = mem_dbg_allocator(mt);

  rbt_table_t *table = rbt_create(compare_int, NULL, alloc);
  ut_assert(t, table != NULL);

  size_t count = rbt_count(table);
  ut_assert(t, count == 0);

  ut_assert(t, verify_tree(table, NULL, 0));

  rbt_destroy(table, NULL);

  mem_dbg_allocator_free(mt);
}

static void
probe_mem_dbg_t (ut_test_t *const t)
{
  /* Test data set is composed by an array of 64 integers: [0..63]. */
  int data[] = { 0,  1,  2,  3,  4,  5,  6,  7,  8,  9,  10, 11, 12, 13, 14, 15,
                 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
                 63, 62, 61, 60, 59, 58, 57, 56, 55, 54, 53, 52, 51, 50, 49, 48,
                 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32};
  const size_t data_size = sizeof(data) / sizeof(data[0]);

  struct mt_type {
    char *desc;
    mem_dbg_policy_t policy;
    int args[2];
    int verbosity;
  };

  struct mt_type mtts[] = {
    { "Untracked memory allocation", MEM_DBG_NO_TRACK,     {0, 0}, 0 },
    { "Tracked memory allocation",   MEM_DBG_TRACK,        {0, 0}, 0 },
    { "Block memory allocation",     MEM_DBG_SUBALLOC, {1024, 64}, 0 }
  };
  const size_t mts_size = sizeof(mtts) / sizeof(mtts[0]);

  for (size_t j = 0; j < mts_size; j++) {
    struct mt_type *mttp = &mtts[j];

    mem_dbg_allocator_t *mt = mem_dbg_allocator_new(mttp->policy, mttp->args, mttp->verbosity);
    mem_allocator_t * alloc = mem_dbg_allocator(mt);

    /* Creates the new empty table. */
    rbt_table_t *table = rbt_create(compare_int, NULL, alloc);
    ut_assert(t, table != NULL);

    /* Count has to be zero. */
    ut_assert(t, rbt_count(table) == 0);

    /* Inserts the [0..63] set of elements in the table in sequential order. */
    for (size_t i = 0; i < data_size; i++) {
      int *item = &data[i];
      int **item_ref = (int **) rbt_probe(table, item);
      ut_assert(t, rbt_count(table) == i + 1);             /* Table count has to be equal to the number of inserted elements. */
      ut_assert(t, *item_ref != NULL);                     /* Item pointer has to be not null. */
      ut_assert(t, *item_ref == &data[i]);                 /* Item pointer has to reference the appropriate array element. */
      ut_assert(t, verify_tree(table, data, i + 1));       /* Runs the verify_tree procedure on the growing table. */
    }

    /* Finally the tree must be consistent. */
    ut_assert(t, verify_tree(table, data, data_size));

    /* Frees the table. */
    rbt_destroy(table, NULL);

    /* Frees the memory tracker allocator. */
    mem_dbg_allocator_free(mt);

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

  local_parse_args(&argc, &argv);

  ut_suite_t *const s = ut_suite_new("red_black_tree");

  ut_suite_add_simple_test(s, "creation_and_destruction", creation_and_destruction_t);
  ut_suite_add_simple_test(s, "probe", probe_t);
  ut_suite_add_simple_test(s, "item_compare_f", item_compare_f_t);
  ut_suite_add_simple_test(s, "item_destroy_f", item_destroy_f_t);
  ut_suite_add_simple_test(s, "copy", copy_t);
  ut_suite_add_simple_test(s, "insert_replace_and_find", insert_replace_and_find_t);
  ut_suite_add_simple_test(s, "delete", delete_t);
  ut_suite_add_simple_test(s, "volume", volume_t);
  ut_suite_add_simple_test(s, "random_key_volume", random_key_volume_t);

  ut_suite_add_simple_test(s, "traverser_basic", traverser_basic_t);
  ut_suite_add_simple_test(s, "traverser_find_and_copy", traverser_find_and_copy_t);
  ut_suite_add_simple_test(s, "traverser_insert", traverser_insert_t);
  ut_suite_add_simple_test(s, "traverser_replace", traverser_replace_t);
  ut_suite_add_simple_test(s, "traverser_on_changing_table", traverser_on_changing_table_t);

  ut_suite_add_simple_test(s, "creation_and_destruction_mem_dbg", creation_and_destruction_mem_dbg_t);
  ut_suite_add_simple_test(s, "probe_mem_dbg", probe_mem_dbg_t);

  if (ut_is_mode_equal_to_perf()) {
    ut_suite_add_simple_test(s, "performance", performance_t);
  }

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
