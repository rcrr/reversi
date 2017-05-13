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



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_init(&argc, &argv);

  ut_suite_t *const s = ut_suite_new("red_black_tree");

  //ut_suite_add_simple_test(s, "creation_and_destruction", creation_and_destruction_t);
  ut_suite_add_simple_test(s, "creation_and_destruction", creation_and_destruction_t);
  ut_suite_add_simple_test(s, "probe", probe_t);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
