/**
 * @file
 *
 *
 *
 * @todo [done] Complete the doxygen documentation for functions.
 *
 * @todo [done] Remove not mandatory definitions from the header file (e.g. rbt_node_t)
 *
 * @todo [done] Move module details to the header file.
 *
 * @todo Write some documentation into the code (for insert, delete, ... Case1, Case2, ....)
 *
 * @todo Complete all the tests and remove the file src/red_black_tree_tests.c
 *
 * @todo [done] Write performance tests.
 *
 * @todo Write a merge function.
 *       This is not as silly as it could be thought.
 *       Two functions are needed: tree_to_vine, and vine_to_tree .... A vine is a linked-list build
 *       using tree nodes, connected using always the same link (right or left subject to convention).
 *       Merging two trees is done by transforming T1, and T2 to vines, merging them, and trasforming
 *       back the result to the final tree.
 *       The functions vine_to_tree and tree_to_vine can be coded O(n) in time and using O(1) space.
 *       Merging the two vines (linked-list) is again O(n) in time, and O(1) in space. So combining
 *       everything, the Union Set operation can be coded O(n) in time and O(1) in space requirements.
 *       Having all these tools also "bulk loading" an array of elements can be coded O(n) in time when
 *       it is already sorted, and O(n*log(n)) if not sorted.
 *       This procedure can also transform the tree n "optimal" shape for searching.
 *
 * @todo Add statistics.
 *
 *
 *
 *
 * @brief Red black tree module implementation.
 *
 * @par red_black_tree.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @author Ben Pfaff mailto:blp@gnu.org
 * @copyright 2015 Roberto Corradini. All rights reserved.
 * @copyright 1998-2002, 2004 Free Software Foundation, Inc.
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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "red_black_tree.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
copy_error_recovery (rbt_node_t **stack,
                     int height,
                     rbt_table_t *new,
                     rbt_item_destroy_f *destroy);

static void
trav_refresh (rbt_traverser_t *trav);

/**
 * @endcond
 */



/*****************************************************/
/*                                                   */
/* Function implementations for the table structure. */
/*                                                   */
/*****************************************************/

/**
 * @brief Creates and returns a new, empty table.
 *
 * @details The table is associated with the given arguments.
 *          The `param` argument is passed as the third argument to the comparison function
 *          when it is called. If the allocator is a null pointer, then #mem_allocator_default is used.
 *
 * Returns `NULL` if memory allocation failed.
 *
 * @invariant The `compare` function pointer cannot be `NULL`.
 *
 * @param [in]     compare the comparison function for data table's items
 * @param [in,out] param   passed to the comparison function
 * @param [in,out] alloc   the memory allocation object to use
 * @return                 a newly created table
 */
rbt_table_t *
rbt_create (rbt_item_compare_f *compare,
            void *param,
            mem_allocator_t *alloc)
{
  rbt_table_t *tree;

  assert(compare != NULL);

  if (alloc == NULL) alloc = &mem_allocator_default;

  tree = alloc->malloc(alloc, sizeof *tree);
  if (tree == NULL) return NULL;

  tree->root = NULL;
  tree->compare = compare;
  tree->param = param;
  tree->alloc = alloc;
  tree->count = 0;
  tree->generation = 0;

  return tree;
}

/**
 * @brief Destroys a table.
 *
 * @details Frees storage allocated for `table`. If `destroy != NULL`, applies it
 *          to each data item in inorder.
 *          During destruction, the `destroy` function provided, if non-null, is called
 *          once for every item in the table, in no particular order. The function,
 *          if provided, must not invoke any table function or macro on the table
 *          being destroyed.
 *
 * @invariant The `table` argument cannot be `NULL`.
 *
 * @param [in] table   the comparison function for data table's items
 * @param [in] destroy passed to the comparison function
 */
void
rbt_destroy (rbt_table_t *table,
             rbt_item_destroy_f *destroy)
{
  rbt_node_t *p, *q;

  assert(table != NULL);

  for (p = table->root; p != NULL; p = q)
    if (p->links[0] == NULL) {
      q = p->links[1];
      if (destroy != NULL && p->data != NULL)
        destroy(p->data, table->param);
      table->alloc->free(table->alloc, p);
    } else {
      q = p->links[0];
      p->links[0] = q->links[1];
      q->links[1] = p;
    }

  table->alloc->free(table->alloc, table);
}

/**
 * @brief Copies `org` to a newly created table, which is returned.
 *
 * @details Creates and returns a new table with the same contents as the existing
 *          table passed as its first argument.
 *          Its other three arguments may all be null pointers.
 *
 * If a `rbt_item_copy_f` is provided, then it is used to make a copy of each table item as it is
 *    inserted into the new table, in no particular order (a deep copy). Otherwise, the `void ∗`
 *    table items are copied verbatim (a shallow copy).
 *
 * If the table copy fails, either due to memory allocation failure or a null pointer returned
 *    by the `rbt_item_copy_f` function. In this case, any provided `rbt_item_destroy_f` function
 *    is called once for each new item already copied, in no particular order.
 *
 * By default, the new table uses the same memory allocator as the existing one. If non-null,
 *    the `mem_allocator_t ∗` given is used instead as the new memory allocator.
 *    To use the `mem_allocator_default` allocator, specify `&mem_allocator_default` explicitly.
 *
 * @invariant The `org` argument cannot be `NULL`.
 *
 * @param [in] org     the original table
 * @param [in] copy    function applied for a deep copy
 * @param [in] destroy function applied in case of failure
 * @param [in] alloc   memory allocator
 * @return             a new table copied from `org`
 */
rbt_table_t *
rbt_copy (const rbt_table_t *org,
          rbt_item_copy_f *copy,
          rbt_item_destroy_f *destroy,
          mem_allocator_t *alloc)
{
  rbt_node_t *stack[2 * (RBT_MAX_HEIGHT + 1)];
  int height = 0;

  rbt_table_t *new;
  const rbt_node_t *x;
  rbt_node_t *y;

  assert (org != NULL);
  new = rbt_create(org->compare, org->param,
                   alloc != NULL ? alloc : org->alloc);
  if (new == NULL) return NULL;

  new->count = org->count;
  if (new->count == 0) return new;

  x = (const rbt_node_t *) &org->root;
  y = (rbt_node_t *) &new->root;
  for (;;) {
    while (x->links[0] != NULL) {
      assert(height < 2 * (RBT_MAX_HEIGHT + 1));

      y->links[0] = new->alloc->malloc(new->alloc, sizeof *y->links[0]);
      if (y->links[0] == NULL) {
        if (y != (rbt_node_t *) &new->root) {
          y->data = NULL;
          y->links[1] = NULL;
        }
        copy_error_recovery(stack, height, new, destroy);
        return NULL;
      }

      stack[height++] = (rbt_node_t *) x;
      stack[height++] = y;
      x = x->links[0];
      y = y->links[0];
    }
    y->links[0] = NULL;

    for (;;) {
      y->color = x->color;
      if (copy == NULL)
        y->data = x->data;
      else {
        y->data = copy(x->data, org->param);
        if (y->data == NULL) {
          y->links[1] = NULL;
          copy_error_recovery(stack, height, new, destroy);
          return NULL;
        }
      }

      if (x->links[1] != NULL) {
        y->links[1] = new->alloc->malloc(new->alloc, sizeof *y->links[1]);
        if (y->links[1] == NULL) {
          copy_error_recovery(stack, height, new, destroy);
          return NULL;
        }

        x = x->links[1];
        y = y->links[1];
        break;
      } else
        y->links[1] = NULL;

      if (height <= 2) return new;

      y = stack[--height];
      x = stack[--height];
    }
  }
}

/**
 * @brief Inserts `item` into `table` and returns a pointer to `item`'s address.
 *
 * @details Searches in `table` for an element matching `item`.
 *          If a duplicate item is found in the table,
 *          returns a pointer to the duplicate without inserting `item`.
 *
 * Returns `NULL` in case of memory allocation failure.
 *
 * Occasionally it is convenient to insert one item into a table, then immediately replace
 *    it by a different item that has identical key data. For instance, if there is a good chance
 *    that a data item already exists within a table, then it might make sense to insert data
 *    allocated as a local variable into a table, then replace it by a dynamically allocated
 *    copy if it turned out that the item wasn’t already in the table. That way, we save the
 *    time required to make an additional copy of the item to insert. The rbt_probe() function
 *    allows for this kind of flexibility.
 *
 * The pointer returned can be used to replace the item found or inserted by a different item.
 *    This must only be done if the replacement item has the same position relative
 *    to the other items in the table as did the original item. That is, for existing item `e`,
 *    replacement item `r`, and the table’s comparison function `f()`, the return values of
 *    `f(e, x)` and `f(r, x)` must have the same sign for every other item `x` currently in the
 *    table. Calling any other table function invalidates the pointer returned and it must
 *    not be referenced subsequently.
 *
 * @invariant The `table` and `item` arguments cannot be `NULL`.
 *
 * @param [in,out] table the table
 * @param [in]     item  the element to be inserterted
 * @return               a pointer to `item`'s address
 */
void **
rbt_probe (rbt_table_t *table,
           void *item)
{
  rbt_node_t *pa[RBT_MAX_HEIGHT];     /* Nodes on stack. */
  unsigned char da[RBT_MAX_HEIGHT];   /* Directions moved from stack nodes. */
  int k;                              /* Stack height. */

  rbt_node_t *p; /* Traverses tree looking for insertion point. */
  rbt_node_t *n; /* Newly inserted node. */

  assert(table != NULL && item != NULL);

  pa[0] = (rbt_node_t *) &table->root;
  da[0] = 0;
  k = 1;
  for (p = table->root; p != NULL; p = p->links[da[k - 1]]) {
    int cmp = table->compare(item, p->data, table->param);
    if (cmp == 0) return &p->data;
    pa[k] = p;
    da[k++] = cmp > 0;
  }

  n = pa[k - 1]->links[da[k - 1]] = table->alloc->malloc(table->alloc, sizeof *n);
  if (n == NULL) return NULL;

  n->data = item;
  n->links[0] = n->links[1] = NULL;
  n->color = RBT_RED;
  table->count++;
  table->generation++;

  while (k >= 3 && pa[k - 1]->color == RBT_RED) {
    if (da[k - 2] == 0) {
      rbt_node_t *y = pa[k - 2]->links[1];
      if (y != NULL && y->color == RBT_RED) {
        pa[k - 1]->color = y->color = RBT_BLACK;
        pa[k - 2]->color = RBT_RED;
        k -= 2;
      } else {
        rbt_node_t *x;

        if (da[k - 1] == 0)
          y = pa[k - 1];
        else {
          x = pa[k - 1];
          y = x->links[1];
          x->links[1] = y->links[0];
          y->links[0] = x;
          pa[k - 2]->links[0] = y;
        }

        x = pa[k - 2];
        x->color = RBT_RED;
        y->color = RBT_BLACK;

        x->links[0] = y->links[1];
        y->links[1] = x;
        pa[k - 3]->links[da[k - 3]] = y;
        break;
      }
    } else {
      rbt_node_t *y = pa[k - 2]->links[0];
      if (y != NULL && y->color == RBT_RED) {
        pa[k - 1]->color = y->color = RBT_BLACK;
        pa[k - 2]->color = RBT_RED;
        k -= 2;
      } else {
        rbt_node_t *x;

        if (da[k - 1] == 1)
          y = pa[k - 1];
        else {
          x = pa[k - 1];
          y = x->links[0];
          x->links[0] = y->links[1];
          y->links[1] = x;
          pa[k - 2]->links[1] = y;
        }

        x = pa[k - 2];
        x->color = RBT_RED;
        y->color = RBT_BLACK;

        x->links[1] = y->links[0];
        y->links[0] = x;
        pa[k - 3]->links[da[k - 3]] = y;
        break;
      }
    }
  }
  table->root->color = RBT_BLACK;

  return &n->data;
}

/**
 * @brief Inserts `item` into `table`, but not if a matching item exists.
 *
 * @details Returns `NULL` if `item` was successfully inserted or if a memory allocation error occurred.
 *          Otherwise, returns the duplicate item.
 *
 * @invariant The `table` and `item` arguments cannot be `NULL`.
 *
 * @param [in,out] table the table
 * @param [in]     item  the element to be inserterted
 * @return               `NULL` or a pointer to the duplicate item
 */
void *
rbt_insert (rbt_table_t *table,
            void *item)
{
  void **p = rbt_probe(table, item);
  return p == NULL || *p == item ? NULL : *p;
}

/**
 * @brief Inserts `item` into `table`, replacing any duplicate item.
 *
 * @details Returns `NULL` if `item` was inserted without replacing a duplicate, or if a memory
 *          allocation error occurred. Otherwise, returns the item that was replaced.
 *
 * @invariant The `table` and `item` arguments cannot be `NULL`.
 *
 * @param [in,out] table the table
 * @param [in]     item  the element to be inserterted
 * @return               `NULL` or a pointer to the replaced `item`
 */
void *
rbt_replace (rbt_table_t *table,
             void *item)
{
  void **p = rbt_probe(table, item);
  if (p == NULL || *p == item)
    return NULL;
  else {
    void *r = *p;
    *p = item;
    return r;
  }
}

/**
 * @brief Deletes from `table` and returns an item matching `item`.
 *
 * @details Returns a null pointer if no matching item found.
 *
 * @invariant The `table` and `item` arguments cannot be `NULL`.
 *
 * @param [in,out] table the table
 * @param [in]     item  the element to be inserterted
 * @return               a pointer to the deleted `item` or `NULL`
 */
void *
rbt_delete (rbt_table_t *table,
            const void *item)
{
  rbt_node_t *pa[RBT_MAX_HEIGHT];     /* Nodes on stack. */
  unsigned char da[RBT_MAX_HEIGHT];   /* Directions moved from stack nodes. */
  int k;                              /* Stack height. */

  rbt_node_t *p;   /* The node to delete, or a node part way to it. */
  int cmp;         /* Result of comparison between item and p. */

  assert(table != NULL && item != NULL);

  k = 0;
  p = (rbt_node_t *) &table->root;
  for (cmp = -1; cmp != 0; cmp = table->compare(item, p->data, table->param)) {
    int dir = cmp > 0;

    pa[k] = p;
    da[k++] = dir;

    p = p->links[dir];
    if (p == NULL)
      return NULL;
  }
  item = p->data;

  if (p->links[1] == NULL)
    pa[k - 1]->links[da[k - 1]] = p->links[0];
  else {
    rbt_color_t t;
    rbt_node_t *r = p->links[1];

    if (r->links[0] == NULL) {
      r->links[0] = p->links[0];
      t = r->color;
      r->color = p->color;
      p->color = t;
      pa[k - 1]->links[da[k - 1]] = r;
      da[k] = 1;
      pa[k++] = r;
    } else {
      rbt_node_t *s;
      int j = k++;

      for (;;) {
        da[k] = 0;
        pa[k++] = r;
        s = r->links[0];
        if (s->links[0] == NULL)
          break;

        r = s;
      }

      da[j] = 1;
      pa[j] = s;
      pa[j - 1]->links[da[j - 1]] = s;

      s->links[0] = p->links[0];
      r->links[0] = s->links[1];
      s->links[1] = p->links[1];

      t = s->color;
      s->color = p->color;
      p->color = t;
    }
  }

  if (p->color == RBT_BLACK) {
    for (;;) {
      rbt_node_t *x = pa[k - 1]->links[da[k - 1]];
      if (x != NULL && x->color == RBT_RED) {
        x->color = RBT_BLACK;
        break;
      }
      if (k < 2)
        break;

      if (da[k - 1] == 0) {
        rbt_node_t *w = pa[k - 1]->links[1];

        if (w->color == RBT_RED) {
          w->color = RBT_BLACK;
          pa[k - 1]->color = RBT_RED;

          pa[k - 1]->links[1] = w->links[0];
          w->links[0] = pa[k - 1];
          pa[k - 2]->links[da[k - 2]] = w;

          pa[k] = pa[k - 1];
          da[k] = 0;
          pa[k - 1] = w;
          k++;

          w = pa[k - 1]->links[1];
        }

        if ((w->links[0] == NULL || w->links[0]->color == RBT_BLACK) &&
            (w->links[1] == NULL || w->links[1]->color == RBT_BLACK))
          w->color = RBT_RED;
        else {
          if (w->links[1] == NULL || w->links[1]->color == RBT_BLACK) {
            rbt_node_t *y = w->links[0];
            y->color = RBT_BLACK;
            w->color = RBT_RED;
            w->links[0] = y->links[1];
            y->links[1] = w;
            w = pa[k - 1]->links[1] = y;
          }

          w->color = pa[k - 1]->color;
          pa[k - 1]->color = RBT_BLACK;
          w->links[1]->color = RBT_BLACK;

          pa[k - 1]->links[1] = w->links[0];
          w->links[0] = pa[k - 1];
          pa[k - 2]->links[da[k - 2]] = w;
          break;
        }
      } else {
        rbt_node_t *w = pa[k - 1]->links[0];

        if (w->color == RBT_RED) {
          w->color = RBT_BLACK;
          pa[k - 1]->color = RBT_RED;

          pa[k - 1]->links[0] = w->links[1];
          w->links[1] = pa[k - 1];
          pa[k - 2]->links[da[k - 2]] = w;

          pa[k] = pa[k - 1];
          da[k] = 1;
          pa[k - 1] = w;
          k++;

          w = pa[k - 1]->links[0];
        }

        if ((w->links[0] == NULL || w->links[0]->color == RBT_BLACK) &&
            (w->links[1] == NULL || w->links[1]->color == RBT_BLACK))
          w->color = RBT_RED;
        else {
          if (w->links[0] == NULL || w->links[0]->color == RBT_BLACK) {
            rbt_node_t *y = w->links[1];
            y->color = RBT_BLACK;
            w->color = RBT_RED;
            w->links[1] = y->links[0];
            y->links[0] = w;
            w = pa[k - 1]->links[0] = y;
          }

          w->color = pa[k - 1]->color;
          pa[k - 1]->color = RBT_BLACK;
          w->links[0]->color = RBT_BLACK;

          pa[k - 1]->links[0] = w->links[1];
          w->links[1] = pa[k - 1];
          pa[k - 2]->links[da[k - 2]] = w;
          break;
        }
      }

      k--;
    }

  }

  table->alloc->free(table->alloc, p);
  table->count--;
  table->generation++;
  return (void *) item;
}

/**
 * @brief Searches `table` for an item matching `item` and returns it if found.
 *
 * @details Returns a null pointer if no matching item exists in the table.
 *
 * @invariant The `table` and `item` arguments cannot be `NULL`.
 *
 * @param [in,out] table the table
 * @param [in]     item  the element to be inserterted
 * @return               a pointer to the matching `item` or `NULL`
 */
void *
rbt_find (const rbt_table_t *table,
          const void *item)
{
  const rbt_node_t *p;

  assert(table != NULL && item != NULL);
  for (p = table->root; p != NULL; ) {
    const int cmp = table->compare(item, p->data, table->param);
    if (cmp < 0) p = p->links[0];
    else if (cmp > 0) p = p->links[1];
    else return p->data;
  }

  return NULL;
}



/*********************************************************/
/*                                                       */
/* Function implementations for the traverser structure. */
/*                                                       */
/* Constructors.                                         */
/*                                                       */
/*********************************************************/

/**
 * @brief Initializes `trav` for use with `table` and selects the null node.
 *
 * @details Returns a null pointer if no matching item exists in the table.
 *
 * @invariant The `table` and `item` arguments cannot be `NULL`.
 *
 * @param [in,out] trav  the traverser to be initialized
 * @param [in]     table the table for use with
 */
void
rbt_t_init (rbt_traverser_t *trav,
            rbt_table_t *table)
{
  assert(table != NULL && trav != NULL);

  trav->table = table;
  trav->node = NULL;
  trav->height = 0;
  trav->generation = table->generation;
}

/**
 * @brief Initializes `trav` for use with `table` and selects its least-valued item.
 *
 * @details Returns a null pointer if `table` contains no nodes.
 *
 * @invariant The `table` and `item` arguments cannot be `NULL`.
 *
 * @param [in,out] trav  the traverser to be initialized
 * @param [in]     table the table for use with
 * @return               a pointer to its least-valued item or `NULL`
 */
void *
rbt_t_first (rbt_traverser_t *trav,
             rbt_table_t *table)
{
  rbt_node_t *x;

  assert(table != NULL && trav != NULL);

  trav->table = table;
  trav->height = 0;
  trav->generation = table->generation;

  x = table->root;
  if (x != NULL)
    while (x->links[0] != NULL) {
      assert(trav->height < RBT_MAX_HEIGHT);
      trav->stack[trav->height++] = x;
      x = x->links[0];
    }
  trav->node = x;

  return x != NULL ? x->data : NULL;
}

/**
 * @brief Initializes `trav` for use with `table` and selects its greatest-valued item.
 *
 * @details Returns a null pointer if `table` contains no nodes.
 *
 * @invariant The `table` and `item` arguments cannot be `NULL`.
 *
 * @param [in,out] trav  the traverser to be initialized
 * @param [in]     table the table for use with
 * @return               a pointer to its greatest-valued item or `NULL`
 */
void *
rbt_t_last (rbt_traverser_t *trav,
            rbt_table_t *table)
{
  rbt_node_t *x;

  assert(table != NULL && trav != NULL);

  trav->table = table;
  trav->height = 0;
  trav->generation = table->generation;

  x = table->root;
  if (x != NULL)
    while (x->links[1] != NULL) {
      assert(trav->height < RBT_MAX_HEIGHT);
      trav->stack[trav->height++] = x;
      x = x->links[1];
    }
  trav->node = x;

  return x != NULL ? x->data : NULL;
}

/**
 * @brief Searches for `item` in `table`.
 *
 * @details Searches `table` for an item matching the one given. If one is found,
 *          initializes `trav` with it. If none is found, initializes `trav` to the null item.
 *          Returns the found item or `NULL` if there is no matching one.
 *
 * @invariant The `trav`, `table`, and `item` arguments cannot be `NULL`.
 *
 * @param [in,out] trav  the traverser to be initialized
 * @param [in]     table the table for use with
 * @param [in]     item  the element to search for
 * @return               a pointer to the found item or `NULL`
 */
void *
rbt_t_find (rbt_traverser_t *trav,
            rbt_table_t *table,
            void *item)
{
  rbt_node_t *p, *q;

  assert(trav != NULL && table != NULL && item != NULL);

  trav->table = table;
  trav->height = 0;
  trav->generation = table->generation;
  for (p = table->root; p != NULL; p = q) {
    int cmp = table->compare(item, p->data, table->param);

    if (cmp < 0)
      q = p->links[0];
    else if (cmp > 0)
      q = p->links[1];
    else {
      trav->node = p;
      return p->data;
    }

    assert(trav->height < RBT_MAX_HEIGHT);
    trav->stack[trav->height++] = p;
  }

  trav->height = 0;
  trav->node = NULL;
  return NULL;
}

/**
 * @brief Attempts to insert `item` into `table`.
 *
 * @details If `item` is inserted successfully, it is returned and `trav` is initialized to its location.
 *          If a duplicate is found, it is returned and `trav` is initialized to its location.
 *          No replacement of the item occurs.
 *          If a memory allocation failure occurs, `NULL` is returned and `trav` is initialized to the null item.
 *
 * @invariant The `trav`, `table`, and `item` arguments cannot be `NULL`.
 *
 * @param [in,out] trav  the traverser to be initialized
 * @param [in,out] table the table for use with
 * @param [in]     item  the element to be inserted
 * @return               a pointer to the inserted or duplicated item
 */
void *
rbt_t_insert (rbt_traverser_t *trav,
              rbt_table_t *table,
              void *item)
{
  void **p;

  assert(trav != NULL && table != NULL && item != NULL);

  p = rbt_probe(table, item);
  if (p != NULL) {
    trav->table = table;
    trav->node = ((rbt_node_t *) ((char *) p - offsetof(rbt_node_t, data)));
    trav->generation = table->generation - 1;
    return *p;
  } else {
    rbt_t_init(trav, table);
    return NULL;
  }
}

/**
 * @brief Initializes `trav` to have the same current node as `src`, a second valid traverser.
 *
 * @details Both arguments pointing to the same valid traverser is valid and causes no change
 *          in either.
 *          Returns a pointer to the current item.
 *
 * @invariant The `trav` and `src` arguments cannot be `NULL`.
 *
 * @param [in,out] trav the traverser to be initialized
 * @param [in]     src  the second source traverser
 * @return              a pointer to the current item
 */
void *
rbt_t_copy (rbt_traverser_t *trav,
            const rbt_traverser_t *src)
{
  assert(trav != NULL && src != NULL);

  if (trav != src) {
    trav->table = src->table;
    trav->node = src->node;
    trav->generation = src->generation;
    if (trav->generation == trav->table->generation) {
      trav->height = src->height;
      memcpy(trav->stack, (const void *) src->stack, sizeof *trav->stack * trav->height);
    }
  }

  return trav->node != NULL ? trav->node->data : NULL;
}



/*********************************************************/
/*                                                       */
/* Function implementations for the traverser structure. */
/*                                                       */
/* Manipulators.                                         */
/*                                                       */
/*********************************************************/

/**
 * @brief Advances `trav` to the next larger item in its table, and returns it.
 *
 * @details Returns the next data item in inorder within the table being traversed with `trav`,
 *          or if there are no more data items returns `NULL`.
 *          If `trav` was at the null item in a nonempty table, then the smallest item in the table
 *          becomes current. If `trav` was already at the greatest item in its table or the table is
 *          empty, the null item becomes current.
 *
 * @invariant The `trav` argument cannot be `NULL`.
 *
 * @param [in,out] trav the traverser being manipulated
 * @return              a pointer to the current item
 */
void *
rbt_t_next (rbt_traverser_t *trav)
{
  rbt_node_t *x;

  assert(trav != NULL);

  if (trav->generation != trav->table->generation)
    trav_refresh(trav);

  x = trav->node;
  if (x == NULL) {
    return rbt_t_first(trav, trav->table);
  } else if (x->links[1] != NULL) {
    assert(trav->height < RBT_MAX_HEIGHT);
    trav->stack[trav->height++] = x;
    x = x->links[1];

    while (x->links[0] != NULL) {
      assert(trav->height < RBT_MAX_HEIGHT);
      trav->stack[trav->height++] = x;
      x = x->links[0];
    }
  } else {
    rbt_node_t *y;

    do {
      if (trav->height == 0) {
        trav->node = NULL;
        return NULL;
      }

      y = x;
      x = trav->stack[--trav->height];
    }  while (y == x->links[1]);
  }
  trav->node = x;

  return x->data;
}

/**
 * @brief Advances `trav` to the next smaller item in its table, and returns it.
 *
 * @details Returns the previous data item in inorder within the table being traversed with `trav`,
 *          or if there are no more data items returns `NULL`.
 *          If `trav` was at the null item in a nonempty table, then the greatest item in the table
 *          becomes current. If `trav` was already at the lowest item in its table or the table is
 *          empty, the null item becomes current.
 *
 * @invariant The `trav` argument cannot be `NULL`.
 *
 * @param [in,out] trav the traverser being manipulated
 * @return              a pointer to the current item
 */
void *
rbt_t_prev (rbt_traverser_t *trav)
{
  rbt_node_t *x;

  assert(trav != NULL);

  if (trav->generation != trav->table->generation)
    trav_refresh(trav);

  x = trav->node;
  if (x == NULL) {
    return rbt_t_last(trav, trav->table);
  } else if (x->links[0] != NULL) {
    assert(trav->height < RBT_MAX_HEIGHT);
    trav->stack[trav->height++] = x;
    x = x->links[0];

    while (x->links[1] != NULL) {
      assert(trav->height < RBT_MAX_HEIGHT);
      trav->stack[trav->height++] = x;
      x = x->links[1];
    }
  } else {
    rbt_node_t *y;

    do {
      if (trav->height == 0) {
        trav->node = NULL;
        return NULL;
      }

      y = x;
      x = trav->stack[--trav->height];
    } while (y == x->links[0]);
  }
  trav->node = x;

  return x->data;
}

/**
 * @brief Returns `trav`'s current item.
 *
 * @invariant The `trav` argument cannot be `NULL`.
 *
 * @param [in] trav the traverser being manipulated
 * @return          a pointer to the current item
 */
void *
rbt_t_cur (rbt_traverser_t *trav)
{
  assert(trav != NULL);

  return trav->node != NULL ? trav->node->data : NULL;
}

/**
 * @brief Replaces the current item in `trav` by `new` and returns the item replaced.
 *
 * @details Replaces the data item currently selected in `trav` by the one provided.
 *          The replacement item is subject to the same restrictions as for the same replacement
 *          using rbt_probe(). The item replaced is returned. If the null item is current, the behavior
 *          is undefined.
 *
 * @invariant The `trav` and `new` arguments cannot be `NULL`, and `trav` must not have the null item selected.
 *
 * @param [in] trav the traverser being manipulated
 * @param [in] new  the replacement for the selected item
 * @return              a pointer to the current item
 */
void *
rbt_t_replace (rbt_traverser_t *trav,
              void *new)
{
  void *old;

  assert(trav != NULL && trav->node != NULL && new != NULL);

  old = trav->node->data;
  trav->node->data = new;
  return old;
}




/**
 * @cond
 */

/*
 * Internal functions.
 */

/* Destroys |new| with |rbt_destroy (new, destroy)|,
   first setting right links of nodes in |stack| within |new|
   to null pointers to avoid touching uninitialized data. */
static void
copy_error_recovery (rbt_node_t **stack,
                     int height,
                     rbt_table_t *new,
                     rbt_item_destroy_f *destroy)
{
  assert(stack != NULL && height >= 0 && new != NULL);

  for (; height > 2; height -= 2)
    stack[height - 1]->links[1] = NULL;
  rbt_destroy(new, destroy);
}

/* Refreshes the stack of parent pointers in |trav|
   and updates its generation number. */
static void
trav_refresh (rbt_traverser_t *trav)
{
  assert(trav != NULL);

  trav->generation = trav->table->generation;

  if (trav->node != NULL) {
    rbt_item_compare_f *cmp = trav->table->compare;
    void *param = trav->table->param;
    rbt_node_t *node = trav->node;
    rbt_node_t *i;

    trav->height = 0;
    for (i = trav->table->root; i != node; ) {
      assert(trav->height < RBT_MAX_HEIGHT);
      assert(i != NULL);

      trav->stack[trav->height++] = i;
      i = i->links[cmp(node->data, i->data, param) > 0];
    }
  }
}

/**
 * @endcond
 */
