/**
 * @file
 *
 * @brief Red black tree module.
 *
 * @details This module defines the abstract concept of a table, also known as dictionary,
 *          by means of an implementation realized by a red-black binary search tree.
 *          The terms table, tree, dictionary, and associative array will be used interchangeably.
 *
 * The purpose of a table is to keep track of a collection of items, all of the same type.
 *    Items can be inserted into and deleted from a table, with no arbitrary limit on the number
 *    of items in the table. We can also search in a table for items that match a given item.
 *
 * Other operations are supported, too. Traversal is the most important of these: all of
 *   the items in a table can be visited, in sorted order from smallest to largest, or from largest
 *   to smallest. Traversals can also start from an item in the middle, or a newly inserted item,
 *   and move in either direction.
 *
 * The data in a table may be of any C type, but all the items in a table must be of the
 *    same type. Structure types are common. Often, only part of each data item is used in item
 *    lookup, with the rest for storage of auxiliary information. A table that contains two-part
 *    data items like this is called a “dictionary” or an “associative array”. The part of table
 *    data used for lookup, whether the table is a dictionary or not, is the key. In a dictionary,
 *    the remainder is the value.
 *
 * Our tables cannot contain duplicates. An attempt to insert an item into a table that
 *    already contains a matching item will fail.
 *
 * The module is a rearrangement of a portion of the libavl library for manipulation of binary trees.
 *   The original work has been written by Ben Pfaff, who may be contacted at <blp@gnu.org> on the Internet,
 *   or write to Ben Pfaff, Stanford University, Computer Science Dept., 353 Serra Mall, Stanford CA 94305, USA.
 *   See also web site http://adtinfo.org/
 *
 * @par red_black_tree.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Ben Pfaff mailto:blp@gnu.org
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 1998-2002, 2004 Free Software Foundation, Inc.
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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "red_black_tree.h"



/*****************************************************/
/* Function implementations for the table structure. */
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

/* Search |tree| for an item matching |item|, and return it if found.
   Otherwise return |NULL|. */
void *
rbt_find (const rbt_table_t *table,
          const void *item)
{
  const rbt_node_t *p;

  assert (table != NULL && item != NULL);
  for (p = table->root; p != NULL; ) {
    const int cmp = table->compare(item, p->data, table->param);
    if (cmp < 0) p = p->links[0];
    else if (cmp > 0) p = p->links[1];
    else return p->data;
  }

  return NULL;
}

/* Inserts |item| into |tree| and returns a pointer to |item|'s address.
   If a duplicate item is found in the tree,
   returns a pointer to the duplicate without inserting |item|.
   Returns |NULL| in case of memory allocation failure. */
void **
rbt_probe (rbt_table_t *tree,
           void *item)
{
  rbt_node_t *pa[RBT_MAX_HEIGHT]; /* Nodes on stack. */
  unsigned char da[RBT_MAX_HEIGHT];   /* Directions moved from stack nodes. */
  int k;                              /* Stack height. */

  rbt_node_t *p; /* Traverses tree looking for insertion point. */
  rbt_node_t *n; /* Newly inserted node. */

  assert (tree != NULL && item != NULL);

  pa[0] = (rbt_node_t *) &tree->root;
  da[0] = 0;
  k = 1;
  for (p = tree->root; p != NULL; p = p->links[da[k - 1]])
    {
      int cmp = tree->compare (item, p->data, tree->param);
      if (cmp == 0)
        return &p->data;

      pa[k] = p;
      da[k++] = cmp > 0;
    }

  n = pa[k - 1]->links[da[k - 1]] =
    tree->alloc->malloc (tree->alloc, sizeof *n);
  if (n == NULL)
    return NULL;

  n->data = item;
  n->links[0] = n->links[1] = NULL;
  n->color = RBT_RED;
  tree->count++;
  tree->generation++;

  while (k >= 3 && pa[k - 1]->color == RBT_RED)
    {
      if (da[k - 2] == 0)
        {
          rbt_node_t *y = pa[k - 2]->links[1];
          if (y != NULL && y->color == RBT_RED)
            {
              pa[k - 1]->color = y->color = RBT_BLACK;
              pa[k - 2]->color = RBT_RED;
              k -= 2;
            }
          else
            {
              rbt_node_t *x;

              if (da[k - 1] == 0)
                y = pa[k - 1];
              else
                {
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
        }
      else
        {
          rbt_node_t *y = pa[k - 2]->links[0];
          if (y != NULL && y->color == RBT_RED)
            {
              pa[k - 1]->color = y->color = RBT_BLACK;
              pa[k - 2]->color = RBT_RED;
              k -= 2;
            }
          else
            {
              rbt_node_t *x;

              if (da[k - 1] == 1)
                y = pa[k - 1];
              else
                {
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
  tree->root->color = RBT_BLACK;


  return &n->data;
}

/* Inserts |item| into |table|.
   Returns |NULL| if |item| was successfully inserted
   or if a memory allocation error occurred.
   Otherwise, returns the duplicate item. */
void *
rbt_insert (rbt_table_t *table,
            void *item)
{
  void **p = rbt_probe (table, item);
  return p == NULL || *p == item ? NULL : *p;
}

/* Inserts |item| into |table|, replacing any duplicate item.
   Returns |NULL| if |item| was inserted without replacing a duplicate,
   or if a memory allocation error occurred.
   Otherwise, returns the item that was replaced. */
void *
rbt_replace (rbt_table_t *table,
             void *item)
{
  void **p = rbt_probe (table, item);
  if (p == NULL || *p == item)
    return NULL;
  else
    {
      void *r = *p;
      *p = item;
      return r;
    }
}

/* Deletes from |tree| and returns an item matching |item|.
   Returns a null pointer if no matching item found. */
void *
rbt_delete (rbt_table_t *tree,
            const void *item)
{
  rbt_node_t *pa[RBT_MAX_HEIGHT]; /* Nodes on stack. */
  unsigned char da[RBT_MAX_HEIGHT];   /* Directions moved from stack nodes. */
  int k;                              /* Stack height. */

  rbt_node_t *p;    /* The node to delete, or a node part way to it. */
  int cmp;              /* Result of comparison between |item| and |p|. */

  assert (tree != NULL && item != NULL);

  k = 0;
  p = (rbt_node_t *) &tree->root;
  for (cmp = -1; cmp != 0;
       cmp = tree->compare (item, p->data, tree->param))
    {
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
  else
    {
      rbt_color_t t;
      rbt_node_t *r = p->links[1];

      if (r->links[0] == NULL)
        {
          r->links[0] = p->links[0];
          t = r->color;
          r->color = p->color;
          p->color = t;
          pa[k - 1]->links[da[k - 1]] = r;
          da[k] = 1;
          pa[k++] = r;
        }
      else
        {
          rbt_node_t *s;
          int j = k++;

          for (;;)
            {
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

  if (p->color == RBT_BLACK)
    {
      for (;;)
        {
          rbt_node_t *x = pa[k - 1]->links[da[k - 1]];
          if (x != NULL && x->color == RBT_RED)
            {
              x->color = RBT_BLACK;
              break;
            }
          if (k < 2)
            break;

          if (da[k - 1] == 0)
            {
              rbt_node_t *w = pa[k - 1]->links[1];

              if (w->color == RBT_RED)
                {
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

              if ((w->links[0] == NULL
                   || w->links[0]->color == RBT_BLACK)
                  && (w->links[1] == NULL
                      || w->links[1]->color == RBT_BLACK))
                w->color = RBT_RED;
              else
                {
                  if (w->links[1] == NULL
                      || w->links[1]->color == RBT_BLACK)
                    {
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
            }
          else
            {
              rbt_node_t *w = pa[k - 1]->links[0];

              if (w->color == RBT_RED)
                {
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

              if ((w->links[0] == NULL
                   || w->links[0]->color == RBT_BLACK)
                  && (w->links[1] == NULL
                      || w->links[1]->color == RBT_BLACK))
                w->color = RBT_RED;
              else
                {
                  if (w->links[0] == NULL
                      || w->links[0]->color == RBT_BLACK)
                    {
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

  tree->alloc->free (tree->alloc, p);
  tree->count--;
  tree->generation++;
  return (void *) item;
}

/* Refreshes the stack of parent pointers in |trav|
   and updates its generation number. */
static void
trav_refresh (rbt_traverser_t *trav)
{
  assert (trav != NULL);

  trav->generation = trav->table->generation;

  if (trav->node != NULL)
    {
      rbt_item_compare_f *cmp = trav->table->compare;
      void *param = trav->table->param;
      rbt_node_t *node = trav->node;
      rbt_node_t *i;

      trav->height = 0;
      for (i = trav->table->root; i != node; )
        {
          assert (trav->height < RBT_MAX_HEIGHT);
          assert (i != NULL);

          trav->stack[trav->height++] = i;
          i = i->links[cmp (node->data, i->data, param) > 0];
        }
    }
}

/* Initializes |trav| for use with |tree|
   and selects the null node. */
void
rbt_t_init (rbt_traverser_t *trav,
            rbt_table_t *tree)
{
  trav->table = tree;
  trav->node = NULL;
  trav->height = 0;
  trav->generation = tree->generation;
}

/* Initializes |trav| for |tree|
   and selects and returns a pointer to its least-valued item.
   Returns |NULL| if |tree| contains no nodes. */
void *
rbt_t_first (rbt_traverser_t *trav,
             rbt_table_t *tree)
{
  rbt_node_t *x;

  assert (tree != NULL && trav != NULL);

  trav->table = tree;
  trav->height = 0;
  trav->generation = tree->generation;

  x = tree->root;
  if (x != NULL)
    while (x->links[0] != NULL)
      {
        assert (trav->height < RBT_MAX_HEIGHT);
        trav->stack[trav->height++] = x;
        x = x->links[0];
      }
  trav->node = x;

  return x != NULL ? x->data : NULL;
}

/* Initializes |trav| for |tree|
   and selects and returns a pointer to its greatest-valued item.
   Returns |NULL| if |tree| contains no nodes. */
void *
rbt_t_last (rbt_traverser_t *trav,
            rbt_table_t *tree)
{
  rbt_node_t *x;

  assert (tree != NULL && trav != NULL);

  trav->table = tree;
  trav->height = 0;
  trav->generation = tree->generation;

  x = tree->root;
  if (x != NULL)
    while (x->links[1] != NULL)
      {
        assert (trav->height < RBT_MAX_HEIGHT);
        trav->stack[trav->height++] = x;
        x = x->links[1];
      }
  trav->node = x;

  return x != NULL ? x->data : NULL;
}

/* Searches for |item| in |tree|.
   If found, initializes |trav| to the item found and returns the item
   as well.
   If there is no matching item, initializes |trav| to the null item
   and returns |NULL|. */
void *
rbt_t_find (rbt_traverser_t *trav,
            rbt_table_t *tree,
            void *item)
{
  rbt_node_t *p, *q;

  assert (trav != NULL && tree != NULL && item != NULL);
  trav->table = tree;
  trav->height = 0;
  trav->generation = tree->generation;
  for (p = tree->root; p != NULL; p = q)
    {
      int cmp = tree->compare (item, p->data, tree->param);

      if (cmp < 0)
        q = p->links[0];
      else if (cmp > 0)
        q = p->links[1];
      else /* |cmp == 0| */
        {
          trav->node = p;
          return p->data;
        }

      assert (trav->height < RBT_MAX_HEIGHT);
      trav->stack[trav->height++] = p;
    }

  trav->height = 0;
  trav->node = NULL;
  return NULL;
}

/* Attempts to insert |item| into |tree|.
   If |item| is inserted successfully, it is returned and |trav| is
   initialized to its location.
   If a duplicate is found, it is returned and |trav| is initialized to
   its location.  No replacement of the item occurs.
   If a memory allocation failure occurs, |NULL| is returned and |trav|
   is initialized to the null item. */
void *
rbt_t_insert (rbt_traverser_t *trav,
              rbt_table_t *tree,
              void *item)
{
  void **p;

  assert (trav != NULL && tree != NULL && item != NULL);

  p = rbt_probe (tree, item);
  if (p != NULL)
    {
      trav->table = tree;
      trav->node =
        ((rbt_node_t *)
         ((char *) p - offsetof (rbt_node_t, data)));
      trav->generation = tree->generation - 1;
      return *p;
    }
  else
    {
      rbt_t_init (trav, tree);
      return NULL;
    }
}

/* Initializes |trav| to have the same current node as |src|. */
void *
rbt_t_copy (rbt_traverser_t *trav,
            const rbt_traverser_t *src)
{
  assert (trav != NULL && src != NULL);

  if (trav != src)
    {
      trav->table = src->table;
      trav->node = src->node;
      trav->generation = src->generation;
      if (trav->generation == trav->table->generation)
        {
          trav->height = src->height;
          memcpy (trav->stack, (const void *) src->stack,
                  sizeof *trav->stack * trav->height);
        }
    }

  return trav->node != NULL ? trav->node->data : NULL;
}

/* Returns the next data item in inorder
   within the tree being traversed with |trav|,
   or if there are no more data items returns |NULL|. */
void *
rbt_t_next (rbt_traverser_t *trav)
{
  rbt_node_t *x;

  assert (trav != NULL);

  if (trav->generation != trav->table->generation)
    trav_refresh (trav);

  x = trav->node;
  if (x == NULL)
    {
      return rbt_t_first (trav, trav->table);
    }
  else if (x->links[1] != NULL)
    {
      assert (trav->height < RBT_MAX_HEIGHT);
      trav->stack[trav->height++] = x;
      x = x->links[1];

      while (x->links[0] != NULL)
        {
          assert (trav->height < RBT_MAX_HEIGHT);
          trav->stack[trav->height++] = x;
          x = x->links[0];
        }
    }
  else
    {
      rbt_node_t *y;

      do
        {
          if (trav->height == 0)
            {
              trav->node = NULL;
              return NULL;
            }

          y = x;
          x = trav->stack[--trav->height];
        }
      while (y == x->links[1]);
    }
  trav->node = x;

  return x->data;
}

/* Returns the previous data item in inorder
   within the tree being traversed with |trav|,
   or if there are no more data items returns |NULL|. */
void *
rbt_t_prev (rbt_traverser_t *trav)
{
  rbt_node_t *x;

  assert (trav != NULL);

  if (trav->generation != trav->table->generation)
    trav_refresh (trav);

  x = trav->node;
  if (x == NULL)
    {
      return rbt_t_last (trav, trav->table);
    }
  else if (x->links[0] != NULL)
    {
      assert (trav->height < RBT_MAX_HEIGHT);
      trav->stack[trav->height++] = x;
      x = x->links[0];

      while (x->links[1] != NULL)
        {
          assert (trav->height < RBT_MAX_HEIGHT);
          trav->stack[trav->height++] = x;
          x = x->links[1];
        }
    }
  else
    {
      rbt_node_t *y;

      do
        {
          if (trav->height == 0)
            {
              trav->node = NULL;
              return NULL;
            }

          y = x;
          x = trav->stack[--trav->height];
        }
      while (y == x->links[0]);
    }
  trav->node = x;

  return x->data;
}

/* Returns |trav|'s current item. */
void *
rbt_t_cur (rbt_traverser_t *trav)
{
  assert (trav != NULL);

  return trav->node != NULL ? trav->node->data : NULL;
}

/* Replaces the current item in |trav| by |new| and returns the item replaced.
   |trav| must not have the null item selected.
   The new item must not upset the ordering of the tree. */
void *
rbt_t_replace (rbt_traverser_t *trav,
              void *new)
{
  void *old;

  assert (trav != NULL && trav->node != NULL && new != NULL);
  old = trav->node->data;
  trav->node->data = new;
  return old;
}

/* Destroys |new| with |rbt_destroy (new, destroy)|,
   first setting right links of nodes in |stack| within |new|
   to null pointers to avoid touching uninitialized data. */
static void
copy_error_recovery (rbt_node_t **stack,
                     int height,
                     rbt_table_t *new,
                     rbt_item_destroy_f *destroy)
{
  assert (stack != NULL && height >= 0 && new != NULL);

  for (; height > 2; height -= 2)
    stack[height - 1]->links[1] = NULL;
  rbt_destroy (new, destroy);
}

/* Copies |org| to a newly created tree, which is returned.
   If |copy != NULL|, each data item in |org| is first passed to |copy|,
   and the return values are inserted into the tree,
   with |NULL| return values taken as indications of failure.
   On failure, destroys the partially created new tree,
   applying |destroy|, if non-null, to each item in the new tree so far,
   and returns |NULL|.
   If |alloc != NULL|, it is used for allocation in the new tree.
   Otherwise, the same alloc used for |org| is used. */
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
  new = rbt_create (org->compare, org->param,
                    alloc != NULL ? alloc : org->alloc);
  if (new == NULL)
    return NULL;
  new->count = org->count;
  if (new->count == 0)
    return new;

  x = (const rbt_node_t *) &org->root;
  y = (rbt_node_t *) &new->root;
  for (;;)
    {
      while (x->links[0] != NULL)
        {
          assert (height < 2 * (RBT_MAX_HEIGHT + 1));

          y->links[0] =
            new->alloc->malloc (new->alloc,
                                           sizeof *y->links[0]);
          if (y->links[0] == NULL)
            {
              if (y != (rbt_node_t *) &new->root)
                {
                  y->data = NULL;
                  y->links[1] = NULL;
                }

              copy_error_recovery (stack, height, new, destroy);
              return NULL;
            }

          stack[height++] = (rbt_node_t *) x;
          stack[height++] = y;
          x = x->links[0];
          y = y->links[0];
        }
      y->links[0] = NULL;

      for (;;)
        {
          y->color = x->color;
          if (copy == NULL)
            y->data = x->data;
          else
            {
              y->data = copy (x->data, org->param);
              if (y->data == NULL)
                {
                  y->links[1] = NULL;
                  copy_error_recovery (stack, height, new, destroy);
                  return NULL;
                }
            }

          if (x->links[1] != NULL)
            {
              y->links[1] =
                new->alloc->malloc (new->alloc,
                                               sizeof *y->links[1]);
              if (y->links[1] == NULL)
                {
                  copy_error_recovery (stack, height, new, destroy);
                  return NULL;
                }

              x = x->links[1];
              y = y->links[1];
              break;
            }
          else
            y->links[1] = NULL;

          if (height <= 2)
            return new;

          y = stack[--height];
          x = stack[--height];
        }
    }
}

/* Frees storage allocated for |tree|.
   If |destroy != NULL|, applies it to each data item in inorder. */
void
rbt_destroy (rbt_table_t *tree,
             rbt_item_destroy_f *destroy)
{
  rbt_node_t *p, *q;

  assert (tree != NULL);

  for (p = tree->root; p != NULL; p = q)
    if (p->links[0] == NULL)
      {
        q = p->links[1];
        if (destroy != NULL && p->data != NULL)
          destroy (p->data, tree->param);
        tree->alloc->free (tree->alloc, p);
      }
    else
      {
        q = p->links[0];
        p->links[0] = q->links[1];
        q->links[1] = p;
      }

  tree->alloc->free (tree->alloc, tree);
}
