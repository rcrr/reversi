/**
 * @file
 *
 * @brief Red black tree module definitions.
 *
 * @details The module is a rearrangement of a portion of the libavl library for manipulation of binary trees.
 *
 * The original work has been written by Ben Pfaff, who may be contacted at <blp@gnu.org> on the Internet,
 * or write to Ben Pfaff, Stanford University, Computer Science Dept., 353 Serra Mall, Stanford CA 94305, USA.
 * See also web site http://adtinfo.org/
 *
 * @par red_black_tree.h
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

#ifndef RED_BLACK_TREE_H
#define RED_BLACK_TREE_H

#include <stddef.h>

#include "memory_manager.h"

/**
 * @brief Maximum red-black tree height.
 */
#define RBT_MAX_HEIGHT 48

/**
 * @brief Returns the number of items collected by `table`.
 */
#define rbt_count(table) ((size_t) (table)->count)



/*******************/
/* Function types. */
/*******************/

/**
 * @brief Compares `item_a` a with `item_b`.
 *
 * @details The implementer has to ensure that when the value refernced by `item_a`
 *          is equal to the one addressed by `item_b`, than zero is returned.
 *          When the value identified by `item_a` is greater than the one pointed by `item_b`,
 *          than the return value has to be `+1`, and otherwise `-1`.
 *
 *          Pseudocode:
 * @code
 *  if (*item_a > *item_b) return +1;
 *  else if (*item_a == *item_b) return 0;
 *  else return -1;
 * @endcode
 *
 * @param [in]     item_a a pointer to the first value
 * @param [in]     item_b a pointer to the second value
 * @param [in,out] param  a utility pointer used to exchange info to and from the function
 * @return                result of comparing the values pointed by `item_a` and `item_b`
 */
typedef int
rbt_item_compare_f (const void *item_a,
                    const void *item_b,
                    void *param);

/**
 * @brief Performs an action on table item
 *
 * @details During destruction, the table item function provided, if non-null, is called once
 *          for every item in the table, in no particular order. The function, if provided,
 *          must not invoke any table function or macro on the table being destroyed.
 *
 * @param [in]     item a pointer to the table item
 * @param [in,out] param  a utility pointer used to exchange data to and from the function
 */
typedef void
rbt_item_destroy_f (void *item,
                    void *param);

/**
 * @brief Returns a copy of the `item` object
 *
 * @details During the execution of the copy table function, if a table copy function is
 *          provided, then it is used to make a copy of each table item as it is inserted
 *          into the new table, in no particular order (a deep copy). Otherwise, the `void ∗`
 *          table items are copied verbatim (a shallow copy).
 *
 * @param [in]     item a pointer to the table item
 * @param [in,out] param  a utility pointer used to exchange data to and from the function
 * @return                a pointer to the newly created object
 */
typedef void *
rbt_item_copy_f (void *item,
                 void *param);




/**
 * @brief Color of a red-black node.
 */
typedef enum rbt_color {
  RBT_BLACK,                                    /**< Black. */
  RBT_RED                                       /**< Red. */
} rbt_color_t;

/**
 * @brief A red-black tree node.
 */
typedef struct rbt_node {
  struct rbt_node *links[2];                    /**< @brief Subtrees. */
  void            *data;                        /**< @brief Pointer to data. */
  rbt_color_t      color;                       /**< @brief Color. */
} rbt_node_t;

/**
 * @brief Red-black tree data structure.
 *
 * @details See Wikipedia page at: https://en.wikipedia.org/wiki/Red-black_tree
 */
typedef struct rbt_table {
  rbt_node_t         *root;                     /**< @brief Tree's root. */
  rbt_item_compare_f *compare;                  /**< @brief Comparison function. */
  void               *param;                    /**< @brief Extra argument to compare function. */
  mem_allocator_t    *alloc;                    /**< @brief Memory allocator. */
  size_t              count;                    /**< @brief Number of items in tree. */
  unsigned long long  generation;               /**< @brief Generation number. */
} rbt_table_t;

/**
 * @brief Red-black tree traverser structure.
 *
 * @details A `struct rbt_traverser` is a table “traverser” that allows the items in a table
 *          to be examined. With a traverser, the items within a table can be enumerated in sorted
 *          ascending or descending order, starting from either end or from somewhere in the middle.
 *
 * The user of the traverser declares its own instance of struct tbl traverser, typically as a
 *    local variable. One of the traverser constructor functions described below can be used to
 *    initialize it. Until then, the traverser is invalid. An invalid traverser must not be passed to
 *    any traverser function other than a constructor.
 *
 * Seen from the viewpoint of a table user, a traverser has only one attribute: the current
 *    item. The current item is either an item in the table or the “null item”, represented by a
 *    null pointer and not associated with any item.
 *
 * Traversers continue to work when their tables are modified. Any number of insertions and
 *    deletions may occur in the table without affecting the current item selected by a traverser,
 *    with only a few exceptions:
 *    - Deleting a traverser’s current item from its table invalidates the traverser (even if the
 *      item is later re-inserted).
 *    - Using the return value of rbt_probe() to replace an item in the table invalidates all
 *      traversers with that item current, unless the replacement item has the same key data
 *      as the original item (that is, the table’s comparison function returns 0 when the two
 *      items are compared).
 *    - Similarly, rbt_replace() invalidates all other traversers with the same item selected,
 *      unless the replacement item has the same key data.
 *    - Destroying a table with rbt_destroy() invalidates all of that table’s traversers.
 *
 * There is no need to destroy a traverser that is no longer needed. An unneeded traverser
 *    can simply be abandoned.
 *
 * Traversers are managed by two classes of functions:
 *     - _Constructors_ <br>
 *       These functions initialize traversers. A traverser must be initialized with one of these
 *       functions before it is passed to any other traverser function.<br>
 *       All of these functions take a traverser to initialize as their first argument, and most take a
 *       table to associate the traverser with as their second argument. These arguments are here
 *       called `trav` and `table`. All, except rbt_t_init(), return the item to which `trav` is initialized,
 *       using a null pointer to represent the null item. None of the arguments to these functions may ever be a null pointer.
 *       - rbt_t_init(): Initializes `trav` to the null item in `table`.
 *       - rbt_t_first(): Initializes `trav` to the least-valued item in `table`. If the table is empty, then
 *         trav is initialized to the null item.
 *       - rbt_t_last(): Same as rbt_t_first(), for the greatest-valued item in `table`.
 *       - rbt_t_find(): Searches `table` for an item matching the one given. If one is found, initializes
 *         `trav` with it. If none is found, initializes `trav` to the null item.
 *       - rbt_t_insert(): Attempts to insert the given item into `table`. If it is inserted succesfully,
 *         `trav` is initialized to its location. If it cannot be inserted because of a duplicate, the
 *         duplicate item is set as `trav`’s current item. If there is a memory allocation error, `trav`
 *         is initialized to the null item.
 *       - rbt_t_copy(): Initializes `trav` to the same table and item as a second valid traverser.
 *         Both arguments pointing to the same valid traverser is valid and causes no change in either.
 *     - _Manipulators_ <br>
 *       Each of these functions takes a valid traverser, here called `trav` , as its first argument, and
 *       returns a data item. All but rbt_t_replace() can also return a null pointer that represents
 *       the null item. All arguments to these functions must be non-null pointers.
 *       - rbt_t_next(): Advances `trav` to the next larger item in its table. If `trav` was at the null
 *         item in a nonempty table, then the smallest item in the table becomes current. If `trav`
 *         was already at the greatest item in its table or the table is empty, the null item becomes
 *         current. Returns the new current item.
 *       - rbt_t_prev(): Advances `trav` to the next smaller item in its table. If `trav` was at the null
 *         item in a nonempty table, then the greatest item in the table becomes current. If `trav`
 *         was already at the lowest item in the table or the table is empty, the null item becomes
 *         current. Returns the new current item.
 *       - rbt_t_cur(): Returns `trav`’s current item.
 *       - rbt_t_replace(): Replaces the data item currently selected in trav by the one provided.
 *         The replacement item is subject to the same restrictions as for the same replacement
 *         using rbt_probe(). The item replaced is returned. If the null item is current, the behavior
 *         is undefined.
 *
 * Seen from the outside, the traverser treats the table as a circular arrangement of items. Moving clockwise in the circle
 *    is equivalent, under our traverser, to moving to the next item with rbt_t_next(). Moving counterclockwise is equivalent
 *    to moving to the previous item with rbt_t_prev().<br>
 *    From this perspective, nodes are arranged from least to greatest in left to right order, and the null node lies in the
 *    middle as a connection between the least and greatest nodes. Moving to the next node is the same as moving to the right
 *    and moving to the previous node is motion to the left, except where the null node is concerned.
 *
 */
typedef struct rbt_traverser {
  rbt_table_t        *table;                    /**< @brief Tree being traversed. */
  rbt_node_t         *node;                     /**< @brief Current node in tree. */
  rbt_node_t         *stack[RBT_MAX_HEIGHT];    /**< @brief All the nodes above nodes. */
  size_t              height;                   /**< @brief Number of nodes in stack. */
  unsigned long long  generation;               /**< @brief Generation number. */
} rbt_traverser_t;



/************************************************/
/* Function prototypes for the table structure. */
/************************************************/

extern rbt_table_t *
rbt_create (rbt_item_compare_f *compare,
            void *param,
            mem_allocator_t *allocator);

extern rbt_table_t *
rbt_copy (const rbt_table_t *org,
          rbt_item_copy_f *copy,
          rbt_item_destroy_f *destroy,
          mem_allocator_t *allocator);

extern void
rbt_destroy (rbt_table_t *table,
             rbt_item_destroy_f *destroy);

extern void **
rbt_probe (rbt_table_t *table,
           void *item);

extern void *
rbt_insert (rbt_table_t *table,
            void *item);

extern void *
rbt_replace (rbt_table_t *table,
             void *item);

extern void *
rbt_delete (rbt_table_t *table,
            const void *item);

extern void *
rbt_find (const rbt_table_t *table,
          const void *item);



/**********************************************************/
/* Function prototypes for the table traverser structure. */
/**********************************************************/

extern void
rbt_t_init (rbt_traverser_t *trav,
            rbt_table_t *table);

extern void *
rbt_t_first (rbt_traverser_t *trav,
             rbt_table_t *table);

extern void *
rbt_t_last (rbt_traverser_t *trav,
            rbt_table_t *table);

extern void *
rbt_t_find (rbt_traverser_t *trav,
            rbt_table_t *table,
            void *item);

extern void *
rbt_t_insert (rbt_traverser_t *trav,
              rbt_table_t *table,
              void *item);

extern void *
rbt_t_copy (rbt_traverser_t *trav,
            const rbt_traverser_t *src);

extern void *
rbt_t_next (rbt_traverser_t *trav);

extern void *
rbt_t_prev (rbt_traverser_t *trav);

extern void *
rbt_t_cur (rbt_traverser_t *trav);

extern void *
rbt_t_replace (rbt_traverser_t *trav,
               void *new);



#endif /* RED_BLACK_TREE_H */
