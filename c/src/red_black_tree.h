/**
 * @file
 *
 * @brief Red black tree module definitions.
 *
 * @details The module is a light rearrangement of a portion of the libavl library for manipulation of binary trees.
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

#include "block_memory_allocator.h"

/* Maximum red-black tree height. */
#define RBT_MAX_HEIGHT 48



/*******************/
/* Function types. */
/*******************/

typedef int rbt_comparison_func (const void *item_a,
                                 const void *item_b,
                                 void *param);

typedef void rbt_item_func (void *item,
                            void *param);

typedef void *rbt_copy_func (void *item,
                             void *param);




/**
 * @brief Color of a red-black node.
 */
typedef enum rbt_color {
  RBT_BLACK,                                      /**< Black. */
  RBT_RED                                         /**< Red. */
} rbt_color_t;

/**
 * @brief A red-black tree node.
 */
typedef struct rbt_node {
  struct rbt_node *links[2];                      /**< @brief Subtrees. */
  void            *data;                          /**< @brief Pointer to data. */
  rbt_color_t      color;                         /**< @brief Color. */
} rbt_node_t;

/**
 * @brief Tree data structure.
 */
typedef struct rbt_table {
  rbt_node_t              *root;                  /**< @brief Tree's root. */
  rbt_comparison_func     *compare;               /**< @brief Comparison function. */
  void                    *param;                 /**< @brief Extra argument to compare function. */
  struct libavl_allocator *alloc;                 /**< @brief Memory allocator. */
  size_t                   count;                 /**< @brief Number of items in tree. */
  unsigned long long       generation;            /**< @brief Generation number. */
} rbt_table_t;

/**
 * @brief RB traverser structure.
 */
typedef struct rbt_traverser {
  rbt_table_t        *table;                      /**< @brief Tree being traversed. */
  rbt_node_t         *node;                       /**< @brief Current node in tree. */
  rbt_node_t         *stack[RBT_MAX_HEIGHT];      /**< @brief All the nodes above nodes. */
  size_t              height;                     /**< @brief Number of nodes in stack. */
  unsigned long long  generation;                 /**< @brief Generation number. */
} rbt_traverser_t;



/************************************************/
/* Function prototypes for the table structure. */
/************************************************/

extern rbt_table_t *
rbt_create (rbt_comparison_func *compare,
            void *param,
            struct libavl_allocator *allocator);

extern rbt_table_t *
rbt_copy (const rbt_table_t *org,
          rbt_copy_func *copy,
          rbt_item_func *destroy,
          struct libavl_allocator *allocator);

extern void
rbt_destroy (rbt_table_t *tree,
             rbt_item_func *destroy);

extern void **
rbt_probe (rbt_table_t *tree,
           void *item);

extern void *
rbt_insert (rbt_table_t *table,
            void *item);

extern void *
rbt_replace (rbt_table_t *table,
             void *item);

extern void *
rbt_delete (rbt_table_t *tree,
            const void *item);

extern void *
rbt_find (const rbt_table_t *tree,
          const void *item);

extern void
rbt_assert_insert (rbt_table_t *table,
                   void *item);

extern void *
rbt_assert_delete (rbt_table_t *table,
                  void *item);

#define rbt_count(table) ((size_t) (table)->count)



/**********************************************************/
/* Function prototypes for the table traverser structure. */
/**********************************************************/

extern void
rbt_t_init (rbt_traverser_t *trav,
            rbt_table_t *tree);

extern void *
rbt_t_first (rbt_traverser_t *trav,
             rbt_table_t *tree);

extern void *
rbt_t_last (rbt_traverser_t *trav,
            rbt_table_t *tree);

extern void *
rbt_t_find (rbt_traverser_t *trav,
            rbt_table_t *tree,
            void *item);

extern void *
rbt_t_insert (rbt_traverser_t *trav,
              rbt_table_t *tree,
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
