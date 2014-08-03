/**
 * @file
 *
 * @brief Game tree logger utility definitions.
 * @details This module defines the functions used to log the game tree.
 *
 * @par game_tree_utils.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2014 Roberto Corradini. All rights reserved.
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

#ifndef GAME_TREE_UTILS_H
#define GAME_TREE_UTILS_H

#include <glib.h>

#include "board.h"



/**
 * @brief A principal variation cell.
 */
typedef struct PVCell_ {
  Square          move;   /**< @brief The current move. */
  struct PVCell_ *next;   /**< @brief The next move. */
} PVCell;

/**
 * @brief A principal variation environment.
 */
typedef struct {
  int      cells_size;   /**< @brief The count of cells contained by the cells array. */
  PVCell  *cells;        /**< @brief The pointer to the array of cells. */
  PVCell **stack;        /**< @brief The pointer to the array of pointers used to manage the cells. */
  PVCell **stack_head;   /**< @brief The pointer to the next, free to be assigned, pointer in the stack. */
  int      lines_size;   /**< @brief The count of lines contained by the lines array. */
  PVCell **lines;        /**< @brief The pointer to the array of pointers used as a reference of the head of a cell-list. */
  PVCell **lines_head;   /**< @brief The pointer to the next, free to be assigned, pointer in the lines array. */
} PVEnv;



/**************************************************/
/* Function implementations for the PVEnv entity. */ 
/**************************************************/

extern PVEnv *
pve_new (const int cells_size,
         const int lines_size);

extern PVEnv *
pve_free (PVEnv *pve);

extern void
pve_print (PVEnv *pve);

extern PVCell *
pvl_add_move (PVEnv *pve,
              PVCell *line,
              Square move);

extern PVCell *
pvl_create_line (PVEnv *pve);

extern void
pvl_delete_line (PVEnv *pve,
                 PVCell *line);

#endif /* GAME_TREE_UTILS_H */
