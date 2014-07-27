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
typedef struct PrincipalVariationCell_ {
  Square move;                          /**< @brief The current move. */
  struct PrincipalVariationCell_ *next; /**< @brief The next move. */
} PrincipalVariationCell;

/**
 * @brief A principal variation line.
 */
typedef struct {
  int                      size;   /**< @brief Tbd. */
  PrincipalVariationCell  *head;   /**< @brief Tbd. */
  PrincipalVariationCell  *cells;  /**< @brief Tbd. */
  PrincipalVariationCell **stack;  /**< @brief Tbd. */
} PrincipalVariationLine;



/*******************************************************************/
/* Function implementations for the PrincipalVariationLine entity. */ 
/*******************************************************************/

extern PrincipalVariationLine *
pvl_new (const int size);

extern PrincipalVariationLine *
pvl_free (PrincipalVariationLine *pvl);

extern void
pvl_print (PrincipalVariationLine *pvl);

#endif /* GAME_TREE_UTILS_H */
