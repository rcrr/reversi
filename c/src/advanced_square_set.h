/**
 * @file
 *
 * @brief Advanced square set module definitions.
 * @details This module defines the functions that operete on #AdvancedSquareSet.
 *
 * @par advanced_square_set.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013 Roberto Corradini. All rights reserved.
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

#include "bit_works.h"
#include "board.h"

#include <glib.h>

#ifndef ADVANCED_SQUARE_SET_H
#define ADVANCED_SQUARE_SET_H

/**
 * @brief The struct collects denormalized info extracted from a #SquareSet.
 *
 * To be detailed ...
 */
typedef struct {
  SquareSet square_set;
  uint8     squares[32];
  uint8     move_count;
} AdvancedSquareSet;



/**************************************************************/
/* Function implementations for the AdvancedSquareSet entity. */ 
/**************************************************************/

extern gboolean
advanced_square_set_is_empty (const AdvancedSquareSet * const s);


#endif /* ADVANCED_SQUARE_SET_H */
