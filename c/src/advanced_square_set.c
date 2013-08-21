/**
 * @file
 *
 * @brief Advanced square set module implementation.
 * @details A specialized square set used to speed up legal moves operations.
 *
 * @par advanced_square_set.c
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

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>

#include "exact_solver.h"
#include "board.h"
#include "bit_works.h"
#include "advanced_square_set.h"



/*
 * Prototypes for internal functions.
 */




/*
 * Internal variables and constants.
 */

/**
 * Fixed square ordering:
 * D4, A1, C1, C3, D1, D3, D2, C2, B1, B2.
 * Taken by Jean-Christophe Weill's order, which is the same
 * proposed in the PAIP book for the weighted-square strategy.
 */
static const Square square_static_ordering[64] =
{
  D4, E4, E5, D5,
  A1, H1, H8, A8,
  C1, F1, F8, C8, A3, H3, H6, A6, 
  C3, F3, F6, C6,
  D1, E1, E8, D8, A4, H4, H5, A5, 
  D3, E3, E6, D6, C4, F4, F5, C5, 
  D2, E2, E7, D7, B4, G4, G5, B5, 
  C2, F2, F7, C7, B3, G3, G6, B6, 
  B1, G1, G8, B8, A2, H2, H7, A7,
  B2, G2, G7, B7
};



/**************************************************************/
/* Function implementations for the AdvancedSquareSet entity. */ 
/**************************************************************/

/**
 * @brief Verify if the set is empty.
 *
 * An assertion checks that the received pointer to the allocated
 * advanced square set structure is not `NULL`.
 *
 * @param s a pointer to the advanced square set
 * @return  true if the set is empty
 */
gboolean
advanced_square_set_is_empty (const AdvancedSquareSet * const s)
{
  g_assert(s);
  return FALSE;
}



/*
 * Internal functions.
 */

