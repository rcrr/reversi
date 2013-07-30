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

