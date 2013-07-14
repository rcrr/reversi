/**
 * @file
 *
 * @brief Exact solver module implementation.
 * @details It searches the end of the game for an exact outcome..
 *
 * @par exact_solver.c
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

#include "board.h"
#include "bit_works.h"

/*
 * Prototypes for internal functions.
 */




/*
 * Internal variables and constants.
 */





/*********************************************************/
/* Function implementations for the GamePosition entity. */ 
/*********************************************************/


/**
 * @brief Returns a formatted string showing a 2d graphical represention of the game position.
 *
 * The returned string has a dynamic extent set by a call to malloc. It must then properly
 * garbage collected by a call to free when no more referenced.
 *
 * @invariant Parameter `gp` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] gp a pointer to the game position structure
 * @return        a string being a 2d representation of the game position
 */
gchar *
game_position_print_x (const GamePosition const *gp)
{
  g_assert(gp);

  const gchar *separator = NULL;

  gchar *gp_to_string;
  gchar *b_to_string;

  b_to_string = board_print(gp->board);

  gp_to_string = g_strjoin(separator,
                           b_to_string,
                           "Player to move: ",
                           (gp->player == BLACK_PLAYER) ? "BLACK" : "WHITE",
                           "\n",
                           NULL);

  g_free(b_to_string);

  return gp_to_string;
}


/*
 * Internal functions.
 */

