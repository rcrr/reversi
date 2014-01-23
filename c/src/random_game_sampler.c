/**
 * @file
 *
 * @brief Random game sampler module implementation.
 * @details Used to generate sample game tree.
 *
 * @par random_game_sampler.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014 Roberto Corradini. All rights reserved.
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
#include "random_game_sampler.h"
#include "board.h"
#include "bit_works.h"
#include "advanced_square_set.h"



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
 * @brief Solves the game position returning a new exact solution pointer.
 *
 * @param [in] root the starting game position to be solved
 * @return          a pointer to a new exact solution structure
 */
ExactSolution *
game_position_random_sampler (const GamePosition * const root,
                              const int                  repeats)
{
  ExactSolution *result; 
  SearchNode    *sn;

  result = exact_solution_new();

  result->solved_game_position = game_position_clone(root);

  //sn = game_position_solve_impl(result, result->solved_game_position, -64, +64);
  sn = NULL;
  
  if (sn) {
    result->principal_variation[0] = sn->move;
    result->outcome = sn->value;
  }
  sn = search_node_free(sn);

  return result;
}



/*
 * Internal functions.
 */
