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

#include <glib.h>

#include "exact_solver.h"
#include "random_game_sampler.h"
#include "board.h"
#include "bit_works.h"



/*
 * Prototypes for internal functions.
 */

static SearchNode *
game_position_random_sampler_impl (      ExactSolution * const result,
                                   const GamePosition  * const gp);



/*
 * Internal variables and constants.
 */



/*********************************************************/
/* Function implementations for the GamePosition entity. */ 
/*********************************************************/

/**
 * @brief Runs a sequence of random games for number of times equal
 * to the `repeats` parameter, starting from the `root` game position.
 *
 * @param [in] root    the starting game position to be solved
 * @param [in] repeats number of random game to play
 * @return             a pointer to a new exact solution structure
 */
ExactSolution *
game_position_random_sampler (const GamePosition * const root,
                              const int                  repeats)
{
  ExactSolution *result; 
  SearchNode    *sn;

  result = exact_solution_new();

  result->solved_game_position = game_position_clone(root);

  sn = game_position_random_sampler_impl(result, result->solved_game_position);
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

static SearchNode *
game_position_random_sampler_impl (      ExactSolution * const result,
                                   const GamePosition  * const gp)
{
  SearchNode *node;

  node  = NULL;
  result->node_count++;


  if (game_position_has_any_player_any_legal_move(gp)) { // the game must go on
    const SquareSet moves = game_position_legal_moves(gp);
    if (0ULL == moves) { // player has to pass
      GamePosition *flipped_players = game_position_pass(gp);
      node = search_node_negated(game_position_random_sampler_impl(result, flipped_players));
      flipped_players = game_position_free(flipped_players);
    } else { // regular move
      const Square move = bit_works_bitscanMS1B_64(moves); // take the first move, must be turned into a random selection.
      GamePosition *next_gp = game_position_make_move(gp, move);
      node = search_node_negated(game_position_random_sampler_impl(result, next_gp));
      next_gp = game_position_free(next_gp);
    }
  } else { // game-over
    result->leaf_count++;
    node = search_node_new((Square) -1, game_position_final_value(gp));
  }

  return node;
}
