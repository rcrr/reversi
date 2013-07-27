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

#include "exact_solver.h"
#include "board.h"
#include "bit_works.h"

/*
 * Prototypes for internal functions.
 */

static SearchNode *
game_position_solve_impl (const GamePosition * const gp,
                          const int                  achievable,
                          const int                  cutoff,
                          const int                  ply);

static int
final_value (const GamePosition * const gp);



/*
 * Internal variables and constants.
 */

static uint64 leaf_count = 0;
static uint64 node_count = 0;




/*******************************************************/
/* Function implementations for the SearchNode entity. */ 
/*******************************************************/

/**
 * @brief Search node structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * search node structure is not `NULL`.
 *
 * @return a pointer to a new search node structure
 */
SearchNode *
search_node_new (const Square move, const int value)
{
  SearchNode * sn;
  static const size_t size_of_search_node = sizeof(SearchNode);

  sn = (SearchNode*) malloc(size_of_search_node);
  g_assert(sn);

  sn->move = move;
  sn->value = value;

  return sn;
}

/**
 * @brief Search node structure destructor.
 *
 * @invariant Parameter `sn` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] sn the pointer to be deallocated
 * @return        always the NULL pointer
 */
SearchNode *
search_node_free (SearchNode *sn)
{
  g_assert(sn);

  free(sn);
  sn = NULL;

  return sn;
}

SearchNode *
search_node_negated (SearchNode *sn)
{
  SearchNode *result;
  result = search_node_new(sn->move, -sn->value);
  search_node_free(sn);
  return result;
}


/**********************************************************/
/* Function implementations for the ExactSolution entity. */ 
/**********************************************************/

/**
 * @brief Exact solution structure constructor.
 *
 * An assertion checks that the received pointer to the allocated
 * exact solution structure is not `NULL`.
 *
 * @return a pointer to a new exact solution structure
 */
ExactSolution *
exact_solution_new (void)
{
  ExactSolution * es;
  static const size_t size_of_exact_solution = sizeof(ExactSolution);

  es = (ExactSolution*) malloc(size_of_exact_solution);
  g_assert(es);

  return es;
}

/**
 * @brief Exact solution structure destructor.
 *
 * @invariant Parameter `es` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] es the pointer to be deallocated
 * @return        always the NULL pointer
 */
ExactSolution *
exact_solution_free (ExactSolution *es)
{
  g_assert(es);

  free(es);
  es = NULL;

  return es;
}

/**
 * @brief Returns a formatted string describing the exact solution structure.
 *
 * @todo MUST BE COMPLETED!
 *
 * The returned string has a dynamic extent set by a call to malloc. It must then properly
 * garbage collected by a call to free when no more referenced.
 *
 * @invariant Parameter `es` must be not `NULL`.
 * Invariants are guarded by assertions.
 *
 * @param [in] es a pointer to the exact solution structure
 * @return        a string being a representation of the structure
 */
gchar *
exact_solution_print (const ExactSolution * const es)
{
  g_assert(es);

  gchar *es_to_string;

  es_to_string = game_position_print(es->solved_game_position);

  return es_to_string;
}



/*********************************************************/
/* Function implementations for the GamePosition entity. */ 
/*********************************************************/

ExactSolution *
game_position_solve (GamePosition * const root)
{
  ExactSolution *result = exact_solution_new();

  result->solved_game_position = root;

  SearchNode *sn = game_position_solve_impl(root, -64, +64, 60);

  printf("Final SearchNode sn: move=%d, value=%d\n", sn->move, sn->value);
  if (sn)
    result->outcome = sn->value;

  printf("[node_count=%llu, leaf_count=%llu]\n", node_count, leaf_count);

  return result;
}



/*
 * Internal functions.
 */

SearchNode *
game_position_solve_impl (const GamePosition * const gp,
                          const int                  achievable,
                          const int                  cutoff,
                          const int                  ply)
{
  SearchNode *node;
  SearchNode *node2;

  node_count++;

  const SquareSet moves = game_position_legal_moves(gp);
  if (0ULL == moves) {
    GamePosition *flipped_players = game_position_pass(gp);
    if (game_position_has_any_legal_move(flipped_players)) {
      node = search_node_negated(game_position_solve_impl(flipped_players, -cutoff, -achievable, ply - 1));
    } else {
      leaf_count++;
      node = search_node_new((Square) -1, final_value(gp));
    }
    flipped_players = game_position_free(flipped_players);
  } else {
    Square first_move = bit_works_bitscanLS1B_64(moves);
    node = search_node_new(first_move, achievable);
    Square move = 0;
    for (SquareSet cursor = 0x0000000000000001; cursor != 0ULL; cursor <<= 1) {
      if ((cursor & moves) != 0ULL) {
        GamePosition *gp2 = game_position_make_move(gp, move);
        node2 = search_node_negated(game_position_solve_impl(gp2, -cutoff, -node->value, ply - 1));
        //const int val = node2->value;
        gp2 = game_position_free(gp2);
        if (node2->value > node->value) {
          search_node_free(node);
          node = node2;
          node->move = move;
          node2 = NULL;
        } else {
          node2 = search_node_free(node2);
        }
        if (node->value >= cutoff) { goto out; }
      }
      move++;
    }
  }
 out:
  return node;
}

int
final_value (const GamePosition * const gp)
{
  return game_position_count_difference(gp);
}
