/**
 * @file
 *
 * @brief Random alpha-beta solver module implementation.
 * @details It searches the end of the game for an exact outcome using the Alpha-Beta algorithm.
 * Moves are sorted by a random criteria.
 *
 * @par rab_solver.c
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

#include <stdio.h>
#include <stdlib.h>

#include <glib.h>

#include "rab_solver.h"

/**
 * @brief Game tree stack size.
 *
 * @details It give the size of the static stack used to pile-up the info
 * computed by deepening the game tree.
 * The value is given by the 64 squares doubled to take into account the possibility
 * to pass. Real tree depth are smaller because the empty square are 60 in a real game,
 * because the number of pass is little, and because there is no capability currently to
 * search so deep.
 */
#define GAME_TREE_MAX_DEPTH 128

/**
 * @brief The info collected on each node.
 */
typedef struct {
  GamePositionX  gpx;          /**< @brief The game position related to the game tree node. */
  uint64         hash;         /**< @brief The hash value of the game position. */
  LegalMoveList  moves;        /**< @brief The list of legal moves for the node. */
} NodeInfo;

/**
 * @brief The info collected by deepening the game tree.
 */
typedef struct {
  int      fill_point;                    /**< @brief The index of the last entry into the stack. */
  NodeInfo nodes[GAME_TREE_MAX_DEPTH];    /**< @brief The stack of node info. */
} GameTreeStack;


/*
 * Prototypes for internal functions.
 */

static SearchNode *
game_position_solve_impl2 (      ExactSolution * const result,
                           const GamePositionX * const gpx);

static void
game_tree_stack_init (void);

inline static void
legal_move_list_from_set (const SquareSet      legal_move_set,
                                LegalMoveList *legal_move_list);



/*
 * Internal variables and constants.
 */

static GameTreeStack stack_structure;

static GameTreeStack *stack = &stack_structure;



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
game_position_rab_solve (const GamePosition * const root)
{
  ExactSolution *result; 
  SearchNode    *sn;

  game_tree_stack_init();
  
  result = exact_solution_new();

  result->solved_game_position = game_position_clone(root);

  //sn = game_position_solve_impl(result, result->solved_game_position);
  GamePositionX *gpx = game_position_x_gp_to_gpx(root);
  sn = game_position_solve_impl2(result, gpx);
  gpx = game_position_x_free(gpx);
  
  result->principal_variation[0] = sn->move;
  result->outcome = sn->value;
  sn = search_node_free(sn);

  return result;
}



/*
 * Internal functions.
 */

/**
 * @brief Initializes the stack structure.
 */
static void
game_tree_stack_init (void)
{
  stack->fill_point = 0;
}

/**
 * @brief Computes the legal move list structure given the set.
 *
 * @param [in]  legal_move_set  the set of legal moves
 * @param [out] legal_move_list the compuetd list of legal moves
 */
inline static void
legal_move_list_from_set (const SquareSet      legal_move_set,
                                LegalMoveList *legal_move_list)
{
  legal_move_list->move_count = 0;
  legal_move_list->square_set = legal_move_set;

  SquareSet remaining_moves = legal_move_set;
  while (remaining_moves) {
    const Square move = bit_works_bitscanLS1B_64(remaining_moves);
    legal_move_list->squares[legal_move_list->move_count] = move;
    legal_move_list->move_count++;
    remaining_moves ^= 1ULL << move;
  }
  
  return;
}

/**
 * @brief Recursive function used to traverse the game tree.
 *
 * @param [in] result  a reference to the exact solution data structure
 * @param [in] current the game position x to traverse
 * @return             a pointer to a new serch node structure
 */
static SearchNode *
game_position_solve_impl2 (      ExactSolution * const result,
                           const GamePositionX * const current)
{
  
  SearchNode *node;
  SearchNode *node2;

  node  = NULL;
  node2 = NULL;
  result->node_count++;
 
  NodeInfo * const node_info = &stack->nodes[++stack->fill_point];
  // to be removed ....
  game_position_x_copy(current, &node_info->gpx);

  NodeInfo * const next_node_info = &stack->nodes[stack->fill_point];
  LegalMoveList * const moves = &node_info->moves;
  const SquareSet move_set = game_position_x_legal_moves(current);
  legal_move_list_from_set(move_set, moves);
  //GamePositionX * const gp = &node_info->gp;
  //GamePositionX * const next_gp = &next_node_info->gp;
  /*
  GamePosition  gp;   
  uint64        hash; 
  */

  if (move_set == empty_square_set) {
    game_position_x_pass(current, &next_node_info->gpx);
    const int previous_move_count = stack->nodes[stack->fill_point - 1].moves.move_count;
    const SquareSet empties = game_position_x_empties(current);
    if (empties != empty_square_set && previous_move_count != 0) {
      node = search_node_negated(game_position_solve_impl2(result, &next_node_info->gpx));
    } else {
      result->leaf_count++;
      node = search_node_new((Square) -1, game_position_x_final_value(current));
    }
  } else {
    node = search_node_new((Square) -1, -65);
    for (int i = 0; i < moves->move_count; i++) {
      const Square move = moves->squares[i];
      game_position_x_make_move(current, move, &next_node_info->gpx);
      node2 = search_node_negated(game_position_solve_impl2(result, &next_node_info->gpx));
      if (node2->value > node->value) {
        search_node_free(node);
        node = node2;
        node->move = move;
        node2 = NULL;
      } else {
        node2 = search_node_free(node2);
      }
    }
  }

  stack->fill_point--;
  return node;
}
