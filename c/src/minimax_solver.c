/**
 * @file
 *
 * @brief Minimax solver module implementation.
 * @details It searches the end of the game for an exact outcome using the MINIMAX algorithm.
 *
 * @par minimax_solver.c
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

#include "minimax_solver.h"



/*
 * Prototypes for internal functions.
 */

static SearchNode *
game_position_solve_impl (      ExactSolution * const result,
                          const GamePosition  * const gp);



/*
 * Internal variables and constants.
 */

/**
 * @brief The log file used to record the game DAG traversing.
 */
static FILE *game_tree_log_file = NULL;

/**
 * @brief True if the module logs to file.
 */
static gboolean log = FALSE;

/**
 * @brief The total number of call to the recursive function that traverse the game DAG.
 */
static uint64 call_count = 0;

/**
 * @brief The predecessor-successor array of game position hash values.
 */
static uint64 gp_hash_stack[128];

/**
 * @brief The index of the last entry into gp_hash_stack.
 */
static int gp_hash_stack_fill_point = 0;

/**
 * @brief The sub_run_id used for logging.
 */
static const int sub_run_id = 0;


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
game_position_minimax_solve (const GamePosition * const root)
{
  ExactSolution *result; 
  SearchNode    *sn;

  log = TRUE;

  if (log) {
    GamePosition *ground = game_position_new(board_new(root->board->blacks, root->board->whites), player_opponent(root->player));
    gp_hash_stack[0] = game_position_hash(ground);
    game_position_free(ground);
    game_tree_log_file = fopen("out/minimax_log.csv", "w");
    fprintf(game_tree_log_file, "%s;%s;%s;%s;%s;%s;%s\n",
            "SUB_RUN_ID",
            "CALL_ID",
            "HASH",
            "PARENT_HASH",
            "BLACKS",
            "WHITES",
            "PLAYER");
  }

  result = exact_solution_new();

  result->solved_game_position = game_position_clone(root);

  sn = game_position_solve_impl(result, result->solved_game_position);

  result->principal_variation[0] = sn->move;
  result->outcome = sn->value;
  sn = search_node_free(sn);

  if (log) {
    fclose(game_tree_log_file);
  }

  return result;
}



/*
 * Internal functions.
 */

/**
 * @brief Recursive function used to traverse the game tree.
 *
 * @param [in] result a reference to the exact solution data structure
 * @param [in] gp     the game position to traverse
 * @return            a pointer to a new serch node structure
 */
static SearchNode *
game_position_solve_impl (      ExactSolution * const result,
                          const GamePosition  * const gp)
{
  SearchNode *node;
  SearchNode *node2;

  node  = NULL;
  node2 = NULL;
  result->node_count++;

  const SquareSet moves = game_position_legal_moves(gp);

  if (log) {
    call_count++;
    gp_hash_stack_fill_point++;
    const uint64 hash = game_position_hash(gp);
    gp_hash_stack[gp_hash_stack_fill_point] = hash;
    const sint64 hash_to_signed = (sint64) hash;
    const sint64 previous_hash_to_signed = (sint64) gp_hash_stack[gp_hash_stack_fill_point - 1];
    const Board  *current_board = gp->board;
    const sint64 *blacks_to_signed = (sint64 *) &current_board->blacks;
    const sint64 *whites_to_signed = (sint64 *) &current_board->whites;
    fprintf(game_tree_log_file, "%6d;%8llu;%+20lld;%+20lld;%+20lld;%+20lld;%1d\n",
            sub_run_id,
            call_count,
            hash_to_signed,
            previous_hash_to_signed,
            *blacks_to_signed,
            *whites_to_signed,
            gp->player);
  }

  if (moves == empty_square_set) {
    GamePosition *flipped_players = game_position_pass(gp);
    if (game_position_has_any_legal_move(flipped_players)) {
      node = search_node_negated(game_position_solve_impl(result, flipped_players));
    } else {
      result->leaf_count++;
      node = search_node_new((Square) -1, game_position_final_value(gp));
    }
    flipped_players = game_position_free(flipped_players);
  } else {
    node = search_node_new(-1, -65);
    SquareSet remaining_moves = moves;
    while (remaining_moves) {
      const Square move = bit_works_bitscanLS1B_64(remaining_moves);
      remaining_moves ^= 1ULL << move;
      GamePosition *gp2 = game_position_make_move(gp, move);
      node2 = search_node_negated(game_position_solve_impl(result, gp2));
      gp2 = game_position_free(gp2);
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

  if (log) {
    gp_hash_stack_fill_point--;
  }

  return node;
}
