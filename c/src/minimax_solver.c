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
 * @copyright 2014, 2016 Roberto Corradini. All rights reserved.
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
#include <stdbool.h>

#include <glib.h>

#include "game_tree_logger.h"
#include "minimax_solver.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static SearchNode *
game_position_solve_impl (ExactSolution *const result,
                          const GamePosition *const gp);

static SearchNode *
game_position_solve_impl2 (ExactSolution *const result,
                           const GamePositionX *const gpx,
                           const Square prev_move);



/*
 * Internal variables and constants.
 */

/* The logging environment structure. */
static LogEnv *log_env = NULL;

/* The total number of call to the recursive function that traverse the game DAG. */
static uint64_t call_count = 0;

/* The predecessor-successor array of game position hash values. */
static uint64_t gp_hash_stack[128];

/* The index of the last entry into gp_hash_stack. */
static int gp_hash_stack_fill_point = 0;

/* The sub_run_id used for logging. */
static const int sub_run_id = 0;

/**
 * @endcond
 */



/*********************************************************/
/* Function implementations for the GamePosition entity. */
/*********************************************************/

/**
 * @brief Solves the game position returning a new exact solution pointer.
 *
 * @param [in] root the starting game position to be solved
 * @param [in] env      parameter envelope
 * @return          a pointer to a new exact solution structure
 */
ExactSolution *
game_position_minimax_solve (const GamePositionX *const root,
                             const endgame_solver_env_t *const env)
{
  g_assert(root);
  g_assert(env);

  log_env = game_tree_log_init(env->log_file);

  if (log_env->log_is_on) {
    GamePosition *ground = game_position_new(board_new(root->blacks,
                                                       root->whites),
                                             player_opponent(root->player));
    gp_hash_stack[0] = game_position_hash(ground);
    game_position_free(ground);
    game_tree_log_open_h(log_env);
  }

  ExactSolution *result = exact_solution_new();

  /* Has to be fixed .... */
  GamePosition *const root_gp = game_position_x_gpx_to_gp(root);
  result->solved_game_position = game_position_clone(root_gp);
  free(root_gp);

  SearchNode *sn = game_position_solve_impl(result, result->solved_game_position);

  result->pv[0] = sn->move;
  result->outcome = sn->value;
  search_node_free(sn);


  // --- 1

  ExactSolution *result2 = exact_solution_new();

  /* Has to be fixed .... */
  GamePosition *const root_gp2 = game_position_x_gpx_to_gp(root);
  result2->solved_game_position = game_position_clone(root_gp2);
  free(root_gp2);

  SearchNode *sn2 = game_position_solve_impl2(result2, root, invalid_move);

  result2->pv[0] = sn2->move;
  result2->outcome = sn2->value;
  search_node_free(sn2);

  printf("result2->pv[0]=%s, result2->outcome=%d\n", square_as_move_to_string(result2->pv[0]), result2->outcome);

  // --- 2

  game_tree_log_close(log_env);

  return result;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

static void
generate_move_array (int *move_count,
                     Square *moves,
                     SquareSet* move_set,
                     const GamePositionX *const gpx)
{
  Square *move = moves;
  SquareSet remaining_moves = game_position_x_legal_moves(gpx);
  *move_set = remaining_moves;
  if (!remaining_moves) {
    *move++ = pass_move;
  }
  while (remaining_moves) {
    const Square m = bit_works_bitscanLS1B_64(remaining_moves);
    remaining_moves ^= 1ULL << m;
    *move++ = m;
  }
  *move_count = move - moves;
}

static bool
is_terminal_node (const Square prev_move,
                  const SquareSet move_set)
{
  if (!move_set && (prev_move == pass_move)) return true;
  return false;
}

/*
 * TODO:
 * - Remove every dependency on GamePosition and Board objects.
 * - Remove pass special case.
 * - Avoid malloc/free calls.
 * - Use the game_tree_utils stack.
 * - Use the most advanced Kogge-Stone routines.
 * - Transform the code from recursion to iteration.
 */
static SearchNode *
game_position_solve_impl2 (ExactSolution *const result,
                           const GamePositionX *const gpx,
                           const Square prev_move)
{
  /*
   *
   *   01 function negamax(node, depth, color)
   *   02     if depth = 0 or node is a terminal node
   *   03         return color * the heuristic value of node
   *
   *   04     bestValue := −∞
   *   05     foreach child of node
   *   06         v := −negamax(child, depth − 1, −color)
   *   07         bestValue := max( bestValue, v )
   *   08     return bestValue
   *
   */

  Square *move;
  SquareSet move_set;
  int move_count;
  Square moves[32];
  GamePositionX child_gpx;

  result->node_count++;
  SearchNode *node = search_node_new(invalid_move, out_of_range_defeat_score);

  generate_move_array(&move_count, moves, &move_set, gpx);

  if (is_terminal_node(prev_move, move_set)) {
    node->value = game_position_x_final_value(gpx);
    return node;
  }

  for (move = moves; move - moves < move_count; move++) {
    if (*move == pass_move) {
      game_position_x_pass(gpx, &child_gpx);
    } else {
      game_position_x_make_move(gpx, *move, &child_gpx);
    }
    SearchNode *tmp_node = search_node_negated(game_position_solve_impl2(result, &child_gpx, *move));
    if (tmp_node->value > node->value) {
      search_node_free(node);
      node = tmp_node;
      node->move = *move;
      tmp_node = NULL;
    } else {
      search_node_free(tmp_node);
    }
  }
  return node;
}

/**
 * @brief Recursive function used to traverse the game tree.
 *
 * @param [in] result a reference to the exact solution data structure
 * @param [in] gp     the game position to traverse
 * @return            a pointer to a new serch node structure
 */
static SearchNode *
game_position_solve_impl (ExactSolution *const result,
                          const GamePosition *const gp)
{
  SearchNode *node  = NULL;
  SearchNode *node2 = NULL;

  result->node_count++;

  const SquareSet moves = game_position_legal_moves(gp);

  if (log_env->log_is_on) {
    call_count++;
    gp_hash_stack_fill_point++;
    LogDataH log_data;
    log_data.sub_run_id = 0;
    log_data.call_id = call_count;
    log_data.hash = game_position_hash(gp);
    gp_hash_stack[gp_hash_stack_fill_point] = log_data.hash;
    log_data.parent_hash = gp_hash_stack[gp_hash_stack_fill_point - 1];
    log_data.blacks = (gp->board)->blacks;
    log_data.whites = (gp->board)->whites;
    log_data.player = gp->player;
    gchar *json_doc = game_tree_log_data_h_json_doc(gp_hash_stack_fill_point, gp);
    log_data.json_doc = json_doc;
    game_tree_log_write_h(log_env, &log_data);
    g_free(json_doc);
  }

  if (moves == empty_square_set) {
    GamePosition *flipped_players = game_position_pass(gp);
    if (game_position_has_any_legal_move(flipped_players)) {
      node = search_node_negated(game_position_solve_impl(result, flipped_players));
    } else {
      result->leaf_count++;
      node = search_node_new(pass_move, game_position_final_value(gp));
    }
    game_position_free(flipped_players);
  } else {
    // -- to be removed -start 1
    SquareSet move_set;
    int move_count;
    Square move_array[32];
    GamePositionX *gpx = game_position_x_gp_to_gpx(gp);
    generate_move_array(&move_count, move_array, &move_set, gpx);
    Square *move_2 = move_array;
    if (move_set != moves) abort();
    if (bit_works_bitcount_64_popcnt(moves) != move_count) abort();
    // -- to be removed -end 1
    node = search_node_new(-1, -65);
    SquareSet remaining_moves = moves;
    while (remaining_moves) {
      const Square move = bit_works_bitscanLS1B_64(remaining_moves);
      if (move != *move_2++) abort(); // -- to be removed 2
      remaining_moves ^= 1ULL << move;
      GamePosition *gp2 = game_position_make_move(gp, move);
      node2 = search_node_negated(game_position_solve_impl(result, gp2));
      game_position_free(gp2);
      if (node2->value > node->value) {
        search_node_free(node);
        node = node2;
        node->move = move;
        node2 = NULL;
      } else {
        search_node_free(node2);
      }
    }
  }

  if (log_env->log_is_on) {
    gp_hash_stack_fill_point--;
  }

  return node;
}

/**
 * @endcond
 */
