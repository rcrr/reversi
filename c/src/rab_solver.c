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

#include "utils.h"
#include "game_tree_logger.h"

#include "rab_solver.h"

/**
 * @brief Game tree stack size.
 *
 * @details It give the size of the static stack used to pile-up the info
 * computed by deepening the game tree.
 * The value is given by the 60 plus 12 moves added to take into account the possibility
 * to pass. Real tree depth are smaller because the number of pass is little,
 * and because there is no capability currently to search so deep.
 */
#define GAME_TREE_MAX_DEPTH 72

/**
 * @brief Max number of legal moves hosted in the stack.
 *
 * @details A game has 60 moves, pass moves are not consuming the stack.
 * Every game stage has been assessed with the random game generator, and a distribution of legal moves
 * has been computed. The maximum for each game stage has been summed up totalling 981.
 * the value 1024 is a further safety added in order to prevent running out of space.
 */
#define MAX_LEGAL_MOVE_STACK_COUNT 1024


/**
 * @brief The info collected on each node.
 */
typedef struct {
  GamePositionX  gpx;                         /**< @brief The game position related to the game tree node. */
  uint64         hash;                        /**< @brief The hash value of the game position. */
  SquareSet      move_set;                    /**< @brief The set of legal moves. */
  int            move_count;                  /**< @brief The count of legal moves. */
  uint8*         head_of_legal_move_list;     /**< @brief A poiter to the first legal move. */
  Square         best_move;                   /**< @brief The best move for the node. */
  int            alpha;                       /**< @brief The node value. */
  int            beta;                        /**< @brief The node cutoff value. */
} NodeInfo;

/**
 * @brief The info collected by deepening the game tree.
 *
 * @details The stack uses 5 kB of memory.
 */
typedef struct {
  int      fill_index;                                     /**< @brief The index of the current entry into the stack, at the beginning of game_position_solve_impl. */
  NodeInfo nodes[GAME_TREE_MAX_DEPTH];                     /**< @brief The stack of node info. */
  uint8    legal_move_stack[MAX_LEGAL_MOVE_STACK_COUNT];   /**< @brief The stack hosting the legal moves for each node. */
} GameTreeStack;


/*
 * Prototypes for internal functions.
 */

static GameTreeStack*
game_tree_stack_new();

static GameTreeStack*
game_tree_stack_free(GameTreeStack* stack);

static void
game_position_solve_impl(ExactSolution* const result,
                         GameTreeStack* const stack,
                         const int sub_run_id);

static void
game_tree_stack_init(const GamePosition* const root,
                     GameTreeStack* const stack);

inline static void
legal_move_list_from_set(const SquareSet legal_move_set,
                         NodeInfo* const current_node_info,
                         NodeInfo* const next_node_info);



/*
 * Internal variables and constants.
 */

/**
 * @brief The logging environment structure.
 */
static LogEnv *log_env = NULL;

/**
 * @brief A null move is an invalid one.
 */
static const Square null_move = -1;

/**
 * @brief An out of range defeat score is a value lesser than the worst case.
 */
static const int out_of_range_defeat_score = -65;

/**
 * @brief The best score achievable.
 */
static const int best_score = +64;

/**
 * @brief The worst score achievable.
 */
static const int worst_score = -64;



/*********************************************************/
/* Function implementations for the GamePosition entity. */
/*********************************************************/

/**
 * @brief Solves the game position returning a new exact solution pointer.
 *
 * @param [in] root     the starting game position to be solved
 * @param [in] log_file if not null turns logging on the given file name
 * @param [in] repeats  number of repetitions
 * @return              a pointer to a new exact solution structure
 */
ExactSolution*
game_position_rab_solve(const GamePosition * const root,
                        const gchar        * const log_file,
                        const int                  repeats)
{
  ExactSolution* result = NULL;
  int n;
  
  if (repeats < 1) {
    n = 1;
  } else {
    n = repeats;
  }

  log_env = game_tree_log_init(log_file);

  if (log_env->log_is_on) {
    game_tree_log_open_h(log_env);
  }

  utils_init_random_seed();

  int game_value = out_of_range_defeat_score;
  Square best_move = null_move;
  
  for (int sub_run_id = 0; sub_run_id < n; sub_run_id++) {
    GameTreeStack* stack = game_tree_stack_new();

    game_tree_stack_init(root, stack);
    NodeInfo* first_node_info = &stack->nodes[1];

    result = exact_solution_new();
    result->solved_game_position = game_position_clone(root);

    game_position_solve_impl(result, stack, sub_run_id);

    best_move = first_node_info->best_move;
    game_value = first_node_info->alpha;

    game_tree_stack_free(stack);
  }

  game_tree_log_close(log_env);

  result->pv[0] = best_move;
  result->outcome = game_value;
  return result;
}



/*
 * Internal functions.
 */

/**********************************************************/
/* Function implementations for the GameTreeStack entity. */
/**********************************************************/

/**
 * @brief GameTreeStack structure constructor.
 *
 * @return a pointer to a new game tree stack structure
 */
GameTreeStack*
game_tree_stack_new()
{
  GameTreeStack* stack;
  static const size_t size_of_stack = sizeof(GameTreeStack);

  stack = (GameTreeStack*) malloc(size_of_stack);
  g_assert(stack);

  return stack;
}

/**
 * @brief GameTreeStack structure destructor.
 *
 * @invariant Parameter `stack` cannot be `NULL`.
 * The invariant is guarded by an assertion.
 *
 * @param [in] stack the pointer to be deallocated
 * @return           always the NULL pointer
 */
GameTreeStack*
game_tree_stack_free(GameTreeStack* stack)
{
  g_assert(stack);

  free(stack);
  stack = NULL;

  return stack;
}

/**
 * @brief Initializes the stack structure.
 */
static void
game_tree_stack_init(const GamePosition*   const root,
                     GameTreeStack* const stack)
{
  NodeInfo* ground_node_info = &stack->nodes[0];
  game_position_x_copy_from_gp(root, &ground_node_info->gpx);
  ground_node_info->gpx.player = player_opponent(ground_node_info->gpx.player);
  ground_node_info->hash = game_position_x_hash(&ground_node_info->gpx);
  ground_node_info->move_set = 0ULL;
  ground_node_info->move_count = 0;
  ground_node_info->head_of_legal_move_list = &stack->legal_move_stack[0];
  ground_node_info->best_move = null_move;
  ground_node_info->alpha = out_of_range_defeat_score;
  ground_node_info->beta = out_of_range_defeat_score;

  NodeInfo* first_node_info  = &stack->nodes[1];
  game_position_x_copy_from_gp(root, &first_node_info->gpx);
  first_node_info->head_of_legal_move_list = &stack->legal_move_stack[0];
  first_node_info->alpha = worst_score;
  first_node_info->beta = best_score;

  stack->fill_index = 1;
}

/**
 * @brief Computes the legal move list given the set.
 *
 * @param [in]  legal_move_set    the set of legal moves
 * @param [out] current_node_info the node info updated with the compuetd list of legal moves
 * @param [out] next_node_info    the node info updated with the new head_of_legal_move_list poiter
 */
inline static void
legal_move_list_from_set(const SquareSet legal_move_set,
                         NodeInfo* const current_node_info,
                         NodeInfo* const next_node_info)
{
  uint8* move_ptr = current_node_info->head_of_legal_move_list;
  SquareSet remaining_moves = legal_move_set;
  current_node_info->move_count = 0;
  while (remaining_moves) {
    const uint8 move = bit_works_bitscanLS1B_64(remaining_moves);
    *move_ptr = move;
    move_ptr++;
    current_node_info->move_count++;
    remaining_moves ^= 1ULL << move;
  }
  next_node_info->head_of_legal_move_list = move_ptr;
  return;
}

/**
 * @brief Recursive function used to traverse the game tree.
 *
 * @param [in] result  a reference to the exact solution data structure
 */
static void
game_position_solve_impl(ExactSolution* const result,
                         GameTreeStack* const stack,
                         const int sub_run_id)
{
  result->node_count++;

  const int current_fill_index = stack->fill_index;
  const int next_fill_index = current_fill_index + 1;
  const int previous_fill_index = current_fill_index - 1;

  stack->fill_index++;

  NodeInfo* const current_node_info = &stack->nodes[current_fill_index];
  NodeInfo* const next_node_info = &stack->nodes[next_fill_index];
  NodeInfo* const previous_node_info = &stack->nodes[previous_fill_index];
  const GamePositionX* const current_gpx = &current_node_info->gpx;
  GamePositionX* const next_gpx = &next_node_info->gpx;
  current_node_info->hash = game_position_x_hash(current_gpx);
  const SquareSet move_set = game_position_x_legal_moves(current_gpx);
  legal_move_list_from_set(move_set, current_node_info, next_node_info);
  utils_shuffle_uint8(current_node_info->head_of_legal_move_list, current_node_info->move_count);
  
  if (log_env->log_is_on) {
    LogDataH log_data;
    log_data.sub_run_id = sub_run_id;
    log_data.call_id = result->node_count;
    log_data.hash = current_node_info->hash;
    log_data.parent_hash = previous_node_info->hash;
    log_data.blacks = current_gpx->blacks;
    log_data.whites = current_gpx->whites;
    log_data.player = current_gpx->player;
    log_data.json_doc = "\"{}\"";
    game_tree_log_write_h(log_env, &log_data);
  }

  if (move_set == empty_square_set) {
    const int previous_move_count = previous_node_info->move_count;
    const SquareSet empties = game_position_x_empties(current_gpx);
    if (empties != empty_square_set && previous_move_count != 0) {
      game_position_x_pass(current_gpx, next_gpx);
      next_node_info->alpha = -current_node_info->beta;
      next_node_info->beta = -current_node_info->alpha;
      game_position_solve_impl(result, stack, sub_run_id);
      current_node_info->alpha = -next_node_info->alpha;
      current_node_info->best_move = next_node_info->best_move;
    } else {
      result->leaf_count++;
      current_node_info->alpha = game_position_x_final_value(current_gpx);
      current_node_info->best_move = null_move;
    }
  } else {
    current_node_info->alpha = out_of_range_defeat_score;
    for (int i = 0; i < current_node_info->move_count; i++) {
      const Square move = * (current_node_info->head_of_legal_move_list + i);
      game_position_x_make_move(current_gpx, move, next_gpx);
      next_node_info->alpha = -current_node_info->beta;
      next_node_info->beta = -current_node_info->alpha;
      game_position_solve_impl(result, stack, sub_run_id);
      if (-next_node_info->alpha > current_node_info->alpha) {
        current_node_info->alpha = -next_node_info->alpha;
        current_node_info->best_move = move;
        if (current_node_info->alpha >= current_node_info->beta) {
          goto out;
        }
      }
    }
  }
out:
  stack->fill_index--;
  return;
}
