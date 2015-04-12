/**
 * @file
 *
 * @brief Exact solver module implementation.
 * @details It searches the end of the game for an exact outcome.
 *
 * @par exact_solver.c
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <glib.h>
#include <glib/gstdio.h>

#include "game_tree_logger.h"
#include "game_tree_utils.h"

#include "exact_solver.h"



/**
 * @cond
 */

/*
 * Internal structures.
 */

/*
 * Elements of a doubly linked list that collects moves.
 */
typedef struct MoveListElement_ {
  Square                   sq;           /**< @brief The square field. */
  uint8_t                  mobility;     /**< @brief The mobility field. */
  struct MoveListElement_ *pred;         /**< @brief A pointer to the predecesor element. */
  struct MoveListElement_ *succ;         /**< @brief A pointer to the successor element. */
} MoveListElement;

/*
 * Move list, having head, tail, and elements fields.
 *
 * Head and tail are not part of the list.
 */
typedef struct {
  MoveListElement elements[64];          /**< @brief Elements array. */
  MoveListElement head;                  /**< @brief Head element, it is not part of the list. */
  MoveListElement tail;                  /**< @brief Tail element, it is not part of the list. */
} MoveList;



/*
 * Prototypes for internal functions.
 */

static void
sort_moves_by_mobility_count (MoveList *move_list,
                              const GamePosition *const gp);

static SearchNode *
game_position_solve_impl (ExactSolution *const result,
                          const GamePosition  *const gp,
                          const int achievable,
                          const int cutoff,
                          PVCell ***pve_parent_line_p);

static void
move_list_init (MoveList *ml);

/*
 * Internal variables and constants.
 */

/* Principal Variation Environmenat. */
static PVEnv *pve = NULL;

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

/* Used d to sort the legal moves based on an heuristic knowledge. */
static const uint64_t legal_moves_priority_mask[] = {
  /* D4, E4, E5, D5 */                 0x0000001818000000,
  /* A1, H1, H8, A8 */                 0x8100000000000081,
  /* C1, F1, F8, C8, A3, H3, H6, A6 */ 0x2400810000810024,
  /* C3, F3, F6, C6 */                 0x0000240000240000,
  /* D1, E1, E8, D8, A4, H4, H5, A5 */ 0x1800008181000018,
  /* D3, E3, E6, D6, C4, F4, F5, C5 */ 0x0000182424180000,
  /* D2, E2, E7, D7, B4, G4, G5, B5 */ 0x0018004242001800,
  /* C2, F2, F7, C7, B3, G3, G6, B6 */ 0x0024420000422400,
  /* B1, G1, G8, B8, A2, H2, H7, A7 */ 0x4281000000008142,
  /* B2, G2, G7, B7 */                 0x0042000000004200
};

/* The size of the legal_moves_priority_mask array. */
static const int legal_moves_priority_cluster_count =
  sizeof(legal_moves_priority_mask) / sizeof(legal_moves_priority_mask[0]);

/**
 * @endcond
 */



/*********************************************************/
/* Function implementations for the GamePosition entity. */
/*********************************************************/

/**
 * @brief Solves the game position returning a new exact solution pointer.
 *
 * @param [in] root     the starting game position to be solved
 * @param [in] log_file if not null turns logging on the given file name
 * @return              a pointer to a new exact solution structure
 */
ExactSolution *
game_position_solve (const GamePosition *const root,
                     const gchar *const log_file)
{
  ExactSolution *result;
  SearchNode    *sn;

  log_env = game_tree_log_init(log_file);

  pve = pve_new(game_position_empty_count(root));
  PVCell **pve_root_line = pve_line_create(pve);

  if (log_env->log_is_on) {
    gp_hash_stack[0] = 0;
    game_tree_log_open_h(log_env);
  }

  result = exact_solution_new();

  result->solved_game_position = game_position_clone(root);

  sn = game_position_solve_impl(result, result->solved_game_position, -64, +64, &pve_root_line);

  if (sn) {
    result->pv[0] = sn->move;
    result->outcome = sn->value;
    pve_line_copy_to_exact_solution(pve, (const PVCell **const) pve_root_line, result);
    exact_solution_compute_final_board(result);
  }
  search_node_free(sn);
  pve_free(pve);

  game_tree_log_close(log_env);

  return result;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

/*
 * Sorts moves in ascending order of mobility.
 */
static void
sort_moves_by_mobility_count (MoveList *move_list,
                              const GamePosition *const gp)
{
  MoveListElement *curr = NULL;
  int move_index = 0;
  const SquareSet moves = game_position_legal_moves(gp);
  SquareSet moves_to_search = moves;
  for (int i = 0; i < legal_moves_priority_cluster_count; i++) {
    moves_to_search = legal_moves_priority_mask[i] & moves;
    while (moves_to_search) {
      curr = &move_list->elements[move_index];
      const Square move = bit_works_bitscanLS1B_64(moves_to_search);
      moves_to_search &= ~(1ULL << move);
      GamePosition *next_gp = game_position_make_move(gp, move);
      const SquareSet next_moves = game_position_legal_moves(next_gp);
      game_position_free(next_gp);
      const int next_move_count = bit_works_popcount(next_moves);
      curr->sq = move;
      curr->mobility = next_move_count;
      for (MoveListElement *element = move_list->head.succ; element != NULL; element = element->succ) {
        if (curr->mobility < element->mobility) { /* Insert current before element. */
          MoveListElement *left  = element->pred;
          MoveListElement *right = element;
          curr->pred  = left;
          curr->succ  = right;
          left->succ  = curr;
          right->pred = curr;
          goto out;
        }
      }
    out:
      move_index++;
    }
  }
  return;
}

/*
 * Main recursive search function.
 */
static SearchNode *
game_position_solve_impl (ExactSolution *const result,
                          const GamePosition  *const gp,
                          const int achievable,
                          const int cutoff,
                          PVCell ***pve_parent_line_p)
{
  result->node_count++;
  SearchNode *node  = NULL;
  SearchNode *node2 = NULL;
  PVCell **pve_line = NULL;

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

  const SquareSet moves = game_position_legal_moves(gp);
  if (0ULL == moves) {
    pve_line = pve_line_create(pve);
    GamePosition *flipped_players = game_position_pass(gp);
    if (game_position_has_any_legal_move(flipped_players)) {
      node = search_node_negated(game_position_solve_impl(result, flipped_players, -cutoff, -achievable, &pve_line));
    } else {
      result->leaf_count++;
      node = search_node_new(pass_move, game_position_final_value(gp));
    }
    pve_line_add_move(pve, pve_line, pass_move);
    pve_line_delete(pve, *pve_parent_line_p);
    *pve_parent_line_p = pve_line;
    game_position_free(flipped_players);
  } else {
    MoveList move_list;
    move_list_init(&move_list);
    sort_moves_by_mobility_count(&move_list, gp);
    for (MoveListElement *element = move_list.head.succ; element != &move_list.tail; element = element->succ) {
      const Square move = element->sq;
      if (!node) node = search_node_new(move, achievable);
      GamePosition *gp2 = game_position_make_move(gp, move);
      pve_line = pve_line_create(pve);
      node2 = search_node_negated(game_position_solve_impl(result, gp2, -cutoff, -node->value, &pve_line));
      game_position_free(gp2);
      if (node2->value > node->value) {
        search_node_free(node);
        node = node2;
        node->move = move;
        node2 = NULL;
        pve_line_add_move(pve, pve_line, move);
        pve_line_delete(pve, *pve_parent_line_p);
        *pve_parent_line_p = pve_line;
        if (node->value >= cutoff) goto out;
      } else {
        if (node2->value == node->value) {
          ; // We have to register that the move is discarded, but equivalent.
        }
        search_node_free(node2);
        pve_line_delete(pve, pve_line);
      }
    }
  }
 out:
  if (log_env->log_is_on) {
    gp_hash_stack_fill_point--;
  }
  return node;
}

/*
 * Initializes the move list.
 */
static void
move_list_init (MoveList *ml)
{
  for (int i = 0; i < 64; i++) {
    ml->elements[i].sq = -1;
    ml->elements[i].mobility = -1;
    ml->elements[i].pred = NULL;
    ml->elements[i].succ = NULL;
  }
  ml->head.sq = -1;
  ml->head.mobility = -1;
  ml->head.pred = NULL;
  ml->head.succ = &ml->tail;
  ml->tail.sq = -1;
  ml->tail.mobility = -1;
  ml->tail.pred = &ml->head;
  ml->tail.succ = NULL;
}

/**
 * @endcond
 */
