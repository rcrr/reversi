/**
 * @file
 *
 * @brief Exact solver module implementation.
 * @details It searches the end of the game for an exact outcome.
 *
 * @par exact_solver2.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014, 2016 Roberto Corradini. All rights reserved.
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
#include <assert.h>
#include <string.h>
#include <stdbool.h>

#include "game_tree_logger.h"
#include "game_tree_utils.h"

#include "exact_solver2.h"



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
  SquareSet                moves;        /**< @brief The move set. */
  GamePositionX            gpx;          /**< @brief The game position. */
  struct MoveListElement_ *next;         /**< @brief A pointer to the successor element. */
} MoveListElement;

/*
 * Move list, having head, tail, and elements fields.
 *
 * Head and tail are not part of the list.
 */
typedef struct {
  MoveListElement *head;                 /**< @brief Head element, it is not part of the list. */
  MoveListElement elements[32];          /**< @brief Elements array. */
} MoveList;



/*
 * Prototypes for internal functions.
 */

static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack,
                          PVCell ***pve_parent_line_p,
                          MoveListElement *mle);

static void
sort_moves_by_mobility_count (MoveList *ml,
                              const GamePositionX *const gpx,
                              const SquareSet moves);

/*
 * Internal variables and constants.
 */

/* Principal Variation Environmenat. */
static PVEnv *pve = NULL;

/* The logging environment structure. */
static LogEnv *log_env = NULL;

/* Drives the PV recording. */
static bool pv_recording = false;

/* Drives the analysis to consider all variants of equal value (slower, but complete).*/
static bool pv_full_recording = false;

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

/* Print debugging info ... */
static const bool pv_internals_to_stream = false;

/**
 * @endcond
 */



/*********************************************************/
/* Function implementations for the GamePosition entity. */
/*********************************************************/

/**
 * @brief Solves the game position returning a new exact solution pointer.
 *
 * @invariant Parameters `root` and `env` must be not `NULL`.
 *             The invariants are guarded by assertions.
 *
 * @param [in] root     the starting game position to be solved
 * @param [in] env      parameter envelope
 * @return              a pointer to a new exact solution structure
 */
ExactSolution *
game_position_es2_solve (const GamePositionX *const root,
                         const endgame_solver_env_t *const env)
{
  assert(root);
  assert(env);

  const GamePosition *const root_gp = game_position_x_gpx_to_gp(root);

  ExactSolution *result = exact_solution_new();
  result->solved_game_position = game_position_clone(root_gp);

  GameTreeStack *stack = game_tree_stack_new();
  game_tree_stack_init(root_gp, stack);
  NodeInfo *first_node_info = &stack->nodes[1];

  pv_recording = env->pv_recording;
  pv_full_recording = env->pv_full_recording;
  if (pv_full_recording) {
    first_node_info->alpha = out_of_range_defeat_score;
    first_node_info->beta = out_of_range_win_score;
  } else {
    first_node_info->alpha = worst_score;
    first_node_info->beta = best_score;
  }

  if (pv_recording) {
    //GamePositionX *rootx = game_position_x_gp_to_gpx(root);
    pve = pve_new(root);
    //game_position_x_free(rootx);
  }

  log_env = game_tree_log_init(env->log_file);
  if (log_env->log_is_on) {
    game_tree_log_open_h(log_env);
  }

  MoveListElement root_mle;
  root_mle.moves = game_position_x_legal_moves(root);
  game_position_solve_impl(result, stack, &(pve->root_line), &root_mle);

  if (pv_recording && pv_full_recording && !env->pv_no_print) {
    printf("\n --- --- pve_line_with_variants_to_string() START --- ---\n");
    pve_line_with_variants_to_stream(pve, stdout);
    printf("\n --- --- pve_line_with_variants_to_string() COMPLETED --- ---\n");
  }

  /* This is for debugging. */
  if (pv_recording && pv_internals_to_stream) {
    printf("\nThe constant \"pv_internals_to_stream\", in source file \"exact_solver.c\", is TRUE. Printing PVE internals:\n");
    printf(" --- --- pve_is_invariant_satisfied() START --- ---\n");
    pve_error_code_t error_code = 0;
    pve_is_invariant_satisfied(pve, &error_code, 0xFF);
    if (error_code) {
      printf("error_code=%d\n", error_code);
      abort();
    }
    printf(" --- --- pve_is_invariant_satisfied() COMPLETED --- ---\n");

    printf("\n --- --- pve_internals_to_stream() START --- ---\n");
    switches_t shown_sections = 0x0000;
    shown_sections |= pve_internals_header_section;
    shown_sections |= pve_internals_index_section;
    shown_sections |= pve_internals_properties_section;
    shown_sections |= pve_internals_structure_section;
    shown_sections |= pve_internals_computed_properties_section;
    //shown_sections |= pve_internals_active_lines_section;
    shown_sections |= pve_internals_cells_segments_section;
    shown_sections |= pve_internals_sorted_cells_segments_section;
    shown_sections |= pve_internals_cells_section;
    shown_sections |= pve_internals_cells_stack_section;
    shown_sections |= pve_internals_lines_segments_section;
    shown_sections |= pve_internals_sorted_lines_segments_section;
    shown_sections |= pve_internals_lines_section;
    shown_sections |= pve_internals_lines_stack_section;
    pve_internals_to_stream(pve, stdout, shown_sections);
    printf("\n --- --- pve_internals_to_stream() COMPLETED --- ---\n");
  }

  const int game_value = first_node_info->alpha;
  const Square best_move = first_node_info->best_move;
  game_tree_stack_free(stack);

  result->pv[0] = best_move;
  result->outcome = game_value;
  if (pv_recording) {
    pve_line_copy_to_exact_solution(pve, (const PVCell **const) pve->root_line, result);
    exact_solution_compute_final_board(result);
    if (env->pve_dump_file) {
      printf("\n --- --- pve_dump_to_binary_file() START --- ---\n");
      pve_dump_to_binary_file(pve, env->pve_dump_file);
      printf(" --- --- pve_dump_to_binary_file() COMPLETED --- ---\n");
    }
    pve_free(pve);
  }

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
 * TODO
 * What to try:
 * - Make a stack of moves with pointers ...... integrated into the tree stack .....
 */
static void
sort_moves_by_mobility_count (MoveList *ml,
                              const GamePositionX *const gpx,
                              const SquareSet moves)
{
  assert(game_position_x_legal_moves(gpx) == moves);

  ml->head = NULL;
  MoveListElement *e = ml->elements;

  SquareSet moves_to_search = moves;
  for (int i = 0; i < legal_moves_priority_cluster_count; i++) {
    moves_to_search = legal_moves_priority_mask[i] & moves;
    while (moves_to_search) {
      const Square move = bit_works_bitscanLS1B_64_bsf(moves_to_search);
      moves_to_search &= ~(1ULL << move);
      game_position_x_make_move(gpx, move, &e->gpx);
      const SquareSet next_moves = game_position_x_legal_moves(&e->gpx);
      const int next_move_count = bit_works_bitcount_64_popcnt(next_moves);
      e->sq = move;
      e->mobility = next_move_count;
      e->moves = next_moves;

      MoveListElement **epp = &(ml->head);
      if (!*epp) {
        ml->head = e;
        e->next = NULL;
        goto out;
      }
      while (true) {
        if (e->mobility < (*epp)->mobility) { /* Insert e before cursor. */
          e->next = *epp;
          *epp = e;
          goto out;
        }
        if ((*epp)->next == NULL) {
          (*epp)->next = e;
          e->next = NULL;
          goto out;
        }
        epp = &((*epp)->next);
      }
    out:
      e++;
    }
  }

  return;
}


/*
 * TODO
 * What to try:
 * - Avoid recursion .... write a "compact" iterative function.
 * - Avoid the special case of PASSING .....
 */
static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack,
                          PVCell ***pve_parent_line_p,
                          MoveListElement *mle)
{
  result->node_count++;
  PVCell **pve_line = NULL;

  const int current_fill_index = stack->fill_index;
  const int next_fill_index = current_fill_index + 1;
  const int previous_fill_index = current_fill_index - 1;
  const int sub_run_id = 0;

  stack->fill_index++;

  NodeInfo *const current_node_info = &stack->nodes[current_fill_index];
  NodeInfo *const next_node_info = &stack->nodes[next_fill_index];
  NodeInfo *const previous_node_info = &stack->nodes[previous_fill_index];
  const GamePositionX *const current_gpx = &current_node_info->gpx;
  GamePositionX *const next_gpx = &next_node_info->gpx;
  const SquareSet move_set = mle->moves;
  current_node_info->move_count = bit_works_bitcount_64_popcnt(move_set);

  if (log_env->log_is_on) {
    current_node_info->hash = game_position_x_hash(current_gpx);
    LogDataH log_data;
    log_data.sub_run_id = sub_run_id;
    log_data.call_id = result->node_count;
    log_data.hash = current_node_info->hash;
    log_data.parent_hash = previous_node_info->hash;
    log_data.blacks = current_gpx->blacks;
    log_data.whites = current_gpx->whites;
    log_data.player = current_gpx->player;
    gchar *json_doc = game_tree_log_data_h_json_doc2(current_fill_index, current_gpx);
    log_data.json_doc = json_doc;
    game_tree_log_write_h(log_env, &log_data);
    g_free(json_doc);
  }

  if (move_set == empty_square_set) {
    if (pv_recording) pve_line = pve_line_create(pve);
    const int previous_move_count = previous_node_info->move_count;
    //const SquareSet empties = game_position_x_empties(current_gpx); // TODO:
    //if (empties != empty_square_set && previous_move_count != 0) {
    if (previous_move_count != 0) {
      game_position_x_pass(current_gpx, next_gpx);
      next_node_info->alpha = -current_node_info->beta;
      next_node_info->beta = -current_node_info->alpha;
      MoveListElement next_mle;
      next_mle.moves = game_position_x_legal_moves(next_gpx);
      game_position_solve_impl(result, stack, &pve_line, &next_mle);
      current_node_info->alpha = -next_node_info->alpha;
      current_node_info->best_move = next_node_info->best_move;
    } else {
      result->leaf_count++;
      current_node_info->alpha = game_position_x_final_value(current_gpx);
      current_node_info->best_move = pass_move;
    }
    if (pv_recording) {
      pve_line_add_move2(pve, pve_line, pass_move, next_gpx);
      pve_line_delete(pve, *pve_parent_line_p);
      *pve_parent_line_p = pve_line;
    }
  } else {
    MoveList ml;
    bool branch_is_active = false;
    sort_moves_by_mobility_count(&ml, current_gpx, move_set);
    if (pv_full_recording) current_node_info->alpha -= 1;
    for (MoveListElement *element = ml.head; element; element = element->next) {
      const Square move = element->sq;
      game_position_x_copy(&element->gpx, next_gpx);
      if (pv_recording) pve_line = pve_line_create(pve);
      next_node_info->alpha = -current_node_info->beta;
      next_node_info->beta = -current_node_info->alpha;
      game_position_solve_impl(result, stack, &pve_line, element);
      const int current_alpha = current_node_info->alpha;
      if (-next_node_info->alpha > current_alpha || (!branch_is_active && -next_node_info->alpha == current_alpha)) {
        branch_is_active = true;
        current_node_info->alpha = -next_node_info->alpha;
        current_node_info->best_move = move;
        if (pv_recording) {
          pve_line_add_move2(pve, pve_line, move, next_gpx);
          pve_line_delete(pve, *pve_parent_line_p);
          *pve_parent_line_p = pve_line;
        }
        if (current_node_info->alpha > current_node_info->beta) goto out;
        if (!pv_full_recording && current_node_info->alpha == current_node_info->beta) goto out;
      } else {
        if (pv_recording) {
          if (pv_full_recording && -next_node_info->alpha == current_alpha) {
            pve_line_add_move2(pve, pve_line, move, next_gpx);
            pve_line_add_variant(pve, *pve_parent_line_p, pve_line);
          } else {
            pve_line_delete(pve, pve_line);
          }
        }
      }
    }
  }
out:
  stack->fill_index--;
  return;
}

/**
 * @endcond
 */
