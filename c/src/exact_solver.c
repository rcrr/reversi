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
 * @copyright 2013, 2014, 2016, 2017 Roberto Corradini. All rights reserved.
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
#include "exact_solver.h"



/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack,
                          PVCell ***pve_parent_line_p);

static void
look_ahead_and_sort_moves_by_mobility_count (GameTreeStack *const stack);

/*
 * Internal variables and constants.
 */

/* Principal Variation Environmenat. */
static PVEnv *pve = NULL;

/* The logging environment structure. */
static gtl_log_env_t *log_env = NULL;

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
game_position_es_solve (const GamePositionX *const root,
                        const endgame_solver_env_t *const env)
{
  assert(root);
  assert(env);

  ExactSolution *result = exact_solution_new();
  exact_solution_set_root(result, root);

  GameTreeStack *stack = game_tree_stack_new();
  game_tree_stack_init(root, stack);
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
    pve = pve_new(root);
    result->pve = pve;
  }

  log_env = gtl_init(env->log_file);
  if (log_env->log_is_on) {
    gtl_open_h(log_env);
    stack->hash_is_on = true;
  }

  first_node_info->move_set = game_position_x_legal_moves(root);
  game_position_solve_impl(result, stack, &(pve->root_line));

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

  result->best_move = best_move;
  result->outcome = game_value;
  if (pv_recording) {
    exact_solution_compute_final_state(result);
    if (env->pve_dump_file) {
      printf("\n --- --- pve_dump_to_binary_file() START --- ---\n");
      pve_dump_to_binary_file(pve, env->pve_dump_file);
      printf(" --- --- pve_dump_to_binary_file() COMPLETED --- ---\n");
    }
  }

  gtl_close(log_env);

  return result;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

/*
 * Insertion sort.
 */
static void
sort_move_pointers (gts_mle_t ** moves,
                    const size_t count)
{
  for (size_t i = 1; i < count; i++) {
    size_t j = i;
    for (;;) {
      gts_mle_t * tmp;
      if (j == 0 || (*(moves + j - 1))->res_move_count <= (*(moves + j))->res_move_count) break;
      tmp = *(moves + j), *(moves + j) = *(moves + j - 1), *(moves + j - 1) = tmp; // Swaps elements.
      j--;
    }
  }
}

/*
 * Generates moves and sort them by mobility_count/priority_cluster.
 * Insertion sort is a stable algorithm, so generating the moves by priority cluster
 * and then sorting them is ok.
 */
static void
look_ahead_and_sort_moves_by_mobility_count (GameTreeStack *const stack)
{
  NodeInfo *const c = stack->active_node;

  gts_mle_t **mle = c->head_of_legal_move_list;

  if (c->move_set) {
    for (int i = 0; i < legal_moves_priority_cluster_count; i++) {
      SquareSet moves_to_search = c->move_set & legal_moves_priority_mask[i];
      while (moves_to_search) {
        (*mle)->move = bitw_bit_scan_forward_64(moves_to_search);
        moves_to_search &= ~(1ULL << (*mle)->move);
        game_position_x_make_move(&c->gpx, (*mle)->move, &(*mle)->res_position);
        (*mle)->res_move_set = game_position_x_legal_moves(&(*mle)->res_position);
        (*mle)->res_move_count = bitw_bit_count_64((*mle)->res_move_set);
        mle++;
      }
    }
    sort_move_pointers(c->head_of_legal_move_list, c->move_count);
  } else {
    (*mle)->move = pass_move;
    game_position_x_pass(&c->gpx, &(*mle)->res_position);
    (*mle)->res_move_set = game_position_x_legal_moves(&(*mle)->res_position);
    (*mle)->res_move_count = bitw_bit_count_64((*mle)->res_move_set);
    mle++;
  }
  (c + 1)->head_of_legal_move_list = mle;
}

static void
update_move_flips (GameTreeStack *const stack)
{
  const NodeInfo *const c = stack->active_node;
  const gts_mle_t *e = *c->move_cursor;

  Square *flip_cursor = stack->flips;

  *flip_cursor++ = e->move;
  const SquareSet bitmove = 1ULL << e->move;
  const SquareSet cu_p = game_position_x_get_player(&c->gpx);
  const SquareSet up_o = game_position_x_get_opponent(&(c + 1)->gpx);
  SquareSet flip_set = up_o & ~(cu_p | bitmove);
  while (flip_set) {
    *flip_cursor++ = bitw_bit_scan_forward_64(flip_set);
    flip_set = bitw_reset_lowest_set_bit_64(flip_set);
  }
  stack->flip_count = flip_cursor - stack->flips;
}

static bool
is_position_leaf (GameTreeStack *const stack)
{
  NodeInfo *const c = stack->active_node;
  return !(c->move_set || (c - 1)->move_set);
}



/*
 * TODO
 * What to try:
 * - Avoid recursion .... write a "compact" iterative function.
 * - Avoid the special case of PASSING .....
 *
 * ab(p, alpha, beta)
 * position p; int alpha, beta;
 * {
 *   int m, n;
 *   if p is leaf return(finalvalue(p));
 *   let p0...pn be the successors of p;
 *   for (i = 0; i < n; i++) {
 *     m = max(m, -ab(pi, -beta, -m));
 *     if (m >= beta) return(m);
 *   }
 *   return(m);
 * }
 *
 */
static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack,
                          PVCell ***pve_parent_line_p)
{
  NodeInfo *c;
  gts_mle_t *e;
  PVCell **pve_line;

  result->node_count++;
  c = ++stack->active_node;
  c->move_cursor = c->head_of_legal_move_list;
  c->move_count = bitw_bit_count_64(c->move_set);
  const int sub_run_id = 0;

  if (stack->hash_is_on) gts_compute_hash(stack);
  if (log_env->log_is_on) gtl_do_log(result, stack, sub_run_id, log_env);

  if (is_position_leaf(stack)) {
    result->leaf_count++;
    c->alpha = game_position_x_final_value(&c->gpx);
    c->best_move = pass_move;
  }

  if (!c->move_set) {
    if (pv_recording) pve_line = pve_line_create(pve);
    if ((c - 1)->move_count != 0) {
      look_ahead_and_sort_moves_by_mobility_count(stack);

      game_position_x_copy(&(*c->move_cursor)->res_position, &(c + 1)->gpx);
      (c + 1)->move_set = (*c->move_cursor)->res_move_set;
      (c + 1)->alpha = - c->beta;
      (c + 1)->beta = - c->alpha;

      if (stack->hash_is_on) update_move_flips(stack);

      game_position_solve_impl(result, stack, &pve_line);
      c->alpha = - (c + 1)->alpha;
      c->best_move = (c + 1)->best_move;
      c->move_cursor++;
    }
    if (pv_recording) {
      pve_line_add_move(pve, pve_line, pass_move, &(c + 1)->gpx);
      pve_line_delete(pve, *pve_parent_line_p);
      *pve_parent_line_p = pve_line;
    }
  } else {
    bool branch_is_active = false;
    look_ahead_and_sort_moves_by_mobility_count(stack);
    if (pv_full_recording) c->alpha -= 1;
    for ( ; c->move_cursor - c->head_of_legal_move_list < c->move_count; c->move_cursor++) {
      e = *c->move_cursor;
      game_position_x_copy(&e->res_position, &(c + 1)->gpx);
      (c + 1)->move_set = e->res_move_set;
      (c + 1)->alpha = -c->beta;
      (c + 1)->beta = -c->alpha;

      if (stack->hash_is_on) update_move_flips(stack);

      if (pv_recording) pve_line = pve_line_create(pve);

      game_position_solve_impl(result, stack, &pve_line);
      if (-(c + 1)->alpha > c->alpha || (!branch_is_active && -(c + 1)->alpha == c->alpha)) {
        branch_is_active = true;
        c->alpha = -(c + 1)->alpha;
        c->best_move = e->move;
        if (pv_recording) {
          pve_line_add_move(pve, pve_line, e->move, &(c + 1)->gpx);
          pve_line_delete(pve, *pve_parent_line_p);
          *pve_parent_line_p = pve_line;
        }
        if (c->alpha > c->beta) goto out;
        if (!pv_full_recording && c->alpha == c->beta) goto out;
      } else {
        if (pv_recording) {
          if (pv_full_recording && -(c + 1)->alpha == c->alpha) {
            pve_line_add_move(pve, pve_line, e->move, &(c + 1)->gpx);
            pve_line_add_variant(pve, *pve_parent_line_p, pve_line);
          } else {
            pve_line_delete(pve, pve_line);
          }
        }
      }
    }
  }
 out:
  stack->active_node--;
  return;
}

/**
 * @endcond
 */
