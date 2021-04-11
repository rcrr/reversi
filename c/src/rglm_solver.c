/**
 * @file
 *
 * @brief Rglm solver module implementation.
 * @details A solver using the REGAB and RGLM knowledge.
 *
 * @par rglm_solver.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2021 Roberto Corradini. All rights reserved.
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

#include "file_utils.h"
#include "cfg.h"
#include "game_tree_logger.h"
#include "rglm_solver.h"
#include "rglm_data_files.h"
#include "rglm_utils.h"

#define EC_SIZE 61

/**
 * @cond
 */

/*
 * Prototypes for internal functions.
 */

static void
game_position_solve_impl (ExactSolution *const result,
                          GameTreeStack *const stack);


static double
rglm_eval_gp (const GamePositionX *const gpx);



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

/* Default rglm_solver_config file. */
static const char *const rglm_solver_def_config_file = "./cfg/rglm_solver.cfg";

/* The RGLM solver config. */
static cfg_t *rglm_solver_config = NULL;

/* The vector of model weighs structures indexed by empty_count. */
static rglmdf_model_weights_t mws_s[EC_SIZE];

/* The vector of pointers to the mws_s structures.
 * It is a convenience, when NULL the model_weighs structure is
 * not populated.
 */
static rglmdf_model_weights_t *mws[EC_SIZE];

/* The labels of the files to be loaded. */
static const char *const mws_l[EC_SIZE] =
  {
   "ec00", "ec01", "ec02", "ec03", "ec04", "ec05", "ec06", "ec07", "ec08", "ec09",
   "ec10", "ec11", "ec12", "ec13", "ec14", "ec15", "ec16", "ec17", "ec18", "ec19",
   "ec20", "ec21", "ec22", "ec23", "ec24", "ec25", "ec26", "ec27", "ec28", "ec29",
   "ec30", "ec31", "ec32", "ec33", "ec34", "ec35", "ec36", "ec37", "ec38", "ec39",
   "ec40", "ec41", "ec42", "ec43", "ec44", "ec45", "ec46", "ec47", "ec48", "ec49",
   "ec50", "ec51", "ec52", "ec53", "ec54", "ec55", "ec56", "ec57", "ec58", "ec59",
   "ec60"
  };

/* The names of the files to be loaded. */
static const char *mws_f[EC_SIZE];

static bool mw_loaded = false;


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
game_position_rglm_solve (const GamePositionX *const root,
                          const endgame_solver_env_t *const env)
{
  ExactSolution *result;
  const bool verbose = false;
  int ret_err;
  ret_err = game_position_rglm_load_model_weights_files(verbose);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading RGLM solver model weights files. Aborting ...\n");
    abort();
  }
  const int ec = game_position_x_empty_count(root);
  const bool mw_available = mws[ec] != NULL;
  if (false) {
    if (mw_available) {
      const double estimated_game_value_transformed = rglm_eval_gp(root);
      const double estimated_game_value = rglmut_gv_scale_back_f(estimated_game_value_transformed);
      printf("RGLM solver: estimated_game_value = %f (%f)\n", estimated_game_value, estimated_game_value_transformed);
    } else {
      printf("RGLM solver: model weights for the game position is not available, ec=%d\n", ec);
    }
  }
  result = game_position_rglm_solve_nlmw(root, env);
  game_position_rglm_release_model_weights();
  return result;
}


/**
 * @brief Solves the game position returning a new exact solution pointer.
 *        No Load Model Weighs
 *
 * @invariant Parameters `root` and `env` must be not `NULL`.
 *             The invariants are guarded by assertions.
 *
 * @param [in] root     the starting game position to be solved
 * @param [in] env      parameter envelope
 * @return              a pointer to a new exact solution structure
 */
ExactSolution *
game_position_rglm_solve_nlmw (const GamePositionX *const root,
                               const endgame_solver_env_t *const env)
{
  assert(root);
  assert(env);

  ExactSolution *result = exact_solution_new();
  exact_solution_init(result);
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
    first_node_info->alpha = env->alpha;
    first_node_info->beta = env->beta;
  }

  result->all_legal_moves_solved = env->all_moves;

  if (pv_recording) {
    pve = pve_new(root);
    result->pve = pve;
  }

  log_env = gtl_init(env->log_file);
  if (log_env->log_is_on) {
    gtl_open_log(log_env);
    stack->hash_is_on = true;
  }

  NodeInfo *const c = stack->active_node;
  if (pv_recording) c->pve_line = pve->root_line;
  first_node_info->move_set = game_position_x_legal_moves(root);
  result->legal_move_count = bitw_bit_count_64_popcnt(first_node_info->move_set);
  game_position_solve_impl(result, stack);
  if (pv_recording) pve->root_line = c->pve_line;

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

  exact_solution_sort_legal_moves_by_value(result);

  gtl_close_log(log_env);

  return result;
}

int
game_position_rglm_load_model_weights_files (bool verbose)
{
  int ret_value;

  if (mw_loaded) return EXIT_SUCCESS;

  /* rglm_solver_config_file should be passed as an argument, and if NULL the default should be assigned. */
  const char *rglm_solver_config_file = rglm_solver_def_config_file;
  if (!fut_file_exists(rglm_solver_config_file))
    return EXIT_FAILURE;

  rglm_solver_config = cfg_load(rglm_solver_config_file);

  for (size_t i = 0; i < EC_SIZE; i++) {
    mws_f[i] = cfg_get(rglm_solver_config, "rglm_solver", mws_l[i]);
  }

  for (size_t i = 0; i < EC_SIZE; i++) {
    rglmdf_model_weights_init(mws_s);
    mws[i] = NULL;
  }

  for (size_t i = 0; i < EC_SIZE; i++) {
    const char *filename = mws_f[i];
    if (filename) {
      ret_value = rglmdf_model_weights_read_from_binary_file(&mws_s[i], filename, verbose);
      if (ret_value != 0) {
        fprintf(stderr, "Unable to load model weight file \"%s\"\n", filename);
        return ret_value;
      }
      if (mws_s[i].empty_count != i) {
        fprintf(stderr, "Empty count mismatch, expected %zu, found %u\n", i, mws_s[i].empty_count);
        return EXIT_FAILURE;
      }
      mws[i] = &mws_s[i];
    }
  }
  mw_loaded = true;
  return EXIT_SUCCESS;
}

void
game_position_rglm_release_model_weights (void)
{
  if (!mw_loaded) return;
  for (size_t i = 0; i < EC_SIZE; i++) {
    rglmdf_model_weights_release(&mws_s[i]);
    mws[i] = NULL;
  }
  mw_loaded = false;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

static double
rglm_eval_gp (const GamePositionX *const gpx)
{
  assert(gpx);

  const int empty_count = game_position_x_empty_count(gpx);

  const rglmdf_model_weights_t *const mw = mws[empty_count];
  assert(mw);

  board_t b_s;
  board_t *const b = &b_s;
  board_from_gpx(b, gpx);

  const board_feature_id_t *const features = mw->features;
  const board_pattern_id_t *const patterns = mw->patterns;

  double feature_values[BOARD_FEATURE_MAX_FIELD_CNT];

  double gp_eval = 0.0;

  for (size_t j = 0; j < mw->feature_cnt; j++) {
    const board_feature_id_t fid = features[j];
    board_features[fid].feature_values_f(b, feature_values);
    for (size_t k = 0; k < board_features[fid].field_cnt; k++) {
      const rglmdf_weight_record_t *const wrec =
        rglmdf_model_weights_table_lookup_record(mw, BOARD_ENTITY_CLASS_FEATURE, fid, k);
      gp_eval += wrec->weight * feature_values[k];
    }
  }

  board_pattern_rotated_t r;
  board_pattern_compute_rotated(b, &r);

  for (size_t j = 0; j < mw->pattern_cnt; j++) {
    const board_pattern_id_t pid = patterns[j];
    const board_pattern_t *const bpp = &board_patterns[pid];
    for (size_t k = 0; k < bpp->n_instances; k++) {
      board_t tr;
      tr.square_sets[0] = bpp->pattern_pack_f(r.board_array[k].square_sets[0]);
      tr.square_sets[1] = bpp->pattern_pack_f(r.board_array[k].square_sets[1]);
      const board_pattern_index_t index_value = board_pattern_packed_to_index(&tr, bpp->n_squares);
      const rglmdf_weight_record_t *const wrec =
        rglmdf_model_weights_table_lookup_record(mw, BOARD_ENTITY_CLASS_PATTERN, pid, index_value);
      gp_eval += wrec->weight;
    }
  }

  return rglmut_logistic_function(gp_eval);
}

/*
 * Insertion sort.
 */
static void
rglm_sort_move_pointers (gts_mle_t **moves,
                         const size_t count,
                         const bool mw_available)
{
  for (size_t i = 1; i < count; i++) {
    size_t j = i;
    for (;;) {
      gts_mle_t * tmp;
      if (j == 0) break;
      if (mw_available) {
        if ((*(moves + j - 1))->evaluation <= (*(moves + j))->evaluation) break;
      } else {
        if ((*(moves + j - 1))->res_move_count <= (*(moves + j))->res_move_count) break;
      }
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

  const int ec = game_position_x_empty_count(&c->gpx);

  gts_mle_t **mle = c->head_of_legal_move_list;

  if (c->move_set) {
    const bool mw_available = mws[ec-1] != NULL;
    for (int i = 0; i < legal_moves_priority_cluster_count; i++) {
      SquareSet moves_to_search = c->move_set & legal_moves_priority_mask[i];
      while (moves_to_search) {
        (*mle)->move = bitw_bit_scan_forward_64(moves_to_search);
        moves_to_search &= ~(1ULL << (*mle)->move);
        game_position_x_make_move(&c->gpx, (*mle)->move, &(*mle)->res_position);
        (*mle)->res_move_set = game_position_x_legal_moves(&(*mle)->res_position);
        (*mle)->res_move_count = bitw_bit_count_64((*mle)->res_move_set);
        bool is_leaf = false;
        if (mw_available) {
          if ((*mle)->res_move_count == 0) {
            GamePositionX next;
            game_position_x_pass(&(*mle)->res_position, &next);
            if (!game_position_x_has_any_legal_move(&next)) is_leaf = true;
          }
          (*mle)->evaluation = is_leaf ?
            (double) game_position_x_final_value(&(*mle)->res_position) :
            rglm_eval_gp(&(*mle)->res_position);
        } else {
          (*mle)->evaluation = -1.0;
        }
        mle++;
      }
    }
    rglm_sort_move_pointers(c->head_of_legal_move_list, c->move_count, mw_available);
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
  const SquareSet cu_m = game_position_x_get_mover(&c->gpx);
  const SquareSet up_o = game_position_x_get_opponent(&(c + 1)->gpx);
  SquareSet flip_set = up_o & ~(cu_m | bitmove);
  while (flip_set) {
    *flip_cursor++ = bitw_bit_scan_forward_64(flip_set);
    flip_set = bitw_reset_lowest_set_bit_64(flip_set);
  }
  stack->flip_count = flip_cursor - stack->flips;
}

static void
recursive_call_setup (GameTreeStack *const stack)
{
  NodeInfo *const c = stack->active_node;
  game_position_x_copy(&(*c->move_cursor)->res_position, &(c + 1)->gpx);
  (c + 1)->move_set = (*c->move_cursor)->res_move_set;
  (c + 1)->alpha = - c->beta;
  (c + 1)->beta = - c->alpha;
}

static uint8_t
adjusted_move_count (GameTreeStack *const stack)
{
  NodeInfo *const c = stack->active_node;
  if (c->move_set) return bitw_bit_count_64(c->move_set);
  else if ((c - 1)->move_set) return 1;
  else return 0;
}

static void
pv_create_first_line (GameTreeStack *const stack)
{
  NodeInfo *const c = stack->active_node;
  c->pv_first_line_created = true;
  pve_line_add_move(pve, c->pve_line, (*c->move_cursor)->move, &(c + 1)->gpx);
  pve_line_delete(pve, (c - 1)->pve_line);
  (c - 1)->pve_line = c->pve_line;
}

static void
pv_add_variant_line (GameTreeStack *const stack)
{
  NodeInfo *const c = stack->active_node;
  if (!c->pv_first_line_created) {
    c->pv_first_line_created = true;
    pve_line_add_move(pve, c->pve_line, (*c->move_cursor)->move, &(c + 1)->gpx);
    pve_line_delete(pve, (c - 1)->pve_line);
    (c - 1)->pve_line = c->pve_line;
  } else {
    pve_line_add_move(pve, c->pve_line, (*c->move_cursor)->move, &(c + 1)->gpx);
    pve_line_add_variant(pve, (c - 1)->pve_line, c->pve_line);
  }
}

/*
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
                          GameTreeStack *const stack)
{
  NodeInfo *c;
  const NodeInfo *const root = stack->active_node;

 begin:
  result->node_count++;
  c = ++stack->active_node;
  c->move_count = adjusted_move_count(stack);
  c->pv_first_line_created = false;

  if (stack->hash_is_on) gts_compute_hash(stack);
  if (log_env->log_is_on) gtl_do_log_head(result, stack, sub_run_id, log_env);
  if (result->all_legal_moves_solved && c - root == 2) {c->alpha = worst_score; c->beta = best_score;}

  if (!c->move_count) {
    result->leaf_count++;
    c->alpha = game_position_x_final_value(&c->gpx);
    c->best_move = pass_move;
    if (pv_recording) pve_line_add_move(pve, (c - 1)->pve_line, pass_move, &(c + 1)->gpx);
    c->move_cursor = c->head_of_legal_move_list;
    goto end;
  }

  look_ahead_and_sort_moves_by_mobility_count(stack);

  if (pv_full_recording && c->move_set) c->alpha -= 1;

  c->best_move = (*c->head_of_legal_move_list)->move;
  for ( c->move_cursor = c->head_of_legal_move_list; c->move_cursor - c->head_of_legal_move_list < c->move_count; c->move_cursor++) {
    if (pv_recording) c->pve_line = pve_line_create(pve);
    recursive_call_setup(stack);
    if (stack->hash_is_on) update_move_flips(stack);
    goto begin;
  entry:
    if (-(c + 1)->alpha > c->alpha || !c->move_set) {
      c->alpha = -(c + 1)->alpha;
      c->best_move = (*c->move_cursor)->move;
      if (pv_recording) pv_create_first_line(stack);
      if (c->alpha > c->beta || (!pv_full_recording && c->alpha == c->beta)) {
        if (result->all_legal_moves_solved && c - root == 1) continue;
        c->move_cursor++;
        goto end;
      }
    } else {
      if (pv_recording) {
        if (pv_full_recording && -(c + 1)->alpha == c->alpha)
          pv_add_variant_line(stack);
        else
          pve_line_delete(pve, c->pve_line);
      }
    }
  }

 end:
  if (result->all_legal_moves_solved && c - root == 2) {
    const Square move = (*(c - 1)->move_cursor)->move;
    const int value = -c->alpha;
    const int move_pos = (c - 1)->move_cursor - (c - 1)->head_of_legal_move_list;
    move_value_t *const mv = &result->legal_move_values[move_pos];
    mv->move = move;
    mv->value = value;
    if (false) printf("  [%02d][%s:%+03d]\n", move_pos, square_as_move_to_string(move), value);
  }
  if (log_env->log_is_on) gtl_do_log_tail(result, stack, sub_run_id, log_env);
  c = --stack->active_node;
  if (stack->active_node == root) return;
  goto entry;
}

/**
 * @endcond
 */
