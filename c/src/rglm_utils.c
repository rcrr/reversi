/**
 * @file
 *
 * @brief Reversi Generalized Linear Model utility functions.
 *
 * @par rglm_utils.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2019, 2020, 2022 Roberto Corradini. All rights reserved.
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
#include <math.h>
#include <assert.h>

#include "rglm_utils.h"


  /*
    function negamax(node, depth, alpha, beta, color) is
      if depth = 0 or node is a terminal node then
          return color x the heuristic value of node

      childNodes := generateMoves(node)
      childNodes := orderMoves(childNodes)
      value := -66
      foreach child in childNodes do
        value := max(value, −negamax(child, depth − 1, −beta, −alpha, −color))
        alpha := max(alpha, value)
        if alpha >= beta then
            break (* cut-off *)
      return value
   */

static uint64_t nid_sec = 0;

typedef struct node_s {
  uint64_t nid;
  int depth;
  struct node_s *parent;
  Square parent_move;
  GamePositionX gpx;
  int empty_count;
  double value;
  SquareSet legal_moves_set;
  int legal_moves_count;
} node_t;

static double
max (double a,
     double b)
{
  return a > b ? a : b;
}

static void
board_to_gpx (const board_t *const board,
              GamePositionX *gpx)
{
  gpx->blacks = board->square_sets[0];
  gpx->whites = board->square_sets[1];
  gpx->player = BLACK_PLAYER;
}

static void
gpx_to_board (const GamePositionX *const gpx,
              board_t *b)
{
  b->square_sets[gpx->player] = gpx->blacks;
  b->square_sets[1 - gpx->player] = gpx->whites;
}

static void
generate_child_nodes (node_t *child_nodes,
                      node_t *n)
{
  node_t *c = child_nodes;
  SquareSet lms = n->legal_moves_set;
  if (lms) {
    int i = 0;
    while (lms) {
      const Square move = bitw_tzcnt_64(lms);
      c->nid = nid_sec++;
      c->depth = n->depth - 1;
      c->parent = n;
      c->parent_move = move;
      game_position_x_make_move(&n->gpx, move, &c->gpx);
      c->empty_count = game_position_x_empty_count(&c->gpx);
      c->value = 0.0;
      c->legal_moves_set = game_position_x_legal_moves(&c->gpx);
      c->legal_moves_count = bitw_bit_count_64(c->legal_moves_set);
      lms = bitw_reset_lowest_set_bit_64(lms);
      c++;
      i++;
    }
    if (i != n->legal_moves_count) abort();
  } else {
    const Square move = pass_move;
    c->nid = nid_sec++;
    c->depth = n->depth;
    c->parent = n;
    c->parent_move = move;
    game_position_x_make_move(&n->gpx, move, &c->gpx);
    c->empty_count = game_position_x_empty_count(&c->gpx);
    c->value = 0.0;
    c->legal_moves_set = game_position_x_legal_moves(&c->gpx);
    c->legal_moves_count = 1;
  }
}

static bool
is_terminal (node_t *n) {
  if (n->empty_count == 0) return true;
  if (n->legal_moves_set == empty_square_set && n->parent_move == pass_move) return true;
  return false;
}

static void
exact_terminal_game_value (node_t *n)
{
  n->value = rglmut_gv_scale(game_position_x_final_value(&n->gpx));
}

static void
negamax (node_t *n,
         double alpha,
         double beta,
         const rglmdf_model_weights_t *const mw)
{
  node_t child_nodes[32];

  if (is_terminal(n)) {
    exact_terminal_game_value(n);
  } else if (n->depth == 0) {
    board_t b;
    gpx_to_board(&n->gpx, &b);
    n->value = rglmut_eval_gp_using_model_weights(mw, &b);
  } else {
    generate_child_nodes(child_nodes, n);
    for (int i = 0; i < n->legal_moves_count; i++) {
      node_t *c = &child_nodes[i];
      if (false) {
        printf("[pnid=%08zu, nid=%08zu, cnid=%08zu] %02d %2s ec=%02d value=%f lms=%016lx lmc=%02d depth=%02d alpha=%f beta=%f\n",
               n->parent->nid, n->nid, c->nid, i, square_as_move_to_string(c->parent_move), c->empty_count, c->value,
               c->legal_moves_set, c->legal_moves_count, c->depth, alpha, beta);
        char buf[256];
        game_position_x_print(buf, &c->gpx);
        printf("%s\n", buf);
      }
      negamax(c, 1 - beta, 1 - alpha, mw);
      n->value = max(n->value, 1 - c->value);
      if (false) printf("[pnid=%08zu, nid=%08zu, cnid=%08zu] c->value = %f, n->value = %f\n", n->parent->nid, n->nid, c->nid, c->value, n->value);
      alpha = max(alpha, n->value);
      if (alpha >= beta) return;
    }
  }
}

double
rglmut_eval_gp_negascout (const rglmdf_model_weights_t *const mw,
                          const board_t *const b)
{
  assert(b);
  assert(mw);

  double alpha, beta;
  node_t root;

  alpha = 0.0;
  beta = 1.0;

  /* Inits the root node. */
  root.nid = nid_sec++;
  root.parent = &root;
  root.parent_move = unknown_move;
  board_to_gpx(b, &root.gpx);
  root.empty_count = game_position_x_empty_count(&root.gpx);
  root.value = 0.0;
  root.legal_moves_set = game_position_x_legal_moves(&root.gpx);
  root.legal_moves_count = bitw_bit_count_64(root.legal_moves_set);
  root.depth = root.empty_count - mw->empty_count;

  /**/
  if (false) {
    char buf[256];
    game_position_x_print(buf, &root.gpx);
    printf("\n");
    printf("root game position:\n");
    printf("%s\n", buf);
    printf("root.empty_count       = %d\n", root.empty_count);
    printf("root.legal_moves_count = %d\n", root.legal_moves_count);
  }
  /**/

  if (root.depth < 0) {
    fprintf(stderr, "Error, depth cannot be negative (depth = %d).\n", root.depth);
    fprintf(stderr, "Board's empty count is %d.\n", root.empty_count);
    fprintf(stderr, "Model Weights empty count is %d.\n", mw->empty_count);
    fprintf(stderr, "Aborting.\n");
  }

  //printf("mw->empty_count = %d\n", mw->empty_count);
  //printf("root.depth      = %d\n", root.depth);

  negamax(&root, alpha, beta, mw);

  return root.value;
}

double
rglmut_eval_gp_using_model_weights (const rglmdf_model_weights_t *const mw,
                                    const board_t *const b)
{
  assert(mw);
  assert(b);

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

double
rglmut_logistic_function (const double x)
{
  const double e =  2.71828182845904523536;
  return (double) 1.0 / ((double) 1.0 + pow(e, -x));
}

double
rglmut_gv_scale (const int v)
{
  return (double) 0.00765625 * (double) v + (double) 0.5;
}

double
rglmut_gv_scale_back_f (const double v)
{
  return (v - 0.5) / 0.00765625;
}

int
rglmut_gv_scale_back_i (const double v)
{
  return 2 * rint(rglmut_gv_scale_back_f(v) * 0.5);
}

void
rglmut_gv_init (const rglmdf_general_data_t *data,
                const size_t emme,
                double *const v)
{
  assert(data);
  assert(emme == rglmdf_get_positions_ntuples(data));
  assert(emme > 0 ? (bool) v  : true);

  const rglmdf_solved_and_classified_gp_record_t *const records = rglmdf_get_positions_records(data);

  for (size_t i = 0; i < emme; i++)
    v[i] = rglmut_gv_scale(records[i].game_value);
}

void
rglmut_evaluation_function_eval (const rglmdf_general_data_t *const data,
                                 const size_t enne,
                                 const double *w,
                                 const size_t emme,
                                 double *const e)
{
  assert(data);
  assert(enne == rglmdf_get_entity_freq_summary_ntuples(data));
  assert(enne > 0 ? (bool) w  : true);
  assert(emme == rglmdf_get_positions_ntuples(data));
  assert(emme > 0 ? (bool) e  : true);

  const int32_t *const i2array = rglmdf_get_positions_i2array(data);
  const double *const farray = rglmdf_get_positions_farray(data);

  /* ni: number of pattern instances considered by the model. */
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(data);

  /* nf: number of feature values considered by the model. */
  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(data);

  for (size_t i = 0; i < emme; i++) {
    double sum = 0.0;
    /* Vector of feature values belonging to a solved and classified game position. */
    const double *const feature_values = &farray[i * nf];
    for (size_t j = 0; j < nf; j++) {
      sum += w[j] * feature_values[j];
    }
    /* Vector of glm variables id belonging to a solved and classified game position. */
    const int32_t *const glm_variable_ids = &i2array[i * ni];
    for (size_t j = 0; j < ni; j++) {
      sum += w[glm_variable_ids[j]];
    }
    e[i] = rglmut_logistic_function(sum);
  }
}

void
rglmut_evaluation_function_derivative_eval (const size_t emme,
                                            const double *const e,
                                            double *const de)
{
  assert(emme > 0 ? (bool) e && (bool) de : true);

  for (size_t i = 0; i < emme; i++)
    de[i] = e[i] * (1 - e[i]);
}

void
rglmut_residual_value_eval (const size_t emme,
                            const double *const e,
                            const double *const v,
                            double *const r)
{
  assert(emme > 0 ? (bool) e && (bool) v && (bool) r : true);

  /* resiual := observed - predicted */
  for (size_t i = 0; i < emme; i++)
    r[i] = v[i] - e[i];
}

void
rglmut_minus_grad_f_eval (const rglmdf_general_data_t *const data,
                          double *const minus_grad_f,
                          const size_t enne,
                          const size_t emme,
                          const double *r,
                          const double *de,
                          const double *w,
                          const double rrc)
{
  assert(data);
  assert(enne == rglmdf_get_entity_freq_summary_ntuples(data));
  assert(enne > 0 ? (bool) minus_grad_f : true);
  assert(emme == rglmdf_get_positions_ntuples(data));
  assert(emme > 0 ? (bool) r && (bool) de : true);

  const int32_t *const i2array = rglmdf_get_positions_i2array(data);
  const double *const farray = rglmdf_get_positions_farray(data);

  /* ni: number of pattern instances considered by the model. */
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(data);

  /* nf: number of feature values considered by the model. */
  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(data);

  /* Initializes the vector. */
  for (size_t k = 0; k < enne; k++) minus_grad_f[k] = 0.0;

  for (size_t i = 0; i < emme; i++) {
    /* Vector of feature values belonging to a solved and classified game position. */
    const double *const feature_values = &farray[i * nf];
    /* Vector of glm variables id belonging to a solved and classified game position. */
    const int32_t *const glm_variable_ids = &i2array[i * ni];
    const double z = r[i] * de[i];
    for (size_t j = 0; j < nf; j++)
      minus_grad_f[j] += z * feature_values[j];
    for (size_t j = 0; j < ni; j++)
      minus_grad_f[glm_variable_ids[j]] += z;
  }
  for (size_t k = 0; k < enne; k++) minus_grad_f[k] -= rrc * w[k];
}

void
rgmlut_big_b_eval (const rglmdf_general_data_t *const data,
                   double **const big_b,
                   const size_t enne,
                   const size_t emme,
                   const double *e,
                   const double *de,
                   const double *v)
{
  assert(data);
  assert(enne == rglmdf_get_entity_freq_summary_ntuples(data));
  assert(enne > 0 ? (bool) big_b : true);
  assert(emme == rglmdf_get_positions_ntuples(data));
  assert(emme > 0 ? (bool) e && (bool) de && (bool) v : true);

  const int32_t *const i2array = rglmdf_get_positions_i2array(data);
  const double *const farray = rglmdf_get_positions_farray(data);

  /* ni: number of pattern instances considered by the model. */
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(data);

  /* nf: number of feature values considered by the model. */
  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(data);

  /* Initializes the upper triangle of the B matrix. */
  for (size_t i = 0; i < enne; i++)
    for (size_t j = i; j < enne; j++)
      big_b[i][j] = 0.0;

  /* Computes the upper triangle of the B matrix. */
  for (size_t k = 0; k < emme; k++) {
    /* Vector of feature values belonging to a solved and classified game position. */
    const double *const feature_values = &farray[k * nf];
    /* Vector of glm variables id belonging to a solved and classified game position. */
    const int32_t *const glm_variable_ids = &i2array[k * ni];
    const double z = de[k] * (v[k] - 2*(1 + v[k])*e[k] + 3*e[k]*e[k]);

    for (size_t i = 0; i < nf; i++) {
      for (size_t j = i; j < nf; j++) {
        big_b[i][j] -= z * feature_values[i] * feature_values[j];
      }
      for (size_t j = 0; j < ni; j++) {
        const size_t idj = glm_variable_ids[j];
        big_b[i][idj] -= z * feature_values[i];
      }
    }
    for (size_t i = 0; i < ni; i++) {
      for (size_t j = 0; j < ni; j++) {
        const size_t idi = glm_variable_ids[i];
        const size_t idj = glm_variable_ids[j];
        if (idj >= idi) big_b[idi][idj] -= z;
      }
    }
  }
}
