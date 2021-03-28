/**
 * @file
 *
 * @brief Game value estimator module.
 * @details Expands the game tree and gives the level-by-level game value estimation of the root position.
 *
 * @par game_value_estimator.c
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
#include <stdbool.h>
#include <inttypes.h>
#include <assert.h>
#include <math.h>

#include "cfg.h"
#include "file_utils.h"
#include "rglm_data_files.h"
#include "rglm_utils.h"

#include "board.h"
#include "game_value_estimator.h"


/* ###
 * ### Model Weights static variables.
 * ###
 */

/* Empty Count Size - [0..60] the game stages ... */
#define EC_SIZE 61

/* Default rglm_solver_config file. */
static const char *const rglm_solver_def_config_file = "./cfg/game_value_estimator.cfg";

/* The RGLM solver config. */
static cfg_t *rglm_solver_config = NULL;

/* The vector of model weighs structures indexed by empty_count. */
static rglmdf_model_weights_t mws_s[EC_SIZE];

/* The vector of pointers to the mws_s structures.
 * It is a convenience, when NULL the model_weights structure is
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

/* True when the Model Weiights have been loaded. */
static bool mw_loaded = false;

/* ###
 * ### End of section for: Model Weights static variables.
 * ###
 */

/* To be moved into the config file. */
static const int min_empty_count = 4;

static uint64_t node_count;
static uint64_t leaf_count;

static int search_depth;


/**
 * @cond
 */

typedef struct {
  Square parent_move;
  Square best_move;
  int value;
  GamePositionX gpx;
} node_t;


/*
 * Prototypes for internal functions.
 */

static int
negamax (node_t *n,
         int depth,
         int alpha,
         int beta);

static int
game_position_rglm_load_model_weights_files (bool verbose);

static void
game_position_rglm_release_model_weights (void);

static double
rglm_eval_gp (const GamePositionX *const gpx);

static int
gv_f2d (const double f);

/*
 * Internal variables and constants.
 */


/**
 * @endcond
 */



/*
 * Public functions.
 */

/**
 * @brief Gives an extimation of the game position returning a new exact solution pointer.
 *
 * @param [in] root the starting game position to be solved
 * @param [in] env  parameter envelope
 * @return          a pointer to a new exact solution structure
 */
ExactSolution *
game_position_value_estimator (const GamePositionX *const root,
                               const endgame_solver_env_t *const env)
{
  assert(root);
  assert(env);

  int estimated_value;

  node_count = 0;
  leaf_count = 0;

  search_depth = env->search_depth;

  ExactSolution *result = NULL;
  const bool verbose = false;
  int ret_err;
  ret_err = game_position_rglm_load_model_weights_files(verbose);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading RGLM solver model weights files. Aborting ...\n");
    abort();
  }

  result = exact_solution_new();
  exact_solution_init(result);
  exact_solution_set_root(result, root);

  const int ec = game_position_x_empty_count(root);
  printf("Empty count = %d, Search depth = %d, Minimum empty count = %d\n", ec, search_depth, min_empty_count);

  const bool mw_available = mws[ec] != NULL;

  if (!mw_available) {
    printf("There is no model weights available for the root node empty count!\n");
    exit(EXIT_FAILURE);
  }
  const double l0_estimated_game_value_transformed = rglm_eval_gp(root);
  const double l0_estimated_game_value_f = rglmut_gv_scale_back_f(l0_estimated_game_value_transformed);
  const int l0_estimated_game_value = gv_f2d(l0_estimated_game_value_f);
  printf("Node Level 0: estimated game value = %6.3f [%+03d] (%6.4f)\n", l0_estimated_game_value_f, l0_estimated_game_value, l0_estimated_game_value_transformed);

  node_t root_node;
  root_node.parent_move = invalid_move;
  root_node.best_move = invalid_move;
  root_node.value = -66;
  root_node.gpx.blacks = root->blacks;
  root_node.gpx.whites = root->whites;
  root_node.gpx.player = root->player;

  estimated_value = negamax(&root_node, search_depth, -64, +64);

  result->best_move = root_node.best_move;
  result->outcome = estimated_value;
  result->node_count = node_count;
  result->leaf_count = leaf_count;

  printf("game_position_value_estimator: search_depth = %d, estimated_value = %d\n", search_depth, estimated_value);

  game_position_rglm_release_model_weights();

  return result;
}


/**
 * @cond
 */

/*
 * Internal functions.
 */

static int
gv_f2d (const double f)
{
  return round(f/2) * 2;
}

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

static int
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

static void
game_position_rglm_release_model_weights (void)
{
  if (!mw_loaded) return;
  for (size_t i = 0; i < EC_SIZE; i++) {
    rglmdf_model_weights_release(&mws_s[i]);
    mws[i] = NULL;
  }
  mw_loaded = false;
}

static bool
is_terminal (node_t *n)
{
  return !game_position_x_has_any_player_any_legal_move(&n->gpx);
}

static int
heuristic_game_value (node_t *n)
{
  const double v0 = rglm_eval_gp(&n->gpx);
  const double v1 = rglmut_gv_scale_back_f(v0);
  const int v = gv_f2d(v1);
  n->value = v;
  return v;
}

static int
exact_terminal_game_value (node_t *n)
{
  return game_position_x_final_value(&n->gpx);
}

static void
generate_child_nodes (int *child_node_count,
                      node_t *child_nodes,
                      node_t *n)
{
  SquareSet lms;
  int lmc;

  lms = game_position_x_legal_moves(&n->gpx);
  if (lms) {
    lmc = bitw_bit_count_64(lms);
    for (int i = 0; i < lmc; i++) {
      const Square move = bitw_bit_scan_forward_64(lms);
      (child_nodes + i)->parent_move = move;
      lms ^= (SquareSet) 1 << move;
      game_position_x_make_move(&n->gpx, move, &(child_nodes + i)->gpx);
    }
  } else {
    lmc = 1;
    game_position_x_pass(&n->gpx, &child_nodes->gpx);
  }

  *child_node_count = lmc;
}

static void
order_moves (int *child_node_count,
             node_t *child_nodes)
{
  ;
}

static int
max (int a,
     int b)
{
  return a > b ? a : b;
}

static int
leaf_negamax (node_t *n,
              int alpha,
              int beta)
{
  int value;
  int child_node_count;
  node_t child_nodes[64];

  node_count++;

  if (is_terminal(n)) {
    leaf_count++;
    return exact_terminal_game_value(n);
  }

  generate_child_nodes(&child_node_count, child_nodes, n);

  value = -64;
  for (int i = 0; i < child_node_count; i++) {
    node_t *child = &child_nodes[i];
    value = max(value, -leaf_negamax(child, -beta, -alpha));
    alpha = max(alpha, value);
    if (alpha >= beta) break;
  }

  return value;
}

/*
  function negamax(node, depth, α, β, color) is
    if depth = 0 or node is a terminal node then
      return color × the heuristic value of node

    childNodes := generateMoves(node)
    childNodes := orderMoves(childNodes)
    value := −∞
    foreach child in childNodes do
      value := max(value, −negamax(child, depth − 1, −β, −α, −color))
      α := max(α, value)
      if α ≥ β then
        break (* cut-off *)
    return value
 */

static int
negamax (node_t *n,
         int depth,
         int alpha,
         int beta)
{
  int child_node_count;
  node_t child_nodes[64];
  int empty_count;

  node_count++;

  if (is_terminal(n)) {
    leaf_count++;
    return exact_terminal_game_value(n);
  }
  if (depth == 0) {
    return heuristic_game_value(n);
  }

  empty_count = game_position_x_empty_count(&n->gpx);
  if (empty_count < min_empty_count) {
    return leaf_negamax(n, alpha, beta);
  }

  generate_child_nodes(&child_node_count, child_nodes, n);
  order_moves(&child_node_count, child_nodes);

  n->value = -64;
  for (int i = 0; i < child_node_count; i++) {
    if (search_depth - depth == 0) alpha = -64;
    node_t *child = &child_nodes[i];
    const int child_value = -negamax(child, depth -1, -beta, -alpha);
    if (child_value > n->value) {
      n->value = child_value;
      n->best_move = child->parent_move;
    }
    alpha = max(alpha, n->value);
    if (alpha >= beta) break;
    if (search_depth - depth == 0) printf("%02d - %2s : %+03d\n", i, square_as_move_to_string(child->parent_move), -child->value);
  }

  return n->value;
}

/**
 * @endcond
 */
