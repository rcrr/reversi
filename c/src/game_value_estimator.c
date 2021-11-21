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

/*
 * Run the solver:
 *
 * $ time ./build/bin/endgame_solver -s gve -f db/gpdb-ffo.txt -q ffo-29
 * $ time ./build/bin/endgame_solver -s gve -f db/gpdb-sample-games.txt -q woc18-FKvsAP-g1-22
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <inttypes.h>
#include <assert.h>
#include <math.h>

#include <immintrin.h>

#include "time_utils.h"
#include "cfg.h"
#include "file_utils.h"
#include "rglm_data_files.h"
#include "rglm_utils.h"
#include "board.h"
#include "transposition_table.h"
#include "game_value_estimator.h"


/* ###
 * ### Model Weights static variables.
 * ###
 */

/* Empty Count Size - [0..60] the game stages ... */
#define EC_SIZE 61

static const SquareSet empty_move_set = 0ULL;

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

static uint64_t rglm_eval_gp_call_count;

static int search_depth;

static ttab_t ttab;
static const size_t ttab_log_size = 25;

static int min (int a, int b);



/**
 * @cond
 */

typedef struct node_s {
  struct node_s *parent;
  SquareSet legal_move_set;
  Square parent_move;
  Square best_move;
  int value;
  GamePositionX gpx;
  uint64_t hash;
} node_t;


/*
 * Prototypes for internal functions.
 */

static void
alphabeta_with_memory (node_t *n,
                       int depth,
                       int alpha,
                       int beta);

static void
mtdf (node_t *n,
      const int depth);

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


/* Search depth iterative deepening. */
static int search_depth_id;



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

  /* Stopwatch variables. */
  timespec_t time_0, time_1, delta_cpu_time, start_time, end_time, delta_time;

  int estimated_value;

  node_count = 0;
  leaf_count = 0;
  rglm_eval_gp_call_count = 0;

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
  if (search_depth < 0) search_depth = ec * 2;

  const bool mw_available = mws[ec] != NULL;

  if (!mw_available) {
    printf("There is no model weights available for the root node empty count!\n");
    exit(EXIT_FAILURE);
  }
  const double l0_estimated_game_value_transformed = rglm_eval_gp(root);
  const double l0_estimated_game_value_f = rglmut_gv_scale_back_f(l0_estimated_game_value_transformed);
  const int l0_estimated_game_value = gv_f2d(l0_estimated_game_value_f);
  printf("Node Level 0: estimated game value = %6.3f [%+03d] (%6.4f)\n", l0_estimated_game_value_f, l0_estimated_game_value, l0_estimated_game_value_transformed);

  ttab = ttab_new(ttab_log_size);
  if (!ttab) abort();

  node_t parent_root_node;
  parent_root_node.parent = NULL;
  parent_root_node.legal_move_set = empty_move_set;
  parent_root_node.parent_move = invalid_move;
  parent_root_node.best_move = invalid_move;
  parent_root_node.value = out_of_range_defeat_score;
  parent_root_node.gpx.blacks = root->blacks;
  parent_root_node.gpx.whites = root->whites;
  parent_root_node.gpx.player = player_opponent(root->player);
  parent_root_node.hash = game_position_x_hash(&parent_root_node.gpx);

  node_t root_node;
  root_node.parent = &parent_root_node;
  root_node.legal_move_set = empty_move_set;
  root_node.parent_move = invalid_move;
  root_node.best_move = invalid_move;
  root_node.value = out_of_range_defeat_score;
  root_node.gpx.blacks = root->blacks;
  root_node.gpx.whites = root->whites;
  root_node.gpx.player = root->player;
  root_node.hash = game_position_x_hash(&root_node.gpx);

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  int id_limit = min(search_depth, ec - min_empty_count);
  printf("id_limit = %d\n", id_limit);
  for (int i = 1; i <= id_limit; i += 3) {
    printf(" ### ### ### id = %d\n", i);
    search_depth_id = i;
    mtdf(&root_node, i);
  }
  if (search_depth > id_limit) {
    printf(" ### ### ### LAST ### search_depth = %d\n", search_depth);
    search_depth_id = search_depth;
    mtdf(&root_node, search_depth); // stima da rivedere
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
  clock_gettime(CLOCK_REALTIME, &end_time);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);
  if (true) {
    printf("MTD(f) evaluation CPU time: ");
    printf("[%6lld.%9ld][%6lld.%9ld]\n",
           (long long) timespec_get_sec(&delta_cpu_time), timespec_get_nsec(&delta_cpu_time),
           (long long) timespec_get_sec(&delta_time), timespec_get_nsec(&delta_time));
  }

  estimated_value = root_node.value;

  result->best_move = root_node.best_move;
  result->outcome = estimated_value;
  result->node_count = node_count;
  result->leaf_count = leaf_count;

  printf("game_position_value_estimator: search_depth = %d, estimated_value = %d\n", search_depth, estimated_value);

  printf("rglm_eval_gp_call_count = %zu\n", rglm_eval_gp_call_count);

  const size_t stats_size = 42;
  size_t stats[stats_size];
  ttab_summary_to_stream(ttab, stdout);
  ttab_bucket_filling_stats(ttab, stats, stats_size);
  printf("TT hashtable stats:\n");
  for (size_t i = 0; i < stats_size; i++) {
    printf("%6zu;%10zu\n", i, stats[i]);
  }
  ttab_free(&ttab);

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

  rglm_eval_gp_call_count++;

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
  board_pattern_compute_rotated_vec(b, &r);

  for (size_t j = 0; j < mw->pattern_cnt; j++) {
    const board_pattern_id_t pid = patterns[j];
    const board_pattern_t *const bpp = &board_patterns[pid];
    for (size_t k = 0; k < bpp->n_instances; k++) {
      board_t tr;
      tr.square_sets[0] = bpp->pattern_pack_f(r.board_array[k].square_sets[0]);
      tr.square_sets[1] = bpp->pattern_pack_f(r.board_array[k].square_sets[1]);
      const board_pattern_index_t index_value = board_pattern_packed_to_index_vec(&tr, bpp->n_squares);
      rglmdf_weight_record_t *const wrec =
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
  return (!n->legal_move_set && !n->parent->legal_move_set);
}

static void
heuristic_game_value (node_t *n)
{
  const double v0 = rglm_eval_gp(&n->gpx);
  const double v1 = rglmut_gv_scale_back_f(v0);
  n->value = gv_f2d(v1);
}

static void
exact_terminal_game_value (node_t *n)
{
  n->value = game_position_x_final_value(&n->gpx);
}

static void
generate_legal_move_set (node_t *n,
                         ttab_item_t it)
{
  n->legal_move_set = it ? it->legal_move_set : game_position_x_legal_moves(&n->gpx);
}

/**
 * @brief Generates child nodes
 *
 * @param [out] child_node_count pointer to the count of child nodes
 * @param [out] child_nodes      array of children nodes
 * @param [in]  n                pointer to the node structure
 */
static void
generate_child_nodes (int *child_node_count,
                      node_t *child_nodes,
                      node_t *n,
                      bool compute_hash)
{
  SquareSet lms; // legal move set
  int lmc;       // legal move count

  lms = n->legal_move_set;
  if (lms) {
    lmc = bitw_bit_count_64(lms);
    for (int i = 0; i < lmc; i++) {
      const Square move = bitw_tzcnt_64(lms);
      node_t *const c = child_nodes + i;
      c->parent = n;
      c->legal_move_set = empty_move_set;
      c->parent_move = move;
      c->best_move = unknown_move;
      c->value = out_of_range_win_score;
      if (compute_hash) game_position_x_make_move_delta_hash(&n->gpx, move, &c->gpx, n->hash, &c->hash);
      else game_position_x_make_move(&n->gpx, move, &c->gpx);
      lms = bitw_reset_lowest_set_bit_64(lms);
    }
  } else {
    lmc = 1;
    node_t *const c = child_nodes + 0;
    c->parent = n;
    c->legal_move_set = empty_move_set;
    c->parent_move = pass_move;
    c->best_move = unknown_move;
    c->value = out_of_range_win_score;
    if (compute_hash) game_position_x_make_move_delta_hash(&n->gpx, pass_move, &c->gpx, n->hash, &c->hash);
    else game_position_x_make_move(&n->gpx, pass_move, &c->gpx);
  }

  *child_node_count = lmc;
}

static void
order_moves (int child_node_count,
             node_t *child_nodes,
             node_t **child_nodes_p,
             ttab_item_t it)
{
  if (it && it->best_moves[0] != unknown_move) {

    for (int i = 0; i < child_node_count; i++) {
      node_t *child = &child_nodes[i];
      child_nodes_p[i] = child;
    }

    for (int i = 0; i < TTAB_RECORDED_BEST_MOVE_COUNT; i++) {
      const int8_t move = it->best_moves[i];
      const int8_t value = it->move_values[i];
      if (move == unknown_move) break;
      for (int j = i; j < child_node_count; j++) {
        if (child_nodes_p[j]->parent_move == move) {
          node_t *tmp;
          tmp = child_nodes_p[j]; child_nodes_p[j] = child_nodes_p[i]; child_nodes_p[i] = tmp;
        }
      }
      child_nodes_p[i]->value = -value;
    }

  } else {
    for (int i = 0; i < child_node_count; i++) {
      node_t *child = &child_nodes[i];
      child_nodes_p[i] = child;
      heuristic_game_value(child);
    }
    for (int i = 1; i < child_node_count; i++) {
      int j = i;
      for (;;) {
        node_t *tmp;
        if (j == 0) break;
        if (child_nodes_p[j-1]->value <= child_nodes_p[j]->value) break;
        tmp = child_nodes_p[j]; child_nodes_p[j] = child_nodes_p[j-1]; child_nodes_p[j-1] = tmp;
        j--;
      }
    }
  }
}

static int
max (int a,
     int b)
{
  return a > b ? a : b;
}

static int
min (int a,
     int b)
{
  return a < b ? a : b;
}

static void
leaf_negamax (node_t *n,
              int alpha,
              int beta)
{
  int child_node_count;
  node_t child_nodes[64];

  node_count++;

  n->legal_move_set = game_position_x_legal_moves(&n->gpx);

  if (is_terminal(n)) {
    leaf_count++;
    exact_terminal_game_value(n);
  } else {
    generate_child_nodes(&child_node_count, child_nodes, n, false);
    n->value = out_of_range_defeat_score;
    for (int i = 0; i < child_node_count; i++) {
      node_t *child = &child_nodes[i];
      leaf_negamax(child, -beta, -alpha);
      n->value = max(n->value, -child->value);
      alpha = max(alpha, n->value);
      if (alpha >= beta) break;
    }
  }

  return;
}

static void
alphabeta_with_memory (node_t *n,
                       const int depth,
                       const int alpha,
                       const int beta)
{
  int child_node_count;
  node_t child_nodes[64];
  node_t *child_nodes_p[64];
  int empty_count;
  int explored_child_count;

  int am = alpha; // alpha-mobile : local value that is adjusted during the search
  int bm = beta;  // beta-mobile  : " ...

  node_count++;
  struct ttab_item_s its;
  ttab_item_t it = &its;
  it->hash = n->hash;
  ttab_retrieve(ttab, &it);
  if (it) { // item found in the TT
    ttab_item_clone_data(it, &its);
    if (its.depth >= depth) {
      if (its.lower_bound >= bm) {
        n->value = its.lower_bound;
        return;
      }
      if (its.upper_bound <= am) {
        n->value = its.upper_bound;
        return;
      }
      am = max(am, its.lower_bound);
      bm = min(bm, its.upper_bound);
    } else {
      its.lower_bound = out_of_range_defeat_score;
      its.upper_bound = out_of_range_win_score;
    }
  } else { // item not found
    its.hash = n->hash;
    its.lower_bound = out_of_range_defeat_score;
    its.upper_bound = out_of_range_win_score;
    its.pq_index = -1;
    its.legal_move_set = empty_move_set;
    its.legal_move_count = 0;
    for (int i = 0; i < TTAB_RECORDED_BEST_MOVE_COUNT; i++) {
      its.best_moves[i] = unknown_move;
      its.move_values[i] = out_of_range_defeat_score;
    }
  }

  generate_legal_move_set(n, it);

  explored_child_count = 0; // logic to be verified when leaf_negamax is selected ...
  if (is_terminal(n)) {
    leaf_count++;
    exact_terminal_game_value(n);
  } else if (depth == 0) {
    if (n->value == out_of_range_win_score) heuristic_game_value(n);
    n->best_move = unknown_move;
  } else if ((empty_count = game_position_x_empty_count(&n->gpx)) < min_empty_count) {
    leaf_negamax(n, am, bm);
  } else {

    generate_child_nodes(&child_node_count, child_nodes, n, true);
    its.legal_move_count = child_node_count;
    order_moves(child_node_count, child_nodes, child_nodes_p, it);

    n->value = out_of_range_defeat_score;
    n->best_move = invalid_move;
    for (int i = 0; i < child_node_count; i++) {
      node_t *child = child_nodes_p[i];
      if (search_depth_id - depth == 0) {
        printf("%02d - %2s (%+03d) [%+03d..%+03d]: ", i, square_as_move_to_string(child->parent_move), -child->value, am, bm);
        //printf("n->hash = %zu", n->hash);
        fflush(stdout);
      }

      alphabeta_with_memory(child, depth -1, -bm, -am);

      if (search_depth_id - depth == 0) {
        char *s;
        if (-child->value <= am) s = "(failing low)  f+";
        else if (-child->value >= bm) s = "(failing high) f-";
        else s = "(success)       f ";
        printf("%s = %+03d - cumulated node count %10zu\n", s, -child->value, node_count);
        fflush(stdout);
      }
      if (-child->value > n->value) {
        n->value = -child->value;
        n->best_move = child->parent_move;
      }
      am = max(am, n->value);
      explored_child_count++;
      if (am >= bm) break;
    }

  } // end-of-else
  if (false && n->hash == 17372629065185580190ULL && depth == 5) abort();

  its.depth = depth;
  its.best_move = n->best_move;
  its.legal_move_set = n->legal_move_set;
  if (n->value < beta) its.upper_bound = n->value;
  if (n->value > alpha) its.lower_bound = n->value;

  for (int i = 1; i < explored_child_count; i++) {
    int j = i;
    for (;;) {
      node_t *tmp;
      if (j == 0) break;
      if (child_nodes_p[j-1]->value <= child_nodes_p[j]->value) break;
      tmp = child_nodes_p[j], child_nodes_p[j] = child_nodes_p[j-1], child_nodes_p[j-1] = tmp;
      j--;
    }
  }

  for (int i = 0; i < min(explored_child_count, TTAB_RECORDED_BEST_MOVE_COUNT); i++) {
    node_t *child = child_nodes_p[i];
    its.best_moves[i] = child->parent_move;
    its.move_values[i] = -child->value;
  }

  ttab_insert(ttab, &its);

  return;
}

static void
mtdf (node_t *n,
      const int depth)
{
  int bound[2] = { out_of_range_defeat_score, out_of_range_win_score };
  do {
    const int beta = n->value + (n->value == bound[0]) * 2;
    alphabeta_with_memory(n, depth, beta -2, beta);
    bound[n->value < beta] = n->value;
  } while (bound[0] < bound[1]);
}



/**
 * @endcond
 */
