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

static tratab_table_t *tt;
static const size_t tt_size = (size_t) 1024 * 1024 * 1024 * 32;
//static const size_t tt_size = (size_t) 256;

static int min (int a, int b);


/**
 * @cond
 */

typedef struct {
  SquareSet move_set;
  Square parent_move;
  Square best_move;
  int value;
  GamePositionX gpx;
} node_t;


/*
 * Prototypes for internal functions.
 */

static void
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



static int64_t c0  = 0;
static int64_t c1  = 0;
static int64_t c2  = 0;
static int64_t c3  = 0;
static int64_t c4  = 0;
static int64_t c5  = 0;
static int64_t c6  = 0;
static int64_t c7  = 0;
static int64_t c8  = 0;
static int64_t c9  = 0;
static int64_t c10 = 0;


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

  tt = tratab_table_create(tt_size);
  if (!tt) {
    printf("Error, unable to allocate space for the transposition table.\n");
    exit(EXIT_FAILURE);
  }
  tratab_table_init(tt);

  node_t root_node;
  root_node.parent_move = invalid_move;
  root_node.best_move = invalid_move;
  root_node.value = -66;
  root_node.gpx.blacks = root->blacks;
  root_node.gpx.whites = root->whites;
  root_node.gpx.player = root->player;

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0);

  bool id = false; // iterative-deepening
  if (id) {
    int id_limit = min(search_depth, ec - min_empty_count);
    printf("id_limit = %d\n", id_limit);
    for (int i = 1; i <= id_limit; i++) {
      printf(" ### ### ### id = %d\n", i);
      search_depth_id = i;
      negamax(&root_node, i, -66, +66);
    }
    if (search_depth > id_limit) {
      printf(" ### ### ### LAST ### search_depth = %d\n", search_depth);
      search_depth_id = search_depth;
      negamax(&root_node, search_depth, -66, +66);
    }
  } else {
    search_depth_id = search_depth;
    negamax(&root_node, search_depth, -66, +66);
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1);
  clock_gettime(CLOCK_REALTIME, &end_time);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time, &start_time, &end_time);
  timespec_diff(&delta_cpu_time, &time_0, &time_1);
  if (true) {
    printf("Negamax evaluation CPU time: ");
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

  tratab_table_header_to_stream(tt, stdout);

  game_position_rglm_release_model_weights();

  tratab_table_destroy(tt);
  tt = NULL;

  printf("c0  = %zu\n", c0);
  printf("c1  = %zu\n", c1);
  printf("c2  = %zu\n", c2);
  printf("c3  = %zu\n", c3);
  printf("c4  = %zu\n", c4);
  printf("c5  = %zu\n", c5);
  printf("c6  = %zu\n", c6);
  printf("c7  = %zu\n", c7);
  printf("c8  = %zu\n", c8);
  printf("c9  = %zu\n", c9);
  printf("c10 = %zu\n", c10);

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
generate_child_nodes (int *child_node_count,
                      node_t *child_nodes,
                      node_t *n)
{
  SquareSet lms; // legal move set
  int lmc;       // legal move count

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
  n->move_set = lms;

  *child_node_count = lmc;
}

static void
order_moves (int child_node_count,
             node_t *child_nodes,
             node_t **child_nodes_p)
{
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
      tmp = child_nodes_p[j], child_nodes_p[j] = child_nodes_p[j-1], child_nodes_p[j-1] = tmp;
      j--;
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

  if (is_terminal(n)) {
    leaf_count++;
    exact_terminal_game_value(n);
  } else {
    generate_child_nodes(&child_node_count, child_nodes, n);
    n->value = -66;
    for (int i = 0; i < child_node_count; i++) {
      node_t *child = &child_nodes[i];
      leaf_negamax(child, -beta, -alpha);
      const int child_value = - child->value;
      n->value = max(n->value, child_value);
      alpha = max(alpha, n->value);
      if (alpha >= beta) break;
    }
  }

  return;
}

/*

int negascout (game_position, depth, alpha, beta)
{
  if (depth == 0 || game_is_over(game_position)) // evaluate leaf game position from current player's standpoint
    return eval(game_position);
  score = -INF; // present return value
  n = beta;
  moves = generate(game_position); // generate successor moves
  for (int i = 0; i < sizeof(moves); i++) { // look over all moves
    make(moves[i]); // execute current move
    cur = -negascout(game_position, depth-1, -n, -alpha); // call other player, and switch sign of returned value
    if (cur > score) {
      if ((n == beta) || ( d <= 2))
        score = cur;
      else
        score = -negascout(game_position, depth-1, -beta, -cur);
    }
    if (score > alpha) alpha = score;
    undo(moves[i]);
    if (alpha >= beta) return alpha;
    n = alpha+1;
  }
  return score;
}

 */

static const bool tt_active = true;

static void
negamax (node_t *n,
         const int depth,
         const int alpha,
         const int beta)
{
  int child_node_count;
  node_t child_nodes[64];
  node_t *child_nodes_p[64];
  int empty_count;

  int am = alpha; // alpha-mobile : local value that is adjusted during the search
  int bm = beta;  // beta-mobile  : " ...

  node_count++;

  c9++;


  uint64_t hash = 0;
  tratab_item_t *item = NULL;
  if (tt_active) {
    hash = game_position_x_hash(&n->gpx);
    item = tratab_item_retrieve(tt, hash, &n->gpx, depth);
    if (item) {
      if (item->data.lower_bound >= bm) {
        n->value = item->data.lower_bound;
        c0++;
        return;
      }
      if (item->data.upper_bound <= am) {
        n->value = item->data.upper_bound;
        c1++;
        return;
      }
      c2++;
      am = max(am, item->data.lower_bound);
      bm = min(bm, item->data.upper_bound);
    }
  }

  if (is_terminal(n)) {
    leaf_count++;
    exact_terminal_game_value(n);
  } else if (depth == 0) {
    heuristic_game_value(n);
  } else if ((empty_count = game_position_x_empty_count(&n->gpx)) < min_empty_count) {
    leaf_negamax(n, am, bm);
  } else {

    generate_child_nodes(&child_node_count, child_nodes, n);
    order_moves(child_node_count, child_nodes, child_nodes_p);

    n->value = -66;
    n->best_move = invalid_move;
    for (int i = 0; i < child_node_count; i++) {
      //if (search_depth - depth == 0) am = -64;
      node_t *child = child_nodes_p[i];
      if (search_depth_id - depth == 0) {
        printf("%02d - %2s (%+03d) [%+03d..%+03d]: ", i, square_as_move_to_string(child->parent_move), -child->value, am, bm);
        fflush(stdout);
      }

      int child_value;
      if (i == 0) {
        c3++;
        negamax(child, depth -1, -bm, -am);
        child_value = - child->value;
      } else {
        c4++;
        negamax(child, depth -1, -am -2, -am);
        child_value = - child->value;
        if (child_value > am && child_value < bm) {
          c5++;
          negamax(child, depth -1, -bm, -child_value);
          child_value = - child->value;
        }
      }

      if (search_depth_id - depth == 0) {
        char *s;
        if (-child->value <= am) s = "(failing low)   f+";
        else if (-child->value >= bm) s = "(failing high) f-";
        else s = "(success)       f ";
        printf("%s = %+03d - cumulated node count %zu\n", s, -child->value, node_count);
      }
      if (child_value > n->value) {
        n->value = child_value;
        n->best_move = child->parent_move;
      }
      am = max(am, n->value);
      if (am >= bm) break;
    }

  } // end-of-else

  if (tt_active) {
    int lower, upper;
    if (item) {
      c6++;
      lower = item->data.lower_bound;
      upper = item->data.upper_bound;
    } else {
      lower = -66;
      upper = +66;
    }
    c10++;
    if (n->value < beta) {
      c7++;
      upper = n->value;
    }
    if (n->value > alpha) {
      c8++;
      lower = n->value;
    }
    tratab_insert_item(tt, hash, &n->gpx, depth, lower, upper, n->best_move);
  }

  return;
}

/**
 * @endcond
 */

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

void
negamax_true (node_t *n,
              int depth,
              int alpha,
              int beta)
{
  int child_node_count;
  node_t child_nodes[64];
  node_t *child_nodes_p[64];
  int empty_count;

  node_count++;

  const uint64_t hash = game_position_x_hash(&n->gpx);
  tratab_item_t *item = tratab_item_retrieve(tt, hash, &n->gpx, depth);
  if (item) {
    if (item->data.lower_bound >= beta) {
      n->value = item->data.lower_bound;
      return;
    }
    if (item->data.upper_bound <= alpha) {
      n->value = item->data.upper_bound;
      return;
    }
    alpha = max(alpha, item->data.lower_bound);
    beta = min(beta, item->data.upper_bound);
  }

  if (is_terminal(n)) {
    leaf_count++;
    exact_terminal_game_value(n);
  } else if (depth == 0) {
    heuristic_game_value(n);
  } else if ((empty_count = game_position_x_empty_count(&n->gpx)) < min_empty_count) {
    leaf_negamax(n, alpha, beta);
  } else {

    generate_child_nodes(&child_node_count, child_nodes, n);
    order_moves(child_node_count, child_nodes, child_nodes_p);

    n->value = -66;
    n->best_move = invalid_move;
    for (int i = 0; i < child_node_count; i++) {
      if (search_depth - depth == 0) alpha = -64;
      node_t *child = child_nodes_p[i];
      negamax(child, depth -1, -beta, -alpha);
      const int child_value = - child->value;
      if (child_value > n->value) {
        n->value = child_value;
        n->best_move = child->parent_move;
      }
      alpha = max(alpha, n->value);
      if (alpha >= beta) break;
      if (search_depth - depth == 0) printf("%02d - %2s : %+03d\n", i, square_as_move_to_string(child->parent_move), -child->value);
    }

  } // end-of-else


  if (n->value <= alpha) {
    tratab_insert_item(tt, hash, &n->gpx, depth, -66, n->value, n->best_move);
  } else if (n->value > alpha && n->value < beta) {
    tratab_insert_item(tt, hash, &n->gpx, depth, n->value, n->value, n->best_move);
  } else if (n->value >= beta) {
    tratab_insert_item(tt, hash, &n->gpx, depth, n->value, +66, n->best_move);
  }

  return;
}
