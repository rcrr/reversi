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
 * $ time ./build/bin/endgame_solver -s gve -c cfg/game_value_estimator.cfg -f db/gpdb-ffo.txt -q ffo-29
 * $ time ./build/bin/endgame_solver -s gve -c cfg/game_value_estimator.cfg -f db/gpdb-sample-games.txt -q woc18-FKvsAP-g1-22
 *
 */

/*
 * Comment this line to enable assertion in the module.
 * The line must be inserted before the inclusion of <assert.h>
 */
#if !defined NDEBUG
#define NDEBUG
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <string.h>

#include "time_utils.h"
#include "file_utils.h"
#include "rglm_utils.h"
#include "transposition_table.h"
#include "game_value_estimator.h"



/**
 * @cond
 */

/*
 * Local data structures.
 */

typedef struct node_s {
  struct node_s *parent;
  SquareSet legal_move_set;
  int legal_move_count;
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
init_node (node_t *node,
           node_t *parent,
           SquareSet legal_move_set,
           Square parent_move,
           Square best_move,
           int value,
           SquareSet gpx_blacks,
           SquareSet gpx_whites,
           Player gpx_player);

static int
get_game_position_evaluation_summary_from_cfg (cfg_t *cfg,
                                               int *game_position_evaluation_summary);

static int
get_ttab_log_verbosity_from_cfg (cfg_t *cfg,
                                 int *ttab_log_verbosity);

static int
get_model_weights_check_digest_from_cfg (cfg_t *cfg,
                                         bool *model_weights_check_digest);

static int
get_model_weights_verbose_loader_from_cfg (cfg_t *cfg,
                                           bool *model_weights_verbose_loader);

static int
get_ttab_log_size_from_cfg (cfg_t *cfg,
                            int *ttab_log_size);

static int
get_id_step_from_cfg (cfg_t *cfg,
                      int *id_step);

static int
get_id_min_empty_count_from_cfg (cfg_t *cfg,
                                 int *id_min_empty_count);

static int
check_config_file (cfg_t *cfg);

static void
alphabeta_with_memory (node_t *n,
                       int depth,
                       int alpha,
                       int beta);

static void
mtdf (node_t *n,
      int alpha,
      int beta,
      const int depth);

static int
get_gve_solver_log_level_from_cfg (cfg_t *cfg,
                                   int *gve_solver_log_level);

static int
game_position_rglm_load_model_weights_files (bool verbose,
                                             bool check_digest,
                                             cfg_t *cfg);

static void
game_position_rglm_release_model_weights (void);

static double
rglm_eval_gp (const GamePositionX *const gpx);

static int
gv_f2d (const double f);

static int
max (int a,
     int b);

static int
min (int a,
     int b);

static void
heuristic_game_value (node_t *n);


/*
 * Internal variables and constants.
 */

/*
 * ###
 * ### Model Weights static variables.
 * ###
 */

/* Empty Count Size - [0..60] the game stages ... */
#define EC_SIZE 61

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

/* The count of game evaluations categorized by empty count. */
static uint64_t gp_evaluations[EC_SIZE];

/*
 * ###
 * ### End of section for: Model Weights static variables.
 * ###
 */

/* Iterative deepening minimum empty count. */
static int id_min_empty_count;

/* Iterative deepening step. */
static int id_step;

/* Search depth iterative deepening. */
static int id_search_depth;

/* Search depth. */
static int search_depth;

/* Transposition Table. */
static ttab_t ttab;

/* Transposition Table binary logarithm of size. */
static int ttab_log_size;

/* Transposition Table log verbosity. */
static int ttab_log_verbosity;

/* Count of nodes and leafs touchd by the algorithm. */
static uint64_t node_count;
static uint64_t leaf_count;

/* This is the empty count level where we start having model weight info available. */
static int first_level_evaluation;

/* Game position evaluation summary. */
static int game_position_evaluation_summary;

/* Log level for the solver. */
static int gve_solver_log_level;

/**
 * @endcond
 */



/*
 * Public functions.
 */

void
init_node (node_t *node,
           node_t *parent,
           SquareSet legal_move_set,
           Square parent_move,
           Square best_move,
           int value,
           SquareSet gpx_blacks,
           SquareSet gpx_whites,
           Player gpx_player)
{
  assert(node);

  node->parent = parent;
  node->legal_move_set = legal_move_set;
  node->legal_move_count = bitw_bit_count_64(legal_move_set);
  node->parent_move = parent_move;
  node->best_move = best_move;
  node->value = value;
  node->gpx.blacks = gpx_blacks;
  node->gpx.whites = gpx_whites;
  node->gpx.player = gpx_player;
  node->hash = game_position_x_hash(&node->gpx);
}

ExactSolution *
game_position_value_estimator (const GamePositionX *const root,
                               const endgame_solver_env_t *const env)
{
  assert(root);
  assert(env);

  /* Used to drive the search to be complete and touch the leafs of the game tree. */
  const int max_search_depth = 99;

  /* Stopwatch variables. */
  //timespec_t time_0, time_1, delta_cpu_time, start_time, end_time, delta_time;
  timespec_t time_0_a, time_1_a, delta_cpu_time_a, start_time_a, end_time_a, delta_time_a;
  timespec_t time_0_b, time_1_b, delta_cpu_time_b, start_time_b, end_time_b, delta_time_b;

  int estimated_value;

  node_count = 0;
  leaf_count = 0;

  for (int i = 0; i < EC_SIZE; i++)
    gp_evaluations[i] = 0;

  search_depth = env->search_depth;

  ExactSolution *result = NULL;
  bool model_weights_verbose_loader;
  bool model_weights_check_digest;
  int ret_err;
  ret_err =  check_config_file(env->cfg);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error in the config file. Exiting ...\n");
    exit(EXIT_FAILURE);
  }
  ret_err = get_model_weights_verbose_loader_from_cfg(env->cfg, &model_weights_verbose_loader);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error in the config file. Exiting ...\n");
    exit(EXIT_FAILURE);
  }
  ret_err = get_model_weights_check_digest_from_cfg(env->cfg, &model_weights_check_digest);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error in the config file. Exiting ...\n");
    exit(EXIT_FAILURE);
  }
  ret_err = game_position_rglm_load_model_weights_files(model_weights_verbose_loader, model_weights_check_digest, env->cfg);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading RGLM solver model weights files. Exiting ...\n");
    exit(EXIT_FAILURE);
  }
  ret_err = get_id_min_empty_count_from_cfg(env->cfg, &id_min_empty_count);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key id_min_empty_count from section gve_solver from the config file. Exiting ...\n");
    exit(EXIT_FAILURE);
  }
  ret_err = get_id_step_from_cfg(env->cfg, &id_step);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key id_step from section gve_solver from the config file. Exiting ...\n");
    exit(EXIT_FAILURE);
  }
  ret_err = get_ttab_log_size_from_cfg(env->cfg, &ttab_log_size);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key ttab_log_size from section gve_solver from the config file. Exiting ...\n");
    exit(EXIT_FAILURE);
  }
  ret_err = get_ttab_log_verbosity_from_cfg(env->cfg, &ttab_log_verbosity);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key ttab_log_verbosity from section gve_solver from the config file. Exiting ...\n");
    exit(EXIT_FAILURE);
  }
  ret_err = get_game_position_evaluation_summary_from_cfg(env->cfg, &game_position_evaluation_summary);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key game_position_evaluation_summary from section gve_solver from the config file. Exiting ...\n");
    exit(EXIT_FAILURE);
  }
  ret_err = get_gve_solver_log_level_from_cfg(env->cfg, &gve_solver_log_level);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key gve_solver_log_level from section gve_solver from the config file. Exiting ...\n");
    exit(EXIT_FAILURE);
  }

  result = exact_solution_new();
  exact_solution_init(result);
  exact_solution_set_root(result, root);

  const int ec = game_position_x_empty_count(root);
  if (search_depth < 0) search_depth = max_search_depth;

  if (gve_solver_log_level >= 1)
    printf("Empty count = %d, Search depth = %d, ID minimum empty count = %d\n", ec, search_depth, id_min_empty_count);

  int idx;
  int model_weight_count = 0;
  int max_model_weight = 0;
  int min_model_weight = 0;
  for (idx = 0; idx <= 60; idx++) {
    if (mws[idx] != NULL)
      model_weight_count++;
  }
  for (idx = 0; idx <= 60; idx++)
    if (mws[idx] != NULL) break;
  min_model_weight = idx;
  for (idx = 60; idx >= 0; idx--)
    if (mws[idx] != NULL) break;
  max_model_weight = idx;
  if (gve_solver_log_level >= 1)
    printf("Model weights data range: [%02d..%02d]. Model weights count: %02d\n", min_model_weight, max_model_weight, model_weight_count);
  if (model_weight_count == 0) {
    printf("There is no model weight defnition into the configuration file. Exiting ...\n");
    exit(EXIT_FAILURE);
  }
  for (idx = min_model_weight; idx <= max_model_weight; idx++) {
    if (mws[idx] == NULL) {
      printf("The range of model weight files must be complete without missing values. File for empty_count = %02d is missing\n", idx);
      exit(EXIT_FAILURE);
    }
  }

  if (id_min_empty_count <= min_model_weight) {
    printf("The minimum model weight value must be smaller than ID minimum empty count. Exiting ...\n");
    exit(EXIT_FAILURE);
  }

  int last_level_evaluation;
  if (ec < min_model_weight) {
    printf("Game position empty count (%02d) cannot be lesser than min_model_weight (%02d). Exiting ...\n", ec, min_model_weight);
    exit(EXIT_FAILURE);
  }
  first_level_evaluation = min(ec, max_model_weight);
  last_level_evaluation = id_min_empty_count - 1;
  if (gve_solver_log_level >= 1)
    printf("Game evaluation will happen in the range [%02d..%02d].\n", first_level_evaluation, last_level_evaluation);

  if (gve_solver_log_level >= 1)
    if (first_level_evaluation < ec) {
      printf("Game evaluation empty count (%02d) is larger than first available model weight (%02d).\n", ec, first_level_evaluation);
    }

  const int search_depth_initial_gap = (ec == first_level_evaluation) ? 1 : ec - first_level_evaluation;
  if (gve_solver_log_level >= 1)
    printf("Search depth initial gap is equal to %02d.\n", search_depth_initial_gap);

  ttab = ttab_new(ttab_log_size);
  if (!ttab) abort();

  node_t parent_root_node;
  init_node(&parent_root_node,
            NULL, empty_square_set, invalid_move, invalid_move, out_of_range_defeat_score,
            root->blacks, root->whites, player_opponent(root->player));

  node_t root_node;
  init_node(&root_node,
            &parent_root_node, empty_square_set, invalid_move, invalid_move, out_of_range_defeat_score,
            root->blacks, root->whites, root->player);

  const int id_limit = min(search_depth, ec - id_min_empty_count);
  if (gve_solver_log_level >= 1) {
    printf("Iterative deepening search_depth limit (id_limit): %d\n", id_limit);
    printf("Iterative deepening search_depth increment step (id_step): %d\n", id_step);
  }

  const bool mw_available = mws[ec] != NULL;
  if (gve_solver_log_level >= 1)
    if (mw_available) {
      heuristic_game_value(&root_node);
      printf("Node Level 0: estimated game value = %+03d\n", root_node.value);
    }

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time_a);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0_a);

  if (search_depth > 0) {
    id_search_depth = search_depth_initial_gap;
    while (true) {
      uint64_t nc = node_count;
      if (gve_solver_log_level >= 2) {
        timespec_print_local_time(stdout);
        printf(" #### start  .%02d. Iterative deepening search depth = %d\n", id_search_depth, id_search_depth);
        clock_gettime(CLOCK_REALTIME, &start_time_b);
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0_b);
      }
      mtdf(&root_node, env->alpha, env->beta, id_search_depth);
      if (gve_solver_log_level >= 2) {
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1_b);
        clock_gettime(CLOCK_REALTIME, &end_time_b);
        timespec_print_local_time(stdout);
        timespec_diff(&delta_time_b, &start_time_b, &end_time_b);
        timespec_diff(&delta_cpu_time_b, &time_0_b, &time_1_b);
        printf(" #### finish .%02d. Best move and game value: [%2s:%+03d] - nc = %12zu - ",
               id_search_depth, square_as_move_to_string(root_node.best_move), root_node.value, node_count - nc);
        printf("Time [REAL][PROC]: [%6lld.%9ld][%6lld.%9ld]\n",
               (long long) timespec_get_sec(&delta_cpu_time_b), timespec_get_nsec(&delta_cpu_time_b),
               (long long) timespec_get_sec(&delta_time_b), timespec_get_nsec(&delta_time_b));
      }
      if (id_search_depth >= search_depth) break;
      if (id_search_depth == id_limit) id_search_depth = search_depth;
      else if (id_search_depth + id_step > id_limit) id_search_depth = id_limit;
      else id_search_depth += id_step;
    }
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1_a);
  clock_gettime(CLOCK_REALTIME, &end_time_a);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time_a, &start_time_a, &end_time_a);
  timespec_diff(&delta_cpu_time_a, &time_0_a, &time_1_a);
  if (gve_solver_log_level >= 1) {
    printf("MTD(f) evaluation [REAL][PROCESS] time: ");
    printf("[%6lld.%9ld][%6lld.%9ld]\n",
           (long long) timespec_get_sec(&delta_cpu_time_a), timespec_get_nsec(&delta_cpu_time_a),
           (long long) timespec_get_sec(&delta_time_a), timespec_get_nsec(&delta_time_a));
  }

  estimated_value = root_node.value;

  result->best_move = root_node.best_move;
  result->outcome = estimated_value;
  result->node_count = node_count;
  result->leaf_count = leaf_count;

  if (ttab_log_verbosity > 0) {
    const size_t tt_stats_max_size = 42;
    size_t tt_stats_size;
    size_t tt_stats[tt_stats_max_size];
    ttab_summary_to_stream(ttab, stdout);
    ttab_bucket_filling_stats(ttab, tt_stats, tt_stats_max_size);
    for (tt_stats_size = tt_stats_max_size -1; tt_stats_size > 0; tt_stats_size--)
      if (tt_stats[tt_stats_size] > 0) break;
    printf("TT hashtable stats:\n");
    printf(" ELEMENTS_X_BUCKET; ELEMENT_CNT\n");
    for (size_t i = 0; i < tt_stats_size; i++) {
      printf("%18zu;%12zu\n", i, tt_stats[i]);
    }
  }
  ttab_free(&ttab);

  game_position_rglm_release_model_weights();

  if (game_position_evaluation_summary > 0) {
    uint64_t total_gp_eval_count = 0;
    for (int i = 0; i < EC_SIZE; i++) {
      total_gp_eval_count += gp_evaluations[i];
    }
    printf("Total game position evaluation count: %zu\n", total_gp_eval_count);
    printf(" EMPTY_COUNT;  EVALUATIONS\n");
    for (int i = 0; i < EC_SIZE; i++)
      if (gp_evaluations[i] > 0) printf("          %02d; %12zu\n", i, gp_evaluations[i]);
  }

  return result;
}


/**
 * @cond
 */

/*
 * Internal functions.
 */

static int
get_gve_solver_log_level_from_cfg (cfg_t *cfg,
                                   int *gve_solver_log_level)
{
  int value;

  const char *gve_solver_log_level_s = cfg_get(cfg, "gve_solver", "gve_solver_log_level");
  if (!gve_solver_log_level_s) {
    fprintf(stderr, "The key gve_solver_log_level is missing from section gve_solver.\n");
    return EXIT_FAILURE;
  }

  value = atoi(gve_solver_log_level_s);
  if (value < 0 || value > 3) {
    fprintf(stderr, "The key gve_solver_log_level is out of range. It must be in [0..3].\n");
    return EXIT_FAILURE;
  }

  *gve_solver_log_level = value;
  return EXIT_SUCCESS;
}

static int
get_game_position_evaluation_summary_from_cfg (cfg_t *cfg,
                                               int *game_position_evaluation_summary)
{
  int value;

  const char *game_position_evaluation_summary_s = cfg_get(cfg, "gve_solver", "game_position_evaluation_summary");
  if (!game_position_evaluation_summary_s) {
    fprintf(stderr, "The key game_position_evaluation_summary is missing from section gve_solver.\n");
    return EXIT_FAILURE;
  }

  value = atoi(game_position_evaluation_summary_s);
  if (value < 0 || value > 1) {
    fprintf(stderr, "The key game_position_evaluation_summary is out of range. It must be in [0..1].\n");
    return EXIT_FAILURE;
  }

  *game_position_evaluation_summary = value;
  return EXIT_SUCCESS;
}

static int
get_ttab_log_verbosity_from_cfg (cfg_t *cfg,
                                 int *ttab_log_verbosity)
{
  int value;

  const char *ttab_log_verbosity_s = cfg_get(cfg, "gve_solver", "ttab_log_verbosity");
  if (!ttab_log_verbosity_s) {
    fprintf(stderr, "The key ttab_log_verbosity is missing from section gve_solver.\n");
    return EXIT_FAILURE;
  }

  value = atoi(ttab_log_verbosity_s);
  if (value < 0 || value > 1) {
    fprintf(stderr, "The key ttab_log_verbosity is out of range. It must be in [0..1].\n");
    return EXIT_FAILURE;
  }

  *ttab_log_verbosity = value;
  return EXIT_SUCCESS;
}

static int
get_model_weights_check_digest_from_cfg (cfg_t *cfg,
                                         bool *model_weights_check_digest)
{
  bool value;

  const char *model_weights_check_digest_s = cfg_get(cfg, "model_weights", "check_digest");
  if (!model_weights_check_digest_s) {
    fprintf(stderr, "The key check_digest is missing from section model_weights.\n");
    return EXIT_FAILURE;
  }

  if (strcmp(model_weights_check_digest_s, "true") == 0) {
    value = true;
  } else if (strcmp(model_weights_check_digest_s, "false") == 0) {
    value = false;
  } else {
    fprintf(stderr, "The key check_digest in section model_weights doesn't have value in the range [true|false].\n");
    return EXIT_FAILURE;
  }

  *model_weights_check_digest = value;
  return EXIT_SUCCESS;
}

static int
get_model_weights_verbose_loader_from_cfg (cfg_t *cfg,
                                           bool *model_weights_verbose_loader)
{
  bool value;

  const char *model_weights_verbose_loader_s = cfg_get(cfg, "model_weights", "verbose_loader");
  if (!model_weights_verbose_loader_s) {
    fprintf(stderr, "The key verbose_loader is missing from section model_weights.\n");
    return EXIT_FAILURE;
  }

  if (strcmp(model_weights_verbose_loader_s, "true") == 0) {
    value = true;
  } else if (strcmp(model_weights_verbose_loader_s, "false") == 0) {
    value = false;
  } else {
    fprintf(stderr, "The key verbose_loader in section model_weights doesn't have value in the range [true|false].\n");
    return EXIT_FAILURE;
  }

  *model_weights_verbose_loader = value;
  return EXIT_SUCCESS;
}

static int
get_ttab_log_size_from_cfg (cfg_t *cfg,
                            int *ttab_log_size)
{
  int value;

  const char *ttab_log_size_s = cfg_get(cfg, "gve_solver", "ttab_log_size");
  if (!ttab_log_size_s) {
    fprintf(stderr, "The key ttab_log_size is missing from section gve_solver.\n");
    return EXIT_FAILURE;
  }

  value = atoi(ttab_log_size_s);
  if (value < 0 || value > 64) {
    fprintf(stderr, "The key ttab_log_size is out of range. It must be in [0..64].\n");
    return EXIT_FAILURE;
  }

  *ttab_log_size = value;
  return EXIT_SUCCESS;
}

static int
get_id_step_from_cfg (cfg_t *cfg,
                      int *id_step)
{
  int value;

  const char *id_step_s = cfg_get(cfg, "gve_solver", "id_step");
  if (!id_step_s) {
    fprintf(stderr, "The key id_step is missing from section gve_solver.\n");
    return EXIT_FAILURE;
  }

  value = atoi(id_step_s);
  if (value < 1 || value > 10) {
    fprintf(stderr, "The key id_step is out of range. It must be in [1..10].\n");
    return EXIT_FAILURE;
  }

  *id_step = value;
  return EXIT_SUCCESS;
}

static int
get_id_min_empty_count_from_cfg (cfg_t *cfg,
                                 int *id_min_empty_count)
{
  int value;

  const char *id_min_empty_count_s = cfg_get(cfg, "gve_solver", "id_min_empty_count");
  if (!id_min_empty_count_s) {
    fprintf(stderr, "The key id_min_empty_count is missing from section gve_solver.\n");
    return EXIT_FAILURE;
  }

  value = atoi(id_min_empty_count_s);
  if (value < 0 || value > 60) {
    fprintf(stderr, "The key id_min_empty_count is out of range. It must be in [0..60].\n");
    return EXIT_FAILURE;
  }

  *id_min_empty_count = value;
  return EXIT_SUCCESS;
}

static int
check_config_file (cfg_t *cfg)
{
  if (!cfg) {
    fprintf(stderr, "The configuration file is mandatory for the gve solver.\n");
    return EXIT_FAILURE;
  }
  const char *gve_solver_check_key = cfg_get(cfg, "gve_solver", "check_key");
  if (gve_solver_check_key == NULL) {
    fprintf(stderr, "The key check_key is missing from section gve_solver.\n");
    return EXIT_FAILURE;
  }
  if (strcmp(gve_solver_check_key, "true") != 0) {
    fprintf(stderr, "The key check_key doesn't have value equal to true. check_key=%s\n", gve_solver_check_key);
    return EXIT_FAILURE;
  }
  const char *model_weights_check_key = cfg_get(cfg, "model_weights", "check_key");
  if (model_weights_check_key == NULL) {
    fprintf(stderr, "The key check_key is missing from section model_weights.\n");
    return EXIT_FAILURE;
  }
  if (strcmp(model_weights_check_key, "true") != 0) {
    fprintf(stderr, "The key check_key doesn't have value equal to true.\n");
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

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

  gp_evaluations[empty_count]++;

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
game_position_rglm_load_model_weights_files (bool verbose,
                                             bool check_digest,
                                             cfg_t *cfg)
{
  int ret_value;

  for (size_t i = 0; i < EC_SIZE; i++) {
    mws_f[i] = cfg_get(cfg, "model_weights", mws_l[i]);
  }

  for (size_t i = 0; i < EC_SIZE; i++) {
    rglmdf_model_weights_init(mws_s);
    mws[i] = NULL;
  }

  for (size_t i = 0; i < EC_SIZE; i++) {
    const char *filename = mws_f[i];
    if (filename) {
      ret_value = rglmdf_model_weights_read_from_binary_file(&mws_s[i], filename, verbose, check_digest);
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
  return EXIT_SUCCESS;
}

static void
game_position_rglm_release_model_weights (void)
{
  for (size_t i = 0; i < EC_SIZE; i++) {
    rglmdf_model_weights_release(&mws_s[i]);
    mws[i] = NULL;
  }
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
  n->legal_move_count = bitw_bit_count_64(n->legal_move_set);
}

/**
 * @brief Generates child nodes
 *
 * @param [out] child_node_count pointer to the count of child nodes
 * @param [out] child_nodes      array of children nodes
 * @param [in]  n                pointer to the node structure
 * @param [in]  compute_hash     drives the computation of the hash for child nodes
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
      c->parent_move = move;
      c->best_move = unknown_move;
      c->value = out_of_range_win_score;
      if (compute_hash) game_position_x_make_move_delta_hash(&n->gpx, move, &c->gpx, n->hash, &c->hash);
      else game_position_x_make_move(&n->gpx, move, &c->gpx);
      c->legal_move_set = game_position_x_legal_moves(&c->gpx);
      c->legal_move_count = bitw_bit_count_64(c->legal_move_set);
      lms = bitw_reset_lowest_set_bit_64(lms);
    }
  } else {
    lmc = 1;
    node_t *const c = child_nodes + 0;
    c->parent = n;
    c->parent_move = pass_move;
    c->best_move = unknown_move;
    c->value = out_of_range_win_score;
    if (compute_hash) game_position_x_make_move_delta_hash(&n->gpx, pass_move, &c->gpx, n->hash, &c->hash);
    else game_position_x_make_move(&n->gpx, pass_move, &c->gpx);
    c->legal_move_set = game_position_x_legal_moves(&c->gpx);
    c->legal_move_count = bitw_bit_count_64(c->legal_move_set);
  }

  *child_node_count = lmc;
}

static void
order_moves (int child_node_count,
             node_t *child_nodes,
             node_t **child_nodes_p,
             ttab_item_t it,
             bool heuristic_sort)
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
      if (heuristic_sort) heuristic_game_value(child);
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
sort_nodes_by_lmc (node_t **nodes,
                   int count)
{
  for (int i = 1; i < count; i++) {
    int j = i;
    for (;;) {
      node_t *tmp;
      if (j == 0 || (*(nodes + j - 1))->legal_move_count <= (*(nodes + j))->legal_move_count) break;
      tmp = *(nodes + j), *(nodes + j) = *(nodes + j - 1), *(nodes + j - 1) = tmp; // Swaps elements.
      j--;
    }
  }
}

static void
leaf_negamax (node_t *n,
              int alpha,
              int beta)
{
  int child_node_count;
  node_t child_nodes[64];
  node_t *sorted_child_nodes[64];

  node_count++;

  n->legal_move_set = game_position_x_legal_moves(&n->gpx);

  if (is_terminal(n)) {
    leaf_count++;
    exact_terminal_game_value(n);
  } else {
    generate_child_nodes(&child_node_count, child_nodes, n, false);
    for (int i = 0; i < child_node_count; i++) sorted_child_nodes[i] = &child_nodes[i];
    sort_nodes_by_lmc(sorted_child_nodes, child_node_count);
    n->value = out_of_range_defeat_score;
    for (int i = 0; i < child_node_count; i++) {
      node_t *child = sorted_child_nodes[i];
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
  int explored_child_count;

  const int ec = game_position_x_empty_count(&n->gpx);

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
    its.legal_move_set = empty_square_set;
    its.legal_move_count = 0;
    for (int i = 0; i < TTAB_RECORDED_BEST_MOVE_COUNT; i++) {
      its.best_moves[i] = unknown_move;
      its.move_values[i] = out_of_range_defeat_score;
    }
  }

  generate_legal_move_set(n, it);

  explored_child_count = 0;
  if (is_terminal(n)) {
    leaf_count++;
    exact_terminal_game_value(n);
  } else if (depth == 0) {
    if (n->value == out_of_range_win_score) heuristic_game_value(n);
    n->best_move = unknown_move;
  } else if (ec < id_min_empty_count) {
    leaf_negamax(n, am, bm);
  } else {

    generate_child_nodes(&child_node_count, child_nodes, n, true);
    its.legal_move_count = child_node_count;
    order_moves(child_node_count, child_nodes, child_nodes_p, it, ec <= first_level_evaluation);

    n->value = out_of_range_defeat_score;
    n->best_move = invalid_move;
    for (int i = 0; i < child_node_count; i++) {
      node_t *child = child_nodes_p[i];

      if (id_search_depth - depth == 0)
        if (gve_solver_log_level >= 3) {
          timespec_print_local_time(stdout);
          printf(" %02d - %2s (%+03d) [%+03d..%+03d]: ", i, square_as_move_to_string(child->parent_move), -child->value, am, bm);
          fflush(stdout);
        }

      const int child_depth = (child->parent_move == pass_move) ? depth : depth -1;
      alphabeta_with_memory(child, child_depth, -bm, -am);

      if (id_search_depth - depth == 0)
        if (gve_solver_log_level >= 3) {
          char *s;
          if (-child->value <= am) s = "(failing low)  f+";
          else if (-child->value >= bm) s = "(failing high) f-";
          else s = "(success)       f ";
          printf("%s = %+03d - cumulated node count %12zu\n", s, -child->value, node_count);
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
      int alpha,
      int beta,
      const int depth)
{
  int bound[2] = { alpha, beta };
  do {
    const int beta = n->value + (n->value == bound[0]) * 2;
    alphabeta_with_memory(n, depth, beta -2, beta);
    bound[n->value < beta] = n->value;
  } while (bound[0] < bound[1]);
}



/**
 * @endcond
 */
