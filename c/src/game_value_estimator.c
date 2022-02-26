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
 * @copyright 2021, 2022 Roberto Corradini. All rights reserved.
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
#include "game_value_estimator.h"



/**
 * @cond
 */

/*
 * Empty Count Size - [0..60] the game stages ...
 */
#define EC_SIZE 61

/*
 * Local data structures.
 */

/*
 * Collects all the info related to the Model Weights used by the GVE solver.
 */
typedef struct model_weights_data_s {
  rglmdf_model_weights_t mws_s[EC_SIZE]; /* Array of model weights structures. */
  rglmdf_model_weights_t *mws[EC_SIZE];  /* Pointers to the mw structures. A NULL value signals a missing data file. */
  char *file_names[EC_SIZE];             /* File names for the model weights data. */
  bool verbose_loader;                   /* When true the file loading process is verbose. */
  bool check_digest;                     /* When true the data files are checked against the SHA hash. */
  int model_weight_count;                /* Count of model weight instances declared in the config file. */
  int max_model_weight;                  /* Maximum empty count value found in the model weighs array. */
  int min_model_weight;                  /* Minimum empty count value found in the model weighs array.*/
} model_weights_data_t;

/*
 * Collects all the info required by the GVE solver to run.
 */
struct gve_context_s {
  GamePositionX root;                    /* Root game position. */
  int root_empty_count;                  /* Empty count at the root node. */
  model_weights_data_t mwd;              /* Model weights data. */
  uint64_t gp_evaluations[EC_SIZE];      /* The count of game evaluations categorized by empty count. */
  int id_min_empty_count;                /* Iterative deepening minimum empty count. */
  int id_step;                           /* Iterative deepening step. */
  int id_search_depth;                   /* Search depth iterative deepening. */
  int id_limit;                          /* Limit of the iterative search depth (maximum id depth). */
  int search_depth;                      /* Search depth. */
  int search_depth_initial_gap;          /* It is the difference between root empty count and the ec at the first available model weigths. */
  ttab_t ttab;                           /* Transposition Table. */
  int ttab_log_size;                     /* Transposition Table binary logarithm of size. */
  int ttab_log_verbosity;                /* Transposition Table log verbosity. */
  uint64_t node_count;                   /* Count of nodes touchd by the algorithm. */
  uint64_t leaf_count;                   /* Count of leafs touchd by the algorithm. */
  int first_level_evaluation;            /* This is the empty count level where we start having model weight info available. */
  int last_level_evaluation;             /* It is the empty count value at the deepest evaluation done using the model weights heuristic. */
  int game_position_evaluation_summary;  /* Game position evaluation summary. */
  int gve_solver_log_level;              /* Log level for the solver. */
};

typedef struct node_s {
  struct node_s *parent;
  SquareSet legal_move_set;
  int legal_move_count;
  Square parent_move;
  Square best_move;
  int value;
  GamePositionX gpx;
  uint64_t hash;
  SquareSet empty_set;
  int empty_count;
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
get_gve_solver_log_level_from_cfg (cfg_t *cfg,
                                   int *gve_solver_log_level);

static int
check_model_weight_config_file (cfg_t *cfg);

static int
check_gve_solver_config_file (cfg_t *cfg);

static void
exact_terminal_game_value (node_t *n);

static void
generate_child_nodes (node_t *child_nodes,
                      node_t *n,
                      bool compute_hash);

static void
order_moves (int child_node_count,
             node_t *child_nodes,
             node_t **child_nodes_p,
             ttab_item_t it,
             bool heuristic_sort,
             gve_context_t ctx);

static void
leaf_end_negamax (node_t *n,
                  int alpha,
                  int beta,
                  gve_context_t ctx);

static void
leaf_negamax (node_t *n,
              int alpha,
              int beta,
              gve_context_t ctx);

static void
sort_nodes_by_lmc (node_t **nodes,
                   int count);

static void
alphabeta_with_memory (node_t *n,
                       int depth,
                       int alpha,
                       int beta,
                       gve_context_t ctx);

static void
mtdf (node_t *n,
      int alpha,
      int beta,
      const int depth,
      gve_context_t ctx);

static double
rglm_eval_gp (const GamePositionX *const gpx,
              gve_context_t ctx);

static int
gv_f2d (const double f);

static int
max (int a,
     int b);

static int
min (int a,
     int b);

static void
heuristic_game_value (node_t *n,
                      gve_context_t ctx);

static bool
is_terminal (node_t *n);

static void
model_weights_data_release (model_weights_data_t *mwd);

static int
model_weights_data_init (model_weights_data_t *mwd,
                         cfg_t *cfg);

static void
game_position_eval_summary_table (FILE *stream,
                                  gve_context_t ctx);


/*
 * Internal variables and constants.
 */

/**
 * @endcond
 */



/*
 * Public functions.
 */

ExactSolution *
game_position_gve_solve (gve_context_t ctx,
                         const endgame_solver_env_t *const env)
{
  assert(ctx);
  assert(env);
  assert(root);

  const GamePositionX *const root = &ctx->root;

  /* Stopwatch variables. */
  timespec_t time_0_a, time_1_a, delta_cpu_time_a, start_time_a, end_time_a, delta_time_a;
  timespec_t time_0_b, time_1_b, delta_cpu_time_b, start_time_b, end_time_b, delta_time_b;

  ExactSolution *result = NULL;
  result = exact_solution_new();
  exact_solution_init(result);
  exact_solution_set_root(result, root);

  node_t parent_root_node;
  init_node(&parent_root_node,
            NULL, empty_square_set, invalid_move, invalid_move, out_of_range_defeat_score,
            root->blacks, root->whites, player_opponent(root->player));

  node_t root_node;
  init_node(&root_node,
            &parent_root_node, game_position_x_legal_moves(root), invalid_move, invalid_move, out_of_range_defeat_score,
            root->blacks, root->whites, root->player);

  if (ctx->gve_solver_log_level >= 1) {
    const bool root_mw_available = ctx->mwd.mws[ctx->root_empty_count] != NULL;
    if (root_mw_available) {
      heuristic_game_value(&root_node, ctx);
      printf("Node Level 0: estimated game value = %+03d\n", root_node.value);
    }
  }

  /* Starts the stop-watch. */
  clock_gettime(CLOCK_REALTIME, &start_time_a);
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0_a);

  if (ctx->search_depth > 0) {
    ctx->id_search_depth = ctx->search_depth_initial_gap;
    while (true) {
      const uint64_t nc0 = ctx->node_count;
      if (ctx->gve_solver_log_level >= 2) {
        timespec_print_local_time(stdout);
        printf(" #### start  .%02d. Iterative deepening search depth = %d\n", ctx->id_search_depth, ctx->id_search_depth);
        clock_gettime(CLOCK_REALTIME, &start_time_b);
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_0_b);
      }

      mtdf(&root_node, env->alpha, env->beta, ctx->id_search_depth, ctx);

      if (ctx->gve_solver_log_level >= 2) {
        clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1_b);
        clock_gettime(CLOCK_REALTIME, &end_time_b);
        timespec_print_local_time(stdout);
        timespec_diff(&delta_time_b, &start_time_b, &end_time_b);
        timespec_diff(&delta_cpu_time_b, &time_0_b, &time_1_b);
        printf(" #### finish .%02d. Best move and game value: [%2s:%+03d] - nc = %12zu - ",
               ctx->id_search_depth, square_as_move_to_string(root_node.best_move), root_node.value, ctx->node_count - nc0);
        printf("Time [REAL][PROC]: [%6lld.%9ld][%6lld.%9ld]\n",
               (long long) timespec_get_sec(&delta_cpu_time_b), timespec_get_nsec(&delta_cpu_time_b),
               (long long) timespec_get_sec(&delta_time_b), timespec_get_nsec(&delta_time_b));
      }
      if (ctx->id_search_depth >= ctx->search_depth) break;
      if (ctx->id_search_depth == ctx->id_limit) ctx->id_search_depth = ctx->search_depth;
      else if (ctx->id_search_depth + ctx->id_step > ctx->id_limit) ctx->id_search_depth = ctx->id_limit;
      else ctx->id_search_depth += ctx->id_step;
    }
  }

  /* Stops the stop-watch. */
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &time_1_a);
  clock_gettime(CLOCK_REALTIME, &end_time_a);

  /* Computes the time taken, and updates the test cpu_time. */
  timespec_diff(&delta_time_a, &start_time_a, &end_time_a);
  timespec_diff(&delta_cpu_time_a, &time_0_a, &time_1_a);
  if (ctx->gve_solver_log_level >= 1) {
    printf("MTD(f) evaluation [REAL][PROCESS] time: ");
    printf("[%6lld.%9ld][%6lld.%9ld]\n",
           (long long) timespec_get_sec(&delta_cpu_time_a), timespec_get_nsec(&delta_cpu_time_a),
           (long long) timespec_get_sec(&delta_time_a), timespec_get_nsec(&delta_time_a));
  }

  result->best_move = root_node.best_move;
  result->outcome = root_node.value;
  result->node_count = ctx->node_count;
  result->leaf_count = ctx->leaf_count;

  if (ctx->ttab_log_verbosity > 0)
    ttab_stats_to_stream(ctx->ttab, stdout);

  if (ctx->game_position_evaluation_summary > 0)
    game_position_eval_summary_table(stdout, ctx);

  return result;
}

ExactSolution *
game_position_value_estimator (const GamePositionX *const root,
                               const endgame_solver_env_t *const env)
{
  assert(root);
  assert(env);

  int ret_err;
  gve_context_t ctx = gve_context_new();
  if (!ctx) {
    fprintf(stderr, "Error during context memory allocation. Exiting ... \n");
    exit(EXIT_FAILURE);
  }
  ret_err = gve_context_init(ctx, env, root);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error during context initialization. Exiting ... \n");
    exit(EXIT_FAILURE);
  }

  ExactSolution *result = game_position_gve_solve(ctx, env);

  gve_context_release(ctx);

  return result;
}

gve_context_t
gve_context_new (void)
{
  gve_context_t ctx;
  ctx = malloc(sizeof(*ctx));
  return ctx;
}

void
gve_context_release (gve_context_t ctx)
{
  if (!ctx) return;

  ttab_free(&ctx->ttab);
  model_weights_data_release(&ctx->mwd);
  free(ctx);
}

int
gve_context_init (gve_context_t ctx,
                  const endgame_solver_env_t *env,
                  const GamePositionX *root)
{
  assert(ctx);
  assert(env);

  /* Used to drive the search to be complete and touch the leafs of the game tree. */
  const int max_search_depth = 99;

  if (root) {
    ctx->root.blacks = root->blacks;
    ctx->root.whites = root->whites;
    ctx->root.player = root->player;
    ctx->root_empty_count = game_position_x_empty_count(root);
  } else {
    ctx->root.blacks = empty_square_set;
    ctx->root.whites = full_square_set;
    ctx->root.player = BLACK_PLAYER;
    ctx->root_empty_count = 0;
  }

  int ret_err;
  ret_err = model_weights_data_init(&ctx->mwd, env->cfg);
  if (ret_err != EXIT_SUCCESS) {
    printf("Error initializing model weights data structure.\n");
    return EXIT_FAILURE;
  }

  for (int i = 0; i < EC_SIZE; i++)
    ctx->gp_evaluations[i] = 0;

  ret_err = get_id_min_empty_count_from_cfg(env->cfg, &ctx->id_min_empty_count);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key id_min_empty_count from section gve_solver from the config file. Exiting ...\n");
    return EXIT_FAILURE;
  }

  ret_err = get_id_step_from_cfg(env->cfg, &ctx->id_step);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key id_step from section gve_solver from the config file. Exiting ...\n");
    return EXIT_FAILURE;
  }

  ret_err = get_ttab_log_size_from_cfg(env->cfg, &ctx->ttab_log_size);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key ttab_log_size from section gve_solver from the config file. Exiting ...\n");
    return EXIT_FAILURE;
  }

  ret_err = get_ttab_log_verbosity_from_cfg(env->cfg, &ctx->ttab_log_verbosity);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key ttab_log_verbosity from section gve_solver from the config file. Exiting ...\n");
    return EXIT_FAILURE;
  }

  ctx->ttab = ttab_new(ctx->ttab_log_size);
  if (!ctx->ttab) {
    fprintf(stderr, "Error allocating memory for the transposition table. Exiting ...\n");
    return EXIT_FAILURE;
  }

  ret_err = get_game_position_evaluation_summary_from_cfg(env->cfg, &ctx->game_position_evaluation_summary);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key game_position_evaluation_summary from section gve_solver from the config file. Exiting ...\n");
    return EXIT_FAILURE;
  }

  ret_err = get_gve_solver_log_level_from_cfg(env->cfg, &ctx->gve_solver_log_level);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error loading key gve_solver_log_level from section gve_solver from the config file. Exiting ...\n");
    return EXIT_FAILURE;
  }

  ret_err = check_gve_solver_config_file(env->cfg);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error in the config file, section gve_solver. Exiting ...\n");
    return EXIT_FAILURE;
  }

  ctx->search_depth = (env->search_depth < 0) ? max_search_depth : env->search_depth;

  if (ctx->id_min_empty_count <= ctx->mwd.min_model_weight) {
    printf("The minimum model weight value must be smaller than ID minimum empty count. Exiting ...\n");
    return EXIT_FAILURE;
  }

  if (root) {
    ret_err = gve_context_set_root(ctx, root);
    if (ret_err != EXIT_SUCCESS) {
      fprintf(stderr, "Error setting root empty count field. Exiting ...\n");
      return EXIT_FAILURE;
    }
  } else {
    ctx->root_empty_count = -1;
    ctx->id_search_depth = 0;
    ctx->node_count = 0;
    ctx->leaf_count = 0;
    ctx->first_level_evaluation = 0;
    ctx->last_level_evaluation = 0;
    ctx->search_depth_initial_gap = 0;
    ctx->id_limit = 0;
  }

  return EXIT_SUCCESS;
}

int
gve_context_set_root (gve_context_t ctx,
                      const GamePositionX *root)
{
  assert(ctx);
  assert(root);

  ctx->root.blacks = root->blacks;
  ctx->root.whites = root->whites;
  ctx->root.player = root->player;

  ctx->root_empty_count = game_position_x_empty_count(root);

  ctx->id_search_depth = 0;
  ctx->node_count = 0;
  ctx->leaf_count = 0;

  if (ctx->root_empty_count < ctx->mwd.min_model_weight) {
    printf("Game position empty count (%02d) cannot be lesser than min_model_weight (%02d). Exiting ...\n",
           ctx->root_empty_count, ctx->mwd.min_model_weight);
    return EXIT_FAILURE;
  }

  ctx->first_level_evaluation = min(ctx->root_empty_count, ctx->mwd.max_model_weight);
  ctx->last_level_evaluation = ctx->id_min_empty_count - 1;

  ctx->search_depth_initial_gap = (ctx->root_empty_count == ctx->first_level_evaluation) ?
    1 : ctx->root_empty_count - ctx->first_level_evaluation;

  ctx->id_limit = min(ctx->search_depth, ctx->root_empty_count - ctx->id_min_empty_count);

  ttab_reinit(ctx->ttab);

  return EXIT_SUCCESS;
}

/**
 * @cond
 */

/*
 * Internal functions.
 */

static void
game_position_eval_summary_table (FILE *stream,
                                  gve_context_t ctx)
{
  uint64_t total_gp_eval_count = 0;
  for (int i = 0; i < EC_SIZE; i++) {
    total_gp_eval_count += ctx->gp_evaluations[i];
  }
  fprintf(stream, "Total game position evaluation count: %zu\n", total_gp_eval_count);
  fprintf(stream, " EMPTY_COUNT;  EVALUATIONS\n");
  for (int i = 0; i < EC_SIZE; i++)
    if (ctx->gp_evaluations[i] > 0)
      fprintf(stream, "          %02d; %12zu\n", i, ctx->gp_evaluations[i]);
}

static void
model_weights_data_release (model_weights_data_t *mwd)
{
  if (!mwd) return;
  for (size_t i = 0; i < EC_SIZE; i++) {
    rglmdf_model_weights_release(&mwd->mws_s[i]);
    mwd->mws[i] = NULL;
  }
}

static int
model_weights_data_init (model_weights_data_t *mwd,
                         cfg_t *cfg)
{
  assert(mwd);
  assert(cfg);

  /* The labels of the files to be loaded. */
  const char *const labels[EC_SIZE] =
    {
     "ec00", "ec01", "ec02", "ec03", "ec04", "ec05", "ec06", "ec07", "ec08", "ec09",
     "ec10", "ec11", "ec12", "ec13", "ec14", "ec15", "ec16", "ec17", "ec18", "ec19",
     "ec20", "ec21", "ec22", "ec23", "ec24", "ec25", "ec26", "ec27", "ec28", "ec29",
     "ec30", "ec31", "ec32", "ec33", "ec34", "ec35", "ec36", "ec37", "ec38", "ec39",
     "ec40", "ec41", "ec42", "ec43", "ec44", "ec45", "ec46", "ec47", "ec48", "ec49",
     "ec50", "ec51", "ec52", "ec53", "ec54", "ec55", "ec56", "ec57", "ec58", "ec59",
     "ec60"
    };

  const int min_model_weights_ec_range = 0;
  const int max_model_weights_ec_range = 60;

  int ret_err;
  int idx;

  for (size_t i = 0; i < EC_SIZE; i++) {
    rglmdf_model_weights_init(&mwd->mws_s[i]);
    mwd->mws[i] = NULL;
    mwd->file_names[i] = NULL;
  }
  mwd->verbose_loader = false;
  mwd->check_digest = false;
  mwd->model_weight_count = 0;

  ret_err = check_model_weight_config_file(cfg);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error in the config file. Exiting ...\n");
    return EXIT_FAILURE;
  }
  ret_err = get_model_weights_verbose_loader_from_cfg(cfg, &mwd->verbose_loader);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error in the config file. Exiting ...\n");
    return EXIT_FAILURE;
  }
  ret_err = get_model_weights_check_digest_from_cfg(cfg, &mwd->check_digest);
  if (ret_err != EXIT_SUCCESS) {
    fprintf(stderr, "Error in the config file. Exiting ...\n");
    return EXIT_FAILURE;
  }

  for (size_t i = 0; i < EC_SIZE; i++) {
    mwd->file_names[i] = (char *) cfg_get(cfg, "model_weights", labels[i]);
  }

  for (size_t i = 0; i < EC_SIZE; i++) {
    const char *filename = mwd->file_names[i];
    if (filename) {
      ret_err = rglmdf_model_weights_read_from_binary_file(&mwd->mws_s[i], filename, mwd->verbose_loader, mwd->check_digest);
      if (ret_err != 0) {
        fprintf(stderr, "Unable to load model weight file \"%s\"\n", filename);
        return ret_err;
      }
      if (mwd->mws_s[i].empty_count != i) {
        fprintf(stderr, "Empty count mismatch, expected %zu, found %u\n", i, mwd->mws_s[i].empty_count);
        return EXIT_FAILURE;
      }
      mwd->mws[i] = &mwd->mws_s[i];
    }
  }

  for (idx = min_model_weights_ec_range; idx <= max_model_weights_ec_range; idx++) {
    if (mwd->mws[idx] != NULL)
      mwd->model_weight_count++;
  }
  for (idx = min_model_weights_ec_range; idx <= max_model_weights_ec_range; idx++)
    if (mwd->mws[idx] != NULL) break;
  mwd->min_model_weight = idx;
  for (idx = max_model_weights_ec_range; idx >= min_model_weights_ec_range; idx--)
    if (mwd->mws[idx] != NULL) break;
  mwd->max_model_weight = idx;
  if (mwd->model_weight_count == 0) {
    printf("There is no model weight defnition into the configuration file. Exiting ...\n");
    return EXIT_FAILURE;
  }
  for (idx = mwd->min_model_weight; idx <= mwd->max_model_weight; idx++) {
    if (mwd->mws[idx] == NULL) {
      printf("The range of model weight files must be complete without missing values. File for empty_count = %02d is missing.\n", idx);
      return EXIT_FAILURE;
    }
  }

  return EXIT_SUCCESS;
}

static void
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
  node->empty_set = game_position_x_empties(&node->gpx);
  node->empty_count = bitw_bit_count_64(node->empty_set);
}

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
check_gve_solver_config_file (cfg_t *cfg)
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
  return EXIT_SUCCESS;
}

static int
check_model_weight_config_file (cfg_t *cfg)
{
  if (!cfg) {
    fprintf(stderr, "The configuration file is mandatory.\n");
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
rglm_eval_gp (const GamePositionX *const gpx,
              gve_context_t ctx)
{
  assert(gpx);
  assert(ctx);

  const int empty_count = game_position_x_empty_count(gpx);

  ctx->gp_evaluations[empty_count]++;

  const rglmdf_model_weights_t *const mw = ctx->mwd.mws[empty_count];
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

static bool
is_terminal (node_t *n)
{
  return (!n->legal_move_set && !n->parent->legal_move_set);
}

static void
heuristic_game_value (node_t *n,
                      gve_context_t ctx)
{
  const double v0 = rglm_eval_gp(&n->gpx, ctx);
  const double v1 = rglmut_gv_scale_back_f(v0);
  n->value = gv_f2d(v1);
}

static void
exact_terminal_game_value (node_t *n)
{
  n->value = game_position_x_final_value(&n->gpx);
}

/**
 * @brief Generates child nodes
 *
 * @param [out]     child_nodes  array of children nodes
 * @param [in,out]  n            pointer to the node structure
 * @param [in]      compute_hash drives the computation of the hash for child nodes
 */
static void
generate_child_nodes (node_t *child_nodes,
                      node_t *n,
                      bool compute_hash)
{
  SquareSet lms; // legal move set
  int lmc;       // legal move count

  lms = n->legal_move_set;
  lmc = n->legal_move_count;
  if (lms) {
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
      const int c_lmc = bitw_bit_count_64(c->legal_move_set);
      c->legal_move_count = c_lmc ? c_lmc : 1;
      c->empty_set = game_position_x_empties(&c->gpx);
      c->empty_count = bitw_bit_count_64(c->empty_set);
      lms = bitw_reset_lowest_set_bit_64(lms);
    }
  } else {
    node_t *const c = child_nodes + 0;
    c->parent = n;
    c->parent_move = pass_move;
    c->best_move = unknown_move;
    c->value = out_of_range_win_score;
    if (compute_hash) game_position_x_make_move_delta_hash(&n->gpx, pass_move, &c->gpx, n->hash, &c->hash);
    else game_position_x_make_move(&n->gpx, pass_move, &c->gpx);
    c->legal_move_set = game_position_x_legal_moves(&c->gpx);
    c->legal_move_count = bitw_bit_count_64(c->legal_move_set);
    c->empty_set = n->empty_set;
    c->empty_count = bitw_bit_count_64(c->empty_set);
  }
}

static void
order_moves (int child_node_count,
             node_t *child_nodes,
             node_t **child_nodes_p,
             ttab_item_t it,
             bool heuristic_sort,
             gve_context_t ctx)
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
      if (heuristic_sort) heuristic_game_value(child, ctx);
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
leaf_end_negamax (node_t *n,
                  int alpha,
                  int beta,
                  gve_context_t ctx)
{
  GamePositionX next_gpx, nnext_gpx;
  SquareSet flips;
  flips = game_position_x_flips(&n->gpx, n->empty_set, &next_gpx);
  if (flips) {
    n->value = - game_position_x_final_value(&next_gpx);
  } else {
    game_position_x_pass(&n->gpx, &next_gpx);
    flips = game_position_x_flips(&next_gpx, n->empty_set, &nnext_gpx);
    if (flips) {
      ctx->node_count++;
      n->value = game_position_x_final_value(&nnext_gpx);
    } else {
      n->value = - game_position_x_final_value(&next_gpx);
    }
  }
  ctx->node_count++;
  ctx->leaf_count++;
}


static void
leaf_end_2_negamax (node_t *n,
                    int alpha,
                    int beta,
                    gve_context_t ctx)
{
  node_t child_nodes[2];

  int lmc = 0;
  SquareSet es = n->empty_set;
  n->value = out_of_range_defeat_score;
  for (int i = 0;; i++) {
    node_t *child = &child_nodes[i];
    const Square move = bitw_tzcnt_64(es);
    const SquareSet move_set = bitw_lowest_set_bit_64(es);
    const SquareSet flips = game_position_x_flips(&n->gpx, move_set, &child->gpx);
    if (flips) {
      ctx->node_count++;
      lmc++;
      child->parent = n;
      child->parent_move = move;
      child->best_move = unknown_move;
      child->value = out_of_range_win_score;
      child->empty_set = n->empty_set & ~move_set;
      child->empty_count = n->empty_count - 1;
      leaf_end_negamax(child, -beta, -alpha, ctx);
      n->value = max(n->value, -child->value);
      alpha = max(alpha, n->value);
      if (alpha >= beta) break;
    }
    es = bitw_reset_lowest_set_bit_64(es);
    if (!es) break;
  }
  if (!lmc) {
    ctx->node_count++;
    if (n->parent_move == pass_move) {
      ctx->leaf_count++;
      exact_terminal_game_value(n);
      return;
    }
    node_t *child = &child_nodes[0];
    child->parent = n;
    child->parent_move = pass_move;
    child->best_move = unknown_move;
    child->value = out_of_range_win_score;
    child->empty_set = n->empty_set;
    child->empty_count = n->empty_count;
    game_position_x_make_move(&n->gpx, pass_move, &child->gpx);
    leaf_end_2_negamax(child, -beta, -alpha, ctx);
    n->value = max(n->value, -child->value);
  }
}


static void
leaf_negamax (node_t *n,
              int alpha,
              int beta,
              gve_context_t ctx)
{
  node_t child_nodes[64];
  node_t *sorted_child_nodes[64];

  ctx->node_count++;

  if (is_terminal(n)) {
    ctx->leaf_count++;
    exact_terminal_game_value(n);
  } else if (n->empty_count <= 2) {
    leaf_end_2_negamax(n, alpha, beta, ctx);
  } else {
    generate_child_nodes(child_nodes, n, false);
    for (int i = 0; i < n->legal_move_count; i++) sorted_child_nodes[i] = &child_nodes[i];
    sort_nodes_by_lmc(sorted_child_nodes, n->legal_move_count);
    n->value = out_of_range_defeat_score;
    for (int i = 0; i < n->legal_move_count; i++) {
      node_t *child = sorted_child_nodes[i];
      leaf_negamax(child, -beta, -alpha, ctx);
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
                       const int beta,
                       gve_context_t ctx)
{
  node_t child_nodes[64];
  node_t *child_nodes_p[64];
  int explored_child_count;

  const int ec = game_position_x_empty_count(&n->gpx);

  int am = alpha; // alpha-mobile : local value that is adjusted during the search
  int bm = beta;  // beta-mobile  : " ...

  ctx->node_count++;
  struct ttab_item_s its;
  ttab_item_t it = &its;
  it->hash = n->hash;
  ttab_retrieve(ctx->ttab, &it);
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

  explored_child_count = 0;
  if (is_terminal(n)) {
    ctx->leaf_count++;
    exact_terminal_game_value(n);
  } else if (depth == 0) {
    if (n->value == out_of_range_win_score) heuristic_game_value(n, ctx);
    n->best_move = unknown_move;
  } else if (ec < ctx->id_min_empty_count) {
    leaf_negamax(n, am, bm, ctx);
  } else {

    generate_child_nodes(child_nodes, n, true);
    its.legal_move_count = n->legal_move_count;
    order_moves(n->legal_move_count, child_nodes, child_nodes_p, it, ec <= ctx->first_level_evaluation, ctx);

    n->value = out_of_range_defeat_score;
    n->best_move = invalid_move;
    for (int i = 0; i < n->legal_move_count; i++) {
      node_t *child = child_nodes_p[i];

      if (ctx->id_search_depth - depth == 0)
        if (ctx->gve_solver_log_level >= 3) {
          timespec_print_local_time(stdout);
          printf(" %02d - %2s (%+03d) [%+03d..%+03d]: ", i, square_as_move_to_string(child->parent_move), -child->value, am, bm);
          fflush(stdout);
        }

      const int child_depth = (child->parent_move == pass_move) ? depth : depth -1;
      alphabeta_with_memory(child, child_depth, -bm, -am, ctx);

      if (ctx->id_search_depth - depth == 0)
        if (ctx->gve_solver_log_level >= 3) {
          char *s;
          if (-child->value <= am) s = "(failing low)  f+";
          else if (-child->value >= bm) s = "(failing high) f-";
          else s = "(success)       f ";
          printf("%s = %+03d - cumulated node count %12zu\n", s, -child->value, ctx->node_count);
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

  ttab_insert(ctx->ttab, &its);

  return;
}

static void
mtdf (node_t *n,
      int alpha,
      int beta,
      const int depth,
      gve_context_t ctx)
{
  int bound[2] = { alpha, beta };
  do {
    const int beta = n->value + (n->value == bound[0]) * 2;
    alphabeta_with_memory(n, depth, beta -2, beta, ctx);
    bound[n->value < beta] = n->value;
  } while (bound[0] < bound[1]);
}



/**
 * @endcond
 */
