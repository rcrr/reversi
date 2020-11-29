/**
 * @file
 *
 * @brief REGAB: Reversi End Game Analytics Base.
 *
 * @details An utility that generates the data base of reversi game positions
 * for analytical studies.
 *
 * @par regab.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2017, 2018, 2019, 2020 Roberto Corradini. All rights reserved.
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
#include <assert.h>
#include <limits.h>
#include <errno.h>
#include <inttypes.h>
#include <ctype.h>
#include <time.h>

#include <libpq-fe.h>

#include "main_option_parse.h"
#include "file_utils.h"
#include "cfg.h"
#include "prng.h"
#include "exact_solver.h"
#include "sort_utils.h"
#include "rglm_data_files.h"
#include "rglm_utils.h"



/**
 * @cond
 */

#define REGAB_PRNG_MAX_DEPTH 72

typedef struct regab_prng_node_s {
  GamePositionX gpx;
  SquareSet     legal_move_set;
  uint8_t       empty_count;
  uint8_t       legal_move_count;
  uint8_t       legal_move_count_adjusted;
  uint8_t       parent_move;
} regab_prng_node_t;

typedef struct regab_prng_stack_s {
  size_t             npos;
  regab_prng_node_t *active_node;
  regab_prng_node_t  nodes[REGAB_PRNG_MAX_DEPTH];
} regab_prng_stack_t;

typedef enum {
  REGAB_ACTION_GENERATE,                // Generates one batch of random games.
  REGAB_ACTION_SOLVE,                   // Solves the selected games.
  REGAB_ACTION_OFFSPRING,               // Generates the offspring record having status CMS and change status from CMQ to CMR.
  REGAB_ACTION_EXTRACT,                 // Extracts classified solved positions.
  REGAB_ACTION_INVALID                  // Not a valid action.
} regab_action_t;

typedef struct regab_prng_gp_record_s {
  int64_t seq;
  int batch_id;
  int game_id;
  int pos_id;
  char ins_time[32];
  char status[4];
  char cst_time[32];
  int64_t mover;
  int64_t opponent;
  uint8_t player;
  int empty_count;
  int64_t legal_move_set;
  int legal_move_count;
  int legal_move_count_adjusted;
  uint8_t parent_move;
  int game_value;
  uint8_t best_move;
  int64_t leaf_count;
  int64_t node_count;
  int64_t parent_gp_id;
} regab_prng_gp_record_t;

/*
 * Static constants.
 */

static const mop_options_long_t olist[] = {
  {"help",             'h', MOP_NONE},
  {"verbose",          'v', MOP_NONE},
  {"action",           'a', MOP_REQUIRED},
  {"config-file",      'c', MOP_REQUIRED},
  {"env",              'e', MOP_REQUIRED},
  {"prng-seed",        's', MOP_REQUIRED},
  {"n-games",          'n', MOP_REQUIRED},
  {"batch-id",         'b', MOP_REQUIRED},
  {"empty-count",      'y', MOP_REQUIRED},
  {"position-status",  'u', MOP_REQUIRED},
  {"feature",          'f', MOP_REQUIRED},
  {"pattern",          'p', MOP_REQUIRED},
  {"game-positions",   'g', MOP_NONE},
  {"out-file",         'o', MOP_REQUIRED},
  {"no-time-out-file", 't', MOP_NONE},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "  regab [OPTION...] - Reversi EndGame Analytics Base\n"
  "\n"
  "Options:\n"
  "  -h, --help             Show help options\n"
  "  -v, --verbose          Verbose output\n"
  "  -a, --action           Action to be performed                                      - Mandatory - Must be in [generate|solve|offspring|extract].\n"
  "  -c, --config-file      Config file name                                            - Mandatory.\n"
  "  -e, --env              Environment                                                 - Mandatory.\n"
  "  -s, --prng-seed        Seed used by the Pseudo Random Number Generator             - Mandatory when action is generate.\n"
  "  -n, --n-games          Number of random game generated, solved, or classified      - Default is one.\n"
  "  -b, --batch-id         Batch id                                                    - Mandatory when action is in [solve].\n"
  "  -y, --empty-count      Number of empty squares in the solved game position         - Mandatory when action is in [solve].\n"
  "  -u, --position-status  One or more status flag for the selection of game positions - Mandatory when action is in [extract].\n"
  "  -f, --feature          One or more features                                        - Optional when action is in [extract].\n"
  "  -p, --pattern          One or more patterns                                        - Mandatory when action is in [extract].\n"
  "  -g, --game-positions   Extract just game positions                                 - Mutually exclusive with option -p when action is [extract].\n"
  "  -o, --out-file         The output file name                                        - Mandatory when action is in [extract].\n"
  "  -t, --no-time-out-file Write a 'zero time' in the output file                      - Optional when action is in [extract].\n"
  "\n"
  "Description:\n"
  "  To be completed ... .\n"
  "\n"
  "Author:\n"
  "  Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2017, 2018, 2019, 2020 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;

static const char *insert_regab_prng_gp_h = "insert-regab-prng-gp-h";
static const char *insert_regab_prng_gp = "insert-regab-prng-gp";



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int v_flag = false;

static int a_flag = false;
static char *a_arg = NULL;

static int c_flag = false;
static char *c_arg = NULL;

static int e_flag = false;
static char *e_arg = NULL;

static int s_flag = false;
static char *s_arg = NULL;

static int n_flag = false;
static char *n_arg = NULL;

static int b_flag = false;
static char *b_arg = NULL;

static int y_flag = false;
static char *y_arg = NULL;

static int u_flag = false;
static char *u_arg = NULL;

static int f_flag = false;
static char *f_arg = NULL;

static int p_flag = false;
static char *p_arg = NULL;

static int g_flag = false;

static int o_flag = false;
static char *o_arg = NULL;

static int t_flag = false;

static regab_action_t action = REGAB_ACTION_INVALID;
static char *config_file = NULL;
static char *env = NULL;
static bool verbose = false;
static uint64_t prng_seed = 0;
static unsigned long int n_games = 1;

/*
 * Prototypes for internal functions.
 */



/**
 * @endcond
 */

static void
regab_get_db_connection (PGconn **conp,
                         char *con_info)
{
  PGresult *res = NULL;

  /* Connects to the database. */
  *conp = PQconnectdb(con_info);
  if (PQstatus(*conp) != CONNECTION_OK) {
    fprintf(stderr, "Connection to database failed, %s", PQerrorMessage(*conp));
    fprintf(stderr, "Connection string: %s", con_info);
    *conp = NULL;
    return;
  }

  /* Sets the search path to reversi. */
  res = PQexec(*conp, "SET search_path TO reversi");
  PQclear(res);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "SET failed, %s", PQerrorMessage(*conp));
    PQfinish(*conp);
    *conp = NULL;
    return;
  }

  /* Logs into the dabase log table. */
  res = PQexec(*conp, "INSERT INTO regab_connection_log (con_time) VALUES (now())");
  PQclear(res);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "INSERT failed, %s", PQerrorMessage(*conp));
    PQfinish(*conp);
    *conp = NULL;
    return;
  }

  /* Prepares statement insert_regab_prng_gp_h. */
  res = PQprepare(*conp,
                  insert_regab_prng_gp_h, //stmtName
                  "INSERT INTO regab_prng_gp_h (ins_time, status, prng_seed, ngames, npositions) "
                  "VALUES ($1::TIMESTAMP, $2::CHAR(3), $3::BIGINT, $4::INTEGER, $5::INTEGER) "
                  "RETURNING seq;",
                  5, // nParams
                  NULL // paramTypes
                  );
  PQclear(res);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "Error while preparing statement, %s", PQerrorMessage(*conp));
    PQfinish(*conp);
    *conp = NULL;
    return;
  }

  /* Prepares statement insert_regab_prng_gp. */
  res = PQprepare(*conp,
                  insert_regab_prng_gp,
                  "INSERT INTO regab_prng_gp ("
                  "batch_id, game_id, pos_id, ins_time, status, cst_time, "
                  "mover, opponent, player, empty_count, legal_move_set, legal_move_count, "
                  "legal_move_count_adjusted, parent_move, game_value, best_move, leaf_count, node_count, "
                  "parent_gp_id"
                  ") "
                  "VALUES ("
                  "$1::INTEGER, $2::INTEGER, $3::INTEGER, $4::TIMESTAMP, $5::CHAR(3), $6::TIMESTAMP, "
                  "$7::SQUARE_SET, $8::SQUARE_SET, $9::PLAYER, $10::SMALLINT, $11::SQUARE_SET, $12::SMALLINT, "
                  "$13::SMALLINT, $14::GAME_MOVE, $15::SMALLINT, $16::GAME_MOVE, $17::BIGINT, $18::BIGINT, "
                  "$19::BIGINT"
                  ")"
                  "RETURNING seq;",
                  19, // nParams
                  NULL // paramTypes
                  );
  PQclear(res);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "Error while preparing statement, %s", PQerrorMessage(*conp));
    PQfinish(*conp);
    *conp = NULL;
    return;
  }

}

static void
do_insert_regab_prng_gp_h (int *result,
                           uint64_t *seq,
                           PGconn *con,
                           char *status,
                           uint64_t prng_seed,
                           unsigned long n_games)
{
  PGresult *res = NULL;

  const char* paramValues[5];
  char prng_seed_to_s[64];
  char n_games_to_s[64];

  if (!result) return;
  if (!status) return;
  if (!con || (strlen(status) > 3)) {
    *result = -1;
    return;
  }

  sprintf(prng_seed_to_s, "%ld", (int64_t) prng_seed);
  sprintf(n_games_to_s, "%lu", n_games);

  paramValues[0] = "now()";
  paramValues[1] = status;
  paramValues[2] = prng_seed_to_s;
  paramValues[3] = n_games_to_s;
  paramValues[4] = "0";

  res = PQexecPrepared(con,
                       insert_regab_prng_gp_h,
                       5,
                       paramValues,
                       NULL,
                       NULL,
                       0);

  if (PQresultStatus(res) != PGRES_TUPLES_OK
      || PQntuples(res) != 1
      || strcmp("seq", PQfname(res, 0)) != 0) {
    fprintf(stderr, "Insertion command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    *seq = atol(PQgetvalue(res, 0, 0));
    *result = 0;
  }
  PQclear(res);
}

static void
do_insert_game_regab_prng_gp (int *result,
                              PGconn *con,
                              uint64_t batch_id,
                              uint64_t game_id,
                              regab_prng_stack_t *s)
{
  static const size_t pxr = 14; // parameters per record
  static const size_t command_size = 16384;
  static const size_t params_size = 8192;

  if (!result) return;
  if (!con || !s) {
    *result = -1;
    return;
  }

  size_t cl, pl;
  char *cp, *pp;
  char command[command_size];
  char params[params_size];
  const int nParams = pxr * s->npos;
  const char *paramValues[REGAB_PRNG_MAX_DEPTH * pxr];
  PGresult *res = NULL;

  for (int i = 0; i < params_size; i++) params[i] = 0;
  for (int i = 0; i < REGAB_PRNG_MAX_DEPTH * pxr; i++) paramValues[i] = NULL;

  const char *c0 =
    "INSERT INTO regab_prng_gp "
    "(batch_id, game_id, pos_id, ins_time, status, cst_time, "
    "mover, opponent, player, empty_count, legal_move_set, "
    "legal_move_count, legal_move_count_adjusted, parent_move) "
    "VALUES ";

  const char *c1 =
    "($%lu::INTEGER, $%lu::INTEGER, $%lu::INTEGER, $%lu::TIMESTAMP, $%lu::CHAR(3), $%lu::TIMESTAMP, "
    "$%lu::SQUARE_SET, $%lu::SQUARE_SET, $%lu::PLAYER, $%lu::SMALLINT, "
    "$%lu::SQUARE_SET, $%lu::SMALLINT, $%lu::SMALLINT, $%lu::GAME_MOVE)%s";

  const char *c2 = ", ";
  const char *c3 = ";";

  cl = snprintf(command, command_size, "%s", c0);
  cp = command + cl;

  pl = 0;
  pp = params;

  for (size_t i = 0; i < s->npos; i++) {
    cl += snprintf(cp, command_size - cl, c1,
                   1 + i * pxr, 2 + i * pxr, 3 + i * pxr,
                   4 + i * pxr, 5 + i * pxr, 6 + i * pxr,
                   7 + i * pxr, 8 + i * pxr, 9 + i * pxr,
                   10 + i * pxr, 11 + i * pxr, 12 + i * pxr,
                   13 + i * pxr, 14 + i * pxr, (i < (s->npos - 1)) ? c2 : c3);
    cp = command + cl;

    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command (4).\n");
      abort();
    }

    const regab_prng_node_t *n = &s->nodes[i];
    const SquareSet mover = game_position_x_get_mover(&n->gpx);
    const SquareSet opponent = game_position_x_get_opponent(&n->gpx);
    const Player player = game_position_x_get_player(&n->gpx);

    pl += snprintf(pp, params_size - pl, "%zu", batch_id);
    paramValues[0 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%zu", game_id);
    paramValues[1 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%zu", i);
    paramValues[2 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%s", "now()");
    paramValues[3 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%s", "INS");
    paramValues[4 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%s", "now()");
    paramValues[5 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%ld", (int64_t) mover);
    paramValues[6 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%ld", (int64_t) opponent);
    paramValues[7 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%u", player);
    paramValues[8 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%u", n->empty_count);
    paramValues[9 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%ld", (int64_t) n->legal_move_set);
    paramValues[10 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%u", n->legal_move_count);
    paramValues[11 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%u", n->legal_move_count_adjusted);
    paramValues[12 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;

    pl += snprintf(pp, params_size - pl, "%s", square_as_move_to_string(n->parent_move));
    paramValues[13 + i * pxr] = pp;
    pp = params + pl++ + 1;
    if (pl >= params_size) goto buffer_overflow;
  }

  res = PQexecParams(con, command, nParams, NULL, paramValues, NULL, NULL, 0);

  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    *result = 0;
  }
  PQclear(res);

  return;
 buffer_overflow:
  fprintf(stderr, "Error: params buffer is not long enough to contain the list of parameters values.\n");
  abort();
}

static void
do_check_load_regab_prng_gp_h (int *result,
                               PGconn *con,
                               uint64_t batch_id,
                               uint64_t expected_ngames,
                               uint64_t expected_npos)
{
  PGresult *res = NULL;
  char command[512];
  int clen;
  size_t ngames, npos;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }
  *result = 0;

  /* Checks that the game count and the position count do match with the expected values. */
  clen = snprintf(command,
                  sizeof(command),
                  "SELECT count(distinct(game_id)) AS games, count(distinct(game_id, pos_id)) AS positions FROM regab_prng_gp WHERE batch_id = %zu;",
                  batch_id);
  if (clen >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL statement (5).\n");
    abort();
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK
      || PQntuples(res) != 1
      || strcmp("games", PQfname(res, 0)) != 0
      || strcmp("positions", PQfname(res, 1)) != 0) {
    fprintf(stderr, "Select command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    ngames = atol(PQgetvalue(res, 0, 0));
    npos = atol(PQgetvalue(res, 0, 1));
    if (ngames != expected_ngames || npos != expected_npos) *result = -1;
  }
  PQclear(res);
  if (*result == -1) return;

  /* Updates the games count in the header table. */
  clen = snprintf(command,
                  sizeof(command),
                  "UPDATE regab_prng_gp_h SET npositions = %zu, status = 'CMP' WHERE seq = %zu;",
                  npos, batch_id);
  if (clen >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL statement (6).\n");
    abort();
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "Update command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  }
  PQclear(res);
}

static void
do_update_solved_position_results (int *result,
                                   PGconn *con,
                                   regab_prng_gp_record_t *record)
{
  PGresult *res = NULL;
  char command[512];
  int clen;

  if (!result) return;
  if (!con || !record) {
    *result = -1;
    return;
  }
  *result = 0;

  /* Updates the solved record. */
  clen = snprintf(command,
                  sizeof(command),
                  "UPDATE regab_prng_gp SET "
                  "game_value = %d, best_move = '%s', leaf_count = %"PRId64", node_count = %"PRId64", "
                  "status = 'CMP', cst_time = now() "
                  "WHERE seq = %"PRId64";",
                  record->game_value, square_as_move_to_string(record->best_move), record->leaf_count,
                  record->node_count, record->seq);
  if (clen >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL statement (7).\n");
    abort();
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "Update command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  }
  PQclear(res);
}

static void
do_insert_regab_prng_gp_record (int *result,
                                uint64_t *seq,
                                PGconn *con,
                                regab_prng_gp_record_t *record)
{
  PGresult *res = NULL;
  uint64_t tmp_seq;

  const char* paramValues[19];
  char batch_id_to_s[64];
  char game_id_to_s[64];
  char pos_id_to_s[64];
  char mover_to_s[64];
  char opponent_to_s[64];
  char player_to_s[64];
  char empty_count_to_s[64];
  char legal_move_set_to_s[64];
  char legal_move_count_to_s[64];
  char legal_move_count_adjusted_to_s[64];
  char parent_move_to_s[64];
  char game_value_to_s[64];
  char best_move_to_s[64];
  char leaf_count_to_s[64];
  char node_count_to_s[64];
  char parent_gp_id_to_s[64];

  if (!result) return;
  if (!con || !record) {
    *result = -1;
    return;
  }

  sprintf(batch_id_to_s, "%d", record->batch_id);
  sprintf(game_id_to_s, "%d", record->game_id);
  sprintf(pos_id_to_s, "%d", record->pos_id);
  sprintf(mover_to_s, "%ld", (int64_t) record->mover);
  sprintf(opponent_to_s, "%ld", (int64_t) record->opponent);
  sprintf(player_to_s, "%u", record->player);
  sprintf(empty_count_to_s, "%d", record->empty_count);
  sprintf(legal_move_set_to_s, "%ld", record->legal_move_set);
  sprintf(legal_move_count_to_s, "%d", record->legal_move_count);
  sprintf(legal_move_count_adjusted_to_s, "%d", record->legal_move_count_adjusted);
  sprintf(parent_move_to_s, "%s", square_as_move_to_string(record->parent_move));
  sprintf(game_value_to_s, "%d", record->game_value);
  sprintf(best_move_to_s, "%s", square_as_move_to_string(record->best_move));
  sprintf(leaf_count_to_s, "%ld", record->leaf_count);
  sprintf(node_count_to_s, "%ld", record->node_count);
  sprintf(parent_gp_id_to_s, "%ld", record->parent_gp_id);

  paramValues[0]  = batch_id_to_s;
  paramValues[1]  = game_id_to_s;
  paramValues[2]  = pos_id_to_s;
  paramValues[3]  = "now()";
  paramValues[4]  = record->status;
  paramValues[5]  = "now()";
  paramValues[6]  = mover_to_s;
  paramValues[7]  = opponent_to_s;
  paramValues[8]  = player_to_s;
  paramValues[9]  = empty_count_to_s;
  paramValues[10] = legal_move_set_to_s;
  paramValues[11] = legal_move_count_to_s;
  paramValues[12] = legal_move_count_adjusted_to_s;
  paramValues[13] = parent_move_to_s;
  paramValues[14] = game_value_to_s;
  paramValues[15] = best_move_to_s;
  paramValues[16] = leaf_count_to_s;
  paramValues[17] = node_count_to_s;
  paramValues[18] = parent_gp_id_to_s;

  res = PQexecPrepared(con,
                       insert_regab_prng_gp,
                       19,
                       paramValues,
                       NULL,
                       NULL,
                       0);

  if (PQresultStatus(res) != PGRES_TUPLES_OK
      || PQntuples(res) != 1
      || strcmp("seq", PQfname(res, 0)) != 0) {
    fprintf(stderr, "Record insertion into regab_prng_gp command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    tmp_seq = atol(PQgetvalue(res, 0, 0));
    *result = 0;
  }
  PQclear(res);
  if (seq) *seq = tmp_seq;
}

static void
do_insert_offspring_and_update_solved_position (int *result,
                                                PGconn *con,
                                                regab_prng_gp_record_t *cms_record,
                                                regab_prng_gp_record_t *cmr_record)
{
  PGresult *res = NULL;
  char command[512];
  int clen;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }

  /* Start a transaction. */
  res = PQexec(con, "BEGIN");
  PQclear(res);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "BEGIN command failed: %s\n", PQerrorMessage(con));
    *result = -1;
    return;
  }

  do_insert_regab_prng_gp_record(result, NULL, con, cmr_record);

  if (*result != 0) {
    res = PQexec(con, "ROLLBACK");
    PQclear(res);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
      fprintf(stderr, "ROLLBACK command failed: %s\n", PQerrorMessage(con));
      *result = -1;
      return;
    } else {
      fprintf(stderr, "ROLLBACK command executed succesfully.\n");
      *result = -1;
      return;
    }
  }

  /* Updates the CMQ/CMS record. */
  clen = snprintf(command,
                  sizeof(command),
                  "UPDATE regab_prng_gp SET "
                  "status = 'CMS', cst_time = now() "
                  "WHERE seq = %"PRId64";",
                  cms_record->seq);
  if (clen >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL statement.\n");
    abort();
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "Update command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  }
  PQclear(res);

  if (*result != 0) {
    res = PQexec(con, "ROLLBACK");
    PQclear(res);
    if (PQresultStatus(res) != PGRES_COMMAND_OK) {
      fprintf(stderr, "ROLLBACK command failed: %s\n", PQerrorMessage(con));
      *result = -1;
      return;
    } else {
      fprintf(stderr, "ROLLBACK command executed succesfully.\n");
      *result = -1;
      return;
    }
  }

  /* Close the transaction. */
  res = PQexec(con, "END");
  PQclear(res);
}

static void
regab_print_connection_log (int *result,
                            PGconn *con,
                            int lines,
                            FILE *stream)
{
  PGresult *res = NULL;
  char statement[512];

  if (!result) return;
  if (lines < 1) return;
  if (!stream) return;
  if (!con) {
    *result = -1;
    return;
  }

  /* Start a transaction. */
  res = PQexec(con, "BEGIN");
  PQclear(res);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "BEGIN command failed: %s", PQerrorMessage(con));
    *result = -1;
    return;
  }

  /* Prepare cursor. */
  if (snprintf(statement,
               sizeof(statement),
               "DECLARE ld CURSOR FOR SELECT sub.* FROM (SELECT * FROM regab_connection_log ORDER BY seq DESC LIMIT %d) sub ORDER BY seq ASC",
               lines) >= sizeof(statement)) {
    fprintf(stderr, "Error: statement buffer is not long enough to contain the SQL command.\n");
    abort();
  }
  res = PQexec(con, statement);
  PQclear(res);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "DECLARE CURSOR failed: %s", PQerrorMessage(con));
    *result = -1;
    return;
  }

  res = PQexec(con, "FETCH ALL IN ld");
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "FETCH ALL failed: %s", PQerrorMessage(con));
    PQclear(res);
    *result = -1;
    return;
  }

  /* First, prints out the table collumn attribute names. */
  int nfields = PQnfields(res);
  for (int i = 0; i < nfields; i++)
    fprintf(stream, "%-30s", PQfname(res, i));
  fprintf(stream, "\n");

  /* Next, print out the rows of data. */
  for (int i = 0; i < PQntuples(res); i++) {
    for (int j = 0; j < nfields; j++)
      fprintf(stream, "%-30s", PQgetvalue(res, i, j));
    fprintf(stream, "\n");
  }

  PQclear(res);

  /* Closes the cursor ... no check for errors ... */
  res = PQexec(con, "CLOSE ld");
  PQclear(res);

  /* Close the transaction. */
  res = PQexec(con, "END");
  PQclear(res);

  *result = 1;
  return;
}

static uint8_t
square_as_move_from_two_chars (char *f)
{
  uint8_t move;
  char *s = f + 1;
  if (*f == '-' && *s == '-') move = pass_move;
  else if (*f == 'U' && *s == 'N') move = unknown_move;
  else if (*f == 'N' && *s == 'A') move = invalid_move;
  else if (*f >= 'A' && *f <= 'H' && *s >= '1' && *s <= '8') {
    move = *f - 'A' + 8 * (*s - '1');
  } else {
    move = invalid_move; // should never happen ...
  }
  return move;
}

static void
do_select_position_to_offspring (int *result,
                                 PGconn *con,
                                 regab_prng_gp_record_t *record)
{
  PGresult *res = NULL;
  char command[512];
  int clen;

  size_t ntuples;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }
  *result = 0;

  /* Selects and updates at most one record ready to be processed for generating the offspring. */
  clen = snprintf(command,
                  sizeof(command),
                  "UPDATE regab_prng_gp SET status = 'CMW', cst_time = now() WHERE seq IN ("
                  "SELECT seq FROM regab_prng_gp WHERE batch_id = %d AND empty_count = %u AND status = 'CMQ' "
                  "FOR UPDATE SKIP LOCKED LIMIT 1) "
                  "RETURNING seq, game_id, pos_id, ins_time, status, cst_time, mover, opponent, player, "
                  "empty_count, legal_move_set, legal_move_count, legal_move_count_adjusted, parent_move, game_value, best_move",
                  record->batch_id, record->empty_count);
  if (clen >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL statement (UPDATE ...).\n");
    abort();
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "Select for update command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    ntuples = PQntuples(res);
    if (ntuples > 1) *result = -1;
    else if (ntuples == 1) {
      if (strcmp("seq", PQfname(res, 0)) != 0
          || strcmp("game_id", PQfname(res, 1)) != 0
          || strcmp("pos_id", PQfname(res, 2)) != 0
          || strcmp("ins_time", PQfname(res, 3)) != 0
          || strcmp("status", PQfname(res, 4)) != 0
          || strcmp("cst_time", PQfname(res, 5)) != 0
          || strcmp("mover", PQfname(res, 6)) != 0
          || strcmp("opponent", PQfname(res, 7)) != 0
          || strcmp("player", PQfname(res, 8)) != 0
          || strcmp("empty_count", PQfname(res, 9)) != 0
          || strcmp("legal_move_set", PQfname(res, 10)) != 0
          || strcmp("legal_move_count", PQfname(res, 11)) != 0
          || strcmp("legal_move_count_adjusted", PQfname(res, 12)) != 0
          || strcmp("parent_move", PQfname(res, 13)) != 0
          || strcmp("game_value", PQfname(res, 14)) != 0
          || strcmp("best_move", PQfname(res, 15)) != 0) {
        *result = -1;
      } else { // Everything is ok, and one record is selected.
        record->seq = atol(PQgetvalue(res, 0, 0));
        record->game_id = atol(PQgetvalue(res, 0, 1));
        record->pos_id = atol(PQgetvalue(res, 0, 2));
        strcpy(record->ins_time, PQgetvalue(res, 0, 3));
        strcpy(record->status, PQgetvalue(res, 0, 4));
        strcpy(record->cst_time, PQgetvalue(res, 0, 5));
        record->mover = atol(PQgetvalue(res, 0, 6));
        record->opponent = atol(PQgetvalue(res, 0, 7));
        record->player = atoi(PQgetvalue(res, 0, 8));
        record->empty_count = atoi(PQgetvalue(res, 0, 9));
        record->legal_move_set = atol(PQgetvalue(res, 0, 10));
        record->legal_move_count = atoi(PQgetvalue(res, 0, 11));
        record->legal_move_count_adjusted = atoi(PQgetvalue(res, 0, 12));
        record->parent_move = square_as_move_from_two_chars(PQgetvalue(res, 0, 13));
        record->game_value = atol(PQgetvalue(res, 0, 14));
        record->best_move = square_as_move_from_two_chars(PQgetvalue(res, 0, 15));
        *result = 1;
      }
    } else { // Ok, but no record selected.
      *result = 0;
    }
  }
  PQclear(res);
}

static void
do_select_position_to_solve (int *result,
                             PGconn *con,
                             regab_prng_gp_record_t *record)
{
  PGresult *res = NULL;
  char command[512];
  int clen;

  size_t ntuples;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }
  *result = 0;

  /* Selects and updates at most one record ready to be solved. */
  clen = snprintf(command,
                  sizeof(command),
                  "UPDATE regab_prng_gp SET status = 'WIP', cst_time = now() WHERE seq IN ("
                  "SELECT seq FROM regab_prng_gp WHERE batch_id = %d AND empty_count = %u AND status = 'INS' "
                  "FOR UPDATE SKIP LOCKED LIMIT 1) "
                  "RETURNING seq, game_id, pos_id, ins_time, status, cst_time, mover, opponent, player, "
                  "empty_count, legal_move_set, legal_move_count, legal_move_count_adjusted, parent_move",
                  record->batch_id, record->empty_count);
  if (clen >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL statement (11).\n");
    abort();
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "Select for update command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    ntuples = PQntuples(res);
    if (ntuples > 1) *result = -1;
    else if (ntuples == 1) {
      if (strcmp("seq", PQfname(res, 0)) != 0
          || strcmp("game_id", PQfname(res, 1)) != 0
          || strcmp("pos_id", PQfname(res, 2)) != 0
          || strcmp("ins_time", PQfname(res, 3)) != 0
          || strcmp("status", PQfname(res, 4)) != 0
          || strcmp("cst_time", PQfname(res, 5)) != 0
          || strcmp("mover", PQfname(res, 6)) != 0
          || strcmp("opponent", PQfname(res, 7)) != 0
          || strcmp("player", PQfname(res, 8)) != 0
          || strcmp("empty_count", PQfname(res, 9)) != 0
          || strcmp("legal_move_set", PQfname(res, 10)) != 0
          || strcmp("legal_move_count", PQfname(res, 11)) != 0
          || strcmp("legal_move_count_adjusted", PQfname(res, 12)) != 0
          || strcmp("parent_move", PQfname(res, 13)) != 0) {
        *result = -1;
      } else { // Everything is ok, and one record is selected.
        record->seq = atol(PQgetvalue(res, 0, 0));
        record->game_id = atol(PQgetvalue(res, 0, 1));
        record->pos_id = atol(PQgetvalue(res, 0, 2));
        strcpy(record->ins_time, PQgetvalue(res, 0, 3));
        strcpy(record->status, PQgetvalue(res, 0, 4));
        strcpy(record->cst_time, PQgetvalue(res, 0, 5));
        record->mover = atol(PQgetvalue(res, 0, 6));
        record->opponent = atol(PQgetvalue(res, 0, 7));
        record->player = atoi(PQgetvalue(res, 0, 8));
        record->empty_count = atoi(PQgetvalue(res, 0, 9));
        record->legal_move_set = atol(PQgetvalue(res, 0, 10));
        record->legal_move_count = atoi(PQgetvalue(res, 0, 11));
        record->legal_move_count_adjusted = atoi(PQgetvalue(res, 0, 12));
        record->parent_move = square_as_move_from_two_chars(PQgetvalue(res, 0, 13));
        *result = 1;
      }
    } else { // Ok, but no record selected.
      *result = 0;
    }
  }
  PQclear(res);
}

static void
do_action_extract_game_pos_cnt (int *result,
                                PGconn *con,
                                uint8_t empty_count,
                                size_t batch_id_cnt,
                                uint64_t *batch_ids,
                                size_t position_status_cnt,
                                char **position_statuses,
                                size_t *record_cnt)
{
  static const size_t command_size = 1024;

  PGresult *res = NULL;
  char command[command_size];
  size_t cl;
  char *cp;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }
  *result = 0;

  const char *c0 =
    "SELECT count(1) FROM regab_prng_gp AS gp RIGHT JOIN regab_prng_gp_pattern_class AS pc ON gp.seq = pc.gp_id WHERE "
    "legal_move_count_adjusted > 0 AND gp.empty_count = ";

  cl = snprintf(command, command_size, "%s", c0);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  cl += snprintf(cp, command_size - cl, "%d AND gp.status = ANY ('{", empty_count);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  for (size_t i = 0; i < position_status_cnt; i++) {
    cl += snprintf(cp, command_size - cl, "\"%s\"%s", position_statuses[i], (i < position_status_cnt - 1) ? ", " : "}'::TEXT[]) AND gp.batch_id = ANY ('{");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  for (size_t i = 0; i < batch_id_cnt; i++) {
    cl += snprintf(cp, command_size - cl, "%ld%s", batch_ids[i], (i < batch_id_cnt - 1) ? ", " : "}'::INT[]);");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK
      || PQntuples(res) != 1
      || strcmp("count", PQfname(res, 0)) != 0) {
    fprintf(stderr, "Select command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    *record_cnt = atol(PQgetvalue(res, 0, 0));
  }
  PQclear(res);
}

static void
do_action_extract_game_pos_prepare_cursor (int *result,
                                           PGconn *con,
                                           bool verbose,
                                           uint8_t empty_count,
                                           size_t batch_id_cnt,
                                           uint64_t *batch_ids,
                                           size_t position_status_cnt,
                                           char **position_statuses,
                                           size_t pattern_cnt,
                                           board_pattern_id_t *patterns,
                                           const char *sql_cursor_name,
                                           size_t *record_cnt)
{
  static const size_t command_size = 1024;

  PGresult *res = NULL;
  char command[command_size];
  size_t cl;
  char *cp;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }
  *result = 0;

  const char *c0 = "SELECT * FROM regab_action_extract_game_pos_prepare_cursor(";

  cl = snprintf(command, command_size, "%s", c0);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  cl += snprintf(cp, command_size - cl, "%d, '{", empty_count);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  for (size_t i = 0; i < batch_id_cnt; i++) {
    cl += snprintf(cp, command_size - cl, "%zu%s", batch_ids[i], (i < batch_id_cnt - 1) ? ", " : "}', '{");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  for (size_t i = 0; i < position_status_cnt; i++) {
    cl += snprintf(cp, command_size - cl, "\"%s\"%s", position_statuses[i], (i < position_status_cnt - 1) ? ", " : "}', '{");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  for (size_t i = 0; i < pattern_cnt; i++) {
    cl += snprintf(cp, command_size - cl, "%d%s", patterns[i], (i < pattern_cnt - 1) ? ", " : "");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  cl += snprintf(cp, command_size - cl, "}', ");
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  cl += snprintf(cp, command_size - cl, "'%s');", sql_cursor_name);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  if (false) fprintf(stdout, "command = \"%s\"\n", command);

  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    if (PQntuples(res) != 1) {
      fprintf(stdout, "Procedure do_action_extract_game_pos_prepare_cursor() returned wrong number of records.\n");
      *result = -2;
    } else {
      *record_cnt = atol(PQgetvalue(res, 0, 0)); // first row, first field: the row count.
      *result = 0;
      if (verbose) fprintf(stdout, "Procedure do_action_extract_game_pos_prepare_cursor() executed succesfully.\n");
    }
  }
  PQclear(res);
}

static void
do_action_extract_game_pos_cursor_fetch (int *result,
                                         PGconn *con,
                                         bool verbose,
                                         rglmdf_general_data_t *gd,
                                         size_t *returned_ntuples,
                                         rglmdf_solved_and_classified_gp_record_t *scgprp,
                                         double *farrayp,
                                         uint32_t *i0arrayp,
                                         const char *sql_cursor_name,
                                         size_t chunk_size)
{
  static const size_t command_size = 1024;

  PGresult *res = NULL;
  char command[command_size];
  size_t cl;
  char *cp;
  size_t fv_idx;
  board_feature_id_t fid;
  board_t b;
  double *feature_values;

  if (!result) return;
  if (!con || !gd) {
    *result = -1;
    return;
  }
  *result = 0;

  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(gd);
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(gd);
  const size_t feature_cnt = rglmdf_get_feature_cnt(gd);
  const board_feature_id_t *features = rglmdf_get_features(gd);
  *returned_ntuples = 0;

  const char *c0 = "FETCH FORWARD ";

  cl = snprintf(command, command_size, "%s", c0);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  cl += snprintf(cp, command_size - cl, "%zu FROM %s;", chunk_size, sql_cursor_name);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    *returned_ntuples = PQntuples(res);
    for (size_t i = 0; i < *returned_ntuples; i++) {
      scgprp[i].row_n = atol(PQgetvalue(res, i, 0));
      scgprp[i].gp_id = atol(PQgetvalue(res, i, 1));
      scgprp[i].mover = atol(PQgetvalue(res, i, 2));
      scgprp[i].opponent = atol(PQgetvalue(res, i, 3));
      scgprp[i].game_value = atoi(PQgetvalue(res, i, 4));
      scgprp[i].game_value_transformed = rglmut_gv_scale(scgprp[i].game_value);
      scgprp[i].evaluation_function = 0.5; // 0.5 is the mid point between 0 and 1.
      scgprp[i].residual = scgprp[i].evaluation_function - scgprp[i].game_value_transformed;
      for (size_t j = 0; j < ni; j++) {
        i0arrayp[i * ni + j] = atol(PQgetvalue(res, i, 5 + j));
      }
      fv_idx = 0;
      for (size_t j = 0; j < feature_cnt; j++) {
        fid = board_features[features[j]].id;
        board_set_square_sets(&b, scgprp[i].mover, scgprp[i].opponent);
        feature_values = &farrayp[i * nf + fv_idx];
        if (board_features[fid].feature_values_f)
          board_features[fid].feature_values_f(&b, feature_values);
        fv_idx += board_features[fid].field_cnt;
      }
    }
    *result = 0;
  }
  PQclear(res);
}

static void
do_action_extract_pattern_freqs_cursor_fetch (int *result,
                                              PGconn *con,
                                              bool verbose,
                                              const char *sql_cursor_name,
                                              size_t chunk_size,
                                              rglmdf_pattern_freq_summary_record_t *rec,
                                              size_t *fetched_record_cnt)
{
  static const size_t command_size = 1024;

  PGresult *res = NULL;
  char command[command_size];
  size_t cl;
  char *cp;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }
  *result = 0;

  const char *c0 = "FETCH FORWARD ";

  cl = snprintf(command, command_size, "%s", c0);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  cl += snprintf(cp, command_size - cl, "%zu FROM %s;", chunk_size, sql_cursor_name);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    *fetched_record_cnt = PQntuples(res);
    for (size_t i = 0; i < *fetched_record_cnt; i++) {
      rec->glm_variable_id = atol(PQgetvalue(res, i, 0));
      rec->entity_class = BOARD_ENTITY_CLASS_PATTERN;
      rec->entity_id = atol(PQgetvalue(res, i, 1));
      rec->principal_index_value = atol(PQgetvalue(res, i, 2));
      rec->total_cnt = atol(PQgetvalue(res, i, 3));
      rec->relative_frequency = atof(PQgetvalue(res, i, 4));
      rec->theoretical_probability = atof(PQgetvalue(res, i, 5));
      rec->weight = 0.0; // Weight is set to 0.0 that is the neutral value (game value equal to zero) for the evaluation function.
      rec++;
    }
    *result = 0;
  }
  PQclear(res);
}

static void
do_action_extract_pattern_freqs_prepare_cursor (int *result,
                                                PGconn *con,
                                                bool verbose,
                                                uint8_t empty_count,
                                                size_t batch_id_cnt,
                                                uint64_t *batch_ids,
                                                size_t position_status_cnt,
                                                char **position_statuses,
                                                size_t pattern_cnt,
                                                board_pattern_id_t *patterns,
                                                int glm_variable_id_offset,
                                                const char *sql_cursor_name_a,
                                                const char *sql_cursor_name_b,
                                                size_t *record_cnt_b)
{
  static const size_t command_size = 1024;

  PGresult *res = NULL;
  char command[command_size];
  size_t cl;
  char *cp;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }
  *result = 0;

  const char *c0 = "SELECT * FROM regab_action_extract_count_pattern_freqs(";

  cl = snprintf(command, command_size, "%s", c0);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  cl += snprintf(cp, command_size - cl, "%d, %d, '{", glm_variable_id_offset, empty_count);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  for (size_t i = 0; i < batch_id_cnt; i++) {
    cl += snprintf(cp, command_size - cl, "%zu%s", batch_ids[i], (i < batch_id_cnt - 1) ? ", " : "}', '{");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  for (size_t i = 0; i < position_status_cnt; i++) {
    cl += snprintf(cp, command_size - cl, "\"%s\"%s", position_statuses[i], (i < position_status_cnt - 1) ? ", " : "}', '{");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  if (pattern_cnt == 0) {
    cl += snprintf(cp, command_size - cl, "}', ");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  for (size_t i = 0; i < pattern_cnt; i++) {
    cl += snprintf(cp, command_size - cl, "%d%s", patterns[i], (i < pattern_cnt - 1) ? ", " : "}', ");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  cl += snprintf(cp, command_size - cl, "'%s', '%s');", sql_cursor_name_a, sql_cursor_name_b);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    if (PQntuples(res) != 2) {
      *result = -2;
      if (verbose) fprintf(stdout, "Procedure regab_action_extract_count_pattern_freqs() returned wrong number of records.\n");
    } else {
      *record_cnt_b = atol(PQgetvalue(res, 1, 1)); // second row, second field: the row count.
      *result = 0;
      if (verbose) {
        fprintf(stdout, "Procedure regab_action_extract_count_pattern_freqs() executed succesfully.\n");
      }
    }
  }
  PQclear(res);
}

static void
do_action_extract_count_positions (int *result,
                                   PGconn *con,
                                   bool verbose,
                                   rglmdf_general_data_t *gd)
{
  static const size_t command_size = 1024;

  PGresult *res = NULL;
  char command[command_size];
  size_t cl, n;
  char *cp;
  rglmdf_position_summary_record_t *rec;

  if (!result) return;
  if (!con || !gd) {
    *result = -1;
    return;
  }
  *result = 0;

  const char *c0 = "SELECT batch_id, status, game_position_cnt, classified_cnt FROM regab_action_extract_count_positions(";

  cl = snprintf(command, command_size, "%s", c0);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  cl += snprintf(cp, command_size - cl, "%d, '{", gd->empty_count);
  cp = command + cl;
  if (cl >= command_size) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
    *result = -1;
    return;
  }

  for (size_t i = 0; i < gd->batch_id_cnt; i++) {
    cl += snprintf(cp, command_size - cl, "%zu%s", gd->batch_ids[i], (i < gd->batch_id_cnt - 1) ? ", " : "}', '{");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  for (size_t i = 0; i < gd->position_status_cnt; i++) {
    cl += snprintf(cp, command_size - cl, "\"%s\"%s", gd->position_statuses[i], (i < gd->position_status_cnt - 1) ? ", " : "}');");
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command.\n");
      *result = -1;
      return;
    }
  }

  if (false) fprintf(stdout, "command = \"%s\"\n", command);

  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    gd->position_summary.ntuples = PQntuples(res);

    n = rglmdf_set_position_summary_ntuples(gd, gd->position_summary.ntuples);
    if (n != gd->position_summary.ntuples) {
      fprintf(stderr, "Unable to allocate memory for position summary table.\n");
      *result = -1;
      goto end;
    }

    for (size_t i = 0; i < gd->position_summary.ntuples; i++) {
      rec = &gd->position_summary.records[i];
      rec->batch_id = atoi(PQgetvalue(res, i, 0));
      strcpy(rec->status, PQgetvalue(res, i, 1));
      rec->game_position_cnt = atol(PQgetvalue(res, i, 2));
      rec->classified_cnt = atol(PQgetvalue(res, i, 3));
    }

    if (verbose) {
      fprintf(stdout, "Procedure regab_action_extract_count_positions() executed succesfully.\n");
      fprintf(stdout, "\n");
      fprintf(stdout, "   ----------------------------\n");
      fprintf(stdout, "      Position Summary Table   \n");
      fprintf(stdout, "   ----------------------------\n");
      fprintf(stdout, "________________________________________________________\n");
      fprintf(stdout, "      |          |        |            |                \n");
      fprintf(stdout, " rec  | batch_id | status |   gp_cnt   | classified_cnt \n");
      fprintf(stdout, "______|__________|________|____________|________________\n");
      for (size_t i = 0; i < gd->position_summary.ntuples; i++) {
        rec = &gd->position_summary.records[i];
        fprintf(stdout, " %04zu | %8d |   %s  | %10zu |     %10zu\n",
                i, rec->batch_id, rec->status, rec->game_position_cnt, rec->classified_cnt);
      }
      fprintf(stdout, "\n");
    }
    *result = 0;
  }

 end:
  PQclear(res);
}

static void
do_action_extract_check_patterns (int *result,
                                  PGconn *con,
                                  bool verbose,
                                  board_pattern_id_t *patterns,
                                  size_t pattern_cnt)
{
  static const size_t command_size = 1024;

  PGresult *res = NULL;
  char command[command_size];
  size_t cl;
  char *cp;

  size_t ntuples;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }
  *result = 0;

  if (pattern_cnt == 0) return;

  const char *c0 = "SELECT * FROM regab_action_extract_check_patterns('{";
  const char *c1 = "%d%s";
  const char *c2 = ", ";
  const char *c3 = "}');";

  cl = snprintf(command, command_size, "%s", c0);
  cp = command + cl;

  for (size_t i = 0; i < pattern_cnt; i++) {
    cl += snprintf(cp, command_size - cl, c1,
                   patterns[i], (i < pattern_cnt - 1) ? c2 : c3);
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command (4).\n");
      *result = -1;
      return;
    }
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "Select for command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    ntuples = PQntuples(res);
    if (verbose) {
      fprintf(stdout, "____________________________________________________\n");
      fprintf(stdout, "\nSearched patterns: ");
      for (size_t i = 0; i < pattern_cnt; i++) fprintf(stdout, "%d%s", patterns[i], i == pattern_cnt - 1 ? "\n" : ", ");
      fprintf(stdout, "Patterns found matching the query\n");
      fprintf(stdout, "____________________________________________________\n");
      fprintf(stdout, "      |            |        |            |          \n");
      fprintf(stdout, " ---- | pattern_id |  name  | ninstances | nsquares \n");
      fprintf(stdout, "______|____________|________|____________|__________\n");
      for (size_t i = 0; i < ntuples; i++) {
        int pattern_id = atol(PQgetvalue(res, i, 0));
        char *pattern_name = PQgetvalue(res, i, 1);
        int ninstances = atol(PQgetvalue(res, i, 2));
        int nsquares = atol(PQgetvalue(res, i, 3));
        fprintf(stdout, " %04zu |     %6d | %6s |         %2d |       %2d\n", i, pattern_id, pattern_name, ninstances, nsquares);
      }
      fprintf(stdout, "\n");
    }
    if (ntuples != pattern_cnt) {
      fprintf(stdout, "Error: the database does not contain all the requested pattern values, number of tuples found is %zu, searched is %lu \n", ntuples, pattern_cnt);
      *result = 1;
    } else {
      *result = 0;
    }
  }

  PQclear(res);
}

static void
do_action_extract_check_batches (int *result,
                                 PGconn *con,
                                 bool verbose,
                                 uint64_t *batch_ids,
                                 size_t batch_id_cnt)
{
  static const size_t command_size = 1024;

  PGresult *res = NULL;
  char command[command_size];
  size_t cl;
  char *cp;

  size_t ntuples;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }
  *result = 0;

  const char *c0 = "SELECT * FROM regab_action_extract_check_batches('{";
  const char *c1 = "%d%s";
  const char *c2 = ", ";
  const char *c3 = "}');";

  cl = snprintf(command, command_size, "%s", c0);
  cp = command + cl;

  for (size_t i = 0; i < batch_id_cnt; i++) {
    cl += snprintf(cp, command_size - cl, c1,
                   batch_ids[i], (i < batch_id_cnt - 1) ? c2 : c3);
    cp = command + cl;
    if (cl >= command_size) {
      fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command (4).\n");
      *result = -1;
      return;
    }
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "Select for command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    ntuples = PQntuples(res);
    if (verbose) {
      fprintf(stdout, "_____________________________________\n");
      fprintf(stdout, "\nSearched batch_ids: ");
      for (size_t i = 0; i < batch_id_cnt; i++) fprintf(stdout, "%lu%s", batch_ids[i], i == batch_id_cnt - 1 ? "\n" : ", ");
      fprintf(stdout, "Batch ids found matching the query\n");
      fprintf(stdout, "_____________________________________\n");
      fprintf(stdout, "      |          |        |          \n");
      fprintf(stdout, " ---- | batch_id | status |   ngames \n");
      fprintf(stdout, "______|__________|________|__________\n");
      for (size_t i = 0; i < ntuples; i++) {
        uint64_t batch_id = atol(PQgetvalue(res, i, 0));
        char *status = PQgetvalue(res, i, 1);
        int64_t ngames = atol(PQgetvalue(res, i, 2));
        fprintf(stdout, " %04zu | %8lu |   %s  | %8ld\n", i, batch_id, status, ngames);
      }
      fprintf(stdout, "\n");
    }
    if (ntuples != batch_id_cnt) {
      fprintf(stdout, "Error: the database does not contain all the requested batch_id values, number of tuples found is %zu, searched is %lu \n", ntuples, batch_id_cnt);
      *result = 1;
    } else {
      for (size_t i = 0; i < ntuples; i++) {
        uint64_t batch_id = atol(PQgetvalue(res, i, 0));
        char *status = PQgetvalue(res, i, 1);
        if (strcmp(status, "CMP") != 0) {
          fprintf(stdout, "Error: batch_id %lu has status %s. It must be CMP\n", batch_id, status);
          *result = 2;
          goto end;
        }
      }
      *result = 0;
    }
  }

 end:
  PQclear(res);
}



/**
 * @brief Main entry to the Reversi EndGame Analytics Base.
 *
 * @todo Documentation has to be completly developed.
 */
int
main (int argc,
      char *argv[])
{
  /* Initializes the board module. */
  board_module_init();

  regab_prng_stack_t gstack;

  int opt;
  int oindex = -1;

  uint64_t batch_id = 0;
  size_t batch_id_cnt = 0;
  uint64_t *batch_ids = NULL;

  uint8_t empty_count = 0;

  size_t position_status_cnt = 0;
  char *position_status_buffer = NULL;
  char **position_statuses = NULL;

  size_t feature_cnt = 0;
  char *feature_buffer = NULL;
  board_feature_id_t *features = NULL;

  size_t pattern_cnt = 0;
  char *pattern_buffer = NULL;
  board_pattern_id_t *patterns = NULL;

  char *output_file_name = NULL;

  size_t nbytes;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, olist, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
      break;
    case 'v':
      v_flag = true;
      break;
    case 'a':
      a_flag = true;
      a_arg = options.optarg;
      break;
    case 'c':
      c_flag = true;
      c_arg = options.optarg;
      break;
    case 'e':
      e_flag = true;
      e_arg = options.optarg;
      break;
    case 's':
      s_flag = true;
      s_arg = options.optarg;
      break;
    case 'n':
      n_flag = true;
      n_arg = options.optarg;
      break;
    case 'b':
      b_flag = true;
      b_arg = options.optarg;
      break;
    case 'y':
      y_flag = true;
      y_arg = options.optarg;
      break;
    case 'u':
      u_flag = true;
      u_arg = options.optarg;
      break;
    case 'f':
      f_flag = true;
      f_arg = options.optarg;
      break;
    case 'p':
      p_flag = true;
      p_arg = options.optarg;
      break;
    case 'g':
      g_flag = true;
      break;
    case 'o':
      o_flag = true;
      o_arg = options.optarg;
      break;
    case 't':
      t_flag = true;
      break;
    case ':':
      fprintf(stderr, "Option parsing failed: %s\n", options.errmsg);
      return EXIT_FAILURE;
    case '?':
      fprintf(stderr, "Option parsing failed: %s\n", options.errmsg);
      return EXIT_FAILURE;
    default:
      fprintf(stderr, "Unexpectd error. Aborting ...\n");
      abort();
    }
  }

  /* Prints documentation and returns, when help option is active. */
  if (h_flag) {
    fprintf(stderr, "%s", documentation);
    return 0;
  }

  /*
   * Checks command line options for consistency.
   */
  if (!a_flag) {
    fprintf(stderr, "Option -a, --action is mandatory.\n");
    return EXIT_FAILURE;
  } else {
    if (strcmp("generate", a_arg) == 0) action = REGAB_ACTION_GENERATE;
    else if (strcmp("solve", a_arg) == 0) action = REGAB_ACTION_SOLVE;
    else if (strcmp("offspring", a_arg) == 0) action = REGAB_ACTION_OFFSPRING;
    else if (strcmp("extract", a_arg) == 0) action = REGAB_ACTION_EXTRACT;
    else {
      fprintf(stderr, "Argument for option -a, --action must be in [generate|solve|offspring|extract] domain.\n");
      return EXIT_FAILURE;
    }
  }
  if (!c_flag) {
    fprintf(stderr, "Option -c, --config-file is mandatory.\n");
    return EXIT_FAILURE;
  } else {
    config_file = c_arg;
  }
  if (!e_flag) {
    fprintf(stderr, "Option -e, --env is mandatory.\n");
    return EXIT_FAILURE;
  } else {
    env = e_arg;
  }
  if (v_flag) verbose = true;
  if (s_flag) {
    char *endptr;
    errno = 0;    /* To distinguish success/failure after call */
    prng_seed = strtoull(s_arg, &endptr, 10);
    /* Check for various possible errors */
    if ((errno == ERANGE && (prng_seed == LONG_MAX || prng_seed == LONG_MIN))
        || (errno != 0 && prng_seed == 0)) {
      perror("strtol");
      return EXIT_FAILURE;
    }
    if (endptr == s_arg) {
      fprintf(stderr, "No digits were found in prng_seed value.\n");
      return EXIT_FAILURE;
    }
    if (*endptr != '\0') {
      fprintf(stderr, "Further characters after number in prng_seed value: %s\n", endptr);
      return EXIT_FAILURE;
    }
  }
  if (n_flag) {
    char *endptr;
    errno = 0;    /* To distinguish success/failure after call */
    n_games = strtoull(n_arg, &endptr, 10);
    /* Check for various possible errors */
    if ((errno == ERANGE && (n_games == LONG_MAX || n_games == LONG_MIN))
        || (errno != 0 && n_games == 0)) {
      perror("strtol");
      return EXIT_FAILURE;
    }
    if (endptr == n_arg) {
      fprintf(stderr, "No digits were found in n_games value.\n");
      return EXIT_FAILURE;
    }
    if (*endptr != '\0') {
      fprintf(stderr, "Further characters after number in n_games value: %s\n", endptr);
      return EXIT_FAILURE;
    }
  }
  /*
   * The --batch-id parameter could have one single value or a set of values.
   * Multiple values are separated by commas witout spaces.
   *
   * -b 7
   * -b 1,5,95
   */
  if (b_flag) {
    int parse_mode = 1; // 0 means digits, 1 means separator.
    char *beginptr = b_arg;
    char *endptr;
    batch_id_cnt = 1;
    while (*beginptr) {
      if (',' == *beginptr) {
        if (parse_mode != 0) {
          fprintf(stderr, "Wrong format in batch_id value.\n");
          return EXIT_FAILURE;
        }
        batch_id_cnt++;
        parse_mode = 1;
      } else if (isdigit(*beginptr)) parse_mode = 0;
      else {
        fprintf(stderr, "Wrong character in batch_id value.\n");
        return EXIT_FAILURE;
      }
      beginptr++;
    }
    if (parse_mode != 0) {
      fprintf(stderr, "The value for batch_id option couldn't end with a comma.\n");
      return EXIT_FAILURE;
    }
    nbytes = sizeof(uint64_t) * batch_id_cnt;
    batch_ids = (uint64_t *) malloc(nbytes);
    if (!batch_ids) {
      fprintf(stderr, "Unable to allocate memory for batch_ids array.\n");
      return EXIT_FAILURE;
    }
    memset(batch_ids, 0, nbytes);
    beginptr = b_arg;
    for (size_t i = 0; i < batch_id_cnt; i++) {
      errno = 0;    /* To distinguish success/failure after call */
      batch_id = strtoull(beginptr, &endptr, 10);
      if ((errno == ERANGE && batch_id == ULONG_MAX)
          || (errno != 0 && batch_id == 0)) {
        perror("strtoull");
        return EXIT_FAILURE;
      }
      batch_ids[i] = batch_id;
      beginptr = endptr + 1;
    }
    if (*endptr != '\0') {
      fprintf(stderr, "Further characters after number in batch_id value: %s\n", endptr);
      return EXIT_FAILURE;
    }
    sort_utils_insertionsort_asc_u64(batch_ids, batch_id_cnt);
    uint64_t previous = batch_ids[0];
    for (size_t i = 1; i < batch_id_cnt; i++) {
      if (previous == batch_ids[i]) {
        fprintf(stderr, "Duplicated values in batch_id option: %lu\n", previous);
        return EXIT_FAILURE;
      }
      previous = batch_ids[i];
    }
  }
  if (y_flag) {
    char *endptr;
    errno = 0;    /* To distinguish success/failure after call */
    empty_count = strtoul(y_arg, &endptr, 10);
    if ((errno == ERANGE && (empty_count == LONG_MAX || empty_count == LONG_MIN))
        || (errno != 0 && empty_count == 0)) {
      perror("strtoul");
      return EXIT_FAILURE;
    }
    if (endptr == y_arg) {
      fprintf(stderr, "No digits were found in empty_count value.\n");
      return EXIT_FAILURE;
    }
    if (*endptr != '\0') {
      fprintf(stderr, "Further characters after number in empty_count value: %s\n", endptr);
      return EXIT_FAILURE;
    }
    if (empty_count > 60) {
      fprintf(stderr, "Value %u, for variable empty_count is out of range [0..60].\n", empty_count);
      return EXIT_FAILURE;
    }
  }
  if (u_flag) {
    nbytes = strlen(u_arg) + 1;
    position_status_buffer = (char *) malloc(nbytes);
    if (!position_status_buffer) {
      fprintf(stderr, "Unable to allocate memory for position_status_buffer array.\n");
      return EXIT_FAILURE;
    }
    memset(position_status_buffer, 0, nbytes);
    int parse_mode = 1; // 0 means char, 1 means separator.
    char *beginptr = u_arg;
    char *c = position_status_buffer;
    position_status_cnt = 1;
    while (*beginptr) {
      *c = *beginptr;
      if (',' == *beginptr) {
        *c = '\0';
        if (parse_mode != 0) {
          fprintf(stderr, "Wrong format in position_status value.\n");
          return EXIT_FAILURE;
        }
        position_status_cnt++;
        parse_mode = 1;
      } else if (isupper(*beginptr)) parse_mode = 0;
      else {
        fprintf(stderr, "Wrong character in position_status value.\n");
        return EXIT_FAILURE;
      }
      c++;
      beginptr++;
    }
    if (parse_mode != 0) {
      fprintf(stderr, "The value for position_status option couldn't end with a comma.\n");
      return EXIT_FAILURE;
    }
    nbytes = sizeof(char *) * position_status_cnt;
    position_statuses = (char **) malloc(nbytes);
    if (!position_statuses) {
      fprintf(stderr, "Unable to allocate memory for position_statuses array.\n");
      return EXIT_FAILURE;
    }
    memset(position_statuses, 0, nbytes);
    c = position_status_buffer;
    parse_mode = 1;
    size_t j = 0;
    for (size_t i = 0; i < strlen(u_arg) + 1; i++, c++) {
      if (parse_mode == 1) {
        position_statuses[j++] = c;
        parse_mode = 0;
      }
      if (*c == '\0') parse_mode = 1;
    }
    if (position_status_cnt != j) {
      fprintf(stderr, "Error when generation tokens from position_status_buffer.\n");
      return EXIT_FAILURE;
    }
    sort_utils_insertionsort_asc_string(position_statuses, position_status_cnt);
    for (size_t i = 1; i < position_status_cnt; i++) {
      const int compare = strcmp(position_statuses[i - 1], position_statuses[i]);
      if (compare == 0) {
        fprintf(stderr, "Duplicate value in position_statuses, %s.\n", position_statuses[i]);
        return EXIT_FAILURE;
      }
    }
    for (size_t i = 0; i < position_status_cnt; i++) {
      if (strlen(position_statuses[i]) != 3) {
        fprintf(stderr, "Status values must have a length equal to three characters. Value %s is not valid.\n", position_statuses[i]);
        return EXIT_FAILURE;
      }
    }
  }
  if (f_flag) {
    bool is_valid_feature;
    nbytes = strlen(f_arg) + 1;
    feature_buffer = (char *) malloc(nbytes);
    if (!feature_buffer) {
      fprintf(stderr, "Unable to allocate memory for feature_buffer array.\n");
      return EXIT_FAILURE;
    }
    memset(feature_buffer, 0, nbytes);
    int parse_mode = 1; // 0 means char, 1 means separator.
    char *beginptr = f_arg;
    char *c = feature_buffer;
    feature_cnt = 1;
    while (*beginptr) {
      *c = *beginptr;
      if (',' == *beginptr) {
        *c = '\0';
        if (parse_mode != 0) {
          fprintf(stderr, "Wrong format in feature value.\n");
          return EXIT_FAILURE;
        }
        feature_cnt++;
        parse_mode = 1;
      } else if (isalnum(*beginptr)) parse_mode = 0;
      else {
        fprintf(stderr, "Wrong character in feature value.\n");
        return EXIT_FAILURE;
      }
      c++;
      beginptr++;
    }
    if (parse_mode != 0) {
      fprintf(stderr, "The value for feature option couldn't end with a comma.\n");
      return EXIT_FAILURE;
    }
    nbytes = sizeof(board_feature_id_t) * feature_cnt;
    features = (board_feature_id_t *) malloc(nbytes);
    if (!features) {
      fprintf(stderr, "Unable to allocate memory for features array.\n");
      return EXIT_FAILURE;
    }
    memset(features, 0, nbytes);
    c = feature_buffer;
    parse_mode = 1;
    size_t j = 0;
    for (size_t i = 0; i < strlen(f_arg) + 1; i++, c++) {
      if (parse_mode == 1) {
        is_valid_feature = board_feature_get_id_by_name(&features[j], c);
        if (!is_valid_feature) {
          fprintf(stderr, "Feature %s does not exists.\n", c);
          return EXIT_FAILURE;
        }
        j++;
        parse_mode = 0;
      }
      if (*c == '\0') parse_mode = 1;
    }
    if (feature_cnt != j) {
      fprintf(stderr, "Error when generation tokens from feature_buffer.\n");
      return EXIT_FAILURE;
    }
    sort_utils_insertionsort(features, feature_cnt, sizeof(board_feature_id_t), sort_utils_int_cmp);
    for (size_t i = 1; i < feature_cnt; i++) {
      if (features[i] == features[i -1]) {
        fprintf(stderr, "Duplicate entry in feature list: %s\n", board_features[features[i]].name);
        return EXIT_FAILURE;
      }
    }
  }
  if (p_flag) {
    bool is_valid_pattern;
    nbytes = strlen(p_arg) + 1;
    pattern_buffer = (char *) malloc(nbytes);
    if (!pattern_buffer) {
      fprintf(stderr, "Unable to allocate memory for pattern_buffer array.\n");
      return EXIT_FAILURE;
    }
    memset(pattern_buffer, 0, nbytes);
    int parse_mode = 1; // 0 means char, 1 means separator.
    char *beginptr = p_arg;
    char *c = pattern_buffer;
    pattern_cnt = 1;
    while (*beginptr) {
      *c = *beginptr;
      if (',' == *beginptr) {
        *c = '\0';
        if (parse_mode != 0) {
          fprintf(stderr, "Wrong format in pattern value.\n");
          return EXIT_FAILURE;
        }
        pattern_cnt++;
        parse_mode = 1;
      } else if (isalnum(*beginptr)) parse_mode = 0;
      else {
        fprintf(stderr, "Wrong character in pattern value.\n");
        return EXIT_FAILURE;
      }
      c++;
      beginptr++;
    }
    if (parse_mode != 0) {
      fprintf(stderr, "The value for pattern option couldn't end with a comma.\n");
      return EXIT_FAILURE;
    }
    nbytes = sizeof(board_pattern_id_t) * pattern_cnt;
    patterns = (board_pattern_id_t *) malloc(nbytes);
    if (!patterns) {
      fprintf(stderr, "Unable to allocate memory for patterns array.\n");
      return EXIT_FAILURE;
    }
    memset(patterns, 0, nbytes);
    c = pattern_buffer;
    parse_mode = 1;
    size_t j = 0;
    for (size_t i = 0; i < strlen(p_arg) + 1; i++, c++) {
      if (parse_mode == 1) {
        is_valid_pattern = board_pattern_get_id_by_name(&patterns[j], c);
        if (!is_valid_pattern) {
          fprintf(stderr, "Pattern %s does not exists.\n", c);
          return EXIT_FAILURE;
        }
        j++;
        parse_mode = 0;
      }
      if (*c == '\0') parse_mode = 1;
    }
    if (pattern_cnt != j) {
      fprintf(stderr, "Error when generation tokens from pattern_buffer.\n");
      return EXIT_FAILURE;
    }
    sort_utils_insertionsort(patterns, pattern_cnt, sizeof(board_pattern_id_t), sort_utils_int_cmp);
    for (size_t i = 1; i < pattern_cnt; i++) {
      if (patterns[i] == patterns[i -1]) {
        fprintf(stderr, "Duplicate entry in pattern list: %s\n", board_patterns[patterns[i]].name);
        return EXIT_FAILURE;
      }
    }
  }
  if (g_flag) {
    if (p_flag || f_flag) {
      fprintf(stderr, "Error flag -g and flags -p or -f are mutually exclusive.\n");
      return EXIT_FAILURE;
    }
  }
  if (o_flag) {
    if (fut_touch_file(o_arg)) {
      output_file_name = o_arg;
      if (verbose) fprintf(stdout, "Output file \"%s\" has been overwritten.\n", o_arg);
    } else {
      fprintf(stderr, "Unable to open for writing file: \"%s\"\n", o_arg);
      return EXIT_FAILURE;
    }
  }
  if (t_flag) {
    if (!o_flag) {
      fprintf(stderr, "Error option -t, --no-time-out-file, requires option -o, --out-file.\n");
      return EXIT_FAILURE;
    }
  }

  /* Verifies that the config input file is available for reading. */
  FILE *fp = fopen(config_file, "r");
  if (!fp) {
    fprintf(stderr, "Unable to open config file resource for reading, file \"%s\" does not exist.\n", config_file);
    return EXIT_FAILURE;
  }
  fclose(fp);

  /* Verifies mandatory options when action is generate. */
  if (action == REGAB_ACTION_GENERATE) {
    if (!s_flag) {
      fprintf(stderr, "Option -s, --prng-seed, is mandatory when option -a, --action, has value \"generate\".\n");
      return EXIT_FAILURE;
    }
    if (!n_flag) {
      fprintf(stderr, "Option -n, --n-games, is mandatory when option -a, --action, has value \"generate\".\n");
      return EXIT_FAILURE;
    }
  }

  /* Verifies mandatory options when action is solve. */
  if (action == REGAB_ACTION_SOLVE) {
    if (!b_flag) {
      fprintf(stderr, "Option -b, --batch-id, is mandatory when option -a, --action, has value \"solve\".\n");
      return EXIT_FAILURE;
      if (batch_id_cnt > 1) {
        fprintf(stderr, "Multiple values for option -b, --batch-id, are not supported when option -a, --action, has value \"solve\".\n");
        return EXIT_FAILURE;
      }
    }
    if (!y_flag) {
      fprintf(stderr, "Option -y, --empty_count, is mandatory when option -a, --action, has value \"solve\".\n");
      return EXIT_FAILURE;
    }
  }

  /* Verifies mandatory options when action is offspring. */
  if (action == REGAB_ACTION_OFFSPRING) {
    if (!b_flag) {
      fprintf(stderr, "Option -b, --batch-id, is mandatory when option -a, --action, has value \"offspring\".\n");
      return EXIT_FAILURE;
      if (batch_id_cnt > 1) {
        fprintf(stderr, "Multiple values for option -b, --batch-id, are not supported when option -a, --action, has value \"offspring\".\n");
        return EXIT_FAILURE;
      }
    }
    if (!y_flag) {
      fprintf(stderr, "Option -y, --empty-count, is mandatory when option -a, --action, has value \"offspring\".\n");
      return EXIT_FAILURE;
    }
    if (!n_flag) {
      fprintf(stderr, "Option -n, --n-games, is mandatory when option -a, --action, has value \"offspring\".\n");
      return EXIT_FAILURE;
    }
  }

  /* Verifies mandatory options when action is extract. */
  if (action == REGAB_ACTION_EXTRACT) {
    if (!b_flag) {
      fprintf(stderr, "Option -b, --batch-id, is mandatory when option -a, --action, has value \"extract\".\n");
      return EXIT_FAILURE;
    }
    if (!u_flag) {
      fprintf(stderr, "Option -u, --position-status, is mandatory when option -a, --action, has value \"extract\".\n");
      return EXIT_FAILURE;
    }
    if (!p_flag && !f_flag && !g_flag) {
      fprintf(stderr, "One option among -p, --pattern, -f --feature, and -g, --game-positions, is mandatory when option -a, --action, has value \"extract\".\n");
      return EXIT_FAILURE;
    }
    if (!o_flag) {
      fprintf(stderr, "Option -o, --out-file, is mandatory when option -a, --action, has value \"extract\".\n");
      return EXIT_FAILURE;
    }
  }

  /*
   * Checks on command line options ends here.
   */

  /*
   * Loads the config file.
   */
  cfg_t *config = cfg_load(config_file);
  const char *config_check = cfg_get(config, "regab", "check");
  if (!config_check) {
    fprintf(stderr, "Missing entry in config file: section=[regab], key=check\n");
    return EXIT_FAILURE;
  }
  if (strcmp("ok", config_check)) {
    fprintf(stderr, "\"%s\": wrong value for section=[regab], key=check. It has to be \"ok\".\n", config_check);
    return EXIT_FAILURE;
  }
  const char *con_info_host = cfg_get(config, env, "host");
  const char *con_info_port = cfg_get(config, env, "port");
  const char *con_info_dbname = cfg_get(config, env, "dbname");
  const char *con_info_user = cfg_get(config, env, "user");
  const char *con_info_password = cfg_get(config, env, "password");
  if (!con_info_host || !con_info_port || !con_info_dbname || !con_info_user || !con_info_password) {
    fprintf(stderr, "Missing info into config file, section=[%s]:\n", env);
    fprintf(stderr, " - host=%s\n", con_info_host);
    fprintf(stderr, " - port=%s\n", con_info_port);
    fprintf(stderr, " - dbname=%s\n", con_info_dbname);
    fprintf(stderr, " - user=%s\n", con_info_user);
    fprintf(stderr, " - password=%s\n", con_info_password);
    return EXIT_FAILURE;
  }
  char con_info[512];
  if (snprintf(con_info, sizeof(con_info), "host=%s port=%s dbname=%s user=%s password=%s",
               con_info_host, con_info_port, con_info_dbname, con_info_user, con_info_password) >= sizeof(con_info) ) {
    fprintf(stderr, "Error: con_info buffer is not long enough to contain the connection string.\n");
    abort();
  }

  /*
   * Establishes the connection to the database.
   */
  PGconn *con = NULL;

  regab_get_db_connection(&con, con_info);
  if (!con) return EXIT_FAILURE;

  int result = 0;
  if (verbose) {
    fprintf(stdout, "\n");
    fprintf(stdout, "-----------------------------------------------------------\n");
    fprintf(stdout, "--- Last entries from the regab database connection log ---\n");
    fprintf(stdout, "-----------------------------------------------------------\n");
    regab_print_connection_log(&result, con, 3, stdout);
    if (result == -1) {
      fprintf(stderr, "Error while getting info from the connection log table.\n");
      PQfinish(con);
      return EXIT_FAILURE;
    }
    fprintf(stdout, "-----------------------------------------------------------\n");
    fprintf(stdout, "\n");
  }



  /*
   * Selects which action to perform.
   */
  switch (action) {
  case REGAB_ACTION_GENERATE: goto regab_action_generate;
  case REGAB_ACTION_SOLVE: goto regab_action_solve;
  case REGAB_ACTION_OFFSPRING: goto regab_action_offspring;
  case REGAB_ACTION_EXTRACT: goto regab_action_extract;
  default: fprintf(stderr, "Out of range action option, aborting...\n"); abort();
  }



  /*
   * Load the prng game.
   */
 regab_action_generate:
  ;

  size_t n_gp_inserted = 0;

  do_insert_regab_prng_gp_h(&result, &batch_id, con, "INS", prng_seed, n_games);
  if (result == -1) {
    fprintf(stderr, "Error while inserting regab_prng_gp_h record.\n");
    PQfinish(con);
    return EXIT_FAILURE;
  }

  prng_mt19937_t *prng = prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, prng_seed);

  for (int igame = 0; igame < n_games; igame++) {
    gstack.npos = 0;
    gstack.active_node = gstack.nodes;
    game_position_x_set_initial_position(&gstack.active_node->gpx);

    bool has_passed = false;
    Square parent_move = invalid_move;

    while (gstack.npos < REGAB_PRNG_MAX_DEPTH) {
      GamePositionX *curr_gpx = &gstack.active_node->gpx;
      GamePositionX *next_gpx = &(gstack.active_node + 1)->gpx;

      SquareSet move_set = game_position_x_legal_moves(curr_gpx);
      uint8_t legal_move_count = bitw_bit_count_64(move_set);
      Square move = pass_move;

      if ((gstack.active_node->legal_move_set = move_set)) {
        has_passed = false;
        move = prng_mt19937_extract_from_set64(prng, &move_set);
        game_position_x_make_move(curr_gpx, move, next_gpx);
      } else {
        if (has_passed) goto game_completed;
        has_passed = true;
        game_position_x_pass(curr_gpx, next_gpx);
      }
      gstack.active_node->empty_count = bitw_bit_count_64(game_position_x_empties(curr_gpx));
      gstack.active_node->legal_move_count = legal_move_count;
      gstack.active_node->legal_move_count_adjusted = (legal_move_count > 0) ? legal_move_count : 1;
      gstack.active_node->parent_move = parent_move;

      parent_move = move;
      gstack.npos++;
      gstack.active_node++;
    }

    /* If the program reaches this point the stack has overflown. */
    fprintf(stderr, "Game stack has overflown.\n");
    abort();

  game_completed:
    (gstack.active_node - 1)->legal_move_count_adjusted = 0;
    n_gp_inserted += gstack.npos;

    /* Load game into the database. */
    do_insert_game_regab_prng_gp(&result, con, batch_id, igame, &gstack);
    if (result == -1) {
      fprintf(stderr, "Error while loading game number %d.\n", igame);
      PQfinish(con);
      return EXIT_FAILURE;
    }
  }

  /* Checks that the number of games inserted is consistent with the batch size.
   * Writes the number of position inserted. */
  do_check_load_regab_prng_gp_h(&result, con, batch_id, n_games, n_gp_inserted);
  if (result == -1) {
    fprintf(stderr, "Error while checking regab_prng_gp table after batch insertion.\n");
    PQfinish(con);
    return EXIT_FAILURE;
  }

  /*
   * Closes the pseudo random number generator.
   */
  prng_mt19937_free(prng);

  goto regab_program_end;



  /*
   * Solves the selected games.
   */
 regab_action_solve:
  for (unsigned long int i = 0; i < n_games; i++) {
    fprintf(stdout, "Solving game %lu of %lu ...\n", i + 1, n_games);
    regab_prng_gp_record_t record;
    memset(&record, 0, sizeof(record));
    record.batch_id = batch_id;
    record.empty_count = empty_count;
    do_select_position_to_solve(&result, con, &record);
    if (result == -1) {
      fprintf(stderr, "Error while selecting game position to solve.\n");
      PQfinish(con);
      return EXIT_FAILURE;
    } else if (result == 1) {
      printf("record(seq=%ld, batch_id=%d, game_id=%d, pos_id=%d, cst_time=%s\n",
             record.seq, record.batch_id, record.game_id, record.pos_id, record.cst_time);

      char buf[1024];
      GamePositionX gpx;
      gpx.blacks = (SquareSet) record.mover;
      gpx.whites = (SquareSet) record.opponent;
      gpx.player = BLACK_PLAYER;
      game_position_x_print(buf, &gpx);
      printf("%s", buf);

      endgame_solver_env_t env =
        { .log_file = NULL,
          .pve_dump_file = NULL,
          .repeats = 0,
          .pv_recording = false,
          .pv_full_recording = false,
          .pv_no_print = false
        };

      ExactSolution *solution = game_position_es_solve(&gpx, &env);

      printf("solution(outcome=%d, best_move=%s, leaf_count=%zu, node_count=%zu)\n\n",
             solution->outcome, square_as_move_to_string(solution->best_move), solution->leaf_count, solution->node_count);

      record.game_value = solution->outcome;
      record.best_move = solution->best_move;
      record.leaf_count = solution->leaf_count;
      record.node_count = solution->node_count;

      do_update_solved_position_results(&result, con, &record);

    } else if (result == 0) {
      printf("No game position to be solved has been returned by the selection.\n");
      goto regab_program_end;
    } else {
      fprintf(stderr, "Error while selecting game position to solve, unknown return value result=%d.\n", result);
      PQfinish(con);
      return EXIT_FAILURE;
    }
  }
  goto regab_program_end;



  /*
   * Generates the offspring record.
   *
   * From psql the to be processed records for batch_id 1 can be generated as:
   * psql> UPDATE regab_prng_gp SET status = 'CMQ', cst_time = now() WHERE status = 'CMP' AND batch_id = 1;
   *
   *
   * TO TEST ....
   * $ ./build/bin/regab -a offspring -c cfg/regab.cfg -e test -n 3 -b 1 -y 21
   *
   * SELECT * FROM regab_prng_gp WHERE status = 'CMQ' AND batch_id = 1;
   * SELECT * FROM regab_prng_gp WHERE status = 'CMW' AND batch_id = 1;
   * UPDATE regab_prng_gp SET status = 'CMQ' WHERE seq = 40;
   */
 regab_action_offspring:
  ;

  for (size_t i = 0; i < n_games; i++) {
    printf("Selecting game position to process: %zu of %lu ... ", i + 1, n_games);
    regab_prng_gp_record_t record, new_record;
    memset(&record, 0, sizeof(record));
    record.batch_id = batch_id;
    record.empty_count = empty_count;
    do_select_position_to_offspring(&result, con, &record);
    if (result == 0) {
      printf("no more game position to process, terminating.\n");
      break;
    }
    printf("selected record [seq = %lu, ", record.seq);
    printf("best_move = %s, game_value = %+02d]", square_as_move_to_string(record.best_move), record.game_value);
    // ---

    /* Computes the move. */
    GamePositionX current_gpx, updated_gpx;
    current_gpx.player = record.player;
    current_gpx.blacks = record.player == BLACK_PLAYER ? record.mover : record.opponent;
    current_gpx.whites = record.player == BLACK_PLAYER ? record.opponent : record.mover;
    game_position_x_make_move(&current_gpx, record.best_move, &updated_gpx);
    const SquareSet updated_gpx_empties = game_position_x_empties(&updated_gpx);
    const SquareSet updated_gpx_legal_moves = game_position_x_legal_moves(&updated_gpx);
    const int updated_gpx_lmc = bitw_bit_count_64(updated_gpx_legal_moves);
    const bool updated_gpx_is_leaf = !game_position_x_has_any_player_any_legal_move(&updated_gpx);

    /*
    char buf[256];
    game_position_x_print(buf, &current_gpx);
    printf("current:\n%s", buf);
    game_position_x_print(buf, &updated_gpx);
    printf("updated:\n%s", buf);
    */

    new_record.seq = 0;
    new_record.batch_id = record.batch_id;
    new_record.game_id = record.game_id;
    new_record.pos_id = record.pos_id + 100;
    strcpy(new_record.ins_time, "");
    strcpy(new_record.status, "CMR"); // CMR is the offspring label.
    strcpy(new_record.cst_time, "");
    new_record.mover = game_position_x_get_mover(&updated_gpx);
    new_record.opponent = game_position_x_get_opponent(&updated_gpx);
    new_record.player = game_position_x_get_player(&updated_gpx);
    new_record.empty_count = bitw_bit_count_64(updated_gpx_empties);
    new_record.legal_move_set = (int64_t) updated_gpx_legal_moves;
    new_record.legal_move_count = updated_gpx_lmc;
    new_record.legal_move_count_adjusted = updated_gpx_lmc + ((updated_gpx_lmc == 0 && !updated_gpx_is_leaf) ? 1 : 0);
    new_record.parent_move = record.best_move;
    new_record.game_value = - record.game_value;
    new_record.best_move = unknown_move;
    new_record.leaf_count = 0;
    new_record.node_count = 0;
    new_record.parent_gp_id = record.seq;

    // Runs the transaction with the new record insertion and the record update.
    do_insert_offspring_and_update_solved_position(&result, con, &record, &new_record);

    // ---
    printf("\n");
  }

  goto regab_program_end;


  /*
   * Extracts the selected, solved and classified positions from the regab database.
   *
   * There are two options, the "full extract", and the "game positions extract", first option is
   * designated by the p_flag or f_flag boolean variables being true, the second one by the g_flag.
   *
   * Steps:
   *  - 00.a - Collects and set the "saved time".
   *  - 00.b - Sets the format type.
   *  - 01   - Starts a DB transaction.
   *  - 02.a - Checks batch_id argument data, then copies it into the gd structure.
   *  - 02.b - Copies the empty_count field into the gd structure.
   *  - 02.c - Copies the position_status field into the gd structure.
   *  - 02.d - Copies feature argument data into the gd structure.
   *  - 02.e - Checks first and then copies pattern argument data into the gd structure.
   *  - 03   - Collects from the DB the game positions statistics (the position summary table) and
   *             stores them into the gd structure.
   *  - 04   - Collects from the DB the feature and pattern index statistics, assigns the global variable index value,
   *             saves them into the pattern_freq_summary table into the general data structure.
   *             This step also assigns the global variable index value to each feature or pattern.
   *             This action generates the mapping between pattern_id, index_value tuples and the GLM global variables.
   *  - 05   - Creates the CURSOR variable over the query collecting game_positions, game_value, pattern values.
   *  - 06   - Iterates by chunks of data collected from the cusrsor, and writes them into the binary file.
   *  - 07   - Commits and closes the DB transaction, this action deletes the temporary database tables and the cursor variable.
   *  - 08   - Writes the binary output file.
   *
   */
 regab_action_extract:
  ;

  rglmdf_general_data_t gd;
  rglmdf_general_data_init(&gd);

  const size_t pattern_freq_summary_chunk_size = 1024;
  const char *sql_cursor_name_pattern_freqs_debug = "sql_cursor_name_pattern_freqs_debug";
  const char *sql_cursor_name_pattern_freqs = "sql_cursor_name_pattern_freqs";
  size_t pattern_freq_summary_fetched_record_cnt = 0;

  const char *sql_cursor_name_gps_data = "sql_cursor_name_gps_data";
  size_t gps_data_total_record_cnt = 0;
  size_t gps_data_fetched_record_cnt = 0;

  size_t selected_gp_record_cnt = 0;

  time_t saved_time;
  size_t n, ntuples;

  size_t glm_f_variable_cnt = 0;
  size_t glm_p_variable_cnt = 0;

  rglmdf_iarray_data_type_t iarray_data_type;
  rglmdf_file_data_format_type_t file_data_format;

  PGresult *res = NULL;
  int ret_code;

  /*
   * - 00.a - Collects and set the "saved time".
   *
   * The time saved into the gd structure is the current time when the t_flag is NULL,
   * when t_flag is selected the value is set to zero.
   * Setting to zero is used for testing, when an always changing value is changing the file hash.
   */
  if (t_flag) {
    saved_time = (time_t) 0;
  } else {
    /* Obtain current time as seconds elapsed since the Epoch. */
    saved_time = time(NULL);
    assert(saved_time != ((time_t) 0));
  }
  if (verbose) fprintf(stdout, "Time saved to file is %s", ctime(&saved_time));
  rglmdf_set_file_creation_time(&gd, saved_time);

  /* - 00.b - Sets the format type. */
  file_data_format = (f_flag || p_flag) ? RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL : RGLMDF_FILE_DATA_FORMAT_TYPE_IS_POSITIONS;
  rglmdf_set_format(&gd, file_data_format);

  /* - 01 - Starts a DB transaction. */
  res = PQexec(con, "BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ");
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "PQexec: BEGIN command failed: %s", PQerrorMessage(con));
    PQfinish(con);
    return EXIT_FAILURE;
  }
  PQclear(res);

  /* - 02.a - Checks batch_id argument data, then copies it into the gd structure. */
  do_action_extract_check_batches(&result, con, verbose, batch_ids, batch_id_cnt);
  if (result != 0) {
    fprintf(stderr, "Error occured into function do_action_extract_check_batches(). Exiting ...\n");
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }
  n = rglmdf_set_batch_id_cnt(&gd, batch_id_cnt);
  if (n != batch_id_cnt) {
    fprintf(stderr, "Unable to allocate memory for batch_ids array.\n");
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }
  memcpy(rglmdf_get_batch_ids(&gd), batch_ids, sizeof(batch_ids) * batch_id_cnt);

  /* - 02.b - Copies the empty_count field into the gd structure. */
  rglmdf_set_empty_count(&gd, empty_count);

  /* - 02.c - Copies the position_status field into the gd structure. */
  n = rglmdf_set_position_status_cnt(&gd, position_status_cnt);
  if (n != position_status_cnt) {
    fprintf(stderr, "Unable to allocate memory for position_statuses array.\n");
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }
  char **cpp = rglmdf_get_position_statuses(&gd);
  memcpy(*cpp, position_status_buffer, RGLMDF_POSITION_STATUS_BUF_SIZE * position_status_cnt);
  if (verbose) {
    fprintf(stdout, "Searched position statuses: ");
    for (size_t i = 0; i < position_status_cnt; i++) {
      fprintf(stdout, "%s%s", position_statuses[i], i == position_status_cnt - 1 ? "\n\n" : ", ");
    }
  }

  /* - 02.d - Copies feature argument data into the gd structure. */
  n = rglmdf_set_feature_cnt(&gd, feature_cnt);
  if (n != feature_cnt) {
    fprintf(stderr, "Unable to allocate memory for features array.\n");
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }
  memcpy(rglmdf_get_features(&gd), features, sizeof(board_feature_id_t) * feature_cnt);

  /* - 02.e - Checks first and then copies pattern argument data into the gd structure. */
  do_action_extract_check_patterns(&result, con, verbose, patterns, pattern_cnt);
  if (result != 0) {
    fprintf(stderr, "Error occured into do_action_extract_check_patterns(). Exiting ...\n");
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }
  n = rglmdf_set_pattern_cnt(&gd, pattern_cnt);
  if (n != pattern_cnt) {
    fprintf(stderr, "Unable to allocate memory for patterns array.\n");
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }
  memcpy(rglmdf_get_patterns(&gd), patterns, sizeof(board_pattern_id_t) * pattern_cnt);

  /* - 03 - Collects from the DB the game positions statistics (the position summary table) and stores them into the gd structure. */
  do_action_extract_count_positions(&result, con, verbose, &gd);
  if (result != 0) {
    fprintf(stderr, "Error occured into function do_action_extract_count_positions(). Exiting ...\n");
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }

  /* - 04 - Collects from the DB the feature and pattern index statistics, assigns the global variable index value,
   *        saves them into the pattern_freq_summary table into the general data structure. */
  if (file_data_format == RGLMDF_FILE_DATA_FORMAT_TYPE_IS_GENERAL) {
    for (size_t i = 0; i < feature_cnt; i++)
      glm_f_variable_cnt += board_features[features[i]].field_cnt;
    do_action_extract_pattern_freqs_prepare_cursor(&result, con, verbose, empty_count, batch_id_cnt, batch_ids, position_status_cnt, position_statuses,
                                                   pattern_cnt, patterns, glm_f_variable_cnt,
                                                   sql_cursor_name_pattern_freqs_debug, sql_cursor_name_pattern_freqs,
                                                   &glm_p_variable_cnt);
    if (result != 0) {
      fprintf(stderr, "Error occured into function do_action_extract_pattern_freqs_prepare_cursor(). Exiting ...\n");
      res = PQexec(con, "ROLLBACK");
      PQfinish(con);
      return EXIT_FAILURE;
    }
    ntuples = glm_f_variable_cnt + glm_p_variable_cnt;
    if (verbose) fprintf(stdout, "GLM variables are %zu, given by features are %zu, and by patterns are %zu.\n",
                         ntuples, glm_f_variable_cnt, glm_p_variable_cnt);
    n = rglmdf_set_entity_freq_summary_ntuples(&gd, glm_f_variable_cnt, glm_p_variable_cnt, ntuples);
    if (n != ntuples) {
      fprintf(stderr, "Unable to allocate memory for frequency summary table.\n");
      res = PQexec(con, "ROLLBACK");
      PQfinish(con);
      return EXIT_FAILURE;
    }

    do_action_extract_game_pos_cnt(&result, con, empty_count, batch_id_cnt, batch_ids, position_status_cnt, position_statuses, &selected_gp_record_cnt);
    if (result != 0) {
      fprintf(stderr, "Error occured into function do_action_extract_game_pos_cnt(). Exiting ...\n");
      res = PQexec(con, "ROLLBACK");
      PQfinish(con);
      return EXIT_FAILURE;
    }
    size_t k = 0;
    rglmdf_pattern_freq_summary_record_t *pfsrp;
    pfsrp = rglmdf_get_pattern_freq_summary_records(&gd);
    for (size_t i = 0; i < feature_cnt; i++) {
      for (size_t j = 0; j < board_features[features[i]].field_cnt; j++) {
        rglmdf_pattern_freq_summary_record_t *rec = &gd.pattern_freq_summary.records[k];
        rec->glm_variable_id = k;
        rec->entity_class = BOARD_ENTITY_CLASS_FEATURE;
        rec->entity_id = features[i];
        rec->principal_index_value = j;
        rec->total_cnt = selected_gp_record_cnt;
        rec->relative_frequency = 1.0;
        rec->theoretical_probability = 1.0;
        rec->weight = 0.0;
        k++;
      }
    }
    pfsrp += glm_f_variable_cnt;

    for (;;) {
      size_t fetched_record_cnt = 0;
      do_action_extract_pattern_freqs_cursor_fetch(&result, con, verbose, sql_cursor_name_pattern_freqs, pattern_freq_summary_chunk_size, pfsrp, &fetched_record_cnt);
      if (result != 0) {
        fprintf(stderr, "Error occured into do_action_extract_pattern_freqs_cursor_fetch(). Exiting ...\n");
        res = PQexec(con, "ROLLBACK");
        PQfinish(con);
        return EXIT_FAILURE;
      }
      pattern_freq_summary_fetched_record_cnt += fetched_record_cnt;
      if (fetched_record_cnt == 0) {
        if (pattern_freq_summary_fetched_record_cnt != glm_p_variable_cnt) {
          res = PQexec(con, "ROLLBACK");
          PQfinish(con);
          fprintf(stderr, "Records count mismatch, do_action_extract_pattern_freqs_cursor_fetch returned an unexpectd number of records.\n");
          return EXIT_FAILURE;
        }
        break;
      }
      pfsrp += fetched_record_cnt;
    }
  }

  /* - 05 - Creates the CURSOR variable, and writes the total amount of expected records. */
  do_action_extract_game_pos_prepare_cursor(&result, con, verbose, empty_count, batch_id_cnt, batch_ids, position_status_cnt, position_statuses,
                                            pattern_cnt, patterns, sql_cursor_name_gps_data, &gps_data_total_record_cnt);
  if (result != 0) {
    fprintf(stderr, "Error occured into do_action_extract_game_pos_prepare_cursor(). Exiting ...\n");
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }
  iarray_data_type = (pattern_cnt != 0) ? RGLMDF_IARRAY_IS_INDEX : RGLMDF_IARRAY_IS_MISSING;
  n = rglmdf_set_positions_ntuples(&gd, gps_data_total_record_cnt, iarray_data_type);
  if (n != gps_data_total_record_cnt) {
    fprintf(stderr, "Unable to allocate memory for solved and classified game positions table.\n");
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }

  /* - 06 - Iterates over chunks of data retrieved from the cursor. */
  rglmdf_solved_and_classified_gp_record_t *scgprp = rglmdf_get_positions_records(&gd);
  double *farrayp = rglmdf_get_positions_farray(&gd);
  uint32_t *i0arrayp = rglmdf_get_positions_i0array(&gd);
  const size_t nf = rglmdf_get_positions_n_fvalues_per_record(&gd);
  const size_t ni = rglmdf_get_positions_n_index_values_per_record(&gd);
  size_t returned_ntuples = 0;
  for (;;) {
    do_action_extract_game_pos_cursor_fetch(&result, con, verbose, &gd, &returned_ntuples, scgprp, farrayp, i0arrayp, sql_cursor_name_gps_data, RGLMDF_GPS_DATA_CHUNK_SIZE);
    if (result != 0) {
      fprintf(stderr, "Error occured into do_action_extract_game_pos_cursor_fetch(). Exiting ...\n");
      res = PQexec(con, "ROLLBACK");
      PQfinish(con);
      return EXIT_FAILURE;
    }
    gps_data_fetched_record_cnt += returned_ntuples;
    if (returned_ntuples == 0) {
      if (gps_data_fetched_record_cnt != gps_data_total_record_cnt) {
        res = PQexec(con, "ROLLBACK");
        PQfinish(con);
        fprintf(stderr, "Records count mismatch, do_action_extract_game_pos_cursor_fetch() returned an unexpectd number of records.\n");
        return EXIT_FAILURE;
      }
      break;
    }
    scgprp += returned_ntuples;
    farrayp += nf * returned_ntuples;
    i0arrayp += ni * returned_ntuples;
  }
  if (verbose) fprintf(stdout, "Total count of Game Position Records is %zu.\n", gps_data_total_record_cnt);

  /* - 7 - Closes the DB transaction. */
  res = PQexec(con, "END");
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "PQexec: END command failed: %s", PQerrorMessage(con));
    PQfinish(con);
    return EXIT_FAILURE;
  }
  PQclear(res);
  if (verbose) fprintf(stdout, "Database transaction has been closed.\n");

  /* - 8 - Writes the binary output file. */
  ret_code = rglmdf_write_general_data_to_binary_file(&gd, output_file_name, saved_time);
  if (ret_code == EXIT_SUCCESS) {
    if (verbose) fprintf(stdout, "Binary output file written to %s, computed SHA3-256 digest, written to file %s.sha3-256.\n", output_file_name, output_file_name);
  } else {
    fprintf(stderr, "Unable to write correctly binary output file: %s\n", output_file_name);
    return ret_code;
  }

  goto regab_program_end;



  /*
   * End of program.
   */
 regab_program_end:

  /*
   * Closes the connection to the database and cleanup.
   */
  PQfinish(con);

  /*
   * Frees resources.
   */
  cfg_free(config);
  free(batch_ids);
  free(position_status_buffer);
  free(position_statuses);
  free(feature_buffer);
  free(pattern_buffer);
  free(patterns);
  rglmdf_general_data_release(&gd);

  return EXIT_SUCCESS;
}
