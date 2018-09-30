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
 * @copyright 2017, 2018 Roberto Corradini. All rights reserved.
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

#include <libpq-fe.h>

#include "main_option_parse.h"
#include "file_utils.h"
#include "cfg.h"
#include "prng.h"
#include "board.h"
#include "board_pattern.h"
#include "endgame_solver.h"
#include "exact_solver.h"
#include "game_tree_utils.h"
#include "sort_utils.h"



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
  {"help",              'h', MOP_NONE},
  {"verbose",           'v', MOP_NONE},
  {"action",            'a', MOP_REQUIRED},
  {"config-file",       'c', MOP_REQUIRED},
  {"env",               'e', MOP_REQUIRED},
  {"prng-seed",         's', MOP_REQUIRED},
  {"n-games",           'n', MOP_REQUIRED},
  {"batch-id",          'b', MOP_REQUIRED},
  {"empty-count",       'y', MOP_REQUIRED},
  {"position-status",   'u', MOP_REQUIRED},
  {"pattern",           'p', MOP_REQUIRED},
  {"out-file",          'o', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "  regab [OPTION...] - Reversi EndGame Analytics Base\n"
  "\n"
  "Options:\n"
  "  -h, --help            Show help options\n"
  "  -v, --verbose         Verbose output\n"
  "  -a, --action          Action to be performed                                      - Mandatory - Must be in [generate|solve|offspring|extract].\n"
  "  -c, --config-file     Config file name                                            - Mandatory.\n"
  "  -e, --env             Environment                                                 - Mandatory.\n"
  "  -s, --prng-seed       Seed used by the Pseudo Random Number Generator             - Mandatory when action is generate.\n"
  "  -n, --n-games         Number of random game generated, solved, or classified      - Default is one.\n"
  "  -b, --batch-id        Batch id                                                    - Mandatory when action is in [solve].\n"
  "  -y, --empty-count     Number of empty squares in the solved game position         - Mandatory when action is in [solve].\n"
  "  -u, --position-status One or more status flag for the selection of game positions - Mandatory when action is in [extract].\n"
  "  -p, --pattern         One or more patterns                                        - Mandatory when action is in [extract].\n"
  "  -o, --out-file        The output file name                                        - Mandatory when action is in [extract].\n"
  "\n"
  "Description:\n"
  "  To be completed ... .\n"
  "\n"
  "Author:\n"
  "  Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2017, 2018 Roberto Corradini. All rights reserved.\n"
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

static int p_flag = false;
static char *p_arg = NULL;

static int o_flag = false;
static char *o_arg = NULL;

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
      abort();
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
      fprintf(stdout, "\nSearched patterns: ");
      for (size_t i = 0; i < pattern_cnt; i++) fprintf(stdout, "%d, ", patterns[i]);
      fprintf(stdout, "\n");
      fprintf(stdout, "Patterns found matching the query\n");
      fprintf(stdout, "____________________________________________________\n");
      fprintf(stdout, "      |            |        |            |          \n");
      fprintf(stdout, " ---- | pattern_id | status | ninstances | nsquares \n");
      fprintf(stdout, "______|_____  _____|________|____________|__________\n");
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
      abort();
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
      for (size_t i = 0; i < batch_id_cnt; i++) fprintf(stdout, "%lu, ", batch_ids[i]);
      fprintf(stdout, "\n");
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

  size_t pattern_cnt = 0;
  char *pattern_buffer = NULL;
  board_pattern_id_t *patterns = NULL;

  char *output_file_name = NULL;

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
    case 'p':
      p_flag = true;
      p_arg = options.optarg;
      break;
    case 'o':
      o_flag = true;
      o_arg = options.optarg;
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
    batch_ids = (uint64_t *) malloc(sizeof(uint64_t) * batch_id_cnt);
    if (!batch_ids) {
      fprintf(stderr, "Unable to allocate memory for batch_ids array.\n");
      return EXIT_FAILURE;
    }
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
    position_status_buffer = (char *) malloc(strlen(u_arg) + 1);
    if (!position_status_buffer) {
      fprintf(stderr, "Unable to allocate memory for position_status_buffer array.\n");
      return EXIT_FAILURE;
    }
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
    position_statuses = (char **) malloc(sizeof(char *) * position_status_cnt);
    if (!position_statuses) {
      fprintf(stderr, "Unable to allocate memory for position_statuses array.\n");
      return EXIT_FAILURE;
    }
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
  if (p_flag) {
    bool is_valid_pattern;
    pattern_buffer = (char *) malloc(strlen(p_arg) + 1);
    if (!pattern_buffer) {
      fprintf(stderr, "Unable to allocate memory for pattern_buffer array.\n");
      return EXIT_FAILURE;
    }
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
        fprintf(stderr, "Wrong character in position_status value.\n");
        return EXIT_FAILURE;
      }
      c++;
      beginptr++;
    }
    if (parse_mode != 0) {
      fprintf(stderr, "The value for pattern option couldn't end with a comma.\n");
      return EXIT_FAILURE;
    }
    patterns = (board_pattern_id_t *) malloc(sizeof(board_pattern_id_t) * pattern_cnt);
    if (!patterns) {
      fprintf(stderr, "Unable to allocate memory for patterns array.\n");
      return EXIT_FAILURE;
    }
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
  if (o_flag) {
    if (fut_touch_file(o_arg)) {
      output_file_name = o_arg;
      if (verbose) fprintf(stdout, "Output file \"%s\" has been overwritten.\n", o_arg);
    } else {
      fprintf(stderr, "Unable to open for writing file: \"%s\"\n", o_arg);
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
    if (!p_flag) {
      fprintf(stderr, "Option -p, --pattern, is mandatory when option -a, --action, has value \"extract\".\n");
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

  do_insert_regab_prng_gp_h (&result, &batch_id, con, "INS", prng_seed, n_games);
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
    fprintf(stderr, "Game stack has oferflown.\n");
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
   * Steps:
   *  - 00 - Starts a DB transaction.
   *  - 01 - Checks that the imput data (batch_id, pattern) is consistent with the DB data.
   *  - 02 - Opens the output binary file, and writes the header.
   *  - 03 - Creates the DB temporary table.
   *  - 04 - Collects from the DB the game positions statistics and writes them to the binary file.
   *  - 05 - Collects from the DB the pattern index statistics, writes them to the binary file,
   *         and accumulates the data into the DB temp table.
   *         This step is organized as a LOOP indexed by pattern_id.
   *  - 06 - Assigns the global variable index value to each record in the DB temp table.
   *         This action generates the mapping between pattern_id, index_value tuples and the GLM global variables.
   *         Writes the mapping, direct and inverse, to the binary file.
   *  - 07 - Creates the CURSOR variable over the query collecting game_positions, game_value, pattern values.
   *  - 08 - Iterates by chunks of data collected from the cusrsor, and writes them into the binary file.
   *  - 09 - Closes the cursor.
   *  - 10 - Drops the DB temporary table.
   *  - 11 - Closes the output file.
   *  - 12 - Commits the DB transaction.
   *
   */
 regab_action_extract:
  ;

  printf("Action extract is not implemented yet.\n");

  for (size_t i = 0; i < batch_id_cnt; i++) {
    printf("batch_ids[%zu] = %lu\n", i, batch_ids[i]);
  }

  for (size_t i = 0; i < position_status_cnt; i++) {
    printf("position_statuses[%zu] = %s\n", i, position_statuses[i]);
  }

  for (size_t i = 0; i < pattern_cnt; i++) {
    printf("pattern[%zu] = %d, %s\n", i, patterns[i], board_patterns[patterns[i]].name);
  }

  PGresult *res = NULL;
  FILE *ofp = NULL;

  /* - 00 - Starts a DB transaction. */
  res = PQexec(con, "BEGIN TRANSACTION ISOLATION LEVEL REPEATABLE READ");
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "PQexec: BEGIN command failed: %s", PQerrorMessage(con));
    PQfinish(con);
    return EXIT_FAILURE;
  }
  PQclear(res);

  /* - 01.a - Checks batch_id argument data. */
  do_action_extract_check_batches(&result, con, verbose, batch_ids, batch_id_cnt);
  if (result != 0) {
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }

  /* - 01.b - Checks pattern argument data. */
  do_action_extract_check_patterns(&result, con, verbose, patterns, pattern_cnt);
  if (result != 0) {
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }

  /* - 02 - TO BE COMPLETED. Open output file. */
  ofp = fopen(output_file_name, "rw");
  if (!ofp) {
    fprintf(stderr, "Unable to open output file: %s\n", output_file_name);
    res = PQexec(con, "ROLLBACK");
    PQfinish(con);
    return EXIT_FAILURE;
  }

  /* - 03 - Creates the DB temporary table. */
  /* - 04 - Collects from the DB the game positions statistics and writes them to the binary file. */
  /* - 05 - Collects from the DB the pattern index statistics, writes them to the binary file ... */

  /* - 06 - Assigns the global variable index value ... */
  /* - 07 - Creates the CURSOR variable ... */
  /* - 08 - Iterates over chunks of data retrieved from the cursor. */

  /* - 09 - Closes the cursor. */

  /* - 10 - Drops the DB temporary table. */

  /* - 11 - Clean up output file. */
  fclose(ofp);

  /* -12 - Closes the DB transaction. */
  res = PQexec(con, "END");
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "PQexec: END command failed: %s", PQerrorMessage(con));
    PQfinish(con);
    return EXIT_FAILURE;
  }
  PQclear(res);

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
  free(pattern_buffer);
  free(patterns);

  return 0;
}
