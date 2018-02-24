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

#include <libpq-fe.h>

#include "main_option_parse.h"
#include "cfg.h"
#include "prng.h"
#include "board.h"
#include "board_pattern.h"
#include "endgame_solver.h"
#include "exact_solver.h"
#include "game_tree_utils.h"



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
  REGAB_ACTION_GENERATE_CLASSIFICATION, // Generates the classification header and classification records.
  REGAB_ACTION_CLASSIFY,                // Classifies the records by computing the proper index.
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
} regab_prng_gp_record_t;

typedef struct regab_prng_gp_classification_record_s {
  int64_t seq;
  char ins_time[32];
  char status[4];
  char cst_time[32];
  int class_id;
  int64_t gp_id;
  int pattern_instance;
  int index_value;
} regab_prng_gp_classification_record_t;



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
  {"pattern",           'p', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "  regab [OPTION...] - Reversi EndGame Analytics Base\n"
  "\n"
  "Options:\n"
  "  -h, --help        Show help options\n"
  "  -v, --verbose     Verbose output\n"
  "  -a, --action      Action to be performed                                 - Mandatory - Must be in [generate|solve|gen_class|classify].\n"
  "  -c, --config-file Config file name                                       - Mandatory.\n"
  "  -e, --env         Environment                                            - Mandatory.\n"
  "  -s, --prng-seed   Seed used by the Pseudo Random Number Generator        - Mandatory when action is generate.\n"
  "  -n, --n-games     Number of random game generated, solved, or classified - Default is one.\n"
  "  -b, --batch-id    Batch id                                               - Mandatory when action is in [solve|gen_class|classify].\n"
  "  -y, --empty-count Number of empty squares in the solved game position    - Mandatory when action is in [solve|gen_class|classify].\n"
  "  -p, --pattern     Board pattern                                          - Mandatory when action is in [gen_class|classify].\n"
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

static int p_flag = false;
static char *p_arg = NULL;

static regab_action_t action = REGAB_ACTION_INVALID;
static char *config_file = NULL;
static char *env = NULL;
static bool verbose = false;
static uint64_t prng_seed = 0;
static unsigned long int n_games = 1;
static board_pattern_id_t bpi = BOARD_PATTERN_INVALID;

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
                  "INSERT INTO regab_prng_gp (batch_id, game_id, pos_id, ins_time, status, cst_time, mover, opponent, player, empty_count, legal_move_set, legal_move_count, legal_move_count_adjusted, parent_move) "
                  "VALUES ($1::INTEGER, $2::INTEGER, $3::INTEGER, $4::TIMESTAMP, $5::CHAR(3), $6::TIMESTAMP, $7::SQUARE_SET, $8::SQUARE_SET, $9::PLAYER, $10::SMALLINT, $11::SQUARE_SET, $12::SMALLINT, $13::SMALLINT, $14::GAME_MOVE);",
                  14,
                  NULL);
  PQclear(res);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "Error while preparing statement, %s", PQerrorMessage(*conp));
    PQfinish(*conp);
    *conp = NULL;
    return;
  }

}

static void
do_insert_regab_prng_gp_classification_h (int *result,
                                          PGconn *con,
                                          unsigned int pattern_id,
                                          unsigned int batch_id,
                                          unsigned int empty_count)
{
  PGresult *res = NULL;
  char command[512];
  int clen, seq;

  const board_pattern_t *bp = board_patterns + pattern_id;

  /* Inserts the new record in the header table. */
  clen = snprintf(command,
                  sizeof(command),
                  "SELECT insert_regab_prng_gp_classification_h('%s', %u, %u) AS output;",
                  bp->name, batch_id, empty_count);
  if (clen >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL statement (3).\n");
    abort();
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK
      || PQntuples(res) != 1
      || strcmp("output", PQfname(res, 0)) != 0) {
    fprintf(stderr, "FUNCTION CALL failed.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    seq = atol(PQgetvalue(res, 0, 0));
    printf("Inserted record into table regab_prng_gp_classification_h (pattern_id=%u, batch_id=%u, empty_count=%u), returned key=%d\n",
           pattern_id, batch_id, empty_count, seq);
  }
  PQclear(res);
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
do_update_classification_index (int *result,
                                PGconn *con,
                                size_t n,
                                regab_prng_gp_classification_record_t records[])
{
  PGresult *res = NULL;
  char command[65536];
  char *cp;
  size_t cl;

  if (!result) return;
  if (!con || !records) {
    *result = -1;
    return;
  }
  *result = 0;

  /* Updates the records with the computed indexes. */
  const char *c0 =
    "UPDATE regab_prng_gp_classification AS r "
    "SET status = b.status, cst_time = b.cst_time, index_value = b.index_value "
    "FROM (VALUES ";
  const char *c1 = "(%"PRId64", 'CMP', now(), %u)%s";
  const char *c2 = ", ";
  const char *c3 = ") AS b(seq, status, cst_time, index_value) WHERE r.seq = b.seq;";

  cl = snprintf(command, sizeof(command), "%s", c0);
  cp = command + cl;

  for (size_t i = 0; i < n; i++) {
    cl += snprintf(cp, sizeof(command) - cl, c1,
                   records[i].seq, records[i].index_value,
                   (i < n - 1) ? c2 : c3);
    cp = command +cl;
  }
  if (cl >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command (8).\n");
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

static int
do_select_db_pattern_id (int *result,
                         PGconn *con,
                         board_pattern_id_t bpi)
{
  PGresult *res = NULL;
  char command[512];
  int clen;

  size_t ntuples;
  int seq;

  if (!result) return -1;
  if (!con) {
    *result = -1;
    return -1;
  }
  *result = 0;
  seq = -1;

  clen = snprintf(command,
                  sizeof(command),
                  "SELECT seq FROM regab_prng_patterns WHERE pattern_name = '%s';",
                  board_patterns[bpi].name);
  if (clen >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL statement (9).\n");
    abort();
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "Select command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    ntuples = PQntuples(res);
    if (ntuples > 1) *result = -1;
    else if (ntuples == 1) {
      if (strcmp("seq", PQfname(res, 0)) != 0) {
        *result = -1;
      } else { // Everything is ok, and one record is selected.
        seq = atol(PQgetvalue(res, 0, 0));
        *result = 1;
      }
    } else { // Ok, but no record selected.
      *result = 0;
    }
  }
  PQclear(res);
  return seq;
}

static int
do_select_class_id (int *result,
                    PGconn *con,
                    board_pattern_id_t bpi,
                    unsigned int batch_id,
                    unsigned int empty_count)
{
  PGresult *res = NULL;
  char command[512];
  int clen;

  size_t ntuples;
  int pattern_id;
  int class_id;

  if (!result) return -1;
  if (!con) {
    *result = -1;
    return -1;
  }
  *result = 0;
  pattern_id = -1;
  class_id = -1;

  pattern_id = do_select_db_pattern_id(result, con, bpi);
  if (pattern_id < 0 || *result < 0) {
    fprintf(stderr, "No matching definition for pattern found into the database table regab_prng_patterns.\n");
    *result = -1;
    return -1;
  }

  /* Selects the class_id key. */
  clen = snprintf(command,
                  sizeof(command),
                  "SELECT seq FROM regab_prng_gp_classification_h WHERE pattern_id = %u AND batch_id = %u AND empty_count = %u;",
                  pattern_id, batch_id, empty_count);
  if (clen >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL statement (10).\n");
    abort();
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "Select command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    ntuples = PQntuples(res);
    if (ntuples > 1) *result = -1;
    else if (ntuples == 1) {
      if (strcmp("seq", PQfname(res, 0)) != 0) {
        *result = -1;
      } else { // Everything is ok, and one record is selected.
        class_id = atol(PQgetvalue(res, 0, 0));
        *result = 1;
      }
    } else { // Ok, but no record selected.
      *result = 0;
    }
  }
  PQclear(res);
  if (*result != 1) return -1;
  return class_id;
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
do_select_position (int *result,
                    PGconn *con,
                    size_t n,
                    regab_prng_gp_record_t records[])
{
  PGresult *res = NULL;
  char command[16384];
  char *cp;
  size_t cl;

  size_t ntuples;

  if (!result) return;
  if (!con) {
    *result = -1;
    return;
  }
  *result = 0;

  /* Selects and updates at most one record ready to be solved. */
  const char *c0 =
    "SELECT game_id, pos_id, ins_time, status, cst_time, mover, opponent, player "
    "FROM regab_prng_gp WHERE seq IN (";
  const char *c1 = "%"PRId64"%s";
  const char *c2 = ", ";
  const char *c3 = ");";

  cl = snprintf(command, sizeof(command), "%s", c0);
  cp = command + cl;

  for (size_t i = 0; i < n; i++) {
    cl += snprintf(cp, sizeof(command) - cl, c1,
                   records[i].seq,
                   (i < n - 1) ? c2 : c3);
    cp = command +cl;
  }
  if (cl >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL command (1).\n");
    abort();
  }

  //printf("QUERY:\n%s\n", command);

  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "Select command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    ntuples = PQntuples(res);
    if (ntuples > n) *result = -1;
    else if (ntuples > 0) {
      if (strcmp("game_id", PQfname(res, 0)) != 0
          || strcmp("pos_id", PQfname(res, 1)) != 0
          || strcmp("ins_time", PQfname(res, 2)) != 0
          || strcmp("status", PQfname(res, 3)) != 0
          || strcmp("cst_time", PQfname(res, 4)) != 0
          || strcmp("mover", PQfname(res, 5)) != 0
          || strcmp("opponent", PQfname(res, 6)) != 0
          || strcmp("player", PQfname(res, 7)) != 0) {
        *result = -1;
      } else { // Everything is ok.
        for (int i = 0; i < ntuples; i++) {
          records[i].game_id = atol(PQgetvalue(res, i, 0));
          records[i].pos_id = atol(PQgetvalue(res, i, 1));
          strcpy(records[i].ins_time, PQgetvalue(res, i, 2));
          strcpy(records[i].status, PQgetvalue(res, i, 3));
          strcpy(records[i].cst_time, PQgetvalue(res, i, 4));
          records[i].mover = atol(PQgetvalue(res, i, 5));
          records[i].opponent = atol(PQgetvalue(res, i, 6));
          records[i].player = atoi(PQgetvalue(res, i, 7));
        }
        *result = ntuples;
      }
    } else { // Ok, but no record selected.
      *result = 0;
    }
  }
  PQclear(res);
}

static void
do_select_index_to_compute (int *result,
                            PGconn *con,
                            size_t buffer_size,
                            int class_id,
                            regab_prng_gp_classification_record_t records[])
{
  PGresult *res = NULL;
  char command[1024];
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
                  "WITH updated AS ("
                  "UPDATE regab_prng_gp_classification SET status = 'WIP', cst_time = now() WHERE seq IN ("
                  "SELECT seq FROM regab_prng_gp_classification WHERE class_id = %d AND status = 'INS' "
                  "ORDER BY class_id, gp_id FOR UPDATE SKIP LOCKED LIMIT %zu) "
                  "RETURNING seq, ins_time, status, cst_time, class_id, gp_id, pattern_instance, index_value) "
                  "SELECT * FROM updated ORDER BY class_id, gp_id, pattern_instance;",
                  class_id, buffer_size);
  if (clen >= sizeof(command)) {
    fprintf(stderr, "Error: command buffer is not long enough to contain the SQL statement (2).\n");
    abort();
  }
  res = PQexec(con, command);
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "Select for update command has problems.\n");
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    ntuples = PQntuples(res);
    if (ntuples > buffer_size) *result = -1;
    else if (ntuples > 0) {
      if (strcmp("seq", PQfname(res, 0)) != 0
          || strcmp("ins_time", PQfname(res, 1)) != 0
          || strcmp("status", PQfname(res, 2)) != 0
          || strcmp("cst_time", PQfname(res, 3)) != 0
          || strcmp("class_id", PQfname(res, 4)) != 0
          || strcmp("gp_id", PQfname(res, 5)) != 0
          || strcmp("pattern_instance", PQfname(res, 6)) != 0
          || strcmp("index_value", PQfname(res, 7)) != 0) {
        *result = -1;
      } else { // Everything is ok.
        for (int i = 0; i < ntuples; i++) {
          records[i].seq = atol(PQgetvalue(res, i, 0));
          strcpy(records[i].ins_time, PQgetvalue(res, i, 1));
          strcpy(records[i].status, PQgetvalue(res, i, 2));
          strcpy(records[i].cst_time, PQgetvalue(res, i, 3));
          records[i].class_id = atol(PQgetvalue(res, i, 4));
          records[i].gp_id = atol(PQgetvalue(res, i, 5));
          records[i].pattern_instance = atoi(PQgetvalue(res, i, 6));
          records[i].index_value = atol(PQgetvalue(res, i, 7));
        }
        *result = ntuples;
      }
    } else { // Ok, but no record selected.
      *result = 0;
    }
  }
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
  uint8_t empty_count = 0;

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
    case 'p':
      p_flag = true;
      p_arg = options.optarg;
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
    else if (strcmp("gen_class", a_arg) == 0) action = REGAB_ACTION_GENERATE_CLASSIFICATION;
    else if (strcmp("classify", a_arg) == 0) action = REGAB_ACTION_CLASSIFY;
    else {
      fprintf(stderr, "Argument for option -a, --action must be in [generate|solve|gen_class|classify] domain.\n");
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
  if (b_flag) {
    char *endptr;
    errno = 0;    /* To distinguish success/failure after call */
    batch_id = strtoull(b_arg, &endptr, 10);
    if ((errno == ERANGE && (batch_id == LONG_MAX || batch_id == LONG_MIN))
        || (errno != 0 && batch_id == 0)) {
      perror("strtol");
      return EXIT_FAILURE;
    }
    if (endptr == b_arg) {
      fprintf(stderr, "No digits were found in batch_id value.\n");
      return EXIT_FAILURE;
    }
    if (*endptr != '\0') {
      fprintf(stderr, "Further characters after number in batch_id value: %s\n", endptr);
      return EXIT_FAILURE;
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
  if (p_flag) {
    if (!board_pattern_get_id_by_name(&bpi, p_arg)) {
      fprintf(stderr, "Pattern %s not found.\n", p_arg);
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
    }
    if (!y_flag) {
      fprintf(stderr, "Option -y, --empty_count, is mandatory when option -a, --action, has value \"solve\".\n");
      return EXIT_FAILURE;
    }
  }

  /* Verifies mandatory options when action is gen_class. */
  if (action == REGAB_ACTION_GENERATE_CLASSIFICATION) {
    if (!b_flag) {
      fprintf(stderr, "Option -b, --batch-id, is mandatory when option -a, --action, has value \"gen_class\".\n");
      return EXIT_FAILURE;
    }
    if (!p_flag) {
      fprintf(stderr, "Option -p, --pattern, is mandatory when option -a, --action, has value \"gen_class\".\n");
      return EXIT_FAILURE;
    }
    if (!y_flag) {
      fprintf(stderr, "Option -y, --empty_count, is mandatory when option -a, --action, has value \"gen_class\".\n");
      return EXIT_FAILURE;
    }
  }

  /* Verifies mandatory options when action is classify. */
  if (action == REGAB_ACTION_CLASSIFY) {
    if (!p_flag) {
      fprintf(stderr, "Option -p, --pattern, is mandatory when option -a, --action, has value \"classify\".\n");
      return EXIT_FAILURE;
    }
    if (!b_flag) {
      fprintf(stderr, "Option -b, --batch-id, is mandatory when option -a, --action, has value \"classify\".\n");
      return EXIT_FAILURE;
    }
    if (!y_flag) {
      fprintf(stderr, "Option -y, --empty-count, is mandatory when option -a, --action, has value \"classify\".\n");
      return EXIT_FAILURE;
    }
    if (!n_flag) {
      fprintf(stderr, "Option -n, --n-games, is mandatory when option -a, --action, has value \"classify\".\n");
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
   * Selects with action to perform.
   */
  switch (action) {
  case REGAB_ACTION_GENERATE: goto regab_action_generate;
  case REGAB_ACTION_SOLVE: goto regab_action_solve;
  case REGAB_ACTION_GENERATE_CLASSIFICATION: goto regab_action_generate_classification;
  case REGAB_ACTION_CLASSIFY: goto regab_action_classify;
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
   * Generates the classification header and the record.
   */
 regab_action_generate_classification:
  result = 0;
  do_insert_regab_prng_gp_classification_h(&result, con, bpi, batch_id, empty_count);
  goto regab_program_end;



  /*
   * Classifies the pattern classification records by computing the proper index.
   */
 regab_action_classify:
  ;
  int class_id = -1;
  result = 0;
  class_id = do_select_class_id(&result, con, bpi, batch_id, empty_count);
  if (result == -1) {
    fprintf(stderr, "Error while selecting the class_id in table regab_prng_classification_h.\n");
    PQfinish(con);
    return EXIT_FAILURE;
  }
  if (class_id < 0) {
    fprintf(stderr, "No value found while selecting the class_id in table regab_prng_classification_h.\n");
    goto regab_program_end;
  }

#define REGAB_CLASSIFY_BUFFER_SIZE 1024
#define REGAB_MAX_IDC_INSTANCES 8

  const size_t i_batch_size = (n_games > REGAB_CLASSIFY_BUFFER_SIZE) ? REGAB_CLASSIFY_BUFFER_SIZE : n_games;
  for (int icomputed = 0; icomputed < n_games; icomputed += i_batch_size) {
    regab_prng_gp_classification_record_t gpc_records[REGAB_CLASSIFY_BUFFER_SIZE];
    regab_prng_gp_record_t gp_records[REGAB_CLASSIFY_BUFFER_SIZE];
    board_pattern_index_t indexes[REGAB_MAX_IDC_INSTANCES];

    size_t ngpc_records = 0;
    size_t ngp_records = 0;
    int64_t gp_id = -1;

    do_select_index_to_compute(&result, con, i_batch_size, class_id, gpc_records);
    if (result < 0) {
      fprintf(stderr, "Error while selecting for update a record in the classification table, result=%d.\n", result);
      PQfinish(con);
      return EXIT_FAILURE;
    } else if (result == 0) {
      printf("No more indexes to compute.\n");
      break;
    }
    ngpc_records = result;
    for (int i = 0; i < ngpc_records; i++) {
      if (gpc_records[i].gp_id != gp_id) {
        gp_id = gpc_records[i].gp_id;
        gp_records[ngp_records].seq = gp_id;
        ngp_records++;
      }
    }
    /*
    printf("ngpc_records=%zu, ngp_records=%zu\n", ngpc_records, ngp_records);
    for (int i = 0; i < ngpc_records; i++) {
      printf("seq=%"PRId64", gp_id=%"PRId64", class_id=%d, pattern_instance=%d\n",
             gpc_records[i].seq, gpc_records[i].gp_id, gpc_records[i].class_id, gpc_records[i].pattern_instance);
    }
    for (int i = 0; i < ngp_records; i++) {
      printf("gp_records[%d].seq=%"PRId64"\n", i, gp_records[i].seq);
    }
    */

    do_select_position(&result, con, ngp_records, gp_records);
    if (result < 0) {
      fprintf(stderr, "Error while selecting for update a record in the classification table, result=%d.\n", result);
      PQfinish(con);
      return EXIT_FAILURE;
    } else if (result != ngp_records) {
      printf("Error: game_position records are missing.\n");
      PQfinish(con);
      return EXIT_FAILURE;
    }

    for (int i = 0; i < ngp_records; i++) {
      //printf("gp_records[%d].seq=%"PRId64"\n", i, gp_records[i].seq);

      board_t board;
      board.square_sets[0] = (SquareSet) gp_records[i].mover;
      board.square_sets[1] = (SquareSet) gp_records[i].opponent;
      board_pattern_compute_indexes(indexes, &board_patterns[bpi], &board);

      /*
      char gpx_to_s[1024];
      GamePositionX gpx;
      gpx.blacks = (SquareSet) gp_records[i].mover;
      gpx.whites = (SquareSet) gp_records[i].opponent;
      gpx.player = 0;
      game_position_x_print(gpx_to_s, &gpx);
      printf("\n%s\n", gpx_to_s);

      printf("indexes: %u, %u, %u, %u\n",
             indexes[0],
             indexes[1],
             indexes[2],
             indexes[3]);
      */

      /* This is not efficient at all!!! */
      for (int j = 0; j < ngpc_records; j++) {
        if (gpc_records[j].gp_id == gp_records[i].seq) {
          gpc_records[j].index_value = indexes[gpc_records[j].pattern_instance];
        }
      }
    }

    do_update_classification_index(&result, con, ngpc_records, gpc_records);
    if (result < 0) {
      fprintf(stderr, "Error while storing the indexes into the classification table, result=%d.\n", result);
      PQfinish(con);
      return EXIT_FAILURE;
    }
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

  return 0;
}
