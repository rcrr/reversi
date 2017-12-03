/**
 * @file
 *
 * @brief REGAB: Reversi End Game Analytics Base.
 *
 * @details This executable analyzes an end game position and
 *          computes the exact outcome.
 *
 * @par endgame_solver.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014, 2015, 2016, 2017 Roberto Corradini. All rights reserved.
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

#include <libpq-fe.h>

#include "main_option_parse.h"
#include "cfg.h"
#include "prng.h"
#include "board.h"



/**
 * @cond
 */

#define REGAB_PRNG_MAX_DEPTH 72

typedef struct regab_prng_node_s {
  GamePositionX gpx;
  uint8_t       empty_count;
  bool          is_leaf;
  uint8_t       legal_move_count;
  uint8_t       legal_move_count_adjusted;
  uint8_t       parent_move;
} regab_prng_node_t;

typedef struct regab_prng_stack_s {
  size_t             npos;
  regab_prng_node_t *active_node;
  regab_prng_node_t  nodes[REGAB_PRNG_MAX_DEPTH];
} regab_prng_stack_t;


/*
 * Static constants.
 */

static const mop_options_long_t olist[] = {
  {"help",              'h', MOP_NONE},
  {"verbose",           'v', MOP_NONE},
  {"config-file",       'c', MOP_REQUIRED},
  {"env",               'e', MOP_REQUIRED},
  {"prng-seed",         's', MOP_REQUIRED},
  {"n-games",           'n', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "  regab [OPTION...] - Reversi EndGame Analytics Base\n"
  "\n"
  "Options:\n"
  "  -h, --help        Show help options\n"
  "  -v, --verbose     Verbose output\n"
  "  -c, --config-file Config file name                                 - Mandatory.\n"
  "  -e, --env         Environment                                      - Mandatory.\n"
  "  -s, --prng-seed   Seed used by the Pseudo Random Number Generator  - Default is 0.\n"
  "  -n, --n-games     Number of random game generated                  - Default is one.\n"
  "\n"
  "Description:\n"
  "  To be completed ... .\n"
  "\n"
  "Author:\n"
  "  Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2017 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;

static const char *insert_regab_prng_gp_h = "insert-regab-prng-gp-h";
static const char *insert_regab_prng_gp = "insert-regab-prng-gp";



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int v_flag = false;

static int c_flag = false;
static char *c_arg = NULL;

static int e_flag = false;
static char *e_arg = NULL;

static int s_flag = false;
static char *s_arg = NULL;

static int n_flag = false;
static char *n_arg = NULL;

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
                  "INSERT INTO regab_prng_gp (run_id, sub_run_id, call_id, ins_time, status, cst_time) "
                  "VALUES ($1, $2, $3, $4, $5, $6);",
                  6,
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
do_insert_regab_prng_gp (int *result,
                         PGconn *con,
                         uint64_t run_id,
                         uint64_t sub_run_id,
                         uint64_t call_id,
                         char *status)
{
  PGresult *res = NULL;

  const char* paramValues[6];
  char run_id_to_s[64];
  char sub_run_id_to_s[64];
  char call_id_to_s[64];

  if (!result) return;
  if (!status) return;
  if (!con || (strlen(status) > 3)) {
    *result = -1;
    return;
  }

  sprintf(run_id_to_s, "%zu", run_id);
  sprintf(sub_run_id_to_s, "%zu", sub_run_id);
  sprintf(call_id_to_s, "%zu", call_id);

  paramValues[0] = run_id_to_s;
  paramValues[1] = sub_run_id_to_s;
  paramValues[2] = call_id_to_s;
  paramValues[3] = "now()";
  paramValues[4] = status;
  paramValues[5] = "now()";

  res = PQexecPrepared(con,
                       insert_regab_prng_gp,
                       6,
                       paramValues,
                       NULL,
                       NULL,
                       0);

  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "%s", PQerrorMessage(con));
    *result = -1;
  } else {
    *result = 0;
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


/**
 * @brief Main entry to the Reversi EndGame Analytics Base.
 *
 * @todo Documentation has to be completly developed.
 */
int
main (int argc,
      char *argv[])
{
  regab_prng_stack_t gstack;

  int opt;
  int oindex = -1;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, olist, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
      break;
    case 'v':
      v_flag = true;
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

  /* Verifies that the config input file is available for reading. */
  FILE *fp = fopen(config_file, "r");
  if (!fp) {
    fprintf(stderr, "Unable to open config file resource for reading, file \"%s\" does not exist.\n", config_file);
    return EXIT_FAILURE;
  }
  fclose(fp);

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




  /* ---- ---- ----- */

  /*
   * Load the prng game.
   */

  uint64_t run_id;
  do_insert_regab_prng_gp_h (&result, &run_id, con, "INS", prng_seed, n_games);
  if (result == -1) {
    fprintf(stderr, "Error while inserting regab_prng_gp_h record.\n");
    PQfinish(con);
    return EXIT_FAILURE;
  }

  prng_mt19937_t *prng = prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, prng_seed);

  for (int igame = 0; igame < n_games; igame++) {
    printf("igame=%d\n", igame);
    gstack.npos = 0;
    gstack.active_node = gstack.nodes;
    game_position_x_set_initial_position(&gstack.active_node->gpx);

    bool has_passed = false;

    while (gstack.npos < REGAB_PRNG_MAX_DEPTH) {
      GamePositionX *curr_gpx = &gstack.active_node->gpx;
      GamePositionX *next_gpx = &(gstack.active_node + 1)->gpx;

      char tmp[256];
      game_position_x_print(tmp, curr_gpx);
      printf("\n\n%s\n", tmp);
      printf("gstack.npos=%zu\n", gstack.npos);

      SquareSet move_set = game_position_x_legal_moves(curr_gpx);

      if (move_set) {
        has_passed = false;
        Square move = prng_mt19937_extract_from_set64(prng, &move_set);
        game_position_x_make_move(curr_gpx, move, next_gpx);
        ;
        ;
      } else {
        if (has_passed) goto game_completed;
        has_passed = true;
        game_position_x_pass(curr_gpx, next_gpx);
      }
      gstack.npos++;
      gstack.active_node++;
    }

    /* If the program reaches this point the stack has overflown. */
    fprintf(stderr, "Game stack has oferflown.\n");
    abort();

  game_completed:
    gstack.npos--; // Last pass is removed.

    printf("gstack.npos=%zu\n", gstack.npos);

    /* Load game into the database. */
    for (int ipos = 0; ipos < gstack.npos; ipos++) {
      do_insert_regab_prng_gp(&result, con, run_id, igame, ipos, "INS");
    }

  }


  /*
   * Closes the pseudo random number generator.
   */
  prng_mt19937_free(prng);

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
