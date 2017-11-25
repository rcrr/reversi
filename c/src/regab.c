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

#include <libpq-fe.h>

#include "main_option_parse.h"
#include "cfg.h"



/**
 * @cond
 */


/*
 * Static constants.
 */

static const mop_options_long_t olist[] = {
  {"help",              'h', MOP_NONE},
  {"config-file",       'c', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "  regab [OPTION...] - Reversi EndGame Analytics Base\n"
  "\n"
  "Options:\n"
  "  -h, --help        Show help options\n"
  "  -c, --config-file Config file name - Mandatory.\n"
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



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int c_flag = false;
static char *c_arg = NULL;

static char *config_file = NULL;



/*
 * Prototypes for internal functions.
 */



/**
 * @endcond
 */


/**
 * @brief Main entry to the Reversi EndGame Analytics Base.
 *
 * @todo Documentation has to be completly developed.
 */
int
main (int argc,
      char *argv[])
{
  int opt;
  int oindex = -1;

  mop_init(&options, argc, argv);
  while ((opt = mop_parse_long(&options, olist, &oindex)) != -1) {
    switch (opt) {
    case 'h':
      h_flag = true;
      break;
    case 'c':
      c_flag = true;
      c_arg = options.optarg;
      break;
    case ':':
      fprintf(stderr, "Option parsing failed: %s\n", options.errmsg);
      return -1;
    case '?':
      fprintf(stderr, "Option parsing failed: %s\n", options.errmsg);
      return -2;
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
    return -3;
  } else {
    config_file = c_arg;
  }

  /* Verifies that the config input file is available for reading. */
  FILE *fp = fopen(config_file, "r");
  if (!fp) {
    fprintf(stderr, "Unable to open config file resource for reading, file \"%s\" does not exist.\n", config_file);
    return -4;
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
    return -5;
  }
  if (strcmp("ok", config_check)) {
    fprintf(stderr, "\"%s\": wrong value for section=[regab], key=check. It has to be \"ok\".\n", config_check);
    return -5;
  }

  const char *con_info_host = "localhost";

  int program_return_code = 0;
  int nfields, i, j;

  PGconn *con;
  PGresult *res;

  const char *con_info = "host=localhost port=5432 dbname=dev_regab user=dev_regab password=dev_regab";
  con = PQconnectdb(con_info);
  if (PQstatus(con) != CONNECTION_OK) {
    fprintf(stderr, "Connection to database failed: %s", PQerrorMessage(con));
    program_return_code = -10;
    goto end;
  }

  /* Logs into the dabase log table. */
  res = PQexec(con, "INSERT INTO connection_log (un, ts) VALUES ('dev_regab', now())");
  PQclear(res);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "INSERT failed: %s", PQerrorMessage(con));
    program_return_code = -12;
    goto end;
  }

  /* Start a transaction. */
  res = PQexec(con, "BEGIN");
  PQclear(res);
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "BEGIN command failed: %s", PQerrorMessage(con));
    program_return_code = -12;
    goto end;
  }

  /* Prepare cursor. */
  res = PQexec(con, "DECLARE ld CURSOR FOR SELECT sub.* FROM (SELECT * FROM connection_log ORDER BY lid DESC LIMIT 5) sub ORDER BY lid ASC");
  if (PQresultStatus(res) != PGRES_COMMAND_OK) {
    fprintf(stderr, "DECLARE CURSOR failed: %s", PQerrorMessage(con));
    program_return_code = -12;
    goto end;
  }
  PQclear(res);

  res = PQexec(con, "FETCH ALL IN ld");
  if (PQresultStatus(res) != PGRES_TUPLES_OK) {
    fprintf(stderr, "FETCH ALL failed: %s", PQerrorMessage(con));
    program_return_code = -12;
    goto end;
  }

  /* First, prints out the table collumn attribute names. */
  nfields = PQnfields(res);
  for (i = 0; i < nfields; i++)
    printf("%-30s", PQfname(res, i));
  printf("\n\n");

  /* Next, print out the rows of data. */
  for (i = 0; i < PQntuples(res); i++) {
    for (j = 0; j < nfields; j++)
      printf("%-30s", PQgetvalue(res, i, j));
    printf("\n");
  }

  PQclear(res);

  /* Closes the cursor ... no check for errors ... */
  res = PQexec(con, "CLOSE ld");
  PQclear(res);

  /* Close the transaction. */
  res = PQexec(con, "END");
  PQclear(res);



 end:

  /*
   * Closes the connection to the database and cleanup.
   */
  PQfinish(con);

  /*
   * Frees resources.
   */
  cfg_free(config);

  return program_return_code;
}
