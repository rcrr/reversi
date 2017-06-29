/**
 * @file
 *
 * @todo Documentation is not complete.
 *
 * @brief Verify a game position database.
 * @details This executable reads a game position db and logs errors.
 *
 * @par gpdb_verify.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014, 2017 Roberto Corradini. All rights reserved.
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
#include <assert.h>
#include <stdio.h>
#include <string.h>

#include <glib.h>

#include "game_position_db2.h"
#include "main_option_parse.h"


/**
 * @cond
 */

/* Static constants. */

static const mop_options_long_t olist[] = {
  {"help",              'h', MOP_NONE},
  {"file",              'f', MOP_REQUIRED},
  {"print-summary",     'p', MOP_NONE},
  {"log-entries",       'l', MOP_NONE},
  {"log-errors",        'e', MOP_NONE},
  {"lookup-entry",      'q', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "  gpdb_verify [OPTION...] - Verify a database of game positions\n"
  "\n"
  "Options:\n"
  "  -h, --help              Show help options\n"
  "  -f, --file              Input file name   - Mandatory\n"
  "  -p, --print-summary     Print summary     - Reports entry count, errors, and file description\n"
  "  -l, --log-entries       Log entries       - Logs all entries\n"
  "  -e, --log-errors        Log errors        - Logs syntax errors\n"
  "  -q, --lookup-entry      Lookup entry      - Displays an entry when it is found\n"
  "\n"
  "Description:\n"
  "  Reads, verifies the proper format of, and quaries game position database files.\n"
  "\n"
  "Author:\n"
  "   Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2013, 2014, 2017 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int f_flag = false;
static char *f_arg = NULL;

static int p_flag = false;
static int l_flag = false;
static int e_flag = false;

static int q_flag = false;
static char *q_arg = NULL;



/* Static functions. */

static bool
file_exists (const char *const file_name)
{
  FILE *f;
  if ((f = fopen(file_name, "r"))) {
    fclose(f);
    return true;
  }
  return false;
}

/**
 * @endcond
 */



/**
 * @brief Verifies a database file of game positions.
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
    case 'f':
      f_flag = true;
      f_arg = options.optarg;
      break;
    case 'p':
      p_flag = true;
      break;
    case 'l':
      l_flag = true;
      break;
    case 'e':
      e_flag = true;
      break;
    case 'q':
      q_flag = true;
      q_arg = options.optarg;
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

  /* Checks command line options for consistency. */
  if (!f_flag) {
    fprintf(stderr, "Option -f, --file is mandatory.\n");
    return -3;
  } else {
    if (!file_exists(f_arg)) {
      fprintf(stderr, "Argument for option -f: file %s does not exist.\n", f_arg);
    return -4;
    }
  }

  fprintf(stdout, "GPDB2: hello user!\n");

  gpdb2_dictionary_t *db = gpdb2_dictionary_new(f_arg);

  /*
  fprintf(stdout, "Game position dictionary, description: %s\n",gpdb2_dictionary_get_description(db));
  gpdb2_dictionary_set_description(db, "changed!");
  fprintf(stdout, "Game position dictionary, description: %s\n",gpdb2_dictionary_get_description(db));

  gpdb2_entry_t *e, *r, *a, *b, *c;
  size_t count;

  GamePositionX gpx1 = {0, 0, BLACK_PLAYER};
  GamePositionX gpx2 = {0, 1, BLACK_PLAYER};
  GamePositionX gpx3 = {0, 3, BLACK_PLAYER};
  e = gpdb2_entry_new("one", "The first entry", &gpx1);
  a = gpdb2_entry_new("one", "A duplicate, but distinct", &gpx1);
  b = gpdb2_entry_new("two", "A second entry", &gpx2);
  c = gpdb2_entry_new("three", "A third entry", &gpx3);

  r = gpdb2_dictionary_add_or_replace_entry(db, e);
  assert(!r);

  r = gpdb2_dictionary_add_or_replace_entry(db, e);
  assert(!r);

  r = gpdb2_dictionary_add_or_replace_entry(db, a);
  assert(r == e);

  r = gpdb2_dictionary_add_or_replace_entry(db, b);
  assert(!r);

  count = gpdb2_dictionary_entry_count(db);
  fprintf(stdout, "count=%zu\n", count);

  r = gpdb2_dictionary_entry_find (db, "four");
  assert(!r);

  r = gpdb2_dictionary_entry_find (db, "one");
  assert(r == a);

  r = gpdb2_dictionary_delete_entry(db, c);
  assert(!r);

  count = gpdb2_dictionary_entry_count(db);
  fprintf(stdout, "count=%zu\n", count);

  r = gpdb2_dictionary_delete_entry(db, e);
  assert(r);

  count = gpdb2_dictionary_entry_count(db);
  fprintf(stdout, "count=%zu\n", count);

  */


  gpdb2_syntax_err_log_t *elog = gpdb2_syntax_err_log_new();
  assert(elog);

  const bool duplicates_are_errors = true;
  const bool replace_duplicates = false;
  const bool stop_on_error = false;

  size_t insertions =  gpdb2_dictionary_load(db,
                                             f_arg,
                                             duplicates_are_errors,
                                             replace_duplicates,
                                             stop_on_error,
                                             elog);

  fprintf(stdout, "insertions=%zu\n", insertions);
  fprintf(stdout, "gpdb2_syntax_err_log_length(elog)=%zu\n", gpdb2_syntax_err_log_length(elog));

  gpdb2_syntax_err_log_print(elog, stdout);

  gpdb2_dictionary_print(db, stdout, true);
  gpdb2_dictionary_print(db, stdout, false);

  gpdb2_dictionary_free(db);
  gpdb2_syntax_err_log_free(elog);

  fprintf(stdout, "GPDB2: good bye user!\n");

  return 0;
}
