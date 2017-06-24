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
 * @copyright 2013, 2014 Roberto Corradini. All rights reserved.
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

#include <glib.h>

#include "game_position_db.h"


/**
 * @cond
 */

static gchar    *input_file    = NULL;
static gboolean  print_summary = FALSE;
static gboolean  log_entries   = FALSE;
static gboolean  log_errors    = FALSE;
static gchar    *lookup_entry  = NULL;

static GOptionEntry entries[] =
  {
    { "file",          'f', 0, G_OPTION_ARG_FILENAME, &input_file,    "Input file name", NULL },
    { "print-summary", 'p', 0, G_OPTION_ARG_NONE,     &print_summary, "Print summary",   NULL },
    { "log-entries",   'l', 0, G_OPTION_ARG_NONE,     &log_entries,   "Log entries",     NULL },
    { "log-errors",    'e', 0, G_OPTION_ARG_NONE,     &log_errors,    "Log errors",      NULL },
    { "lookup-entry",  'q', 0, G_OPTION_ARG_STRING,   &lookup_entry,  "Lookup entry",    NULL },
    { NULL }
  };

/**
 * @endcond
 */



/**
 * @brief Verifies a database file of game positions.
 */
int
main (int argc, char *argv[])
{
  GamePositionDb *db;
  gpdb_syntax_error_log_t *syntax_error_log;
  FILE *fp;
  GError *error;
  gchar *source;
  int number_of_errors;

  GOptionContext *context;
  GOptionGroup *option_group;

  error = NULL;

  /* GLib command line options and argument parsing. */
  option_group = g_option_group_new("name", "description", "help_description", NULL, NULL);
  context = g_option_context_new ("- Verify a database of game positions");
  g_option_context_add_main_entries (context, entries, NULL);
  g_option_context_add_group (context, option_group);
  if (!g_option_context_parse (context, &argc, &argv, &error)) {
    g_print("Option parsing failed: %s\n", error->message);
    return -1;
  }

  /* Checks command line options for consistency. */
  if (input_file) {
    source = g_strdup(input_file);
  } else {
    g_print("Option -f, --file is mandatory.\n.");
    return -2;
  }

  /* Opens the source file for reading. */
  fp = fopen(source, "r");
  if (!fp) {
    g_print("Unable to open database resource for reading, file \"%s\" does not exist.\n.", source);
    return -3;
  }

  /* Loads the game position database. */
  db = gpdb_new(g_strdup(source));
  syntax_error_log = NULL;
  error = NULL;
  gpdb_load(fp, source, db, &syntax_error_log, &error);
  g_free(source);
  fclose(fp);

  /* Compute the number of errors logged. */
  number_of_errors = gpdb_syntax_error_log_length(syntax_error_log);

  /* Prints the database summary if the OPTION -p is turned on. */
  if (print_summary) {
    gchar *summary = gpdb_print_summary(db);
    g_print("%s", summary);
    g_free(summary);
    g_print("Number of errors: %d\n", number_of_errors);
  }

  /* Prints the error log if the OPTION -e is turned on. */
  if (log_errors) {
    gchar *syntax_error_log_to_string = gpdb_syntax_error_log_print(syntax_error_log);
    g_print("%s", syntax_error_log_to_string);
    g_free(syntax_error_log_to_string);
  }

  /* Prints the entry list if the OPTION -l is turned on. */
  if (log_entries) {
    gchar *gpdb_to_string = gpdb_print(db);
    g_print("%s", gpdb_to_string);
    g_free(gpdb_to_string);
  }

  /* Lookup for a given key. */
  if (lookup_entry) {
    GamePositionDbEntry *entry = gpdb_lookup(db, lookup_entry);
    if (entry) {
      gchar *tmp = gpdb_entry_print(entry);
      g_print("%s", tmp);
      g_free(tmp);
    }
  }

  /* Frees the resources. */
  g_free(error);
  gpdb_free(db, TRUE);
  if (syntax_error_log)
    gpdb_syntax_error_log_free(syntax_error_log);

  g_option_context_free(context);

  return 0;
}
