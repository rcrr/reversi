/**
 * @file
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
 * @copyright 2013 Roberto Corradini. All rights reserved.
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

static gchar    *input_file  = NULL;
static gboolean  log_entries = FALSE;
static gboolean  log_errors  = FALSE;

static GOptionEntry entries[] =
  {
    { "file",        'f', 0, G_OPTION_ARG_FILENAME, &input_file,  "Input file name", NULL }, 
    { "log-entries", 'l', 0, G_OPTION_ARG_NONE,     &log_entries, "Log entries",     NULL },
    { "log-errors",  'e', 0, G_OPTION_ARG_NONE,     &log_errors,  "Log errors",      NULL },
    { NULL }
  };

/**
 * Verifies a database file of game positions.
 */
int
main (int argc, char *argv[])
{
  GamePositionDb               *db;
  GamePositionDbSyntaxErrorLog *syntax_error_log;
  FILE                         *fp;
  GError                       *error;
  GString                      *syntax_error_log_to_string;
  gchar                        *source;


  GOptionContext *context;
  GOptionGroup   *option_group;

  error = NULL;
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
  db = gpdb_new(g_strdup("Game Position Database under check."));
  syntax_error_log = NULL;
  error = NULL;
  gpdb_load(fp, source, db, &syntax_error_log, &error);
  g_free(source);
  fclose(fp);

  gchar *gpdb_to_string = gpdb_print(db);
  g_print("%s", gpdb_to_string);
  g_free(gpdb_to_string);

  GamePositionDbEntry *entry = gpdb_lookup(db, "early-game-c-12-moves");
  if (entry) {
    gchar *tmp = gpdb_entry_print(entry);
    g_print("%s", tmp);
    g_free(tmp);
  }

  syntax_error_log_to_string = gpdb_syntax_error_log_print(syntax_error_log);
  g_print("%s", syntax_error_log_to_string->str);
  g_string_free(syntax_error_log_to_string, TRUE);

  /* Removes the tmp file, frees the resources. */
  g_free(error);
  gpdb_free(db, TRUE);
  if (syntax_error_log)
    gpdb_syntax_error_log_free(syntax_error_log);

  return 0;
}
