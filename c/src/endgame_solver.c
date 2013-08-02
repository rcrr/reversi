/**
 * @file
 *
 * @brief Endgame Solver.
 * @details This executable analyzes an end game position and
 * computes the exact outcome.
 *
 * @par endgame_solver.c
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
#include "board.h"
#include "game_position_db.h"
#include "exact_solver.h"



/* Helper function prototypes. */




/* Static variables. */

static gchar    *input_file    = NULL;
static gchar    *lookup_entry  = NULL;

static GOptionEntry entries[] =
  {
    { "file",          'f', 0, G_OPTION_ARG_FILENAME, &input_file,    "Input file name", NULL }, 
    { "lookup-entry",  'q', 0, G_OPTION_ARG_STRING,   &lookup_entry,  "Lookup entry",    NULL },
    { NULL }
  };



/**
 * Main entry to the Reversi C implementation.
 * 
 * Has to be completly developed.
 */
int
main (int argc, char *argv[])
{
  GamePositionDb               *db;
  GamePositionDbSyntaxErrorLog *syntax_error_log;
  FILE                         *fp;
  GError                       *error;
  gchar                        *source;
  int                           number_of_errors;

  GOptionContext *context;
  GOptionGroup   *option_group;

  GamePositionDbEntry *entry;

  error = NULL;
  entry = NULL;

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
    g_print("Unable to open database resource for reading, file \"%s\" does not exist.\n", source);
    return -3;
  }

  /* Loads the game position database. */
  db = gpdb_new(g_strdup(source));
  syntax_error_log = NULL;
  error = NULL;
  gpdb_load(fp, source, db, &syntax_error_log, &error);
  fclose(fp);

  /* Compute the number of errors logged. */
  number_of_errors = gpdb_syntax_error_log_length(syntax_error_log);
  if (number_of_errors != 0) {
    g_print("The database resource, file \"%s\" contains errors, debug it using the gpdb_verify utility.\n", source);
    return -4;
  }

  /* Lookup for a given key. */
  if (lookup_entry) {
    entry = gpdb_lookup(db, lookup_entry);
    if (entry) {
      gchar *tmp = gpdb_entry_print(entry);
      g_print("%s", tmp);
      g_free(tmp);
    } else {
      g_print("Entry %s not found in file %s.\n", lookup_entry, source);
      return -6;      
    }
  } else {
    g_print("No entry provided.\n");
    return -6;
  }

  /* Initialize the board module. */
  board_module_init();

  g_print("Solving the game position %s ...\n", entry->id);

  GamePosition *gp = entry->game_position;

  ExactSolution *solution = game_position_solve(gp);

  gchar *solution_to_string = exact_solution_print(solution);
  printf("\n%s\n", solution_to_string);
  g_free(solution_to_string);

  printf("\nsolution outcome: %d\n", solution->outcome);


  /* Frees the resources. */
  g_free(error);
  gpdb_free(db, TRUE);
  if (syntax_error_log)
    gpdb_syntax_error_log_free(syntax_error_log);
  g_option_context_free(context);
  g_free(source);
  exact_solution_free(solution);

  return 0;
}



/*
 * Internal functions.
 */

