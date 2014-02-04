/**
 * @file
 *
 *
 * @todo [done] Complete the random game player.
 *
 * @todo Add to the solver selection a full MINIMAX implementation.
 *
 * @todo Add to the solver selection an alpha-beta implementation that
 * sort the legal move list with a random function.
 *
 * @todo Add to the exact_solver logging strategy a second file that logs
 * when the function returns. There we can add the return value and the
 * number of moves serched against the number of legal ones. This can give
 * back a kpi of the alpha-beta efficiency in cutting unrelevant branches.
 *
 * @todo Refine and refactor the exact_solver implementation.
 *
 * @todo Write a test suite that solves a selection of the FFO test cases.
 *
 * @todo Profile exact_solver against improved_fast_endgame_solver.
 *
 * @todo Analyze the parity feature in improved_fast_endgame.
 *
 * @todo Try to avoid having malloc/free calls during game tree expansion.
 *
 * @todo Organize the SQL scripts into a modular practice.
 *
 * @todo Develop a full description of the evidence of all the variants.
 *
 * @todo Introduce the node cache by means of a shared hashtable. 
 *
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

#include "game_position_db.h"
#include "exact_solver.h"
#include "improved_fast_endgame_solver.h"
#include "random_game_sampler.h"
#include "minimax_solver.h"



/* Static constants. */

static const gchar *solvers[] = {"es", "ifes", "rand", "minimax"};
static const int n_solver = sizeof(solvers) / sizeof(solvers[0]);




/* Static variables. */

static gchar *input_file   = NULL;
static gchar *lookup_entry = NULL;
static gchar *solver       = NULL;
static gint   repeats      = 1;

static GOptionEntry entries[] =
  {
    { "file",          'f', 0, G_OPTION_ARG_FILENAME, &input_file,   "Input file name   - Mandatory",                                     NULL },
    { "lookup-entry",  'q', 0, G_OPTION_ARG_STRING,   &lookup_entry, "Lookup entry      - Mandatory",                                     NULL },
    { "solver",        's', 0, G_OPTION_ARG_STRING,   &solver,       "Solver            - Mandatory - Must be in [es|ifes|rand|minimax]", NULL },
    { "repeats",       'n', 0, G_OPTION_ARG_INT,      &repeats,      "N. of repetitions - Used with the rand solver",                     NULL },
    { NULL }
  };



/**
 * @brief Main entry to the Reversi C endgame solver implementation.111
 * 
 * @todo Documentation has to be completly developed.
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
  int                  solver_index;

  error = NULL;
  entry = NULL;
  solver_index = -1;

  /* GLib command line options and argument parsing. */
  option_group = g_option_group_new("name", "description", "help_description", NULL, NULL);
  context = g_option_context_new("- Solve an endgame position");
  g_option_context_add_main_entries(context, entries, NULL);
  g_option_context_add_group(context, option_group);
  g_option_context_set_description(context, "The rand solver is a way to play a sequence of random game from the given position.\n");
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
  if (solver) {
    for (int index = 0; index < n_solver; index++) {
      if (g_strcmp0(solver, solvers[index]) == 0)
        solver_index = index;
    }
    if (solver_index == -1) {
      g_print("Option -s, --solver is out of range.\n.");
      return -8;
    }
    if (solver_index == 2) { // solver == random
      if (repeats < 1) {
        g_print("Option -n, --repeats is out of range.\n.");
        return -9;
      }
    }
  } else {
    g_print("Option -s, --solver is mandatory.\n.");
    return -5;
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
    return -7;
  }

  /* Initialize the board module. */
  board_module_init();

  /* Solving the position. */
  GamePosition *gp = entry->game_position;
  ExactSolution *solution = NULL;
  g_print("Solving the game position %s using solver %s ...\n", entry->id, solvers[solver_index]);
  switch (solver_index) {
  case 0:
    solution = game_position_solve(gp);
    break;
  case 1:
    solution = game_position_ifes_solve(gp);
    break;
  case 2:
    solution = game_position_random_sampler(gp, repeats);
    break;
  case 3:
    solution = game_position_minimax_solve(gp);
    break;
  default:
    g_print("This should never happen! solver_index = %d. Aborting ...\n", solver_index);
    return -9;
  }

  /* Printing results. */
  gchar *solution_to_string = exact_solution_print(solution);
  printf("\n%s\n", solution_to_string);
  g_free(solution_to_string);

  printf("[node_count=%llu, leaf_count=%llu]\n",
         solution->node_count, 
         solution->leaf_count);
  gchar *move_to_s = square_to_string(solution->principal_variation[0]);
  printf("Final SearchNode sn: move=%s, value=%d\n", move_to_s, solution->outcome);
  g_free(move_to_s);

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
