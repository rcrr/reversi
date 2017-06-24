/**
 * @file
 *
 * @todo Unit tests are far behind, a complete check is needed to ensure all functions have the appropriate testing.
 *
 * @todo Dependency from glib shall be removed from sources:
 *       - game_tree_logger has file and directory tools based on glib
 *       - all the main programs have args managed by mean of glib
 *       - game_position_db module is heavily based on glib
 *
 * @todo Dependency from glib shall be removed from unit tests. This requires to complete the utest module.
 *
 * @todo Checks all the programs for memory leaks.
 *
 * @todo Remove GamePositionX and add board_t and game_position_t, where game_position_t has a bitboard for player and one for the opponent.
 *       board_t has also the player moving next.
 *
 * @todo Add a way to check a PV for variants. - Priority 1 -
 *       The PV code is quite OK. What is missing now are a few improvements:
 *       - [done] I mean, add moves to the structure when the value is equal to the one of the move recorded in the principal tree.
 *       - [done] PVCell should have a variant pointer to another cell. Alpha-Beta has to populate it when moves have equal values.
 *       - [done] The pve_verify_consistency function has to be rewritten.
 *       - [done] The pve_internals_to_stream function has to be completed.
 *       - [done] Add an argument to the function pve_internals_to_stream. It should be a bit-mask that turn on-off the different blocks of the function.
 *       - [done] The pve_index_lines function has to be run on the real stack.
 *       - [done] Cells has to be converted as lines to segment management.
 *       - [done] The size of the cell stack has to be increased, line_delete has to take care of variants.
 *       - [done] A couple of flags --pve_variants and --pve_variants_full should be added to drive the exact_solution_to_string function.
 *         [done] A command line option asking to run the full analysis.
 *       - [done] The proper memory management for the pve structure.
 *       - [done] PV is a tree structure composed by cells and lines. Could be, I strongly believe so, that there is quite a bit of duplication:
 *         [done] Investigate the statistics of this concept.
 *         [done] If the analysis proves it to be worthwhile, improve the pve structure in order to exploit the duplication.
 *       - [done] Prepare a new utility for PV dump/load to/from a binary file.
 *         [done] Write the root_game_position and read it. Add a paragraph for printing it.
 *         [done] Write a function for to-from-map creation. Purge printf statements ....
 *         [done] Add a flag for dumping the file at the end of the analysis..... Better adding an endgame_env having am open list of key-value pairs ....
 *         [done] Test the new utility and run it for all the ffo game positions.
 *         [done] Develop a dedicated output for SQL COPY function. Verify the "pve duplication" hypothesis!
 *       - *** - *** -> PVE has to be fully reorganized (a major refactoring) into a module.
 *                      A new module, block memory allocator, or bma, is required.
 *                      PVE is composed by lines, cells, and a cell dictionary, the dictionary is implemented by a binary search tree.
 *                      A new module, binary red-black tree, or rbt, is required. The first solution in my evaluation list is Ben Pfaff's avl library.
 *                      Cells and lines leverage the bma module.
 *                      Bma module allocates constant size blocks (no more exponetial size increases), and has a more space-efficient stack.
 *                      The rbt based dictionary prevents game position duplications, transforming the tree in a DAG, reducing the pve required size.
 *                      Moving to a DAG requires to keep track of ref count for cell.
 *                      A pack function is required in order to achieve the stack space reduction.
 *       - More optional checks could be added to pve_is_invariant_satisfied.
 *       - Add game values computation and output for first level moves.
 *       - [done] Add a function to compute PV hash code (it is useful for testing and database processing).
 *       - [done] The ExactSolution object should be replaced by a PVE reference.
 *
 *
 * @todo [done] Complete the random game player.
 *
 * @todo [done] Add to the solver selection a full MINIMAX implementation.
 *
 * @todo [done] Add to the solver selection an alpha-beta implementation that
 *              sort the legal move list with a random function.
 *
 * @todo Add to the exact_solver logging strategy a second file that logs
 *       when the function returns. There we can add the return value and the
 *       number of moves serched against the number of legal ones. This can give
 *       back a kpi of the alpha-beta efficiency in cutting unrelevant branches.
 *
 * @todo [done] The solvers do not return a PV, but just the best move and its value.
 *              Now a new sub-module, pve in game_tree_utils, has al the functions that are needed.
 *              It is used by the exact_solver, the other has to be updated if usefull.
 *
 * @todo The output of the solvers is not always appropriate:
 *         - [done] Final board is not reported
 *         - The value of all the first level moves is not recorded
 *         - For random game sampler the output is meaningless
 *
 * @todo [done] Refine and refactor the exact_solver implementation.
 *
 * @todo [done] Write SIMD (AVX2) versions of make_move and legal_moves functions.
 *
 * @todo [done] Solvers rab and ab share the same stack solution. Refactor it sharing
 *              the same utilities brougth to a dedicated module.
 *       [done] The es solver should do the same.
 *              2016-08-21: legal_moves and make_move functions have an AVX2 version.
 *                          Now malloc/free functions are consuming the largest slice of the es solver.
 *                          It is time to address this task.
 *
 * @todo [done] Port the stack practice to all the other solver, refactor the structures used (Stack, NodeInfo, GamePositionX).
 *
 * @todo [done] Write a test suite that solves a selection of the FFO test cases.
 *
 * @todo Profile exact_solver against improved_fast_endgame_solver.
 *
 * @todo Analyze the parity feature in improved_fast_endgame.
 *
 * @todo [done] Try to avoid having malloc/free calls during game tree expansion.
 *
 * @todo [done] Refactor the game-tree static stack implementation.
 *       [done] Avoid to put it in the stack (implement one call to malloc for the complete structure).
 *       [done] Pass the stack pointer in the parameter list.
 *       [done] Make the code as much readable as possible.
 *
 * @todo [done] Organize the SQL scripts into a modular practice.
 *       [done] This task is evolving into a complete porting of the board engine in PLPGSQL.
 *       [done] Missing features are: - game_position_pretty_print
 *       [done]                       - game_position_make_move
 *       [done]                       - a minimax solver
 *       [done]                       - an alpha-beta solver
 *       [done] Organize the logging activity for the solvers.
 *       [done] Organize the files used to read and analyze the C program outputs.
 *       [done]   - pg_variance.query.sql
 *       [done]   - pg_pivot_rand.sql
 *       [done]   - pg_load_rand.sql
 *       [done]   - pg_load_log.sql
 *       [done] Refactor logging into a module and a few function calls.
 *
 * @todo Develop an R replacement of the rand_log_elaboration.ods Calc file.
 *
 * @todo Develop a full description of the evidence of all the variants, basically a paper describing:
 *        - The statisctical data obtained by the random sampler.
 *        - The statistical data obtained by the minimax and rab solvers.
 *        - A kind of relation with the minimal tree (this has to be detailed more).
 *        - A description of the solvers.
 *        - A description of the PL/SQL reversi package.
 *
 * @todo [done] A new section of the Makefile that regenerates the log csv files used by the PL/SQL reload_everything.sql script.
 *
 * @todo Introduce the node cache by means of a shared hashtable.
 *
 * @todo [no longer relevant] Optimize (better saying improve) the bitrow_changes_for_player array .... the number of entries is ten times the required ones.
 *                            A complete rethinking of the index function is then needed.
 *                            The AVX2 implementation makes this improvement useless.
 *
 * @todo [no longer relevant] Verify if it is possible to optimize the definition of bitboard_mask_for_all_directions removing squares that do not flip (inner frame).
 *                            The AVX2 implementation makes this improvement useless.
 *
 * @todo [done] Remove the dependency from GSL GNU library by replacing it with Mersenne Twister 64bit version foud at http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt64.html
 *              The reason is to avoid the dependency, but more important to have a 64 bit generator (the gsl library has a 32 bit one).
 *
 * @todo Create a C extension to PostgreSQL that leverages the C game position functions.
 *
 * @todo Write a GUI using GTK+.
 *
 * @todo Write a wthor format reader. See: http://www.ffothello.org/wthor/Format_WThor.pdf and http://www.ffothello.org/informatique/la-base-wthor/
 *
 *
 *
 * @brief Endgame Solver.
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

#include "endgame_solver.h"
#include "game_position_db.h"
#include "improved_fast_endgame_solver.h"
#include "minimax_solver.h"
#include "exact_solver.h"



/**
 * @cond
 */

/**
 * @brief The endgame solver structure collects identity, description,
 *        and the function pointer resolving the solver.
 */
typedef struct {
  char             *id;              /**< @brief The solver id, it must be equal to the command line label. */
  char             *description;     /**< @brief The solver description. */
  char             *function_name;   /**< @brief The solver function name. */
  endgame_solver_f  fn;              /**< @brief The solver function pointer. */
} endgame_solver_t;



/*
 * Static constants.
 */

static const endgame_solver_t solvers[] =
  {
    { .id = "es",      .description = "exact solver",                 .function_name = "game_position_es_solve",       .fn = game_position_es_solve },
    { .id = "ifes",    .description = "improved fast endgame solver", .function_name = "game_position_ifes_solve",     .fn = game_position_ifes_solve },
    { .id = "rand",    .description = "random game sampler",          .function_name = "game_position_random_sampler", .fn = game_position_random_sampler },
    { .id = "minimax", .description = "minimax solver",               .function_name = "game_position_minimax_solve",  .fn = game_position_minimax_solve },
    { .id = "rab",     .description = "random alpha-beta solver",     .function_name = "game_position_rab_solve",      .fn = game_position_rab_solve },
    { .id = "ab",      .description = "alpha-beta solver",            .function_name = "game_position_ab_solve",       .fn = game_position_ab_solve }
  };

static const int solvers_count = sizeof(solvers) / sizeof(solvers[0]);

static const gchar *program_documentation_string =
  "Description:\n"
  "Endgame solver is the front end for a group of algorithms aimed to analyze the final part of the game and to asses the game tree structure.\n"
  "Available engines are: es (exact solver), ifes (improved fast endgame solver), rand (random game sampler), minimax (minimax solver),\n"
  "ab (alpha-beta solver), and rab (random alpha-beta solver).\n"
  "\n"
  " - es (exact solver)\n"
  "   My fully featured implementation of a Reversi Endgame Exact Solver. A sample call is:\n"
  "     $ endgame_solver -f db/gpdb-ffo.txt -q ffo-40 -s es\n"
  "\n"
  " - ifes (improved fast endgame solver)\n"
  "   A solver derived from the Gunnar Andersson work. A sample call is:\n"
  "     $ endgame_solver -f db/gpdb-ffo.txt -q ffo-40 -s ifes\n"
  "\n"
  " - rand (random game sampler)\n"
  "   The rand solver is a way to play a sequence of random game from the given position.\n"
  "   This option works together with the -n flag, that assigns the number of repeats, a sample call is:\n"
  "     $ endgame_solver -f db/gpdb-sample-games.txt -q initial -s rand -n 100 -l logfile\n"
  "\n"
  " - minimax (minimax solver)\n"
  "   It applies the plain vanilla minimax algorithm, a sample call is:\n"
  "     $ endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified -s minimax\n"
  "\n"
  " - ab (alpha-beta solver)\n"
  "   It applies the alpha-beta algorithm sorting legal moves by their natural order, a sample call is:\n"
  "     $ endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified -s ab\n"
  "\n"
  " - rab (random alpha-beta solver)\n"
  "   It uses the alpha-beta pruning, ordering the moves by mean of a random criteria, a sample call is:\n"
  "     $ endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified-4 -s rab -l out/log -n 3\n"
  "\n"
  "Author:\n"
  "   Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2013, 2014, 2015, 2016 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;



/* Static variables. */

static gchar   *input_file    = NULL;
static gchar   *lookup_entry  = NULL;
static gchar   *solver_id     = NULL;
static gint     repeats       = 1;
static gchar   *log_file      = NULL;
static gchar   *pve_dump_file = NULL;
static gboolean pv_rec        = FALSE;
static gboolean pv_full_rec   = FALSE;
static gboolean pv_no_print   = FALSE;

static const GOptionEntry entries[] =
  {
    { "file",          'f', 0, G_OPTION_ARG_FILENAME, &input_file,    "Input file name          - Mandatory",                                               NULL },
    { "lookup-entry",  'q', 0, G_OPTION_ARG_STRING,   &lookup_entry,  "Lookup entry             - Mandatory",                                               NULL },
    { "solver",        's', 0, G_OPTION_ARG_STRING,   &solver_id,     "Solver                   - Mandatory - Must be in [es|ifes|rand|minimax|ab|rab]",    NULL },
    { "repeats",       'n', 0, G_OPTION_ARG_INT,      &repeats,       "N. of repetitions        - Used with the rand/rab solvers",                          NULL },
    { "log",           'l', 0, G_OPTION_ARG_FILENAME, &log_file,      "Turns logging on         - Requires a filename prefx",                               NULL },
    { "pve-dump",      'd', 0, G_OPTION_ARG_FILENAME, &pve_dump_file, "Dumps PV                 - Requires a filename path. Available only for es solver.", NULL },
    { "pv-rec",          0, 0, G_OPTION_ARG_NONE,     &pv_rec,        "Collects PV info         - Available only for es solver.",                           NULL },
    { "pv-full-rec",     0, 0, G_OPTION_ARG_NONE,     &pv_full_rec,   "Analyzes all PV variants - Available only for es solver.",                           NULL },
    { "pv-no-print",     0, 0, G_OPTION_ARG_NONE,     &pv_no_print,   "Does't print PV variants - Available only in conjuction with option pv-full-rec.",   NULL },
    { NULL }
  };



/*
 * Prototypes for internal functions.
 */

static int
egs_select_solver (const char *const id);

/**
 * @endcond
 */



/**
 * @brief Main entry to the Reversi C endgame solver implementation.
 *
 * @todo Documentation has to be completly developed.
 */
int
main (int argc, char *argv[])
{
  gpdb_dictionary_t *db;
  gpdb_syntax_error_log_t *syntax_error_log;
  FILE *fp;

  gpdb_entry_t *entry = NULL;
  int solver_index = -1;

  endgame_solver_env_t env =
    { .log_file = NULL,
      .pve_dump_file = NULL,
      .repeats = 0,
      .pv_recording = false,
      .pv_full_recording = false,
      .pv_no_print = false
    };

  /* GLib command line options and argument parsing. */
  GError *error = NULL;
  GOptionGroup *option_group = g_option_group_new("name", "description", "help_description", NULL, NULL);
  GOptionContext *context = g_option_context_new("- Solve an endgame position");
  g_option_context_add_main_entries(context, entries, NULL);
  g_option_context_add_group(context, option_group);
  g_option_context_set_description(context, program_documentation_string);
  if (!g_option_context_parse (context, &argc, &argv, &error)) {
    g_print("Option parsing failed: %s\n", error->message);
    return -1;
  }

  /* Checks command line options for consistency, and selects the solver. */
  if (!input_file) {
    g_print("Option -f, --file is mandatory.\n");
    return -2;
  }
  if (solver_id) {
    solver_index = egs_select_solver(solver_id);
    if (solver_index == -1) {
      g_print("Option -s, --solver is out of range.\n");
      return -8;
    }
    if (strcmp(solvers[solver_index].id, "rand") == 0) {
      if (repeats < 1) {
        g_print("Option -n, --repeats is out of range.\n");
        return -9;
      }
    }
  } else {
    g_print("Option -s, --solver is mandatory.\n");
    return -5;
  }
  const endgame_solver_t *const solver = &solvers[solver_index];
  if (pv_rec && (strcmp(solver->id, "es") != 0)) {
    g_print("Option --pv-rec can be used only with solver \"es\".\n");
    return -10;
  }
  if (pv_full_rec && (strcmp(solver->id, "es") != 0)) {
    g_print("Option --pv-full-rec can be used only with solver \"es\".\n");
    return -10;
  }
  if (pv_no_print && (strcmp(solver->id, "es") || !pv_full_rec)) {
    g_print("Option --pv-no-print can be used only with solver \"es\", and when option --pv-full-rec is turned on.\n");
    return -11;
  }

  /* Opens the source file for reading. */
  fp = fopen(input_file, "r");
  if (!fp) {
    g_print("Unable to open database resource for reading, file \"%s\" does not exist.\n", input_file);
    return -3;
  }

  /* Loads the game position database. */
  db = gpdb_new(g_strdup(input_file));
  syntax_error_log = NULL;
  error = NULL;
  gpdb_load(fp, input_file, db, &syntax_error_log, &error);
  fclose(fp);

  /* Compute the number of errors logged. */
  const int number_of_errors = gpdb_syntax_error_log_length(syntax_error_log);
  if (number_of_errors != 0) {
    g_print("The database resource, file \"%s\" contains errors, debug it using the gpdb_verify utility.\n", input_file);
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
      g_print("Entry %s not found in file %s.\n", lookup_entry, input_file);
      return -6;
    }
  } else {
    g_print("No entry provided.\n");
    return -7;
  }

  /* Initializes the board module. */
  board_module_init();

  /* Sets env structure. */
  env.log_file = log_file;
  env.pve_dump_file = pve_dump_file;
  env.repeats = repeats;
  env.pv_recording = pv_rec || pv_full_rec;
  env.pv_full_recording = pv_full_rec;
  env.pv_no_print = pv_no_print;

  /* Solves the position. */
  GamePositionX *gpx = gpdb_get_gpx(entry);
  ExactSolution *solution = NULL;
  g_print("Solving game position %s, from source %s, using solver %s (%s) ...\n", entry->id, input_file, solver->id, solver->description);
  solution = solver->fn(gpx, &env);

  /* Prints results. */
  printf("\n");
  exact_solution_to_stream(stdout, solution);
  printf("\n");

  /* Frees the resources. */
  g_free(error);
  gpdb_free(db, TRUE);
  if (syntax_error_log)
    gpdb_syntax_error_log_free(syntax_error_log);
  g_option_context_free(context);
  exact_solution_free(solution);
  free(gpx);

  return 0;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

/**
 * @brief Ruturns the index in the solvers arry of the endgame solver identified by `id`.
 *
 * @details Compares the `id` parameter with the values in the solvers static array,
 *          if the `id` parameter matches with the solver id field then the solver
 *          position is returned. If no solver matches, a `-1` value is returned.
 *
 * @param [in] id the label of the solver
 * @return        the solver index
 */
static int
egs_select_solver (const char *const id)
{
  g_assert(id);
  for (size_t i = 0; i < solvers_count; i++) {
    endgame_solver_t egs = solvers[i];
    if (strcmp(id, egs.id) == 0) return i;
  }
  return -1;
}

/**
 * @endcond
 */
