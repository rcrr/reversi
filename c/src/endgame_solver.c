/**
 * @file
 *
 * @todo Prepare an analytical study that show efficiency in the search down the game tree.
 *       We need to measure the effectiveness of legal move sorting, and the benefit generated
 *       by different sorting policies in terms of reduction of visited nodes, reduction obtained
 *       thank to the increased efficiency of the alpha-beta algorithm when a good sorting is applied.
 *       Requirements are:
 *         - Solved game positions having the legal move list paired with the respective game values.
 *           . Modify the es solver in order to compute and report the 1st level move:value pairs.
 *           . Modify the REGAB database adding the full move profile for solved positions.
 *           . Modify the es solver so that it saves the full move profile into the REGAB database.
 *           . Modify the binary RGLM formats adding the moves:value pairs.
 *             A good solution should be realized having the field optional in each game position,
 *             so to have rglm files without the move:value pairs populated, and files where
 *             some records have it, and some records don't.
 *         - A best game tree expansion algorithm (minimal tree) used as a reference for comparison.
 *           The endgame solver should have a flag asking for "minimal tree" solution.
 *           The flag has a file as argument, this file contains the PV information, and the game value.
 *           The alpha-beta search runs a zero-window search using the game value, and orders the moves
 *           according to the PV.
 *           The resulting game tree should then be "minimal".
 *           The minimal tree function is a fast proof of the game value.
 *         - Compare the exact-solver ordering policy with a policy that applies the RGLM evaluation function.
 *         - Prepare an evaluation function going down multiple ply.
 *         - Compare the number of nodes expanded by a solver that orders the moves with the RGLM evaluation function.
 *         - Analyze the effectiveness of the a-b cut operated by es, compare it with a-b, random a-b, and the new rglm solver.
 *
 * @todo When available a new evaluation function that enable the sorting based on output
 *       estimation we are ready to develop an iterative-deepening game search algorithm, as well as
 *       a zero-windows search.
 *
 * @todo Analyze the effectiveness of the a-b cut operated by es, compare it with a-b, and random a-b. The analysis require to have the tail-log-file tool.
 *
 * @todo The output of the solvers is not always appropriate:
 *         - [done] Final board is not reported
 *         - The value of all the first level moves is not recorded
 *         - For random game sampler the output is meaningless
 *
 * @todo When es searches the full pv the a-b values are taking values outside of the worst-best outcome (-72 for instance). This is very alarming.
 *       The reason is that during the alpha-beta deepening at each iteration alpha is decreased by one.
 *       Is it correct? It seems not, but a full investigation is required. Best done after the tail-log-file is operational.
 *
 * @todo Refactor the board module. Get rid of all game_position and board structures, prepare the new one with the {player, opponent} definition.
 *       Change the definition of game_position and board from: {black, white}, to {player, opponent}. This is a big change, the reason for it is that
 *       all the key for a position in a game hash table, or in a postgres database, are values for a position regardless of the player holding it.
 *       This is reducing the key size to just the two 64 bit square set, no longer requiring the include also the player.
 *
 * @todo Remove GamePositionX and add board_t and game_position_t, where game_position_t has a bit-board for player and one for the opponent.
 *       board_t has also the player moving next.
 *
 * @todo Change typedef from camel based to _t syntax. In all modules.
 *
 * @todo Refactor game_tree_utils.
 *
 * @todo Add a new function in game_tree_utils that prints a human readable dump of the stack. Evaluate the option to have a binary dump, and next an
 *       independent reader.
 *       Could be relevant to have a binary format that supports multiple stack dumps, in order to log different evolution of the stack during program
 *       execution. The reader than should be able to list the dumps saved in the binary format and to print the requested selection.
 *
 * @todo Re-make from scratch the PV module. This is large and big, and painful, we know it. Objectives:
 *       - Have a dedicated module.
 *       - Discard the current (geometric) memory allocation, and leverage the new object allocator (pre-requisite).
 *       - Evaluate a design without the line, using just the cell.
 *       - Organize an index of game positions and avoid to store duplicates. The format for printing the full pv has to change to accommodate
 *         the syntax for reference.
 *         This index when done properly should also affect the search algorithm: already touched nodes do not need a further search.
 *       All utils using the pv will need a refresh and an adaptation.
 *
 * @todo Change the way the pv is computed. Instead of starting from "no-knowledge", run in advance a fast exact solver run, without pv tracking.
 *       Store the final value and then run a pv search with a narrow a-b window (zero window search).
 *       The removed duplication in the pv dag, obtained by implementing the index, and the sharp search due to the zero window search should
 *       contain to the minimum the time and memory overhead of this analysis.
 *
 * @todo Introduce the node cache by means of a shared hash-table (transposition-table).
 *
 * @todo Unit tests are far behind, a complete check is needed to ensure all functions have the appropriate testing.
 *
 * @todo Memory Object Pool refinements:
 *       - Develop an optional check in the mopool_free function that verifies that the pointer released is a good allocated object.
 *       - Apply the allocator to core utilities like the red-black trees, and the linked list.
 *       - Look for all the call to malloc and properly handle the failure that may arise when NULL is returned.
 *       - Create the call-back functions for check the allocated objects, and for shrink the pool.
 *
 * @todo Complete the Red-Black Tree implementation with set (merge, difference, intersection) functions.
 *
 * @todo Review all the memory allocation procedures in all modules.
 *
 * @todo Develop an R replacement of the rand_log_elaboration.ods Calc file.
 *
 * @todo Create a C extension to PostgreSQL that leverages the C game position functions.
 *
 * @todo Write a GUI using GTK+.
 *
 * @todo Write a wthor format reader. See: http://www.ffothello.org/wthor/Format_WThor.pdf and http://www.ffothello.org/informatique/la-base-wthor/
 *
 * @todo Develop a full description of the evidence of all the variants, basically a paper describing:
 *        - The statistical data obtained by the random sampler.
 *        - The statistical data obtained by the minimax and rab solvers.
 *        - A kind of relation with the minimal tree (this has to be detailed more).
 *        - A description of the solvers.
 *        - A description of the PL/SQL reversi package.
 *
 * --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 *
 *
 * @todo [2021-02-06 - done]
 *       Write a new effective solver (rglm) based on the new RGLM Evaluation Function.
 *       The solver has to replace the es one in the task to mine new game positions.
 *       The REGAB program has a flag to select the solver. The solve action now save to the database the solver being used.
 *
 *       [2021-02-06]
 *       The solver has been prepared, it is not really "optimized", but the way it traverse the game tree is almost correct.
 *       Here some results on the ff0-42 game position (empty_count = 22).
 *
 *       $ time ./build/bin/endgame_solver -s es -f db/gpdb-ffo.txt -q ffo-42
 *       [node_count=586.695.875, leaf_count=109.953.274]
 *       Final outcome: best move=G2, position value=6
 *       real 0m33.436s
 *
 *       rglm 14-22 means that when empty_count is in [13..22] moves are sorted using the rglm evaluation.
 *       The time spent by the rglm solver contains the load of the .w.dat data files.
 *       Around 11 and 10 empty count, the use of the rglm knowledge doesn't pay off.
 *
 *        rglm   node_count  leaf_count       real
 *       14-22  127.510.254  22.856.963  0m10.161s
 *       13-22  122.193.435  21.871.728  0m10.084s
 *       12-22   95.210.368  16.640.160   0m8.447s
 *       11-22   91.625.664  15.959.447   0m8.499s
 *       10-22   80.969.399  13.700.789   0m9.005s
 *       09-22   78.598.271  13.231.276   0m9.644s
 *       08-22   77.314.901  12.690.430  0m12.914s
 *       07-22   76.223.643  12.457.218  0m15.279s
 *       06-22   79.173.975  12.766.751  0m24.515s
 *
 *       [2021-02-21 - done] The solver has two quirks to be worked out:
 *       - The value of the missing weights is set to 0.0, but I believe it is not appropriate.
 *         The value that should be used is the mean of the values taken by the weights belonging to the same pattern.
 *       - When the position is terminal (leaf node), the exact value should be used.
 *
 *       [2021-02-21 - done] The solver should read the weighs files based on a cfg file given by a command line argument.
 *
 *       [2021-02-07 - done] The value of the missing pattern configuration has been set as the weighed average of the weights of the pattern.
 *
 * @todo [2021-01-24 - no longer relevant] Profile exact_solver against improved_fast_endgame_solver.
 *
 * @todo [2021-01-24 - no longer relevant] Analyze the parity feature in improved_fast_endgame.
 *
 * @todo [2021-01-24 - done] Develop procedures that consume the position db, computing values and best moves.
 *                           Moving from almost completed games backwards ...
 *                           it is the REGAB programs and database.
 *
 * @todo [2021-01-24 - done] Growth a db of solved, random, game position. Run on the server (Vilya) the batches.
 *
 * @todo [2021-01-24 - done] Analyze the statistical properties of the solved random games. If needed develop SQL,
 *                           spreadsheet, R procedures to understand the data.
 *                           Select one or more patterns (features) and develop the appropriate statistics and correlation
 *                           between game output and position features.
 *                           We have done something more, and something less.
 *                           Now there is a statistical model that correlates patterns and features with the game output.
 *                           There is a very detailed analysis too.
 *                           There is no direct analysis between features and game values, they are mutuated by the evaluation
 *                           function.
 *
 * @todo [2021-01-24 - no longer relevant] Evaluate completely the possible pattern feature. Select the final set of feature for the exact solver.
 *                                         Evaluate possible optimization for the extraction of features from the game position.
 *                                         The direction taken has been a different one.
 *                                         There is no need to move forward with furter enhancements on the exact solver code.
 *
 * @todo [2021-01-24 - done] Everything down from yere has been completed before Januay 24th, 2021.
 *                           The development on REGAB and RGLM has arrived to a point where there is a need to
 *                           pour the accumulated knowledge into the game engine.
 *                           First step is the refresh of this TODO list.
 *
 * @todo [2021-01-24 - no longer relevant] Develop a new game machinery based on AVX-512.
 *                                         AVX-512 has not been recognized as a breakthrough in the Cholesky Solver.
 *                                         The BLAS function using AVX2 are as fast as the corresponding AVX-512 on
 *                                         hardware capable of it.
 *
 * @todo [done] Complete the removal of MoveList and MoveList Element from exact_solver.
 *
 * @todo [done] Harmonize the exact_solver algorithm to minimax, full use of the stack and no recursion.
 *
 * @todo [done] Add to the exact_solver logging strategy a second file that logs
 *              when the function returns. There we can add the return value and the
 *              number of moves searched against the number of legal ones. This can give
 *              back a kpi of the alpha-beta efficiency in cutting irrelevant branches.
 *              It is needed than to develop the postgres tables and procedures to properly handle this new data.
 *
 * @todo [done] Remove json fields in the logging procedures, and from PostgreSQL.
 *
 * @todo [done] Develop an object allocator tool.
 *
 * @todo [done] Meter, and verify, performances of the new harmonized exact_solver vs the legacy one.
 *              Remove the legacy one when tests, and performances are OK.
 *
 * @todo [done] Develop the architecture and the procedures in C and in PostgreSQL to generate and store random games.
 *              It is needed:
 *              - a unique game id, generated by a sequence in the DB.
 *              - an header table having:
 *                * game_id
 *                * ARRAY of int8, that collects the random generator state at the beginning of the game (so the game is reproducible)
 *              - a game position table having:
 *                * game_id
 *                * sequence in the game
 *                * game position
 *                * empty_squares
 *                * is_solved
 *                * value
 *                * best move
 *              - add to the prng module a dump/load api functions that save and restore a random number generator from its internal state
 *              - prepare C procedures that interact with PostgreSQL for generating and storing the random games.
 *              - Establish a procedure to backup and restore the PostgreSQL DB.
 *
 * @todo [done] Dependency from glib shall be removed from sources:
 *       - [done] game_tree_logger has file and directory tools based on glib
 *       - [done] all the main programs have args managed by mean of glib
 *       - [done] game_position_db module is heavily based on glib
 *
 * @todo [done] Dependency from glib shall be removed from unit tests. This requires to complete the utest module.
 *
 * @todo [done] Checks all the programs for memory leaks.
 *
 * @todo [closed] Add a way to check a PV for variants.
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
 *       - [diverted to new entries] PVE has to be fully reorganized (a major refactoring) into a module.
 *                      A new module, block memory allocator, or bma, is required.
 *                      PVE is composed by lines, cells, and a cell dictionary, the dictionary is implemented by a binary search tree.
 *                      [done] A new module, binary red-black tree, or rbt, is required. The first solution in my evaluation list is Ben Pfaff's avl library.
 *                             Cells and lines leverage the bma module.
 *                      Bma module allocates constant size blocks (no more exponetial size increases), and has a more space-efficient stack.
 *                      The rbt based dictionary prevents game position duplications, transforming the tree in a DAG, reducing the pve required size.
 *                      Moving to a DAG requires to keep track of ref count for cell.
 *                      A pack function is required in order to achieve the stack space reduction.
 *       - [diverted to new entries] More optional checks could be added to pve_is_invariant_satisfied.
 *       - [duplicated] Add game values computation and output for first level moves.
 *       - [done] Add a function to compute PV hash code (it is useful for testing and database processing).
 *       - [done] The ExactSolution object should be replaced by a PVE reference.
 *
 * @todo [done] Complete the random game player.
 *
 * @todo [done] Add to the solver selection a full MINIMAX implementation.
 *
 * @todo [done] Add to the solver selection an alpha-beta implementation that
 *              sort the legal move list with a random function.
 *
 * @todo [done] The solvers do not return a PV, but just the best move and its value.
 *              Now a new sub-module, pve in game_tree_utils, has al the functions that are needed.
 *              It is used by the exact_solver, the other has to be updated if useful.
 *
 * @todo [done] Refine and refactor the exact_solver implementation.
 *
 * @todo [done] Write SIMD (AVX2) versions of make_move and legal_moves functions.
 *
 * @todo [done] Solvers rab and ab share the same stack solution. Refactor it sharing
 *              the same utilities brought to a dedicated module.
 *       [done] The es solver should do the same.
 *              2016-08-21: legal_moves and make_move functions have an AVX2 version.
 *                          Now malloc/free functions are consuming the largest slice of the es solver.
 *                          It is time to address this task.
 *
 * @todo [done] Port the stack practice to all the other solver, refactor the structures used (Stack, NodeInfo, GamePositionX).
 *
 * @todo [done] Write a test suite that solves a selection of the FFO test cases.
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
 * @todo [done] A new section of the Makefile that regenerates the log csv files used by the PL/SQL reload_everything.sql script.
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
 * --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
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
 * @copyright 2013, 2014, 2015, 2016, 2017, 2018, 2021 Roberto Corradini. All rights reserved.
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
#include <ctype.h>
#include <errno.h>

#include "main_option_parse.h"
#include "file_utils.h"
#include "endgame_utils.h"
#include "game_position_db.h"
#include "game_tree_logger.h"
#include "board_pattern.h"

#include "improved_fast_endgame_solver.h"
#include "minimax_solver.h"
#include "exact_solver.h"
#include "rglm_solver.h"

#include "game_value_estimator.h"



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

static const mop_options_long_t olist[] = {
  {"help",              'h', MOP_NONE},
  {"file",              'f', MOP_REQUIRED},
  {"lookup-entry",      'q', MOP_REQUIRED},
  {"solver",            's', MOP_REQUIRED},
  {"repeats",           'n', MOP_REQUIRED},
  {"prng-seed",         'r', MOP_REQUIRED},
  {"pattern",           'P', MOP_REQUIRED},
  {"log",               'l', MOP_REQUIRED},
  {"pve-dump",          'd', MOP_REQUIRED},
  {"pv-rec",            'R', MOP_NONE},
  {"pv-full-rec",       'F', MOP_NONE},
  {"pv-no-print",       'N', MOP_NONE},
  {"ab-window",         'w', MOP_REQUIRED},
  {"all-moves",         'a', MOP_NONE},
  {"search-depth",      'p', MOP_REQUIRED},
  {"config-file",       'c', MOP_REQUIRED},
  {0, 0, 0}
};

static const char *documentation =
  "Usage:\n"
  "  endgame_solver [OPTION...] - Solve an endgame position\n"
  "\n"
  "Options:\n"
  "  -h, --help             Show help options\n"
  "  -f, --file             Input file name          - Mandatory.\n"
  "  -q, --lookup-entry     Lookup entry             - Mandatory.\n"
  "  -s, --solver           Solver                   - Mandatory - Must be in [es|ifes|rand|minimax|ab|rab|rglm|gve].\n"
  "  -n, --repeats          N. of repetitions        - Used with the rand/rab solvers.\n"
  "  -r, --prng-seed        random generator seed    - Used with rand/rab solvers.\n"
  "  -P, --pattern          Pattern                  - Used with the rand solver - Must be in [EDGE|CORNER|XEDGE|R2|R3|R4|DIAG4|DIAG5|DIAG6|DIAG7|DIAG8|2X5COR|DIAG3].\n"
  "  -l, --log              Turns logging on         - Requires a filename prefx.\n"
  "  -d, --pve-dump         Dumps PV                 - Requires a filename path. Available only for es solver.\n"
  "  -R, --pv-rec           Collects PV info         - Available only for es solver.\n"
  "  -F, --pv-full-rec      Analyzes all PV variants - Available only for es solver.\n"
  "  -N, --pv-no-print      Does't print PV variants - Available only in conjuction with option pv-full-rec.\n"
  "  -w, --ab-window        Alpha-beta search window - Available only for es solver.\n"
  "  -a, --all-moves        Values all moves         - Available only for es and rglm solvers.\n"
  "  -p, --search-depth     Search depth             - Available only for gve solver.\n"
  "  -c, --config-file      Configuration file       - Requires a filename path.\n"
  "\n"
  "Description:\n"
  "  Endgame solver is the front end for a group of algorithms aimed to analyze the final part of the game and to asses the game tree structure.\n"
  "  Available engines are: es (exact solver), ifes (improved fast endgame solver), rand (random game sampler), minimax (minimax solver),\n"
  "  ab (alpha-beta solver), and rab (random alpha-beta solver).\n"
  "\n"
  "  - es (exact solver)\n"
  "    My fully featured implementation of a Reversi Endgame Exact Solver. A sample call is:\n"
  "    $ endgame_solver -f db/gpdb-ffo.txt -q ffo-40 -s es\n"
  "\n"
  "  - ifes (improved fast endgame solver)\n"
  "    A solver derived from the Gunnar Andersson work. A sample call is:\n"
  "    $ endgame_solver -f db/gpdb-ffo.txt -q ffo-40 -s ifes\n"
  "\n"
  "  - rand (random game sampler)\n"
  "    The rand solver is a way to play a sequence of random game from the given position.\n"
  "    This option works together with the -n flag, that assigns the number of repeats, a sample call is:\n"
  "    $ endgame_solver -f db/gpdb-sample-games.txt -q initial -s rand -n 100 -l logfile\n"
  "    Or it may be used to compute pattern frequencies:\n"
  "    $ endgame_solver -f db/gpdb-sample-games.txt -q initial -s rand -n 1000000 -P EDGE\n"
  "\n"
  "  - minimax (minimax solver)\n"
  "    It applies the plain vanilla minimax algorithm, a sample call is:\n"
  "    $ endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified -s minimax\n"
  "\n"
  "  - ab (alpha-beta solver)\n"
  "    It applies the alpha-beta algorithm sorting legal moves by their natural order, a sample call is:\n"
  "    $ endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified -s ab\n"
  "\n"
  "  - rab (random alpha-beta solver)\n"
  "    It uses the alpha-beta pruning, ordering the moves by mean of a random criteria, a sample call is:\n"
  "    $ endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified-4 -s rab -l out/log -n 3\n"
  "\n"
  "  - rglm (reversi generalized linear model solver)\n"
  "    It is an experimental solver under development, a sample call is:\n"
  "    $ endgame_solver -f db/gpdb-sample-games.txt -q ffo-01-simplified-4 -s rglm\n"
  "\n"
  "Author:\n"
  "  Written by Roberto Corradini <rob_corradini@yahoo.it>\n"
  "\n"
  "Copyright (c) 2013, 2014, 2015, 2016, 2017, 2018, 2021 Roberto Corradini. All rights reserved.\n"
  "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  "This is free software: you are free to change and redistribute it. There is NO WARRANTY, to the extent permitted by law.\n"
  ;

static const endgame_solver_t solvers[] =
  {
    { .id = "es",      .description = "exact solver",                 .function_name = "game_position_es_solve",        .fn = game_position_es_solve },
    { .id = "ifes",    .description = "improved fast endgame solver", .function_name = "game_position_ifes_solve",      .fn = game_position_ifes_solve },
    { .id = "rand",    .description = "random game sampler",          .function_name = "game_position_random_sampler",  .fn = game_position_random_sampler },
    { .id = "minimax", .description = "minimax solver",               .function_name = "game_position_minimax_solve",   .fn = game_position_minimax_solve },
    { .id = "rab",     .description = "random alpha-beta solver",     .function_name = "game_position_rab_solve",       .fn = game_position_rab_solve },
    { .id = "ab",      .description = "alpha-beta solver",            .function_name = "game_position_ab_solve",        .fn = game_position_ab_solve },
    { .id = "rglm",    .description = "rglm solver",                  .function_name = "game_position_rglm_solve",      .fn = game_position_rglm_solve },
    { .id = "gve",     .description = "game value estimator",         .function_name = "game_position_value_estimator", .fn = game_position_value_estimator },
  };

static const int solvers_count = sizeof(solvers) / sizeof(solvers[0]);



/* Static variables. */

static mop_options_t options;

static int h_flag = false;

static int f_flag = false;
static char *f_arg = NULL;

static int q_flag = false;
static char *q_arg = NULL;

static int s_flag = false;
static char *s_arg = NULL;

static int n_flag = false;
static char *n_arg = NULL;

static int r_flag = false;
static char *r_arg = NULL;

static int P_flag = false;
static char *P_arg = NULL;

static int l_flag = false;
static char *l_arg = NULL;

static int d_flag = false;
static char *d_arg = NULL;

static int R_flag = false;
static int F_flag = false;
static int N_flag = false;

static int w_flag = false;
static char *w_arg = NULL;

static int a_flag = false;

static int p_flag = false;
static char *p_arg = NULL;

static int c_flag = false;
static char *c_arg = NULL;

static char *input_file = NULL;
static char *lookup_entry = NULL;
static int solver_index = -1;
static unsigned long long int repeats = 0;
static char *log_file = NULL;
static char *pve_dump_file = NULL;
static bool pv_rec = false;
static bool pv_full_rec = false;
static bool pv_no_print = false;
static int board_pattern_index = -1;
static uint64_t prng_seed = 0;
static bool prng_seed_is_set = false;
static int alpha = worst_score;
static int beta = best_score;
static int search_depth = -1;


/*
 * Prototypes for internal functions.
 */

static int
egs_select_solver (const char *const id);

static int
egs_select_pattern (const char *const name);

static void
egs_parse_ab (int *alpha, int *beta, char *arg, int *err);


/**
 * @endcond
 */



/**
 * @brief Main entry to the Reversi C endgame solver implementation.
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
    case 'f':
      f_flag = true;
      f_arg = options.optarg;
      break;
    case 'q':
      q_flag = true;
      q_arg = options.optarg;
      break;
    case 's':
      s_flag = true;
      s_arg = options.optarg;
      break;
    case 'n':
      n_flag = true;
      n_arg = options.optarg;
      break;
    case 'r':
      r_flag = true;
      r_arg = options.optarg;
      break;
    case 'P':
      P_flag = true;
      P_arg = options.optarg;
      break;
    case 'l':
      l_flag = true;
      l_arg = options.optarg;
      break;
    case 'd':
      d_flag = true;
      d_arg = options.optarg;
      break;
    case 'R':
      R_flag = true;
      break;
    case 'F':
      F_flag = true;
      break;
    case 'N':
      N_flag = true;
      break;
    case 'w':
      w_flag = true;
      w_arg = options.optarg;
      break;
    case 'a':
      a_flag = true;
      break;
    case 'p':
      p_flag = true;
      p_arg = options.optarg;
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

  if (!f_flag) {
    fprintf(stderr, "Option -f, --file is mandatory.\n");
    return -2;
  } else {
    input_file = f_arg;
  }

  if (!s_flag) {
    fprintf(stderr, "Option -s, --solver is mandatory.\n");
    return -4;
  } else {
    solver_index = egs_select_solver(s_arg);
    if (solver_index == -1) {
      fprintf(stderr, "Option -s, --solver is out of range.\n");
      return -5;
    }
  }
  const endgame_solver_t *const solver = &solvers[solver_index];

  if (!q_flag) {
    fprintf(stderr, "Option -q, --lookup-entry is mandatory.\n");
    return -6;
  } else {
    lookup_entry = q_arg;
  }

  if (n_flag) {
    if ((strcmp("rand", solver->id) != 0) && (strcmp("rab", solver->id) != 0)) { // solver is not rab or rand ...
      fprintf(stderr, "Option -n, --repeats can be used only with solvers \"rab\" or \"rand\".\n");
      return -7;
    }
    char *endptr;
    long long int n_arg_to_int = strtoll(n_arg, &endptr, 10);
    if (endptr - n_arg != strlen(n_arg)) {
      fprintf(stderr, "Argument for option -n, --repeats: %s is invalid.\n", n_arg);
      return -8;
    }
    if (n_arg_to_int < 1) {
      fprintf(stderr, "Argument for option -n, --repeats is %lld, it must be a positive integer.\n", n_arg_to_int);
      return -9;
    }
    repeats = n_arg_to_int;
  } else {
    repeats = 1;
  }

  if (r_flag) {
    if ((strcmp("rand", solver->id) != 0) && (strcmp("rab", solver->id) != 0)) { // solver is not rab or rand ...
      fprintf(stderr, "Option -r, --prng-seed can be used only with solvers \"rab\" or \"rand\".\n");
      return EXIT_FAILURE;
    }
    char *endptr;
    long int r_arg_to_int = strtol(r_arg, &endptr, 10);
    if (endptr - r_arg != strlen(r_arg)) {
      fprintf(stderr, "Argument for option -r, --prng-seed: %s is invalid.\n", r_arg);
      return EXIT_FAILURE;
    }
    if (r_arg_to_int < 1) {
      fprintf(stderr, "Argument for option -r, --prng-seed is %ld, it must be a positive integer.\n", r_arg_to_int);
      return EXIT_FAILURE;
    }
    prng_seed = r_arg_to_int;
    prng_seed_is_set = true;
  }

  if (P_flag) {
    if (strcmp("rand", solver->id) != 0) { // solver is not rab ...
      fprintf(stderr, "Option -P, --pattern can be used only with solver \"rand\".\n");
      return EXIT_FAILURE;
    }
    board_pattern_index = egs_select_pattern(P_arg);
    if (board_pattern_index == -1) {
      fprintf(stderr, "Option -P, --pattern is out of range.\n");
      return EXIT_FAILURE;
    }
  }

  if (l_flag) {
    log_file = l_arg;
    if (gtl_touch_log_file(l_arg))
      log_file = l_arg;
    else {
      fprintf(stderr, "Unable to open log files with prefix \"%s\".\n", l_arg);
      return -10;
    }
  }

  if (d_flag) {
    if (fut_touch_file(d_arg))
      pve_dump_file = d_arg;
    else {
      fprintf(stderr, "Unable to open dump file \"%s\".\n", d_arg);
      return -11;
    }
  }

  if (R_flag) {
    if (!(strcmp("es", solver->id) == 0)) {
      fprintf(stderr, "Option -R, --pv-rec can be used only with solver \"es\".\n");
      return -12;
    }
    pv_rec = true;
  }

  if (F_flag) {
    if (!((strcmp("es", solver->id) == 0) || (strcmp("rglm", solver->id) == 0))) {
      fprintf(stderr, "Option -F, --pv-full-rec can be used only with solver \"es\".\n");
      return -13;
    }
    if (pv_rec) {
      fprintf(stderr, "Option -F, --pv-full-rec and option -R, cannot be used together.\n");
      return -14;
    }
    pv_full_rec = true;
  }

  if (N_flag) {
    if (!(strcmp("es", solver->id) == 0) || !pv_full_rec) {
      fprintf(stderr, "Option -N, --pv-no-print can be used only with solver \"es\", and when option -F, --pv-full-rec is turned on.\n");
      return -15;
    }
    pv_no_print = true;
  }

  if (w_flag) {
    if (!((strcmp("es", solver->id) == 0) || (strcmp("rglm", solver->id) == 0))) {
      fprintf(stderr, "Option -w, --ab-window can be used only with solvers [es,rglm].\n");
      return EXIT_FAILURE;
    }
    if (pv_rec || pv_full_rec) {
      fprintf(stderr, "Option -w, --ab-window cannot be used when computing the PV.\n");
      return EXIT_FAILURE;
    }
    int return_error = 0;
    egs_parse_ab(&alpha, &beta, w_arg, &return_error);
    if (return_error != 0) {
      fprintf(stderr, "Option -w, --ab-window, must be two integers values, separated by comma, ordered, not equal, in the range [-65..+65].\n");
      return EXIT_FAILURE;
    }
  }

  if (a_flag) {
    if (!((strcmp("es", solver->id) == 0) || (strcmp("rglm", solver->id) == 0))) {
      fprintf(stderr, "Option -a, --all-moves, can be used only with solver \"es\" or \"rglm\".\n");
      return -15;
    }
    if (pv_rec || pv_full_rec) {
      fprintf(stderr, "Option -a, --all-moves, and options -R or -F, cannot be used together.\n");
      return -16;
    }
    if (w_flag) {
      fprintf(stderr, "Option -a, --all-moves, and options -w, cannot be used together.\n");
      return -17;
    }
  }

  if (p_flag) {
    if (!(strcmp("gve", solver->id) == 0)) {
      fprintf(stderr, "Option -p, --search-depth can be used only with solver \"gve\".\n");
      return -99;
    }
    char *endptr;
    errno = 0;
    search_depth = strtol(p_arg, &endptr, 10);
    if (errno != 0) {
      perror("strtol");
      return EXIT_FAILURE;
    }
    if (endptr == p_arg) {
      fprintf(stderr, "No conversion was possible for search_depth: %s\n", p_arg);
      return EXIT_FAILURE;
    }
    if (*endptr != '\0') {
      fprintf(stderr, "Further characters after number in search_depth value: %s\n", endptr);
      return EXIT_FAILURE;
    }
    if (search_depth < 0) {
      fprintf(stderr, "Option -p, --search-depth, must not be negative.\n");
      return EXIT_FAILURE;
    }
  }

  cfg_t *cfg = NULL;
  if (c_flag) {
    cfg = cfg_load(c_arg);
    if (!cfg) {
      fprintf(stderr, "Option -c, --config-file, file doesn't exist.\n");
      return EXIT_FAILURE;
    }
  }

  /* Verifies that the database input file is available for reading. */
  FILE *fp = fopen(input_file, "r");
  if (!fp) {
    fprintf(stderr, "Unable to open database resource for reading, file \"%s\" does not exist.\n", input_file);
    return -18;
  }
  fclose(fp);

  /*
   * Checks on command line options ends here.
   */



  gpdb_dictionary_t *db;
  gpdb_syntax_err_log_t *syntax_error_log;

  gpdb_entry_t *entry = NULL;

  endgame_solver_env_t env =
    { .log_file = NULL,
      .pve_dump_file = NULL,
      .repeats = 0,
      .pv_recording = false,
      .pv_full_recording = false,
      .pv_no_print = false,
      .board_pattern_index = board_pattern_index,
      .prng_seed_is_set = prng_seed_is_set,
      .prng_seed = prng_seed,
      .alpha = alpha,
      .beta = beta,
      .all_moves = a_flag,
      .search_depth = search_depth,
      .cfg = cfg,
    };

  /* Loads the game position database. */
  db = gpdb_dictionary_new(input_file);
  syntax_error_log = gpdb_syntax_err_log_new();
  gpdb_dictionary_load(db, syntax_error_log, input_file, true, false, true);

  /* Compute the number of errors logged. */
  const int number_of_errors = gpdb_syntax_err_log_length(syntax_error_log);
  gpdb_syntax_err_log_free(syntax_error_log);
  if (number_of_errors != 0) {
    fprintf(stderr, "The database resource, file \"%s\" contains errors, debug it using the gpdb_verify utility.\n", input_file);
    return -4;
  }

  /* Lookup for a given key. */
  if (lookup_entry) {
    entry = gpdb_dictionary_find_entry(db, lookup_entry);
    if (entry) {
      gpdb_entry_print(entry, stdout, true);
    } else {
      fprintf(stderr, "Entry %s not found in file %s.\n", lookup_entry, input_file);
      return -6;
    }
  } else {
    printf("No entry provided.\n");
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
  GamePositionX *gpx = gpdb_entry_get_gpx(entry);
  ExactSolution *solution = NULL;
  fprintf(stdout, "Solving game position %s, from source %s, using solver %s (%s) ...\n", entry->id, input_file, solver->id, solver->description);
  solution = solver->fn(gpx, &env);

  /* Prints results. */
  printf("\n");
  exact_solution_to_stream(stdout, solution);
  printf("\n");

  /* Frees the resources. */
  gpdb_dictionary_free(db);
  exact_solution_free(solution);
  cfg_free(env.cfg);

  return 0;
}



/**
 * @cond
 */

/*
 * Internal functions.
 */

/**
 * @brief Ruturns the index in the solvers array of the endgame solver identified by `id`.
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
  assert(id);
  for (size_t i = 0; i < solvers_count; i++) {
    endgame_solver_t egs = solvers[i];
    if (strcmp(id, egs.id) == 0) return i;
  }
  return -1;
}

static int
egs_select_pattern (const char *const name)
{
  assert(name);
  for (size_t i = 0;; i++) {
    const board_pattern_t *bp = &board_patterns[i];
    if (bp->id == BOARD_PATTERN_INVALID) break;
    if (strcmp(name, bp->name) == 0) return i;
  }
  return -1;
}

static void
egs_parse_ab (int *alpha,
              int *beta,
              char *arg,
              int *err)
{
  int parse_mode; // 0 means digits, 1 means separator.
  char *beginptr;
  char *endptr;
  bool is_first_char;
  int field_cnt;
  int value;
  int min, max;

  parse_mode = 1;
  beginptr = arg;
  field_cnt = 1;
  is_first_char = true;
  while (*beginptr) {
    if (',' == *beginptr) {
      if (parse_mode != 0) {
        fprintf(stderr, "Wrong format in -w, --ab-windows, argument value.\n");
        *err = -1;
        return;
      }
      field_cnt++;
      parse_mode = 1;
      is_first_char = true;
    } else if (*beginptr == '+' || *beginptr == '-') {
      if (!is_first_char) {
        fprintf(stderr, "Wrong character in -w, --ab-windows, argument value.\n");
        *err = -4;
        return;
      }
      parse_mode = 0;
      is_first_char = false;
    } else if (isdigit(*beginptr)) {
      parse_mode = 0;
      is_first_char = false;
    } else {
      fprintf(stderr, "Wrong character in -w, --ab-windows, argument value.\n");
      *err = -2;
      return;
    }
    beginptr++;
  }
  if (parse_mode != 0) {
    fprintf(stderr, "The value for -w, --ab-windows, argument value couldn't end with a comma.\n");
    *err = -3;
    return;
  }
  if (field_cnt != 2) {
    fprintf(stderr, "The value for -w, --ab-windows, argument must have two fields separated by a comma.\n");
    *err = -5;
    return;
  }

  beginptr = arg;
  for (size_t i = 0; i < field_cnt; i++) {
    value = strtol(beginptr, &endptr, 10);
    if (value < -65 || value > +65) {
      fprintf(stderr, "The value for -w, --ab-windows, argument value couldn't end with a comma.\n");
      *err = -10;
      return;
    }
    if (i == 0) min = value;
    if (i == 1) max = value;
    beginptr = endptr + 1;
  }
  if (*endptr != '\0') {
    fprintf(stderr, "Further characters after number in batch_id value: %s\n", endptr);
    *err = -12;
    return;
  }

  if (min >= max) {
    fprintf(stderr, "The value for -w, --ab-windows, argument are not properly ordered, or are equal.\n");
    *err = -14;
    return;
  }

  *alpha = min;
  *beta = max;
  *err= 0;
  return;
}

/**
 * @endcond
 */
