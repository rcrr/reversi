/**
 * @file
 *
 * @brief Game position unit test suite.
 * @details Collects advanced tests and helper methods for the game position entity.
 *
 * @par ut_game_position.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2015, 2017 Roberto Corradini. All rights reserved.
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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include "unit_test.h"
#include "game_position_db.h"



/*
 * Auxiliary functions.
 */

static void
aux_dummy (void)
{
  return;
}

static void
aux_fixture_setup (ut_test_t *const t)
{
  assert(t);

  const char *const file_name = "db/gpdb-sample-games.txt";

  gpdb_dictionary_t *db = gpdb_dictionary_new("Test db from file: db/gpdb-sample-games.txt");
  assert(db);

  gpdb_syntax_err_log_t *elog = gpdb_syntax_err_log_new();
  assert(elog);

  const bool duplicates_are_errors = true;
  const bool replace_duplicates = false;
  const bool stop_on_error = false;

  gpdb_dictionary_load(db,
                       elog,
                       file_name,
                       duplicates_are_errors,
                       replace_duplicates,
                       stop_on_error);

  assert(0 == gpdb_syntax_err_log_length(elog));

  gpdb_syntax_err_log_free(elog);

  t->fixture = db;
}

static void
aux_fixture_teardown (ut_test_t *const t)
{
  assert(t->fixture != NULL);
  gpdb_dictionary_free(t->fixture);
}

static GamePositionX *
aux_get_gpx_from_db (ut_test_t *const t,
                     gpdb_dictionary_t *db,
                     char *id)
{
  gpdb_entry_t *entry = gpdb_dictionary_find_entry(db, id);
  ut_assert(t, entry);
  return gpdb_entry_get_gpx(entry);
}



/*
 * Test functions.
 */

static void
dummy_t (ut_test_t *const t)
{
  aux_dummy();
  ut_assert(t, true);
}

static void
game_position_legal_moves_t (ut_test_t *const t)
{
  gpdb_dictionary_t *db = (gpdb_dictionary_t *) t->fixture;

  char to_string[512];
  size_t length;

  length = square_set_to_string(to_string, game_position_x_legal_moves(aux_get_gpx_from_db(t, db, "initial")));
  ut_assert(t, strcmp("D3 C4 F5 E6", to_string) == 0);
  ut_assert(t, length == 11);

  length = square_set_to_string(to_string, game_position_x_legal_moves(aux_get_gpx_from_db(t, db, "early-game-b-9-moves")));
  ut_assert(t, strcmp("C3 C6",to_string) == 0);
  ut_assert(t, length == 5);

  length = square_set_to_string(to_string, game_position_x_legal_moves(aux_get_gpx_from_db(t, db, "black-has-to-pass")));
  ut_assert(t, strcmp("", to_string) == 0);
  ut_assert(t, length == 0);

  length = square_set_to_string(to_string, game_position_x_legal_moves(aux_get_gpx_from_db(t, db, "early-game-c-12-moves")));
  ut_assert(t, strcmp("H2 A4 C4 G4 A5 F5 B6 E6 G7", to_string) == 0);
  ut_assert(t, length == 26);

  length = square_set_to_string(to_string, game_position_x_legal_moves(aux_get_gpx_from_db(t, db, "final-b37-w27")));
  ut_assert(t, strcmp("", to_string) == 0);
  ut_assert(t, length == 0);
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_prog_arg_config_t config;
  ut_init(&config, &argc, &argv);

  ut_suite_t *const s = ut_suite_new(&config, "game_position");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "dummy", dummy_t);

  ut_suite_add_regular_test(s, UT_MODE_STND, UT_QUICKNESS_0001,
                            "game_position_legal_moves",
                            NULL,
                            aux_fixture_setup,
                            game_position_legal_moves_t,
                            aux_fixture_teardown);

  int failure_count = ut_suite_run(s);

  ut_suite_free(s);

  return failure_count;
}
