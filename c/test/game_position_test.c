/**
 * @file
 *
 * @brief Advanced game position unit test suite.
 * @details Collects tests and helper methods for the board module.
 *
 * @par game_position_test.c
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

#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include <glib.h>

#include "board.h"
#include "game_position_db.h"


/* gpdb_dictionary_t fixture */
typedef struct {
  gpdb_dictionary_t *db;
} gpdb_fixture_t;



/* Test function prototypes. */

static void
game_position_legal_moves_test (gpdb_fixture_t *fixture,
                                gconstpointer test_data);

static void
game_position_make_move_test (gpdb_fixture_t *fixture,
                              gconstpointer test_data);

static void
game_position_has_any_legal_move_test (gpdb_fixture_t *fixture,
                                       gconstpointer test_data);

static void
game_position_has_any_player_any_legal_move_test (gpdb_fixture_t *fixture,
                                                  gconstpointer test_data);



/* Helper function prototypes. */

static void
gpdb_fixture_setup (gpdb_fixture_t *fixture,
                    gconstpointer test_data);

static void
gpdb_fixture_teardown (gpdb_fixture_t *fixture,
                       gconstpointer test_data);

static GamePositionX *
get_gpx_from_db (gpdb_dictionary_t *db,
                 gchar *id);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  board_module_init();

  g_test_add ("/game_position/game_position_legal_moves_test",
              gpdb_fixture_t,
              (gconstpointer) NULL,
              gpdb_fixture_setup,
              game_position_legal_moves_test,
              gpdb_fixture_teardown);

  g_test_add ("/game_position/game_position_has_any_legal_move_test",
              gpdb_fixture_t,
              (gconstpointer) NULL,
              gpdb_fixture_setup,
              game_position_has_any_legal_move_test,
              gpdb_fixture_teardown);

  g_test_add ("/game_position/game_position_has_any_player_any_legal_move_test",
              gpdb_fixture_t,
              (gconstpointer) NULL,
              gpdb_fixture_setup,
              game_position_has_any_player_any_legal_move_test,
              gpdb_fixture_teardown);

  g_test_add ("/game_position/game_position_make_move_test",
              gpdb_fixture_t,
              (gconstpointer) NULL,
              gpdb_fixture_setup,
              game_position_make_move_test,
              gpdb_fixture_teardown);

  return g_test_run();
}



/*
 * Test functions.
 */

static void
game_position_legal_moves_test (gpdb_fixture_t *fixture,
                                gconstpointer test_data)
{
  gpdb_dictionary_t *db = fixture->db;

  char to_string[512];
  size_t length;

  length = square_set_to_string(to_string, game_position_x_legal_moves(get_gpx_from_db(db, "initial")));
  g_assert_cmpstr("D3 C4 F5 E6", ==, to_string);
  g_assert(length == 11);

  length = square_set_to_string(to_string, game_position_x_legal_moves(get_gpx_from_db(db, "early-game-b-9-moves")));
  g_assert_cmpstr("C3 C6", ==, to_string);
  g_assert(length == 5);

  length = square_set_to_string(to_string, game_position_x_legal_moves(get_gpx_from_db(db, "black-has-to-pass")));
  g_assert_cmpstr("", ==, to_string);
  g_assert(length == 0);

  length = square_set_to_string(to_string, game_position_x_legal_moves(get_gpx_from_db(db, "early-game-c-12-moves")));
  g_assert_cmpstr("H2 A4 C4 G4 A5 F5 B6 E6 G7", ==, to_string);
  g_assert(length == 26);

  length = square_set_to_string(to_string, game_position_x_legal_moves(get_gpx_from_db(db, "final-b37-w27")));
  g_assert_cmpstr("", ==, to_string);
  g_assert(length == 0);
}

static void
game_position_has_any_legal_move_test (gpdb_fixture_t *fixture,
                                       gconstpointer test_data)
{
  gpdb_dictionary_t *db = fixture->db;

  g_assert(true  == game_position_x_has_any_legal_move(get_gpx_from_db(db, "initial")));
  g_assert(true  == game_position_x_has_any_legal_move(get_gpx_from_db(db, "early-game-b-9-moves")));
  g_assert(false == game_position_x_has_any_legal_move(get_gpx_from_db(db, "black-has-to-pass")));
  g_assert(true  == game_position_x_has_any_legal_move(get_gpx_from_db(db, "early-game-c-12-moves")));
  g_assert(false == game_position_x_has_any_legal_move(get_gpx_from_db(db, "final-b37-w27")));
}

static void
game_position_has_any_player_any_legal_move_test (gpdb_fixture_t *fixture,
                                                  gconstpointer test_data)
{
  gpdb_dictionary_t *db = fixture->db;

  g_assert(true  == game_position_x_has_any_player_any_legal_move(get_gpx_from_db(db, "initial")));
  g_assert(true  == game_position_x_has_any_player_any_legal_move(get_gpx_from_db(db, "early-game-b-9-moves")));
  g_assert(true  == game_position_x_has_any_player_any_legal_move(get_gpx_from_db(db, "black-has-to-pass")));
  g_assert(true  == game_position_x_has_any_player_any_legal_move(get_gpx_from_db(db, "early-game-c-12-moves")));
  g_assert(false == game_position_x_has_any_player_any_legal_move(get_gpx_from_db(db, "final-b37-w27")));
}

static void
game_position_make_move_test (gpdb_fixture_t *fixture,
                              gconstpointer test_data)
{
  gpdb_dictionary_t *db = fixture->db;

  GamePositionX after_make_move_struct;
  GamePositionX *after_make_move = &after_make_move_struct;


  GamePositionX *initial = get_gpx_from_db(db, "initial");
  game_position_x_make_move(initial, D3, after_make_move);
  GamePositionX *first_move_d3 = get_gpx_from_db(db, "first-move-d3");
  g_assert(0 == game_position_x_compare(first_move_d3, after_make_move));


  GamePositionX *early_game_b_9_moves = get_gpx_from_db(db, "early-game-b-9-moves");
  game_position_x_make_move(early_game_b_9_moves, C3, after_make_move);
  GamePositionX *early_game_bc3_10_moves = get_gpx_from_db(db, "early-game-bc3-10-moves");
  g_assert(0 == game_position_x_compare(early_game_bc3_10_moves, after_make_move));

  game_position_x_make_move(early_game_b_9_moves, C6, after_make_move);
  GamePositionX *early_game_bc6_10_moves = get_gpx_from_db(db, "early-game-bc6-10-moves");
  g_assert(0 == game_position_x_compare(early_game_bc6_10_moves, after_make_move));


  GamePositionX *before;
  GamePositionX *expected;

  before = get_gpx_from_db(db, "make-move-test-case-a-before");
  game_position_x_make_move(before, D4, after_make_move);
  expected = get_gpx_from_db(db, "make-move-test-case-a-after");
  g_assert(0 == game_position_x_compare(expected, after_make_move));

  before = get_gpx_from_db(db, "make-move-test-case-b-before");
  game_position_x_make_move(before, D4, after_make_move);
  expected = get_gpx_from_db(db, "make-move-test-case-b-after");
  g_assert(0 == game_position_x_compare(expected, after_make_move));

  before = get_gpx_from_db(db, "make-move-test-case-c-before");
  game_position_x_make_move(before, D4, after_make_move);
  expected = get_gpx_from_db(db, "make-move-test-case-c-after");
  g_assert(0 == game_position_x_compare(expected, after_make_move));

  before = get_gpx_from_db(db, "make-move-test-case-d-before");
  game_position_x_make_move(before, B4, after_make_move);
  expected = get_gpx_from_db(db, "make-move-test-case-d-after");
  g_assert(0 == game_position_x_compare(expected, after_make_move));
}



/*
 * Internal functions.
 */

static void
gpdb_fixture_setup (gpdb_fixture_t *fixture,
                    gconstpointer test_data)
{
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

  g_assert(0 == gpdb_syntax_err_log_length(elog));

  gpdb_syntax_err_log_free(elog);

  fixture->db = db;
}

static void
gpdb_fixture_teardown (gpdb_fixture_t *fixture,
                       gconstpointer test_data)
{
  g_assert(fixture->db != NULL);
  gpdb_dictionary_free(fixture->db);
}

static GamePositionX *
get_gpx_from_db (gpdb_dictionary_t *db,
                 gchar *id)
{
  gpdb_entry_t *entry = gpdb_dictionary_find_entry(db, id);
  if (!entry) {
    g_test_message("The entry \"%s\" is missing from game position database.\n", id);
    g_test_fail();
  }
  g_assert(entry);
  return gpdb_entry_get_gpx(entry);
}
