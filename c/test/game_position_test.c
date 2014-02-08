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

#include <stdlib.h>
#include <stdio.h>

#include <glib.h>

#include "board.h"
#include "game_position_db.h"


/* GamePositionDb fixture */
typedef struct {
  GamePositionDb *db;
} GamePositionDbFixture;



/* Test function prototypes. */

static void
game_position_legal_moves_test (GamePositionDbFixture *fixture,
                                gconstpointer          test_data);

static void
game_position_make_move_test (GamePositionDbFixture *fixture,
                              gconstpointer          test_data);

static void
game_position_has_any_legal_move_test (GamePositionDbFixture *fixture,
                                       gconstpointer          test_data);

static void
game_position_has_any_player_any_legal_move_test (GamePositionDbFixture *fixture,
                                       gconstpointer          test_data);



/* Helper function prototypes. */

static void
gpdb_fixture_setup (GamePositionDbFixture *fixture,
                    gconstpointer          test_data);

static void
gpdb_fixture_teardown (GamePositionDbFixture *fixture,
                       gconstpointer          test_data);

static GamePosition *
get_gp_from_db (GamePositionDb *db, gchar *id);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  board_module_init();

  g_test_add ("/game_position/game_position_legal_moves_test",
              GamePositionDbFixture,
              (gconstpointer) NULL,
              gpdb_fixture_setup,
              game_position_legal_moves_test,
              gpdb_fixture_teardown);

  g_test_add ("/game_position/game_position_has_any_legal_move_test",
              GamePositionDbFixture,
              (gconstpointer) NULL,
              gpdb_fixture_setup,
              game_position_has_any_legal_move_test,
              gpdb_fixture_teardown);

  g_test_add ("/game_position/game_position_has_any_player_any_legal_move_test",
              GamePositionDbFixture,
              (gconstpointer) NULL,
              gpdb_fixture_setup,
              game_position_has_any_player_any_legal_move_test,
              gpdb_fixture_teardown);

  g_test_add ("/game_position/game_position_make_move_test",
              GamePositionDbFixture,
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
game_position_legal_moves_test (GamePositionDbFixture *fixture,
                                gconstpointer          test_data)
{
  gchar *legal_moves_to_string;

  GamePositionDb *db = fixture->db;

  legal_moves_to_string = square_set_to_string(game_position_legal_moves(get_gp_from_db(db, "initial")));
  g_assert_cmpstr("D3 C4 F5 E6", ==, legal_moves_to_string);
  g_free(legal_moves_to_string);

  legal_moves_to_string = square_set_to_string(game_position_legal_moves(get_gp_from_db(db, "early-game-b-9-moves")));
  g_assert_cmpstr("C3 C6", ==, legal_moves_to_string);
  g_free(legal_moves_to_string);

  legal_moves_to_string = square_set_to_string(game_position_legal_moves(get_gp_from_db(db, "black-has-to-pass")));
  g_assert_cmpstr("", ==, legal_moves_to_string);
  g_free(legal_moves_to_string);

  legal_moves_to_string = square_set_to_string(game_position_legal_moves(get_gp_from_db(db, "early-game-c-12-moves")));
  g_assert_cmpstr("H2 A4 C4 G4 A5 F5 B6 E6 G7", ==, legal_moves_to_string);
  g_free(legal_moves_to_string);

  legal_moves_to_string = square_set_to_string(game_position_legal_moves(get_gp_from_db(db, "final-b37-w27")));
  g_assert_cmpstr("", ==, legal_moves_to_string);
  g_free(legal_moves_to_string);

}

static void
game_position_has_any_legal_move_test (GamePositionDbFixture *fixture,
                                       gconstpointer          test_data)
{
  GamePositionDb *db = fixture->db;

  g_assert(TRUE  == game_position_has_any_legal_move(get_gp_from_db(db, "initial")));
  g_assert(TRUE  == game_position_has_any_legal_move(get_gp_from_db(db, "early-game-b-9-moves")));
  g_assert(FALSE == game_position_has_any_legal_move(get_gp_from_db(db, "black-has-to-pass")));
  g_assert(TRUE  == game_position_has_any_legal_move(get_gp_from_db(db, "early-game-c-12-moves")));
  g_assert(FALSE == game_position_has_any_legal_move(get_gp_from_db(db, "final-b37-w27")));
}

static void
game_position_has_any_player_any_legal_move_test (GamePositionDbFixture *fixture,
                                                  gconstpointer          test_data)
{
  GamePositionDb *db = fixture->db;

  g_assert(TRUE  == game_position_has_any_player_any_legal_move(get_gp_from_db(db, "initial")));
  g_assert(TRUE  == game_position_has_any_player_any_legal_move(get_gp_from_db(db, "early-game-b-9-moves")));
  g_assert(TRUE  == game_position_has_any_player_any_legal_move(get_gp_from_db(db, "black-has-to-pass")));
  g_assert(TRUE  == game_position_has_any_player_any_legal_move(get_gp_from_db(db, "early-game-c-12-moves")));
  g_assert(FALSE == game_position_has_any_player_any_legal_move(get_gp_from_db(db, "final-b37-w27")));
}

static void
game_position_make_move_test (GamePositionDbFixture *fixture,
                              gconstpointer          test_data)
{
  GamePositionDb *db = fixture->db;

  GamePosition *after_make_move;


  GamePosition *initial = get_gp_from_db(db, "initial");
  after_make_move = game_position_make_move(initial, D3);
  GamePosition *first_move_d3 = get_gp_from_db(db, "first-move-d3");
  g_assert(0 == game_position_compare(first_move_d3, after_make_move));
  game_position_free(after_make_move);


  GamePosition *early_game_b_9_moves = get_gp_from_db(db, "early-game-b-9-moves");
  after_make_move = game_position_make_move(early_game_b_9_moves, C3);
  GamePosition *early_game_bc3_10_moves = get_gp_from_db(db, "early-game-bc3-10-moves");
  g_assert(0 == game_position_compare(early_game_bc3_10_moves, after_make_move));
  game_position_free(after_make_move);

  after_make_move = game_position_make_move(early_game_b_9_moves, C6);
  GamePosition *early_game_bc6_10_moves = get_gp_from_db(db, "early-game-bc6-10-moves");
  g_assert(0 == game_position_compare(early_game_bc6_10_moves, after_make_move));
  game_position_free(after_make_move);


  GamePosition *before;
  GamePosition *expected;

  before = get_gp_from_db(db, "make-move-test-case-a-before");
  after_make_move = game_position_make_move(before, D4);
  expected = get_gp_from_db(db, "make-move-test-case-a-after");
  g_assert(0 == game_position_compare(expected, after_make_move));
  game_position_free(after_make_move);

  before = get_gp_from_db(db, "make-move-test-case-b-before");
  after_make_move = game_position_make_move(before, D4);
  expected = get_gp_from_db(db, "make-move-test-case-b-after");
  g_assert(0 == game_position_compare(expected, after_make_move));
  game_position_free(after_make_move);

  before = get_gp_from_db(db, "make-move-test-case-c-before");
  after_make_move = game_position_make_move(before, D4);
  expected = get_gp_from_db(db, "make-move-test-case-c-after");
  g_assert(0 == game_position_compare(expected, after_make_move));
  game_position_free(after_make_move);

  before = get_gp_from_db(db, "make-move-test-case-d-before");
  after_make_move = game_position_make_move(before, B4);
  expected = get_gp_from_db(db, "make-move-test-case-d-after");
  g_assert(0 == game_position_compare(expected, after_make_move));
  game_position_free(after_make_move);
}



/*
 * Internal functions.
 */

static void
gpdb_fixture_setup (GamePositionDbFixture *fixture,
                    gconstpointer          test_data)
{
  gchar *source = g_strdup("db/gpdb-sample-games.txt");

  GamePositionDb               *db;
  GamePositionDbSyntaxErrorLog *syntax_error_log;
  FILE                         *fp;
  GError                       *error;

  /* Loads the game position database. */
  fp = fopen(source, "r");
  if (!fp) {
    printf("Unable to open database test file \"%s\" for reading.\n", source);
    g_test_fail();
  }
  db = gpdb_new(g_strdup("Testing Database"));
  syntax_error_log = NULL;
  error = NULL;
  gpdb_load(fp, source, db, &syntax_error_log, &error);
  fclose(fp);
  g_free(source);

  /* Removes the tmp file, frees the resources. */
  g_free(error);
  if (syntax_error_log)
    gpdb_syntax_error_log_free(syntax_error_log);

  fixture->db = db;
  g_assert (fixture->db != NULL);
}

static void
gpdb_fixture_teardown (GamePositionDbFixture *fixture,
                       gconstpointer          test_data)
{
  g_assert(fixture->db != NULL);
  gpdb_free(fixture->db, TRUE);
}

GamePosition *
get_gp_from_db (GamePositionDb *db, gchar *id)
{
  GamePositionDbEntry *entry = gpdb_lookup(db, id);
  if (!entry) {
    g_test_message("The entry \"%s\" is missing from game position database.\n", id);
    g_test_fail();
  }
  g_assert(entry);
  return entry->game_position;
}
