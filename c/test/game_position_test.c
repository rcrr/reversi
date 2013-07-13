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



/* Test function prototypes. */

static void dummy_test (void);

static void game_position_legal_moves_test (void);
static void game_position_make_move_test (void);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  board_module_init();

  g_test_add_func("/board/dummy_test", dummy_test);

  g_test_add_func("/board/game_position_legal_moves_test", game_position_legal_moves_test);
  g_test_add_func("/board/game_position_make_move_test", game_position_make_move_test);

  return g_test_run();
}



/*
 * Test functions.
 */


static void
dummy_test (void)
{
  g_assert(TRUE);
}

static void
game_position_make_move_test (void)
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

  GamePositionDbEntry *entry;


  entry = gpdb_lookup(db, "initial");
  GamePosition *after_make_move = game_position_make_move(entry->game_position, D3);
  entry = gpdb_lookup(db, "first-move-d3");
  g_assert(0 == game_position_compare(after_make_move, entry->game_position));
  game_position_free(after_make_move);


  gpdb_free(db, TRUE);
}

static void
game_position_legal_moves_test (void)
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

  GamePositionDbEntry *entry;
  SquareSet            legal_moves;
  gchar               *legal_moves_to_string;

  entry = gpdb_lookup(db, "initial");
  legal_moves = game_position_legal_moves(entry->game_position);
  legal_moves_to_string = square_set_print_as_moves(legal_moves);
  g_assert(g_strcmp0("D3 C4 F5 E6", legal_moves_to_string) == 0);
  g_assert(TRUE == game_position_has_any_legal_move(entry->game_position));
  g_assert(TRUE == game_position_has_any_player_any_legal_move(entry->game_position));
  g_free(legal_moves_to_string);

  entry = gpdb_lookup(db, "early-game-b-9-moves");
  legal_moves = game_position_legal_moves(entry->game_position);
  legal_moves_to_string = square_set_print_as_moves(legal_moves);
  g_assert(g_strcmp0("C3 C6", legal_moves_to_string) == 0);
  g_assert(TRUE == game_position_has_any_legal_move(entry->game_position));
  g_assert(TRUE == game_position_has_any_player_any_legal_move(entry->game_position));
  g_free(legal_moves_to_string);

  entry = gpdb_lookup(db, "black-has-to-pass");
  legal_moves = game_position_legal_moves(entry->game_position);
  legal_moves_to_string = square_set_print_as_moves(legal_moves);
  g_assert(g_strcmp0("", legal_moves_to_string) == 0);
  g_assert(FALSE == game_position_has_any_legal_move(entry->game_position));
  g_assert(TRUE == game_position_has_any_player_any_legal_move(entry->game_position));
  g_free(legal_moves_to_string);

  entry = gpdb_lookup(db, "early-game-c-12-moves");
  legal_moves = game_position_legal_moves(entry->game_position);
  legal_moves_to_string = square_set_print_as_moves(legal_moves);
  g_assert(g_strcmp0("H2 A4 C4 G4 A5 F5 B6 E6 G7", legal_moves_to_string) == 0);
  g_assert(TRUE == game_position_has_any_legal_move(entry->game_position));
  g_assert(TRUE == game_position_has_any_player_any_legal_move(entry->game_position));
  g_free(legal_moves_to_string);

  entry = gpdb_lookup(db, "final-b37-w27");
  legal_moves = game_position_legal_moves(entry->game_position);
  legal_moves_to_string = square_set_print_as_moves(legal_moves);
  g_assert(g_strcmp0("", legal_moves_to_string) == 0);
  g_assert(FALSE == game_position_has_any_legal_move(entry->game_position));
  g_assert(FALSE == game_position_has_any_player_any_legal_move(entry->game_position));
  g_free(legal_moves_to_string);


  gpdb_free(db, TRUE);
}
