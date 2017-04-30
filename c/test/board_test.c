/**
 * @file
 *
 * @brief Board unit test suite.
 * @details Collects tests and helper methods for the board module.
 *
 * @par board_test.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014, 2016, 2017 Roberto Corradini. All rights reserved.
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
#include <inttypes.h>

#include <glib.h>

#include "board.h"



/* Test function prototypes. */

static void dummy_test (void);

static void square_to_string_test (void);
static void square_as_move_to_string_test (void);
static void square_array_to_string_test (void);
static void square_as_move_array_to_string_test (void);
static void square_belongs_to_enum_set_test (void);
static void square_is_valid_move_test (void);

static void square_set_to_pg_json_array_test (void);
static void square_set_to_string_test (void);
static void square_set_random_selection_test (void);
static void square_set_to_array_test (void);
static void square_set_from_array_test (void);

static void player_color_test (void);
static void player_description_test (void);
static void player_opponent_test (void);

static void game_position_x_empties_test (void);
static void game_position_x_get_player_test (void);
static void game_position_x_get_opponent_test (void);
static void game_position_x_legal_moves_test (void);
static void game_position_x_count_difference_test (void);
static void game_position_x_print_test (void);
static void game_position_x_to_string_test (void);
static void game_position_x_get_square_test (void);
static void game_position_x_compare_test (void);
static void game_position_x_clone_test (void);
static void game_position_x_copy_test (void);
static void game_position_x_pass_test (void);
static void game_position_x_hash_test (void);
static void game_position_x_delta_hash_test (void);
static void game_position_x_final_value_test (void);
static void game_position_x_has_any_legal_move_test (void);
static void game_position_x_has_any_player_any_legal_move_test (void);
static void game_position_x_is_move_legal_test (void);
static void game_position_x_make_move_test (void);

int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  board_module_init();

  g_test_add_func("/board/dummy_test", dummy_test);

  g_test_add_func("/board/square_to_string_test", square_to_string_test);
  g_test_add_func("/board/square_as_move_to_string_test", square_as_move_to_string_test);
  g_test_add_func("/board/square_array_to_string_test", square_array_to_string_test);
  g_test_add_func("/board/square_as_move_array_to_string_test", square_as_move_array_to_string_test);
  g_test_add_func("/board/square_belongs_to_enum_set_test", square_belongs_to_enum_set_test);
  g_test_add_func("/board/square_is_valid_move_test", square_is_valid_move_test);

  g_test_add_func("/board/square_set_to_pg_json_array_test", square_set_to_pg_json_array_test);
  g_test_add_func("/board/square_set_to_string_test", square_set_to_string_test);
  g_test_add_func("/board/square_set_random_selection_test", square_set_random_selection_test);
  g_test_add_func("/board/square_set_to_array_test", square_set_to_array_test);
  g_test_add_func("/board/square_set_from_array_test", square_set_from_array_test);

  g_test_add_func("/board/player_color_test", player_color_test);
  g_test_add_func("/board/player_description_test", player_description_test);
  g_test_add_func("/board/player_opponent_test", player_opponent_test);

  g_test_add_func("/board/game_position_x_empties_test", game_position_x_empties_test);
  g_test_add_func("/board/game_position_x_get_player_test", game_position_x_get_player_test);
  g_test_add_func("/board/game_position_x_get_opponent_test", game_position_x_get_opponent_test);
  g_test_add_func("/board/game_position_x_legal_moves_test", game_position_x_legal_moves_test);
  g_test_add_func("/board/game_position_x_count_difference_test", game_position_x_count_difference_test);
  g_test_add_func("/board/game_position_x_to_string_test", game_position_x_to_string_test);
  g_test_add_func("/board/game_position_x_print_test", game_position_x_print_test);
  g_test_add_func("/board/game_position_x_get_square_test", game_position_x_get_square_test);
  g_test_add_func("/board/game_position_x_compare_test", game_position_x_compare_test);
  g_test_add_func("/board/game_position_x_clone_test", game_position_x_clone_test);
  g_test_add_func("/board/game_position_x_copy_test", game_position_x_copy_test);
  g_test_add_func("/board/game_position_x_pass_test", game_position_x_pass_test);
  g_test_add_func("/board/game_position_x_hash_test", game_position_x_hash_test);
  g_test_add_func("/board/game_position_x_delta_hash_test", game_position_x_delta_hash_test);
  g_test_add_func("/board/game_position_x_final_value_test", game_position_x_final_value_test);
  g_test_add_func("/board/game_position_x_has_any_legal_move_test", game_position_x_has_any_legal_move_test);
  g_test_add_func("/board/game_position_x_has_any_player_any_legal_move_test", game_position_x_has_any_player_any_legal_move_test);
  g_test_add_func("/board/game_position_x_is_move_legal_test", game_position_x_is_move_legal_test);
  g_test_add_func("/board/game_position_x_make_move_test", game_position_x_make_move_test);

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



/*************************************/
/* Unit tests for the Square entity. */
/*************************************/

static void
square_to_string_test (void)
{
  //! [square_to_string usage]
  g_assert_cmpstr("D5", ==, square_to_string(D5));
  g_assert_cmpstr("A1", ==, square_to_string(A1));
  g_assert_cmpstr("H8", ==, square_to_string(H8));
  g_assert_cmpstr("NA", ==, square_to_string((Square) invalid_move));
  //! [square_to_string usage]
}

static void
square_as_move_to_string_test (void)
{
  //! [square_as_move_to_string usage]
  g_assert_cmpstr("--", ==, square_as_move_to_string(pass_move));
  g_assert_cmpstr("D5", ==, square_as_move_to_string(D5));
  //! [square_as_move_to_string usage]
}

static void
square_array_to_string_test (void)
{
  //! [square_array_to_string usage]
  const Square sqa[] = {A1, D7, H8};
  const int length = 3;
  char *expected = square_array_to_string(sqa, length);
  g_assert_cmpstr(expected, ==, "A1 D7 H8");
  g_free(expected);
  //! [square_array_to_string usage]
}

static void
square_as_move_array_to_string_test (void)
{
  //! [square_as_move_array_to_string usage]
  const Square mova[] = {A1, D7, H8, pass_move};
  const int length = 4;
  char *expected = square_as_move_array_to_string(mova, length);
  g_assert_cmpstr(expected, ==, "A1 D7 H8 --");
  g_free(expected);
  //! [square_as_move_array_to_string usage]
}

static void
square_belongs_to_enum_set_test (void)
{
  //! [square_belongs_to_enum_set usage]
  g_assert(square_belongs_to_enum_set(A1));
  g_assert(square_belongs_to_enum_set(D3));
  g_assert(square_belongs_to_enum_set(H8));
  g_assert(!square_belongs_to_enum_set(invalid_square));
  //! [square_belongs_to_enum_set usage]
}

static void
square_is_valid_move_test (void)
{
  //! [square_is_valid_move usage]
  g_assert(square_is_valid_move(A1));
  g_assert(square_is_valid_move(D3));
  g_assert(square_is_valid_move(H8));
  g_assert(square_is_valid_move(pass_move));
  g_assert(!square_is_valid_move(invalid_move));
  //! [square_is_valid_move usage]
}



/****************************************/
/* Unit tests for the SquareSet entity. */
/****************************************/

static void
square_set_to_pg_json_array_test (void)
{
  //! [square_set_to_pg_json_array usage]
  size_t length;
  char pg_json_string[513];
  length = square_set_to_pg_json_array(pg_json_string, (SquareSet) 0);
  g_assert_cmpstr(pg_json_string, ==, "[]");
  g_assert(length == 2);
  length = square_set_to_pg_json_array(pg_json_string, (SquareSet) 5);
  g_assert_cmpstr(pg_json_string, ==, "[\"\"A1\"\", \"\"C1\"\"]");
  g_assert(length == 16);
  //! [square_set_to_pg_json_array usage]
  length = square_set_to_pg_json_array(pg_json_string, (SquareSet) 0xFFFFFFFFFFFFFFFF);
  g_assert(length == 512);
}

static void
square_set_to_string_test (void)
{
  //! [square_set_to_string usage]
  char *ss_to_string;
  ss_to_string = square_set_to_string((SquareSet) 0);
  g_assert_cmpstr(ss_to_string, ==, "");
  g_free(ss_to_string);
  ss_to_string = square_set_to_string((SquareSet) 5);
  g_assert_cmpstr(ss_to_string, ==, "A1 C1");
  g_free(ss_to_string);
  //! [square_set_to_string usage]
}

static void
square_set_random_selection_test (void)
{
  //! [square_set_random_selection usage]
  const unsigned int seed = 1739;
  prng_mt19937_t *prng = prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, seed);
  const SquareSet squares = (SquareSet) 7; // 7 is {A1, B1, C1}
  const Square sq = square_set_random_selection(prng, squares);
  g_assert(sq == A1 || sq == B1 || sq == C1);
  prng_mt19937_free(prng);
  //! [square_set_random_selection usage]

  prng_mt19937_t *prng1 = prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, 17598);

  const int max_iteration_count = 1000000;
  unsigned long int observations[square_cardinality];
  for (int i = 0; i < square_cardinality; i++) {
    observations[i] = 0;
  }
  for (int j = 0; j < max_iteration_count; j++) {
    const Square sq = square_set_random_selection(prng1, (SquareSet) 0xFFFFFFFFFFFFFFFF);
    g_assert(sq >= A1 || sq <= H8);
    observations[sq]++;
    int touched_count = 0;
    for (int i = 0; i < square_cardinality; i++) {
      if (observations[i] != 0) touched_count++;
    }
    if (touched_count == 64) goto out;
  }
  g_assert(FALSE);
 out:

  for (int i = 0; i < 1000; i++) {
    const Square sq = square_set_random_selection(prng1, (SquareSet) 5);
    g_assert(sq == A1 || sq == C1);
  }

  for (int i = 0; i < 100; i++) {
    const Square sq = square_set_random_selection(prng1, (SquareSet) 2);
    g_assert(sq == B1);
  }

  prng_mt19937_free(prng1);
}

static void
square_set_to_array_test (void)
{
  //! [square_set_to_array usage]
  int sq_count;
  Square *sq_array;
  square_set_to_array(&sq_count,
                      &sq_array,
                      (SquareSet) 5);
  g_assert(sq_count == 2);
  g_assert(sq_array[0] == A1);
  g_assert(sq_array[1] == C1);
  free(sq_array);
  //! [square_set_to_array usage]

  square_set_to_array(&sq_count,
                      &sq_array,
                      (SquareSet) 0);
  g_assert(sq_count == 0);
  free(sq_array);

  square_set_to_array(&sq_count,
                      &sq_array,
                      (SquareSet) 1);
  g_assert(sq_count == 1);
  g_assert(sq_array[0] == A1);
  free(sq_array);

  square_set_to_array(&sq_count,
                      &sq_array,
                      (SquareSet) 0xFFFFFFFFFFFFFFFF);
  g_assert(sq_count == 64);
  for (int i = 0; i < 64; i++) {
    g_assert_cmpint(sq_array[i], ==, (Square) i);
  }
  free(sq_array);
}

static void
square_set_from_array_test (void)
{
  //! [square_set_from_array usage]
  const Square sq_array[] = {A1, C1};
  const int sq_count = 2;
  const SquareSet computed = square_set_from_array(sq_array, sq_count);
  g_assert_cmpint(computed, ==, (SquareSet) 5);
  //! [square_set_from_array usage]

  const Square sq_array_1[] = {A1, C1, A1};
  const int sq_count_1 = 3;
  const SquareSet computed_1 = square_set_from_array(sq_array_1, sq_count_1);
  g_assert_cmpint(computed_1, ==, (SquareSet) 5);

  const Square *sq_array_2 = NULL;
  const int sq_count_2 = 0;
  const SquareSet computed_2 = square_set_from_array(sq_array_2, sq_count_2);
  g_assert_cmpint(computed_2, ==, (SquareSet) 0);
}



/*************************************/
/* Unit tests for the Player entity. */
/*************************************/

static void
player_color_test (void)
{
  //! [player_color usage]
  g_assert(BLACK_SQUARE == player_color(BLACK_PLAYER));
  g_assert(WHITE_SQUARE == player_color(WHITE_PLAYER));
  //! [player_color usage]
}

static void
player_description_test (void)
{
  //! [player_description usage]
  g_assert(g_strcmp0(player_description(BLACK_PLAYER), "The Black player") == 0);
  g_assert(g_strcmp0(player_description(WHITE_PLAYER), "The White player") == 0);
  //! [player_description usage]
}

static void
player_opponent_test (void)
{
  //! [player_opponent usage]
  g_assert(BLACK_PLAYER == player_opponent(WHITE_PLAYER));
  g_assert(WHITE_PLAYER == player_opponent(BLACK_PLAYER));
  //! [player_opponent usage]
}



/********************************************/
/* Unit tests for the GamePositionX entity. */
/********************************************/

static void
game_position_x_print_test (void)
{
  GamePositionX gpx_struct = {
    .blacks = 1,
    .whites = 4,
    .player = WHITE_PLAYER
  };
  GamePositionX *gpx = &gpx_struct;

  char *gpx_to_string = game_position_x_print(gpx);

  GString *expected = g_string_sized_new(220);

  g_string_append(expected, "    a b c d e f g h \n");
  g_string_append(expected, " 1  @ . O . . . . . \n");
  g_string_append(expected, " 2  . . . . . . . . \n");
  g_string_append(expected, " 3  . . . . . . . . \n");
  g_string_append(expected, " 4  . . . . . . . . \n");
  g_string_append(expected, " 5  . . . . . . . . \n");
  g_string_append(expected, " 6  . . . . . . . . \n");
  g_string_append(expected, " 7  . . . . . . . . \n");
  g_string_append(expected, " 8  . . . . . . . . \n");
  g_string_append(expected, "Player to move: WHITE\n");

  g_assert(g_strcmp0(expected->str, gpx_to_string) == 0);

  g_free(gpx_to_string);
  g_string_free(expected, TRUE);
}

static void
game_position_x_empties_test (void)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x0000000000000000,
                            0x0000000000000000,
                            BLACK_PLAYER);
  g_assert(0xFFFFFFFFFFFFFFFF == game_position_x_empties(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000000,
                            0x0000000000000001,
                            BLACK_PLAYER);
  g_assert(0xFFFFFFFFFFFFFFFE == game_position_x_empties(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0xFF00000000000001,
                            0x00FF000000000002,
                            WHITE_PLAYER);
  g_assert(0x0000FFFFFFFFFFFC == game_position_x_empties(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_get_player_test (void)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x00000000000000FF,
                            0x000000000000FF00,
                            BLACK_PLAYER);
  g_assert(0x00000000000000FF == game_position_x_get_player(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x00000000000000FF,
                            0x000000000000FF00,
                            WHITE_PLAYER);
  g_assert(0x000000000000FF00 == game_position_x_get_player(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_get_opponent_test (void)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x00000000000000FF,
                            0x000000000000FF00,
                            BLACK_PLAYER);
  g_assert(0x000000000000FF00 == game_position_x_get_opponent(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x00000000000000FF,
                            0x000000000000FF00,
                            WHITE_PLAYER);
  g_assert(0x00000000000000FF == game_position_x_get_opponent(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_legal_moves_test (void)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x0000000000000001,
                            0x0040201008040200,
                            BLACK_PLAYER);
  g_assert(0x8000000000000000 == game_position_x_legal_moves(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_count_difference_test (void)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x00000000000000FF,
                            0x000000000000FF00,
                            BLACK_PLAYER);
  g_assert(0 == game_position_x_count_difference(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x00000000000000FF,
                            0x000000000000FF00,
                            WHITE_PLAYER);
  g_assert(0 == game_position_x_count_difference(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000000,
                            0x000000000000FF00,
                            BLACK_PLAYER);
  g_assert(-8 == game_position_x_count_difference(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000000,
                            0x000000000000FF00,
                            WHITE_PLAYER);
  g_assert(+8 == game_position_x_count_difference(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_to_string_test (void)
{
  GamePositionX *gpx;
  char gpx_to_string[66];

  gpx = game_position_x_new(0x00000000000000FF,
                            0x000000000000FF00,
                            BLACK_PLAYER);
  game_position_x_to_string(gpx, gpx_to_string);
  g_assert(g_strcmp0("bbbbbbbbwwwwwwww................................................b",
                     gpx_to_string) == 0);
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x00000000000000FF,
                            0xFF0000000000FF00,
                            WHITE_PLAYER);
  game_position_x_to_string(gpx, gpx_to_string);
  g_assert(g_strcmp0("bbbbbbbbwwwwwwww........................................wwwwwwwww",
                     gpx_to_string) == 0);
  game_position_x_free(gpx);
}

static void
game_position_x_get_square_test (void)
{
  GamePositionX *gpx;
  gpx = game_position_x_new(0x0000000000000001,
                            0x0000000000000002,
                            BLACK_PLAYER);

  g_assert(BLACK_SQUARE == game_position_x_get_square(gpx, A1));
  g_assert(WHITE_SQUARE == game_position_x_get_square(gpx, B1));
  g_assert(EMPTY_SQUARE == game_position_x_get_square(gpx, C1));

  game_position_x_free(gpx);
}

static void
game_position_x_compare_test (void)
{
  GamePositionX *a;
  GamePositionX *b;

  a = game_position_x_new(0x0000000000000001,
                          0x0000000000000002,
                          BLACK_PLAYER);
  b = game_position_x_new(0x0000000000000001,
                          0x0000000000000002,
                          BLACK_PLAYER);
  g_assert(0 == game_position_x_compare(a, b));
  game_position_x_free(a);
  game_position_x_free(b);

  a = game_position_x_new(0x0000000000000001,
                          0x0000000000000002,
                          BLACK_PLAYER);
  b = game_position_x_new(0x0000000000000001,
                          0x0000000000000002,
                          WHITE_PLAYER);
  g_assert(-1 == game_position_x_compare(a, b));
  game_position_x_free(a);
  game_position_x_free(b);

  a = game_position_x_new(0x0000000000000001,
                          0x0000000000000002,
                          WHITE_PLAYER);
  b = game_position_x_new(0x0000000000000001,
                          0x0000000000000002,
                          BLACK_PLAYER);
  g_assert(+1 == game_position_x_compare(a, b));
  game_position_x_free(a);
  game_position_x_free(b);

  a = game_position_x_new(0x0000000000000001,
                          0x0000000000000002,
                          BLACK_PLAYER);
  b = game_position_x_new(0x0000000000000004,
                          0x0000000000000002,
                          BLACK_PLAYER);
  g_assert(-1 == game_position_x_compare(a, b));
  game_position_x_free(a);
  game_position_x_free(b);
}

static void
game_position_x_clone_test (void)
{
  GamePositionX *a;
  GamePositionX *b;
  a = game_position_x_new(0x0000000000000001,
                          0x0000000000000002,
                          BLACK_PLAYER);
  b = game_position_x_clone(a);
  g_assert(0 == game_position_x_compare(a, b));
  game_position_x_free(a);
  game_position_x_free(b);
}

static void
game_position_x_copy_test (void)
{
  GamePositionX *a;
  GamePositionX *b;
  a = game_position_x_new(0x0000000000000001,
                          0x0000000000000002,
                          BLACK_PLAYER);
  b = game_position_x_new(0x00000000000000FF,
                          0x0000000000000000,
                          WHITE_PLAYER);
  g_assert(0 != game_position_x_compare(a, b));
  game_position_x_copy(a, b);
  g_assert(0 == game_position_x_compare(a, b));
}

static void
game_position_x_pass_test (void)
{
  GamePositionX *current;
  GamePositionX *next;

  current = game_position_x_new(0x0000000000000001,
                                0x0000000000000002,
                                BLACK_PLAYER);

  next = game_position_x_new(0x0000000000000000,
                             0x0000000000000000,
                             BLACK_PLAYER);

  game_position_x_pass(current, next);

  g_assert(0x0000000000000001 == next->blacks);
  g_assert(0x0000000000000002 == next->whites);
  g_assert(WHITE_PLAYER == next->player);

  game_position_x_free(current);
  game_position_x_free(next);
}

static void
game_position_x_hash_test (void)
{
  GamePositionX *gpx;
  SquareSet      blacks;
  SquareSet      whites;
  Player         player;
  uint64_t       expected;

  expected = 0;
  blacks = 0x0000000000000000;
  whites = 0x0000000000000000;
  player = BLACK_PLAYER;
  gpx = game_position_x_new(blacks, whites, player);
  g_assert(expected == game_position_x_hash(gpx));
  game_position_x_free(gpx);

  expected = 0xFFFFFFFFFFFFFFFF;
  blacks = 0x0000000000000000;
  whites = 0x0000000000000000;
  player = WHITE_PLAYER;
  gpx = game_position_x_new(blacks, whites, player);
  g_assert(expected == game_position_x_hash(gpx));
  game_position_x_free(gpx);

  expected = 0x4689879C5E2B6C8D ^ 0x1C10E0B05C7B3C49;
  blacks = 0x0000000000000002;
  whites = 0x0000000000000004;
  player = BLACK_PLAYER;
  gpx = game_position_x_new(blacks, whites, player);
  g_assert(expected == game_position_x_hash(gpx));
  game_position_x_free(gpx);

  /*
   * The hash of two game position A, and B having the same board but different player satisfy
   * this property:
   * hash(A) & hash(B) == 0;
   */
  player = WHITE_PLAYER;
  gpx = game_position_x_new(blacks, whites, player);
  g_assert(~expected == game_position_x_hash(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_delta_hash_test (void)
{
  const GamePositionX a = { 4ULL, 2ULL, BLACK_PLAYER};
  const GamePositionX b = { 7ULL, 0ULL, WHITE_PLAYER};
  const uint64_t hash_a = game_position_x_hash(&a);
  const uint64_t hash_b = game_position_x_hash(&b);

  const Square flips[] = { A1, B1 };
  const int flip_count = 2;

  const uint64_t computed = game_position_x_delta_hash(hash_a, flips, flip_count, b.player);
  const uint64_t expected =hash_b;

  g_assert(expected == computed);
}

static void
game_position_x_final_value_test (void)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0xFFFFFFFFFFFFFFFF,
                            0x0000000000000000,
                            BLACK_PLAYER);
  g_assert(64 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000001,
                            0x0000000000000002,
                            BLACK_PLAYER);
  g_assert(0 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000001,
                            0x0000000000000002,
                            WHITE_PLAYER);
  g_assert(0 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000001,
                            0x0000000000000000,
                            BLACK_PLAYER);
  g_assert(64 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000001,
                            0x0000000000000000,
                            WHITE_PLAYER);
  g_assert(-64 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x00FF000000000001,
                            0xFF00000000000000,
                            BLACK_PLAYER);
  g_assert(48 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_has_any_legal_move_test (void)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x8000000000000000,
                            WHITE_PLAYER);
  g_assert(TRUE == game_position_x_has_any_legal_move(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0xFFFFFFFFFFFFFFFE,
                            0x0000000000000000,
                            WHITE_PLAYER);
  g_assert(FALSE == game_position_x_has_any_legal_move(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_has_any_player_any_legal_move_test (void)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x8000000000000000,
                            WHITE_PLAYER);
  g_assert(TRUE == game_position_x_has_any_player_any_legal_move(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x8000000000000000,
                            BLACK_PLAYER);
  g_assert(TRUE == game_position_x_has_any_player_any_legal_move(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0xFFFFFFFFFFFFFFFE,
                            0x0000000000000000,
                            WHITE_PLAYER);
  g_assert(FALSE == game_position_x_has_any_player_any_legal_move(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0xFFFFFFFFFFFFFFFE,
                            0x0000000000000000,
                            BLACK_PLAYER);
  g_assert(FALSE == game_position_x_has_any_player_any_legal_move(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_is_move_legal_test (void)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x8000000000000000,
                            WHITE_PLAYER);
  g_assert(TRUE == game_position_x_is_move_legal(gpx, 0));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x8000000000000000,
                            WHITE_PLAYER);
  g_assert(FALSE == game_position_x_is_move_legal(gpx, 1));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x0000000000000000,
                            WHITE_PLAYER);
  g_assert(FALSE == game_position_x_is_move_legal(gpx, 0));
  game_position_x_free(gpx);
}

static void
game_position_x_make_move_test (void)
{
  GamePositionX *current;
  GamePositionX *updated;
  GamePositionX *expected;

  current = game_position_x_new(0x0000000000000002,
                                0x0000000000000004,
                                WHITE_PLAYER);

  updated = game_position_x_new(0x0000000000000000,
                                0x0000000000000000,
                                WHITE_PLAYER);

  expected = game_position_x_new(0x0000000000000000,
                                 0x0000000000000007,
                                 BLACK_PLAYER);

  g_assert(TRUE == game_position_x_is_move_legal(current, 0));
  game_position_x_make_move(current, 0, updated);
  g_assert(0 == game_position_x_compare(expected, updated));

  game_position_x_free(current);
  game_position_x_free(updated);
  game_position_x_free(expected);
}
