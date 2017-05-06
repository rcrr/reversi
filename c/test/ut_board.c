/**
 * @file
 *
 * @brief Board module unit test suite.
 * @details Collects tests and helper methods for the board module.
 *
 * @par ut_board.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2017 Roberto Corradini. All rights reserved.
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
#include <assert.h>
#include <string.h>

#include "unit_test.h"
#include "board.h"



/*
 * Auxiliary functions.
 */

static void
aux_dummy (void)
{
  return;
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
square_to_string_t (ut_test_t *const t)
{
  ut_assert(t, strcmp("D5", square_to_string(D5)) == 0);
  ut_assert(t, strcmp("A1", square_to_string(A1)) == 0);
  ut_assert(t, strcmp("H8", square_to_string(H8)) == 0);
  ut_assert(t, strcmp("NA", square_to_string((Square) invalid_move)) == 0);
}

static void
square_as_move_to_string_t (ut_test_t *const t)
{
  ut_assert(t, strcmp("--", square_as_move_to_string(pass_move)) == 0);
  ut_assert(t, strcmp("D5", square_as_move_to_string(D5)) == 0);
}

static void
square_array_to_string_t (ut_test_t *const t)
{
  char to_string[32];
  size_t s_length;
  const Square sqa[] = {A1, D7, H8};
  const int length = 3;
  s_length = square_array_to_string(to_string, sqa, length);
  ut_assert(t, strcmp("A1 D7 H8", to_string) == 0);
  ut_assert(t, 8 == s_length);
}

static void
square_as_move_array_to_string_t (ut_test_t *const t)
{
  char to_string[32];
  size_t s_length;
  const Square mova[] = {A1, D7, H8, pass_move};
  const int length = 4;
  s_length = square_as_move_array_to_string(to_string, mova, length);
  ut_assert(t, strcmp("A1 D7 H8 --", to_string) == 0);
  ut_assert(t, 11 == s_length);
}

static void
square_belongs_to_enum_set_t (ut_test_t *const t)
{
  ut_assert(t, square_belongs_to_enum_set(A1));
  ut_assert(t, square_belongs_to_enum_set(D3));
  ut_assert(t, square_belongs_to_enum_set(H8));
  ut_assert(t, !square_belongs_to_enum_set(invalid_square));
}

static void
square_is_valid_move_t (ut_test_t *const t)
{
  ut_assert(t, square_is_valid_move(A1));
  ut_assert(t, square_is_valid_move(D3));
  ut_assert(t, square_is_valid_move(H8));
  ut_assert(t, square_is_valid_move(pass_move));
  ut_assert(t, !square_is_valid_move(invalid_move));
}

static void
square_set_to_pg_json_array_t (ut_test_t *const t)
{
  size_t length;
  char pg_json_string[513];
  length = square_set_to_pg_json_array(pg_json_string, (SquareSet) 0);
  ut_assert(t, strcmp("[]", pg_json_string) == 0);
  ut_assert(t, 2 == length);
  length = square_set_to_pg_json_array(pg_json_string, (SquareSet) 5);
  ut_assert(t, strcmp("[\"\"A1\"\", \"\"C1\"\"]", pg_json_string) == 0);
  ut_assert(t, 16 == length);
  length = square_set_to_pg_json_array(pg_json_string, (SquareSet) 0xFFFFFFFFFFFFFFFF);
  ut_assert(t, 512 == length);
}

static void
square_set_to_string_t (ut_test_t *const t)
{
  char to_string[32];
  size_t s_length;
  s_length = square_set_to_string(to_string, (SquareSet) 0);
  ut_assert(t, strcmp("", to_string) == 0);
  ut_assert(t, 0 == s_length);
  s_length = square_set_to_string(to_string, (SquareSet) 9);
  ut_assert(t, strcmp("A1 D1", to_string) == 0);
  ut_assert(t, 5 == s_length);
}

static void
square_set_random_selection_t (ut_test_t *const t)
{
  const unsigned int seed = 1739;
  prng_mt19937_t *prng = prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, seed);
  const SquareSet squares = (SquareSet) 7; // 7 is {A1, B1, C1}
  const Square sq = square_set_random_selection(prng, squares);
  ut_assert(t, sq == A1 || sq == B1 || sq == C1);
  prng_mt19937_free(prng);

  prng_mt19937_t *prng1 = prng_mt19937_new();
  prng_mt19937_init_by_seed(prng, 17598);

  const int max_iteration_count = 1000000;
  unsigned long int observations[square_cardinality];
  for (int i = 0; i < square_cardinality; i++) {
    observations[i] = 0;
  }
  for (int j = 0; j < max_iteration_count; j++) {
    const Square sq = square_set_random_selection(prng1, (SquareSet) 0xFFFFFFFFFFFFFFFF);
    ut_assert(t, sq >= A1 || sq <= H8);
    observations[sq]++;
    int touched_count = 0;
    for (int i = 0; i < square_cardinality; i++) {
      if (observations[i] != 0) touched_count++;
    }
    if (touched_count == 64) goto out;
  }
  ut_assert(t, false);
 out:

  for (int i = 0; i < 1000; i++) {
    const Square sq = square_set_random_selection(prng1, (SquareSet) 5);
    ut_assert(t, sq == A1 || sq == C1);
  }

  for (int i = 0; i < 100; i++) {
    const Square sq = square_set_random_selection(prng1, (SquareSet) 2);
    ut_assert(t, sq == B1);
  }

  prng_mt19937_free(prng1);
}

static void
square_set_to_array_t (ut_test_t *const t)
{
  int sq_count;
  Square *sq_array;
  square_set_to_array(&sq_count, &sq_array, (SquareSet) 5);
  ut_assert(t, sq_count == 2);
  ut_assert(t, sq_array[0] == A1);
  ut_assert(t, sq_array[1] == C1);
  free(sq_array);

  square_set_to_array(&sq_count, &sq_array, (SquareSet) 0);
  ut_assert(t, sq_count == 0);
  free(sq_array);

  square_set_to_array(&sq_count, &sq_array, (SquareSet) 1);
  ut_assert(t, sq_count == 1);
  ut_assert(t, sq_array[0] == A1);
  free(sq_array);

  square_set_to_array(&sq_count, &sq_array, (SquareSet) 0xFFFFFFFFFFFFFFFF);
  ut_assert(t, sq_count == 64);
  for (int i = 0; i < 64; i++)
    ut_assert(t, sq_array[i] == (Square) i);
  free(sq_array);
}

static void
square_set_from_array_t (ut_test_t *const t)
{
  const Square sq_array_0[] = {A1, C1};
  const int sq_count_0 = 2;
  const SquareSet computed_0 = square_set_from_array(sq_array_0, sq_count_0);
  ut_assert(t, (SquareSet) 5 == computed_0);

  const Square sq_array_1[] = {A1, C1, A1};
  const int sq_count_1 = 3;
  const SquareSet computed_1 = square_set_from_array(sq_array_1, sq_count_1);
  ut_assert(t, (SquareSet) 5 == computed_1);

  const Square *sq_array_2 = NULL;
  const int sq_count_2 = 0;
  const SquareSet computed_2 = square_set_from_array(sq_array_2, sq_count_2);
  ut_assert(t, (SquareSet) 0 == computed_2);
}

static void
player_color_t (ut_test_t *const t)
{
  ut_assert(t, BLACK_SQUARE == player_color(BLACK_PLAYER));
  ut_assert(t, WHITE_SQUARE == player_color(WHITE_PLAYER));
}

static void
player_description_t (ut_test_t *const t)
{
  ut_assert(t, strcmp(player_description(BLACK_PLAYER), "The Black player") == 0);
  ut_assert(t, strcmp(player_description(WHITE_PLAYER), "The White player") == 0);
}

static void
player_opponent_t (ut_test_t *const t)
{
  ut_assert(t, BLACK_PLAYER == player_opponent(WHITE_PLAYER));
  ut_assert(t, WHITE_PLAYER == player_opponent(BLACK_PLAYER));
}

static void
game_position_x_print_t (ut_test_t *const t)
{
  GamePositionX gpx_struct = {
    .blacks = 1,
    .whites = 4,
    .player = WHITE_PLAYER
  };
  GamePositionX *gpx = &gpx_struct;

  char expected[256];
  char *s = expected;

  char gpx_to_string[256];
  size_t length;

  length = game_position_x_print(gpx_to_string, gpx);
  ut_assert(t, 211 == length);

  s += sprintf(s, "    a b c d e f g h \n");
  s += sprintf(s, " 1  @ . O . . . . . \n");
  s += sprintf(s, " 2  . . . . . . . . \n");
  s += sprintf(s, " 3  . . . . . . . . \n");
  s += sprintf(s, " 4  . . . . . . . . \n");
  s += sprintf(s, " 5  . . . . . . . . \n");
  s += sprintf(s, " 6  . . . . . . . . \n");
  s += sprintf(s, " 7  . . . . . . . . \n");
  s += sprintf(s, " 8  . . . . . . . . \n");
  s += sprintf(s, "Player to move: WHITE\n");

  ut_assert(t, strcmp(expected, gpx_to_string) == 0);
}

static void
game_position_x_empties_t (ut_test_t *const t)
{
  GamePositionX gpx;

  gpx.blacks = 0x0000000000000000;
  gpx.whites = 0x0000000000000000;
  gpx.player = BLACK_PLAYER;
  ut_assert(t, 0xFFFFFFFFFFFFFFFF == game_position_x_empties(&gpx));

  gpx.blacks = 0x0000000000000000;
  gpx.whites = 0x0000000000000001;
  gpx.player = BLACK_PLAYER;
  ut_assert(t, 0xFFFFFFFFFFFFFFFE == game_position_x_empties(&gpx));

  gpx.blacks = 0xFF00000000000001;
  gpx.whites = 0x00FF000000000002;
  gpx.player = WHITE_PLAYER;
  ut_assert(t, 0x0000FFFFFFFFFFFC == game_position_x_empties(&gpx));
}

static void
game_position_x_get_player_t (ut_test_t *const t)
{
  GamePositionX gpx;

  gpx.blacks = 0x00000000000000FF;
  gpx.whites = 0x000000000000FF00;
  gpx.player = BLACK_PLAYER;
  ut_assert(t, 0x00000000000000FF == game_position_x_get_player(&gpx));

  gpx.blacks = 0x00000000000000FF;
  gpx.whites = 0x000000000000FF00;
  gpx.player = WHITE_PLAYER;
  ut_assert(t, 0x000000000000FF00 == game_position_x_get_player(&gpx));
}

static void
game_position_x_get_opponent_t (ut_test_t *const t)
{
  GamePositionX gpx;

  gpx.blacks = 0x00000000000000FF;
  gpx.whites = 0x000000000000FF00;
  gpx.player = BLACK_PLAYER;
  ut_assert(t, 0x000000000000FF00 == game_position_x_get_opponent(&gpx));

  gpx.blacks = 0x00000000000000FF;
  gpx.whites = 0x000000000000FF00;
  gpx.player = WHITE_PLAYER;
  ut_assert(t, 0x00000000000000FF == game_position_x_get_opponent(&gpx));
}

static void
game_position_x_legal_moves_t (ut_test_t *const t)
{
  GamePositionX gpx;

  gpx.blacks = 0x0000000000000001;
  gpx.whites = 0x0040201008040200;
  gpx.player = BLACK_PLAYER;
  ut_assert(t, 0x8000000000000000 == game_position_x_legal_moves(&gpx));
}

static void
game_position_x_count_difference_t (ut_test_t *const t)
{
  GamePositionX gpx;

  gpx.blacks = 0x00000000000000FF;
  gpx.whites = 0x000000000000FF00;
  gpx.player = BLACK_PLAYER;
  ut_assert(t, 0 == game_position_x_count_difference(&gpx));

  gpx.blacks = 0x00000000000000FF;
  gpx.whites = 0x000000000000FF00;
  gpx.player = WHITE_PLAYER;
  ut_assert(t, 0 == game_position_x_count_difference(&gpx));

  gpx.blacks = 0x0000000000000000;
  gpx.whites = 0x000000000000FF00;
  gpx.player = BLACK_PLAYER;
  ut_assert(t, -8 == game_position_x_count_difference(&gpx));

  gpx.blacks = 0x0000000000000000;
  gpx.whites = 0x000000000000FF00;
  gpx.player = WHITE_PLAYER;
  ut_assert(t, +8 == game_position_x_count_difference(&gpx));
}

static void
game_position_x_to_string_t (ut_test_t *const t)
{
  GamePositionX gpx;
  char gpx_to_string[66];

  gpx.blacks = 0x00000000000000FF;
  gpx.whites = 0x000000000000FF00;
  gpx.player = BLACK_PLAYER;
  game_position_x_to_string(&gpx, gpx_to_string);
  ut_assert(t, strcmp("bbbbbbbbwwwwwwww................................................b",
                      gpx_to_string) == 0);

  gpx.blacks = 0x00000000000000FF;
  gpx.whites = 0xFF0000000000FF00;
  gpx.player = WHITE_PLAYER;
  game_position_x_to_string(&gpx, gpx_to_string);
  ut_assert(t, strcmp("bbbbbbbbwwwwwwww........................................wwwwwwwww",
                      gpx_to_string) == 0);
}

static void
game_position_x_get_square_t (ut_test_t *const t)
{
  GamePositionX gpx;

  gpx.blacks = 0x0000000000000001;
  gpx.whites = 0x0000000000000002;
  gpx.player = BLACK_PLAYER;

  ut_assert(t, BLACK_SQUARE == game_position_x_get_square(&gpx, A1));
  ut_assert(t, WHITE_SQUARE == game_position_x_get_square(&gpx, B1));
  ut_assert(t, EMPTY_SQUARE == game_position_x_get_square(&gpx, C1));
}
static void
game_position_x_compare_t (ut_test_t *const t)
{
  GamePositionX a, b;

  a.blacks = 0x0000000000000001;
  a.whites = 0x0000000000000002;
  a.player = BLACK_PLAYER;
  b.blacks = 0x0000000000000001;
  b.whites = 0x0000000000000002;
  b.player = BLACK_PLAYER;
  ut_assert(t, 0 == game_position_x_compare(&a, &b));

  a.blacks = 0x0000000000000001;
  a.whites = 0x0000000000000002;
  a.player = BLACK_PLAYER;
  b.blacks = 0x0000000000000001;
  b.whites = 0x0000000000000002;
  b.player = WHITE_PLAYER;
  ut_assert(t, -1 == game_position_x_compare(&a, &b));

  a.blacks = 0x0000000000000001;
  a.whites = 0x0000000000000002;
  a.player = WHITE_PLAYER;
  b.blacks = 0x0000000000000001;
  b.whites = 0x0000000000000002;
  b.player = BLACK_PLAYER;
  ut_assert(t, +1 == game_position_x_compare(&a, &b));

  a.blacks = 0x0000000000000001;
  a.whites = 0x0000000000000002;
  a.player = BLACK_PLAYER;
  b.blacks = 0x0000000000000004;
  b.whites = 0x0000000000000002;
  b.player = BLACK_PLAYER;
  ut_assert(t, -1 == game_position_x_compare(&a, &b));
}

static void
game_position_x_clone_t (ut_test_t *const t)
{
  GamePositionX a;
  GamePositionX *b;
  a.blacks = 0x0000000000000001;
  a.whites = 0x0000000000000002;
  a.player = BLACK_PLAYER;
  b = game_position_x_clone(&a);
  ut_assert(t, 0 == game_position_x_compare(&a, b));
  game_position_x_free(b);
}

static void
game_position_x_copy_t (ut_test_t *const t)
{
  GamePositionX *a;
  GamePositionX *b;
  a = game_position_x_new(0x0000000000000001,
                          0x0000000000000002,
                          BLACK_PLAYER);
  b = game_position_x_new(0x00000000000000FF,
                          0x0000000000000000,
                          WHITE_PLAYER);
  ut_assert(t, 0 != game_position_x_compare(a, b));
  game_position_x_copy(a, b);
  ut_assert(t, 0 == game_position_x_compare(a, b));
  game_position_x_free(a);
  game_position_x_free(b);
}

static void
game_position_x_pass_t (ut_test_t *const t)
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

  ut_assert(t, 0x0000000000000001 == next->blacks);
  ut_assert(t, 0x0000000000000002 == next->whites);
  ut_assert(t, WHITE_PLAYER == next->player);

  game_position_x_free(current);
  game_position_x_free(next);
}

static void
game_position_x_hash_t (ut_test_t *const t)
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
  ut_assert(t, expected == game_position_x_hash(gpx));
  game_position_x_free(gpx);

  expected = 0xFFFFFFFFFFFFFFFF;
  blacks = 0x0000000000000000;
  whites = 0x0000000000000000;
  player = WHITE_PLAYER;
  gpx = game_position_x_new(blacks, whites, player);
  ut_assert(t, expected == game_position_x_hash(gpx));
  game_position_x_free(gpx);

  expected = 0x4689879C5E2B6C8D ^ 0x1C10E0B05C7B3C49;
  blacks = 0x0000000000000002;
  whites = 0x0000000000000004;
  player = BLACK_PLAYER;
  gpx = game_position_x_new(blacks, whites, player);
  ut_assert(t, expected == game_position_x_hash(gpx));
  game_position_x_free(gpx);

  /*
   * The hash of two game position A, and B having the same board but different player satisfy
   * this property:
   * hash(A) & hash(B) == 0;
   */
  player = WHITE_PLAYER;
  gpx = game_position_x_new(blacks, whites, player);
  ut_assert(t, ~expected == game_position_x_hash(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_delta_hash_t (ut_test_t *const t)
{
  const GamePositionX a = { 4ULL, 2ULL, BLACK_PLAYER };
  const GamePositionX b = { 7ULL, 0ULL, WHITE_PLAYER };
  const uint64_t hash_a = game_position_x_hash(&a);
  const uint64_t hash_b = game_position_x_hash(&b);

  const Square flips[] = { A1, B1 };
  const int flip_count = 2;

  const uint64_t computed = game_position_x_delta_hash(hash_a, flips, flip_count, b.player);
  const uint64_t expected =hash_b;

  ut_assert(t, expected == computed);
}
static void
game_position_x_final_value_t (ut_test_t *const t)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0xFFFFFFFFFFFFFFFF,
                            0x0000000000000000,
                            BLACK_PLAYER);
  ut_assert(t, 64 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000001,
                            0x0000000000000002,
                            BLACK_PLAYER);
  ut_assert(t, 0 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000001,
                            0x0000000000000002,
                            WHITE_PLAYER);
  ut_assert(t, 0 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000001,
                            0x0000000000000000,
                            BLACK_PLAYER);
  ut_assert(t, 64 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x0000000000000001,
                            0x0000000000000000,
                            WHITE_PLAYER);
  ut_assert(t, -64 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x00FF000000000001,
                            0xFF00000000000000,
                            BLACK_PLAYER);
  ut_assert(t, 48 == game_position_x_final_value(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_has_any_legal_move_t (ut_test_t *const t)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x8000000000000000,
                            WHITE_PLAYER);
  ut_assert(t, true == game_position_x_has_any_legal_move(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0xFFFFFFFFFFFFFFFE,
                            0x0000000000000000,
                            WHITE_PLAYER);
  ut_assert(t, false == game_position_x_has_any_legal_move(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_has_any_player_any_legal_move_t (ut_test_t *const t)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x8000000000000000,
                            WHITE_PLAYER);
  ut_assert(t, true == game_position_x_has_any_player_any_legal_move(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x8000000000000000,
                            BLACK_PLAYER);
  ut_assert(t, true == game_position_x_has_any_player_any_legal_move(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0xFFFFFFFFFFFFFFFE,
                            0x0000000000000000,
                            WHITE_PLAYER);
  ut_assert(t, false == game_position_x_has_any_player_any_legal_move(gpx));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0xFFFFFFFFFFFFFFFE,
                            0x0000000000000000,
                            BLACK_PLAYER);
  ut_assert(t, false == game_position_x_has_any_player_any_legal_move(gpx));
  game_position_x_free(gpx);
}

static void
game_position_x_is_move_legal_t (ut_test_t *const t)
{
  GamePositionX *gpx;

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x8000000000000000,
                            WHITE_PLAYER);
  ut_assert(t, true == game_position_x_is_move_legal(gpx, 0));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x8000000000000000,
                            WHITE_PLAYER);
  ut_assert(t, false == game_position_x_is_move_legal(gpx, 1));
  game_position_x_free(gpx);

  gpx = game_position_x_new(0x7FFFFFFFFFFFFFFE,
                            0x0000000000000000,
                            WHITE_PLAYER);
  ut_assert(t, false == game_position_x_is_move_legal(gpx, 0));
  game_position_x_free(gpx);
}

static void
game_position_x_make_move_t (ut_test_t *const t)
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

  ut_assert(t, true == game_position_x_is_move_legal(current, 0));
  game_position_x_make_move(current, 0, updated);
  ut_assert(t, 0 == game_position_x_compare(expected, updated));

  game_position_x_free(current);
  game_position_x_free(updated);
  game_position_x_free(expected);
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_init(&argc, &argv);
  ut_suite_t *const s = ut_suite_new("board");

  board_module_init();

  ut_suite_add_simple_test(s, "dummy", dummy_t);

  ut_suite_add_simple_test(s, "square_to_string", square_to_string_t);
  ut_suite_add_simple_test(s, "square_as_move_to_string", square_as_move_to_string_t);
  ut_suite_add_simple_test(s, "square_array_to_string", square_array_to_string_t);
  ut_suite_add_simple_test(s, "square_as_move_array_to_string", square_as_move_array_to_string_t);
  ut_suite_add_simple_test(s, "square_belongs_to_enum_set", square_belongs_to_enum_set_t);
  ut_suite_add_simple_test(s, "square_is_valid_move", square_is_valid_move_t);

  ut_suite_add_simple_test(s, "square_set_to_pg_json_array", square_set_to_pg_json_array_t);
  ut_suite_add_simple_test(s, "square_set_to_string", square_set_to_string_t);
  ut_suite_add_simple_test(s, "square_set_random_selection", square_set_random_selection_t);
  ut_suite_add_simple_test(s, "square_set_to_array", square_set_to_array_t);
  ut_suite_add_simple_test(s, "square_set_from_array", square_set_from_array_t);

  ut_suite_add_simple_test(s, "player_color", player_color_t);
  ut_suite_add_simple_test(s, "player_description", player_description_t);
  ut_suite_add_simple_test(s, "player_opponent", player_opponent_t);

  ut_suite_add_simple_test(s, "game_position_x_print", game_position_x_print_t);
  ut_suite_add_simple_test(s, "game_position_x_empties", game_position_x_empties_t);
  ut_suite_add_simple_test(s, "game_position_x_get_player", game_position_x_get_player_t);
  ut_suite_add_simple_test(s, "game_position_x_get_opponent", game_position_x_get_opponent_t);
  ut_suite_add_simple_test(s, "game_position_x_legal_moves", game_position_x_legal_moves_t);
  ut_suite_add_simple_test(s, "game_position_x_count_difference", game_position_x_count_difference_t);
  ut_suite_add_simple_test(s, "game_position_x_to_string", game_position_x_to_string_t);
  ut_suite_add_simple_test(s, "game_position_x_get_square", game_position_x_get_square_t);
  ut_suite_add_simple_test(s, "game_position_x_compare", game_position_x_compare_t);
  ut_suite_add_simple_test(s, "game_position_x_clone", game_position_x_clone_t);
  ut_suite_add_simple_test(s, "game_position_x_copy", game_position_x_copy_t);
  ut_suite_add_simple_test(s, "game_position_x_pass", game_position_x_pass_t);
  ut_suite_add_simple_test(s, "game_position_x_hash", game_position_x_hash_t);
  ut_suite_add_simple_test(s, "game_position_x_delta_hash", game_position_x_delta_hash_t);
  ut_suite_add_simple_test(s, "game_position_x_final_value", game_position_x_final_value_t);
  ut_suite_add_simple_test(s, "game_position_x_has_any_legal_move", game_position_x_has_any_legal_move_t);
  ut_suite_add_simple_test(s, "game_position_x_has_any_player_any_legal_move", game_position_x_has_any_player_any_legal_move_t);
  ut_suite_add_simple_test(s, "game_position_x_is_move_legal", game_position_x_is_move_legal_t);
  ut_suite_add_simple_test(s, "game_position_x_make_move", game_position_x_make_move_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
