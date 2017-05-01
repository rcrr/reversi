/**
 * @file
 *
 * @brief Board module unit test suite.
 * @details Collects tests and helper methods for the board module.
 *
 * @par ut_board_test.c
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

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
