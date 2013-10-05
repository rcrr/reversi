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



/* Test function prototypes. */

static void dummy_test (void);

static void square_to_string_test (void);

static void player_color_test (void);
static void player_description_test (void);
static void player_opponent_test (void);

static void direction_shift_square_set_test (void);

static void axis_shift_distance_test (void);
static void axis_move_ordinal_position_in_bitrow_test (void);
static void axis_transform_to_row_one_test (void);
static void axis_transform_back_from_row_one_test (void);

static void board_get_square_test (void);
static void board_count_difference_test (void);
static void board_compare_test (void);
static void board_count_pieces_test (void);
static void board_new_test (void);
static void board_print_test (void);
static void board_is_move_legal_test (void);
static void board_legal_moves_test (void);

static void game_position_print_test (void);
static void game_position_compare_test (void);
static void game_position_count_difference_test (void);
static void game_position_hash_test (void);

int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  board_module_init();

  g_test_add_func("/board/dummy_test", dummy_test);

  g_test_add_func("/board/square_to_string_test", square_to_string_test);

  g_test_add_func("/board/player_color_test", player_color_test);
  g_test_add_func("/board/player_description_test", player_description_test);
  g_test_add_func("/board/player_opponent_test", player_opponent_test);

  g_test_add_func("/board/direction_shift_square_set_test", direction_shift_square_set_test);

  g_test_add_func("/board/axis_shift_distance_test", axis_shift_distance_test);
  g_test_add_func("/board/axis_move_ordinal_position_in_bitrow_test", axis_move_ordinal_position_in_bitrow_test);
  g_test_add_func("/board/axis_transform_to_row_one_test", axis_transform_to_row_one_test);
  g_test_add_func("/board/axis_transform_back_from_row_one_test", axis_transform_back_from_row_one_test);

  g_test_add_func("/board/board_get_square_test", board_get_square_test);
  g_test_add_func("/board/board_count_difference_test", board_count_difference_test);
  g_test_add_func("/board/board_compare_test", board_compare_test);
  g_test_add_func("/board/board_count_pieces_test", board_count_pieces_test);
  g_test_add_func("/board/board_new_test", board_new_test);
  g_test_add_func("/board/board_print_test", board_print_test);
  g_test_add_func("/board/board_is_move_legal_test", board_is_move_legal_test);
  g_test_add_func("/board/board_legal_moves_test", board_legal_moves_test);

  g_test_add_func("/board/game_position_print_test", game_position_print_test);
  g_test_add_func("/board/game_position_compare_test", game_position_compare_test);
  g_test_add_func("/board/game_position_count_difference_test", game_position_count_difference_test);
  g_test_add_func("/board/game_position_hash_test", game_position_hash_test);

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
square_to_string_test (void)
{
  gchar *symbol;
  symbol = square_to_string(D5);
  g_assert(g_strcmp0("D5", symbol) == 0);
  g_free(symbol);

  symbol = square_to_string(A1);
  g_assert(g_strcmp0("A1", symbol) == 0);
  g_free(symbol);

  symbol = square_to_string(H8);
  g_assert(g_strcmp0("H8", symbol) == 0);
  g_free(symbol);  
}

static void
axis_transform_to_row_one_test (void)
{
  /*
    uint8
    axis_transform_to_row_one (const Axis      axis,
                               const SquareSet squares);
   */
  g_assert(0x00 == axis_transform_to_row_one(HO, 0x0000000000000000));
  g_assert(0xFF == axis_transform_to_row_one(HO, 0xFFFFFFFFFFFFFFFF));
  g_assert(0xFF == axis_transform_to_row_one(HO, 0x00000000000000FF));

  g_assert(0xFF == axis_transform_to_row_one(VE, 0x0101010101010101));
  g_assert(0x00 == axis_transform_to_row_one(VE, 0x1010101010101010));
  g_assert(0x88 == axis_transform_to_row_one(VE, 0x0100000001000000));

  g_assert(0x00 == axis_transform_to_row_one(DD, 0x0000000000000000));
  g_assert(0xFF == axis_transform_to_row_one(DD, 0xFFFFFFFFFFFFFFFF));
  g_assert(0xFF == axis_transform_to_row_one(DD, 0x8040201008040201));

  g_assert(0x00 == axis_transform_to_row_one(DU, 0x0000000000000000));
  g_assert(0xFF == axis_transform_to_row_one(DU, 0xFFFFFFFFFFFFFFFF));
  g_assert(0xFF == axis_transform_to_row_one(DU, 0x0102040810204080));
}

static void
axis_transform_back_from_row_one_test (void)
{
  /*
    SquareSet
    axis_transform_back_from_row_one (const Axis   axis,
                                      const uint32 bitrow);
   */
  g_assert(0x0101010101010101 == axis_transform_back_from_row_one(VE, 0x000000FF));
}

static void
axis_move_ordinal_position_in_bitrow_test (void)
{
  /*
    uint8
    axis_move_ordinal_position_in_bitrow (const Axis  axis,
                                          const uint8 column,
                                          const uint8 row);

   */
  g_assert(0 == axis_move_ordinal_position_in_bitrow(HO, 0, 0));
  g_assert(3 == axis_move_ordinal_position_in_bitrow(HO, 3, 5));

  g_assert(0 == axis_move_ordinal_position_in_bitrow(VE, 0, 0));
  g_assert(5 == axis_move_ordinal_position_in_bitrow(VE, 3, 5));

  g_assert(0 == axis_move_ordinal_position_in_bitrow(DD, 0, 0));
  g_assert(3 == axis_move_ordinal_position_in_bitrow(DD, 3, 5));

  g_assert(0 == axis_move_ordinal_position_in_bitrow(DU, 0, 0));
  g_assert(3 == axis_move_ordinal_position_in_bitrow(DU, 3, 5));
}

static void
axis_shift_distance_test (void)
{
  /*
    int
    axis_shift_distance (const Axis  axis,
                         const uint8 column,
                         const uint8 row);
  */
  g_assert(  0 == axis_shift_distance(HO, 0, 0));
  g_assert( -8 == axis_shift_distance(HO, 5, 1));
  g_assert(-56 == axis_shift_distance(HO, 5, 7));

  g_assert(  0 == axis_shift_distance(VE, 0, 0));
  g_assert( -1 == axis_shift_distance(VE, 1, 3));
  g_assert( -7 == axis_shift_distance(VE, 7, 4));

  g_assert(  0 == axis_shift_distance(DD, 0, 0));
  g_assert(  0 == axis_shift_distance(DD, 1, 1));
  g_assert(-16 == axis_shift_distance(DD, 2, 4));
  g_assert( 16 == axis_shift_distance(DD, 4, 2));

  g_assert( 56 == axis_shift_distance(DU, 0, 0));
  g_assert( 40 == axis_shift_distance(DU, 1, 1));
  g_assert(-56 == axis_shift_distance(DU, 7, 7));
}

static void
player_color_test (void)
{
  g_assert(BLACK_SQUARE == player_color(BLACK_PLAYER));
  g_assert(WHITE_SQUARE == player_color(WHITE_PLAYER));
}

static void
player_description_test (void)
{
  g_assert(g_strcmp0(player_description(BLACK_PLAYER), "The Black player") == 0);
  g_assert(g_strcmp0(player_description(WHITE_PLAYER), "The White player") == 0);
}

static void
player_opponent_test (void)
{
  g_assert(BLACK_PLAYER == player_opponent(WHITE_PLAYER));
  g_assert(WHITE_PLAYER == player_opponent(BLACK_PLAYER));
}

static void
direction_shift_square_set_test (void)
{
  g_assert(direction_shift_square_set(N,  0xFFFFFFFFFFFFFFFFULL) == 0x00FFFFFFFFFFFFFFULL);
  g_assert(direction_shift_square_set(S,  0xFFFFFFFFFFFFFFFFULL) == 0xFFFFFFFFFFFFFF00ULL);
  g_assert(direction_shift_square_set(E,  0xFFFFFFFFFFFFFFFFULL) == 0xFEFEFEFEFEFEFEFEULL);
  g_assert(direction_shift_square_set(W,  0xFFFFFFFFFFFFFFFFULL) == 0x7F7F7F7F7F7F7F7FULL);
  g_assert(direction_shift_square_set(NE, 0xFFFFFFFFFFFFFFFFULL) == 0x00FEFEFEFEFEFEFEULL);
  g_assert(direction_shift_square_set(SE, 0xFFFFFFFFFFFFFFFFULL) == 0xFEFEFEFEFEFEFE00ULL);
  g_assert(direction_shift_square_set(NW, 0xFFFFFFFFFFFFFFFFULL) == 0x007F7F7F7F7F7F7FULL);
  g_assert(direction_shift_square_set(SW, 0xFFFFFFFFFFFFFFFFULL) == 0x7F7F7F7F7F7F7F00ULL);
}

static void
board_get_square_test (void)
{
  Board *b;

  b = board_new(1LLU, 2LLU);

  g_assert(board_get_square(b, A1) == BLACK_SQUARE);
  g_assert(board_get_square(b, B1) == WHITE_SQUARE);
  g_assert(board_get_square(b, C1) == EMPTY_SQUARE);

  b = board_free(b);
}

static void
board_count_difference_test (void)
{
  Board     *b;
  SquareSet  blacks;
  SquareSet  whites;

  blacks = 0xFFFFFFFFFFFFFFFFULL;
  whites = 0x0000000000000000ULL;

  b = board_new(blacks, whites);

  g_assert(board_count_difference(b, BLACK_PLAYER) == +64);
  g_assert(board_count_difference(b, WHITE_PLAYER) == -64);

  b = board_free(b);
}

static void
board_compare_test (void)
{
  Board *a;
  Board *b;

  a = board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL);
  b = board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL);
  g_assert(board_compare(a, b) == 0);
  a = board_free(a);
  b = board_free(b);

  a = board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL);
  b = board_new(0x0000000000000000ULL, 0x0000000000000000ULL);
  g_assert(board_compare(a, b) == +1);
  a = board_free(a);
  b = board_free(b);

  a = board_new(0x0000000000000000ULL, 0x0000000000000000ULL);
  b = board_new(0x0000000000000001ULL, 0x0000000000000000ULL);
  g_assert(board_compare(a, b) == -1);
  a = board_free(a);
  b = board_free(b);

  a = board_new(0x0000000000000007ULL, 0x0000000000000000ULL);
  b = board_new(0x0000000000000007ULL, 0x0000000000000000ULL);
  g_assert(board_compare(a, b) == 0);
  a = board_free(a);
  b = board_free(b);

  a = board_new(0x0000000000000007ULL, 0x0100000000000000ULL);
  b = board_new(0x0000000000000007ULL, 0x0000000000000000ULL);
  g_assert(board_compare(a, b) == +1);
  a = board_free(a);
  b = board_free(b);

  a = board_new(0x0000000000000007ULL, 0x0000000000000000ULL);
  b = board_new(0x0000000000000007ULL, 0x0100000000000000ULL);
  g_assert(board_compare(a, b) == -1);
  a = board_free(a);
  b = board_free(b);
}

static void
board_count_pieces_test (void)
{
  Board *b;

  b = board_new(1LLU, 2LLU);

  g_assert(board_count_pieces(b, BLACK_SQUARE) ==  1);
  g_assert(board_count_pieces(b, WHITE_SQUARE) ==  1);
  g_assert(board_count_pieces(b, EMPTY_SQUARE) == 62);

  b = board_free(b);
}

static void
board_new_test (void)
{
  SquareSet  w;
  SquareSet  b;
  Board     *empty_board;

  b = 0LLU;
  w = 0LLU;

  empty_board = board_new(b, w);
  g_assert(empty_board != NULL);

  empty_board = board_free(empty_board);
  g_assert(empty_board == NULL);
}

static void
board_print_test (void)
{
  Board *b;
  char *b_to_string;
  GString *expected;

  b = board_new(1LLU, 2LLU);
  b_to_string = board_print(b);

  expected = g_string_sized_new(220);
  g_string_append(expected, "    a b c d e f g h \n");
  g_string_append(expected, " 1  @ O . . . . . . \n");
  g_string_append(expected, " 2  . . . . . . . . \n");
  g_string_append(expected, " 3  . . . . . . . . \n");
  g_string_append(expected, " 4  . . . . . . . . \n");
  g_string_append(expected, " 5  . . . . . . . . \n");
  g_string_append(expected, " 6  . . . . . . . . \n");
  g_string_append(expected, " 7  . . . . . . . . \n");
  g_string_append(expected, " 8  . . . . . . . . \n");

  g_assert(g_strcmp0(expected->str, b_to_string) == 0);

  board_free(b);
  g_free(b_to_string);
  g_string_free(expected, TRUE);
}

static void
game_position_print_test (void)
{
  Board        *b;
  Player        p;
  GamePosition *gp;
  char         *gp_to_string;
  GString      *expected;

  b = board_new(1LLU, 4LLU);
  p = WHITE_PLAYER;
  gp = game_position_new(b, p);

  gp_to_string = game_position_print(gp);

  expected = g_string_sized_new(220);
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

  g_assert(g_strcmp0(expected->str, gp_to_string) == 0);

  game_position_free(gp);
  g_free(gp_to_string);
  g_string_free(expected, TRUE);
}

static void
board_is_move_legal_test (void)
{
  Board *b;

  b = board_new(1LLU, 2LLU);
  g_assert(FALSE == board_is_move_legal(b, A1, WHITE_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, A1, BLACK_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, B1, WHITE_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, B1, BLACK_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, C1, WHITE_PLAYER));
  g_assert(TRUE  == board_is_move_legal(b, C1, BLACK_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, D1, WHITE_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, D1, BLACK_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, A2, WHITE_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, A2, BLACK_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, B2, WHITE_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, B2, BLACK_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, C2, WHITE_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, C2, BLACK_PLAYER));
  b = board_free(b);

  b = board_new(0x0000000000000001, 0x0040201008040200);
  g_assert(TRUE == board_is_move_legal(b, H8, BLACK_PLAYER));
  b = board_free(b);
}

static void
board_legal_moves_test (void)
{
  Board *b;

  b = board_new(0x0000000000000001, 0x0040201008040200);
  g_assert(0x8000000000000000 == board_legal_moves(b, BLACK_PLAYER));
  b = board_free(b);
}

static void
game_position_compare_test (void)
{
  GamePosition *a;
  GamePosition *b;

  a = game_position_new(board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL), BLACK_PLAYER);
  b = a;
  g_assert(game_position_compare(a, b) == 0);
  a = game_position_free(a);
  b = a;

  a = game_position_new(board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL), BLACK_PLAYER);
  b = game_position_new(board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL), BLACK_PLAYER);
  g_assert(game_position_compare(a, b) == 0);
  a = game_position_free(a);
  b = game_position_free(b);

  a = game_position_new(board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL), BLACK_PLAYER);
  b = game_position_new(board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL), WHITE_PLAYER);
  g_assert(game_position_compare(a, b) < 0);
  a = game_position_free(a);
  b = game_position_free(b);

  a = game_position_new(board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL), WHITE_PLAYER);
  b = game_position_new(board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL), BLACK_PLAYER);
  g_assert(game_position_compare(a, b) > 0);
  a = game_position_free(a);
  b = game_position_free(b);

  a = game_position_new(board_new(0xFFFFFFFFFFFFFFFFULL, 0x0000000000000000ULL), BLACK_PLAYER);
  b = game_position_new(board_new(0xFFFFFFFFFFFFFFFEULL, 0x0000000000000000ULL), BLACK_PLAYER);
  g_assert(game_position_compare(a, b) > 0);
  a = game_position_free(a);
  b = game_position_free(b);
}

static void
game_position_count_difference_test (void)
{
  GamePosition *gp;
  Board        *b;
  SquareSet     blacks;
  SquareSet     whites;
  Player        p;

  blacks = 0xFFFFFFFFFFFFFFFFULL;
  whites = 0x0000000000000000ULL;
  b = board_new(blacks, whites);
  p = BLACK_PLAYER;
  gp = game_position_new(b, p);

  g_assert(game_position_count_difference(gp) == +64);

  gp = game_position_free(gp);
}

static void
game_position_hash_test (void)
{
  GamePosition *gp;
  Board        *b;
  SquareSet     blacks;
  SquareSet     whites;
  Player        p;
  uint64        expected;

  blacks = 0x0000000000000000ULL;
  whites = 0x0000000000000000ULL;
  b = board_new(blacks, whites);
  p = BLACK_PLAYER;
  gp = game_position_new(b, p);
  g_assert(game_position_hash(gp) == 0ULL);
  gp = game_position_free(gp);

  blacks = 0x0000000000000000ULL;
  whites = 0x0000000000000000ULL;
  b = board_new(blacks, whites);
  p = WHITE_PLAYER;
  gp = game_position_new(b, p);
  g_assert(game_position_hash(gp) == 0x6EF5203CCAF44DBF);
  gp = game_position_free(gp);

  expected = 0x4689879C5E2B6C8D ^ 0x056C0070CF70F8DD ^ 0x81AD4662EE05E75A;
  blacks = 0x0000000000000002ULL;
  whites = 0x0000000000000003ULL;
  b = board_new(blacks, whites);
  p = BLACK_PLAYER;
  gp = game_position_new(b, p);
  g_assert(game_position_hash(gp) == expected);
  gp = game_position_free(gp);

}
