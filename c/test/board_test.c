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

static void game_position_print_test (void);
static void direction_shift_square_set_test (void);
static void board_get_square_test (void);
static void board_count_difference_test (void);
static void board_compare_test (void);
static void board_count_pieces_test (void);
static void board_new_test (void);
static void board_print_test (void);
static void board_is_move_legal_test (void);
static void player_color_test (void);
static void player_description_test (void);
static void player_opponent_test (void);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/board/game_position_print_test", game_position_print_test);
  g_test_add_func("/board/direction_shift_square_set_test", direction_shift_square_set_test);
  g_test_add_func("/board/board_get_square_test", board_get_square_test);
  g_test_add_func("/board/board_count_difference_test", board_count_difference_test);
  g_test_add_func("/board/board_compare_test", board_compare_test);
  g_test_add_func("/board/board_count_pieces_test", board_count_pieces_test);
  g_test_add_func("/board/board_new_test", board_new_test);
  g_test_add_func("/board/board_print_test", board_print_test);
  g_test_add_func("/board/board_is_move_legal_test", board_is_move_legal_test);
  g_test_add_func("/board/player_color_test", player_color_test);
  g_test_add_func("/board/player_description_test", player_description_test);
  g_test_add_func("/board/player_opponent_test", player_opponent_test);

  return g_test_run();
}



/*
 * Test functions.
 */

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
  Board    *b;

  b = board_new(1LLU, 2LLU);

  g_assert(FALSE == board_is_move_legal(b, A1, WHITE_PLAYER));
  g_assert(FALSE == board_is_move_legal(b, A1, BLACK_PLAYER));
  g_assert(TRUE  == board_is_move_legal(b, C1, BLACK_PLAYER));
}
