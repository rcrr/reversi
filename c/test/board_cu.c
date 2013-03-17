/**
 * @file
 *
 * @brief Reversi C - Board unit test suite
 */

/**
 * @cond
 *
 * board_cu.c
 *
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 *
 * Copyright (c) 2013 Roberto Corradini. All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the
 * Free Software Foundation; either version 3, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 * or visit the site <http://www.gnu.org/licenses/>.
 *
 * @endcond
 */

#include "CUnit/Basic.h"
#include <stdlib.h>
#include <errno.h>

#include "board.h"

extern int errno;

/**
 * The suite initialization function.
 * Returns zero on success, non-zero otherwise.
 */
int init_board_suite(void)
{
  return 0;
}

/**
 * The suite cleanup function.
 * Returns zero on success, non-zero otherwise.
 */
int clean_board_suite(void)
{
  return 0;
}

void test_board_get_square(void)
{
  Board *b;

  b = new_board(1LLU, 2LLU);

  CU_ASSERT_EQUAL(board_get_square(b, A1), BLACK_SQUARE);
  CU_ASSERT_EQUAL(board_get_square(b, B1), WHITE_SQUARE);
  CU_ASSERT_EQUAL(board_get_square(b, C1), EMPTY_SQUARE);
}

void test_new_board(void)
{
  SquareSet b = 0LLU;
  SquareSet w = 0LLU;
  Board *empty_board = new_board(b, w);
  CU_ASSERT_PTR_NOT_NULL(empty_board);

  empty_board = delete_board(empty_board);
  CU_ASSERT_PTR_NULL(empty_board)
}

/**
 * Unit test of function: SquareState player_color(Player p).
 */
void test_player_color(void)
{
  CU_ASSERT(BLACK_SQUARE == player_color(BLACK_PLAYER));
  CU_ASSERT(WHITE_SQUARE == player_color(WHITE_PLAYER));
}

/**
 * Unit test of function: char *player_description(Player p).
 */
void test_player_description()
{
  CU_ASSERT_STRING_EQUAL(player_description(BLACK_PLAYER), "The Black player");
  CU_ASSERT_STRING_EQUAL(player_description(WHITE_PLAYER), "The White player");
}

/**
 * Unit test of function: Player player_opponent(Player p).
 */
void test_player_opponent(void)
{
  CU_ASSERT(BLACK_PLAYER == player_opponent(WHITE_PLAYER));
  CU_ASSERT(WHITE_PLAYER == player_opponent(BLACK_PLAYER));
}

/**
 * The main() function for setting up and running the tests.
 * Returns a CUE_SUCCESS on successful running, another
 * CUnit error code on failure.
 */
int main()
{
   CU_pSuite pSuite = NULL;

   /* Initialize the CUnit test registry */
   if (CUE_SUCCESS != CU_initialize_registry())
      return CU_get_error();

   /* Add a suite to the registry */
   pSuite = CU_add_suite("Board Unit Test", init_board_suite, clean_board_suite);
   if (NULL == pSuite) {
      CU_cleanup_registry();
      return CU_get_error();
   }

   /* Add the tests to the suite */
   if ((NULL == CU_add_test(pSuite, "Player player_opponent(Player p)", test_player_opponent))                   ||
       (NULL == CU_add_test(pSuite, "SquareState player_color(Player p)", test_player_color))                    ||
       (NULL == CU_add_test(pSuite, "char *player_description(Player p)", test_player_description))              ||
       (NULL == CU_add_test(pSuite, "Board *new_board(SquareSet b, SquareSet w)", test_new_board))               ||
       (NULL == CU_add_test(pSuite, "SquareState board_get_square(Board *b, Square sq)", test_board_get_square)) )
   {
      CU_cleanup_registry();
      return CU_get_error();
   }

   /* Run all tests using the CUnit Basic interface */
   CU_basic_set_mode(CU_BRM_VERBOSE);
   CU_basic_run_tests();
   CU_cleanup_registry();
   return CU_get_error();
}

