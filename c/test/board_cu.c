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

#include "board.h"

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

/**
 * Simple test of function: SquareState color(Player).
 */
void test_color(void)
{
  Player b = BLACK_PLAYER;
  Player w = WHITE_PLAYER;
  CU_ASSERT(BLACK_SQUARE == color(b));
  CU_ASSERT(WHITE_SQUARE == color(w));
}

/**
 * Simple test of function: Player opponent(Player).
 */
void test_opponent(void)
{
  Player b = BLACK_PLAYER;
  Player w = WHITE_PLAYER;
  CU_ASSERT(BLACK_PLAYER == opponent(w));
  CU_ASSERT(WHITE_PLAYER == opponent(b));
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
   if ((NULL == CU_add_test(pSuite, "Player opponent(Player)", test_opponent)) ||
       (NULL == CU_add_test(pSuite, "SquareState color(Player)", test_color)))
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

