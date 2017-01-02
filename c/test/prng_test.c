/**
 * @file
 *
 * @brief Pseudo random number generator unit test suite.
 * @details Collects tests and helper methods for the pseudo random number generator module.
 *
 * @par prng_test.c
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
#include <math.h>
#include <stdint.h>

#include <glib.h>

#include "prng.h"



/* Test function prototypes. */

static void dummy_test (void);
static void basic_test (void);



/* Helper function prototypes. */



/* Main function. */

int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/prng/dummy", dummy_test);
  g_test_add_func("/prng/basic", basic_test);

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
basic_test (void)
{
  int i;
  uint64_t init[4] = { 0x12345ULL, 0x23456ULL, 0x34567ULL, 0x45678ULL }, length = 4;
  prng_mt19937_init_by_array(init, length);
  printf("1000 outputs of genrand64_int64()\n");
  for (i = 0; i < 1000; i++) {
    printf("%20zu ", prng_mt19937_get_uint64());
    if (i % 5 == 4) printf("\n");
  }
  printf("\n1000 outputs of genrand64_real2()\n");
  for (i = 0; i < 1000; i++) {
    printf("%10.8f ", prng_mt19937_get_double_in_c0_o1());
    if (i % 5 == 4) printf("\n");
  }

  g_assert(TRUE);
}
