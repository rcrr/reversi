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
  const uint64_t init_key[] = { 0x12345ULL, 0x23456ULL, 0x34567ULL, 0x45678ULL };

  const size_t prand_array_length = 1000;

  const uint64_t expected_uint64_head[] = { 0x64D79B552A559D7F,
                                            0x44A572665A6EE240,
                                            0xEB2BF6DC3D72135C,
                                            0xE3836981F9F82EA0,
                                            0x43A38212350EE392,
                                            0xCE77502BFFCACF8B,
                                            0x5D8A82D90126F0E7,
                                            0xC0510C6F402C1E3C,
                                            0x48D895BF8B69F77B,
                                            0x8D9FBB371F1DE07F };

  const uint64_t expected_uint64_tail[] = { 0x77C8DA91B5313675,
                                            0x4CDB66AD515E0717,
                                            0x2EC4712B0BFDFCD6,
                                            0x6C6F5767FFF27330,
                                            0x071083B972D80C0C,
                                            0x8D8325E82C4FDCDC,
                                            0xB47A658DAD8E13A4,
                                            0x88710BF005FDA027,
                                            0x69BD3EDAF7111200,
                                            0x0DCCDD0C65C810FF };

  const double epsilon = 0.00000001;

  const double expected_double_head[] = { 0.35252031,
                                          0.51052342,
                                          0.79771733,
                                          0.39300273,
                                          0.27216673,
                                          0.72151068,
                                          0.43144703,
                                          0.38522290,
                                          0.20270676,
                                          0.58227313 };

  const double expected_double_tail[] = { 0.47863741,
                                          0.68796498,
                                          0.31526949,
                                          0.41180883,
                                          0.23022147,
                                          0.82342139,
                                          0.83003381,
                                          0.53571829,
                                          0.41081533,
                                          0.48600142 };

  const size_t key_length = sizeof(init_key) / sizeof(init_key[0]);
  const size_t e0_length = sizeof(expected_uint64_head) / sizeof(expected_uint64_head[0]);
  const size_t e1_length = sizeof(expected_uint64_tail) / sizeof(expected_uint64_tail[0]);
  const size_t e2_length = sizeof(expected_double_head) / sizeof(expected_double_head[0]);
  const size_t e3_length = sizeof(expected_double_tail) / sizeof(expected_double_tail[0]);

  prng_mt19937_t *prng = prng_mt19937_new();

  prng_mt19937_init_by_array(prng, init_key, key_length);

  uint64_t *prand_uint64_array = (uint64_t *) malloc(prand_array_length * sizeof(uint64_t));
  for (size_t i = 0; i < prand_array_length; i++) {
    prand_uint64_array[i] = prng_mt19937_get_uint64(prng);
  }

  for (size_t i = 0; i < e0_length; i++) {
    g_assert(prand_uint64_array[i] == expected_uint64_head[i]);
  }

  for (size_t i = 0; i < e1_length; i++) {
    g_assert(prand_uint64_array[(prand_array_length - e1_length) + i] == expected_uint64_tail[i]);
  }

  double *prand_double_array = (double *) malloc(prand_array_length * sizeof(double));
  for (size_t i = 0; i < prand_array_length; i++) {
    prand_double_array[i] = prng_mt19937_get_double_in_c0_o1(prng);
  }

  for (size_t i = 0; i < e2_length; i++) {
    g_assert(fabs(prand_double_array[i] - expected_double_head[i]) < epsilon);
  }

  for (size_t i = 0; i < e3_length; i++) {
    g_assert(fabs(prand_double_array[(prand_array_length - e3_length) + i] - expected_double_tail[i]) < epsilon);
  }

  prng_mt19937_free(prng);
  free(prand_uint64_array);
  free(prand_double_array);
}
