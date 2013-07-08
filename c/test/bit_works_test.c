/**
 * @file
 *
 * @brief Bit works unit test suite.
 * @details Collects tests and helper methods for the board module.
 *
 * @par bit_works_test.c
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

#include "bit_works.h"



/* Test function prototypes. */

static void dummy_test (void);
static void bit_works_type_size_test (void);
static void bit_works_bitscan_MS1B_to_base8_test (void);
static void bit_works_popcount_test (void);
static void bit_works_signed_left_shift_test (void);

/*
extern uint64
bit_works_signed_left_shift (uint64 bit_sequence, int shift);

extern uint32
bit_works_highest_bit_set_32 (uint32 bit_sequence);

extern uint8
bit_works_highest_bit_set_8 (uint8 bit_sequence);

extern uint8
bit_works_fill_in_between (uint8 bit_sequence);

extern uint8
bit_works_bitscanMS1B_64 (const uint64 bit_sequence);

extern uint8
bit_works_bitscanMS1B_8 (const uint8 bit_sequence);
*/


int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/bit_works/dummy", dummy_test);
  g_test_add_func("/bit_works/bit_works_signed_left_shift_test", bit_works_signed_left_shift_test);
  g_test_add_func("/bit_works/bit_works_popcount_test", bit_works_popcount_test);
  g_test_add_func("/bit_works/bit_works_type_size_test", bit_works_type_size_test);
  g_test_add_func("/bit_works/bit_works_bitscan_MS1B_to_base8_test", bit_works_bitscan_MS1B_to_base8_test);

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
bit_works_signed_left_shift_test (void)
{
  /* Sanity check. */
  g_assert(0x0000000000000000 == bit_works_signed_left_shift(0x0000000000000000,  0));
  g_assert(0x0000000000000001 == bit_works_signed_left_shift(0x0000000000000001,  0));
  g_assert(0xFFFFFFFFFFFFFFFF == bit_works_signed_left_shift(0xFFFFFFFFFFFFFFFF,  0));
  g_assert(0xFFFFFFFFFFFFFF00 == bit_works_signed_left_shift(0xFFFFFFFFFFFFFFFF, +8));
  g_assert(0x00FFFFFFFFFFFFFF == bit_works_signed_left_shift(0xFFFFFFFFFFFFFFFF, -8));
}

static void
bit_works_popcount_test (void)
{
  g_assert(0  == bit_works_popcount(0x00ULL));
  g_assert(1  == bit_works_popcount(0x01ULL));
  g_assert(1  == bit_works_popcount(0x02ULL));
  g_assert(2  == bit_works_popcount(0x03ULL));
  g_assert(64 == bit_works_popcount(0xFFFFFFFFFFFFFFFFULL));
}

static void
bit_works_type_size_test (void)
{
  g_assert(8 == sizeof(uint64));
  g_assert(4 == sizeof(uint32));
  g_assert(1 == sizeof(uint8));
}

static void
bit_works_bitscan_MS1B_to_base8_test (void)
{
  HiLo ret;
  uint64 bit_sequence;

  ret = (HiLo) { .hi = 0, .lo = 0 };
  bit_sequence = 1ULL << 0;
  bit_works_bitscan_MS1B_to_base8 (&ret, bit_sequence);
  g_assert((0 == ret.hi) && (0 == ret.lo));

  ret = (HiLo) { .hi = 0, .lo = 0 };
  bit_sequence = 1ULL << 1;
  bit_works_bitscan_MS1B_to_base8 (&ret, bit_sequence);
  g_assert((0 == ret.hi) && (1 == ret.lo));

  ret = (HiLo) { .hi = 0, .lo = 0 };
  bit_sequence = 1ULL << 7;
  bit_works_bitscan_MS1B_to_base8 (&ret, bit_sequence);
  g_assert((0 == ret.hi) && (7 == ret.lo));

  ret = (HiLo) { .hi = 0, .lo = 0 };
  bit_sequence = 1ULL << 8;
  bit_works_bitscan_MS1B_to_base8 (&ret, bit_sequence);
  g_assert((1 == ret.hi) && (0 == ret.lo));

  ret = (HiLo) { .hi = 0, .lo = 0 };
  bit_sequence = 1ULL << 63;
  bit_works_bitscan_MS1B_to_base8 (&ret, bit_sequence);
  g_assert((7 == ret.hi) && (7 == ret.lo));

}

