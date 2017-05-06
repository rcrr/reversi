/**
 * @file
 *
 * @brief Bit works module unit test suite.
 * @details Collects tests and helper methods for the bit works module.
 *
 * @par ut_bit_works.c
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



/*
 * Test functions.
 */

static void
bit_works_int_size_definition_checks (ut_test_t *const t)
{
  uint8_t  ui8;
  uint16_t ui16;
  uint32_t ui32;
  uint64_t ui64;
  int8_t   si8;

  /* uint8_t must be one byte. */
  ui8 = 0xFF;
  ut_assert(t, 255 == ui8);
  ut_assert(t, 0 == ++ui8);
  ut_assert(t, 1 == sizeof(ui8));

  /* uint16_t must be two bytes. */
  ui16 = 0xFFFF;
  ut_assert(t, 65535 == ui16);
  ut_assert(t, 0 == ++ui16);
  ui16 = 0x00FF;
  ut_assert(t, 0x0100 == ++ui16);
  ut_assert(t, 2 == sizeof(ui16));

  /* uint32_t must be four bytes. */
  ui32 = 0xFFFFFFFF;
  ut_assert(t, 4294967295 == ui32);
  ut_assert(t, 0 == ++ui32);
  ui32 = 0x0000FFFF;
  ut_assert(t, 0x00010000 == ++ui32);
  ut_assert(t, 4 == sizeof(ui32));

  /* uint64_t must be eight bytes. */
  ui64 = 0xFFFFFFFFFFFFFFFF;
  ut_assert(t, 18446744073709551615ULL == ui64);
  ut_assert(t, 0 == ++ui64);
  ui64 = 0x00000000FFFFFFFF;
  ut_assert(t, 0x0000000100000000 == ++ui64);
  ut_assert(t, 8 == sizeof(ui64));

  /* int8_t must be one byte. */
  si8 = 127;
  ut_assert(t, 1 == sizeof(si8));

}

static void
bit_works_bitcount_64_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bit_works_bitcount_64(0x00ULL));
  ut_assert(t,  1 == bit_works_bitcount_64(0x01ULL));
  ut_assert(t,  1 == bit_works_bitcount_64(0x02ULL));
  ut_assert(t,  2 == bit_works_bitcount_64(0x03ULL));
  ut_assert(t, 64 == bit_works_bitcount_64(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t, 32 == bit_works_bitcount_64(0xAAAAAAAAAAAAAAAAULL));
  ut_assert(t, 32 == bit_works_bitcount_64(0x5555555555555555ULL));
  ut_assert(t,  4 == bit_works_bitcount_64(0xF000000000000000ULL));
  ut_assert(t,  4 == bit_works_bitcount_64(0x000000000000000FULL));
  ut_assert(t,  8 == bit_works_bitcount_64(0xFF00000000000000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64(0x00FF000000000000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64(0x0000FF0000000000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64(0x000000FF00000000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64(0x00000000FF000000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64(0x0000000000FF0000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64(0x000000000000FF00ULL));
  ut_assert(t,  8 == bit_works_bitcount_64(0x00000000000000FFULL));
}

static void
bit_works_bitcount_64_popcnt_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bit_works_bitcount_64_popcnt(0x00ULL));
  ut_assert(t,  1 == bit_works_bitcount_64_popcnt(0x01ULL));
  ut_assert(t,  1 == bit_works_bitcount_64_popcnt(0x02ULL));
  ut_assert(t,  2 == bit_works_bitcount_64_popcnt(0x03ULL));
  ut_assert(t, 64 == bit_works_bitcount_64_popcnt(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t, 32 == bit_works_bitcount_64_popcnt(0xAAAAAAAAAAAAAAAAULL));
  ut_assert(t, 32 == bit_works_bitcount_64_popcnt(0x5555555555555555ULL));
  ut_assert(t,  4 == bit_works_bitcount_64_popcnt(0xF000000000000000ULL));
  ut_assert(t,  4 == bit_works_bitcount_64_popcnt(0x000000000000000FULL));
  ut_assert(t,  8 == bit_works_bitcount_64_popcnt(0xFF00000000000000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64_popcnt(0x00FF000000000000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64_popcnt(0x0000FF0000000000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64_popcnt(0x000000FF00000000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64_popcnt(0x00000000FF000000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64_popcnt(0x0000000000FF0000ULL));
  ut_assert(t,  8 == bit_works_bitcount_64_popcnt(0x000000000000FF00ULL));
  ut_assert(t,  8 == bit_works_bitcount_64_popcnt(0x00000000000000FFULL));
}



/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_init(&argc, &argv);
  ut_suite_t *const s = ut_suite_new("bit_works");

  ut_suite_add_simple_test(s, "bit_works_int_size_definition_checks", bit_works_int_size_definition_checks);
  ut_suite_add_simple_test(s, "bit_works_bitcount_64", bit_works_bitcount_64_t);
  ut_suite_add_simple_test(s, "bit_works_bitcount_64_popcnt", bit_works_bitcount_64_popcnt_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
