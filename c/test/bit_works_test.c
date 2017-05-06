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
 * @copyright 2013, 2016, 2017 Roberto Corradini. All rights reserved.
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
static void bit_works_int_size_definition_checks (void);
static void bit_works_lowest_bit_set_8_test (void);
static void bit_works_fill_in_between_test (void);
static void bit_works_bitscanMS1B_8_test (void);
static void bit_works_bitscanMS1B_64_test (void);
static void bit_works_bitscanLS1B_64_test (void);
static void bit_works_highest_bit_set_8_test (void);
static void bit_works_highest_bit_set_32_test (void);
static void bit_works_type_size_test (void);
static void bit_works_bitcount_64_test (void);
static void bit_works_signed_left_shift_test (void);
static void bit_works_ror_64_test (void);
static void bit_works_rol_64_test (void);



int
main (int   argc,
      char *argv[])
{
  g_test_init (&argc, &argv, NULL);

  g_test_add_func("/bit_works/dummy", dummy_test);
  g_test_add_func("/bit_works/bit_works_int_size_definition_checks", bit_works_int_size_definition_checks);
  g_test_add_func("/bit_works/bit_works_lowest_bit_set_8_test", bit_works_lowest_bit_set_8_test);
  g_test_add_func("/bit_works/bit_works_fill_in_between_test", bit_works_fill_in_between_test);
  g_test_add_func("/bit_works/bit_works_bitscanMS1B_8_test", bit_works_bitscanMS1B_8_test);
  g_test_add_func("/bit_works/bit_works_bitscanMS1B_64_test", bit_works_bitscanMS1B_64_test);
  g_test_add_func("/bit_works/bit_works_bitscanLS1B_64_test", bit_works_bitscanLS1B_64_test);
  g_test_add_func("/bit_works/bit_works_highest_bit_set_8_test", bit_works_highest_bit_set_8_test);
  g_test_add_func("/bit_works/bit_works_highest_bit_set_32_test", bit_works_highest_bit_set_32_test);
  g_test_add_func("/bit_works/bit_works_signed_left_shift_test", bit_works_signed_left_shift_test);
  g_test_add_func("/bit_works/bit_works_bitcount_64_test", bit_works_bitcount_64_test);
  g_test_add_func("/bit_works/bit_works_type_size_test", bit_works_type_size_test);
  g_test_add_func("/bit_works/bit_works_ror_64", bit_works_ror_64_test);
  g_test_add_func("/bit_works/bit_works_rol_64", bit_works_rol_64_test);

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
bit_works_int_size_definition_checks (void)
{
  uint8_t  ui8;
  uint16_t ui16;
  uint32_t ui32;
  uint64_t ui64;
  int8_t   si8;

  /* uint8_t must be one byte. */
  ui8 = 0xFF;
  g_assert(255 == ui8);
  g_assert(0 == ++ui8);
  g_assert(1 == sizeof(ui8));

  /* uint16_t must be two bytes. */
  ui16 = 0xFFFF;
  g_assert(65535 == ui16);
  g_assert(0 == ++ui16);
  ui16 = 0x00FF;
  g_assert(0x0100 == ++ui16);
  g_assert(2 == sizeof(ui16));

  /* uint32_t must be four bytes. */
  ui32 = 0xFFFFFFFF;
  g_assert(4294967295 == ui32);
  g_assert(0 == ++ui32);
  ui32 = 0x0000FFFF;
  g_assert(0x00010000 == ++ui32);
  g_assert(4 == sizeof(ui32));

  /* uint64_t must be eight bytes. */
  ui64 = 0xFFFFFFFFFFFFFFFF;
  g_assert(18446744073709551615ULL == ui64);
  g_assert(0 == ++ui64);
  ui64 = 0x00000000FFFFFFFF;
  g_assert(0x0000000100000000 == ++ui64);
  g_assert(8 == sizeof(ui64));

  /* int8_t must be one byte. */
  si8 = 127;
  g_assert(1 == sizeof(si8));

}

static void
bit_works_lowest_bit_set_8_test (void)
{
 g_assert(0x01 == bit_works_lowest_bit_set_8(0xFF));
 g_assert(0x02 == bit_works_lowest_bit_set_8(0xFE));
 g_assert(0x80 == bit_works_lowest_bit_set_8(0x80));
 g_assert(0x40 == bit_works_lowest_bit_set_8(0xC0));
}

static void
bit_works_fill_in_between_test (void)
{
  g_assert(0x02 == bit_works_fill_in_between(0x05));
  g_assert(0x06 == bit_works_fill_in_between(0x09));
  g_assert(0x0E == bit_works_fill_in_between(0x11));
  g_assert(0x1E == bit_works_fill_in_between(0x21));
  g_assert(0x3E == bit_works_fill_in_between(0x41));
  g_assert(0x7E == bit_works_fill_in_between(0x81));

  g_assert(0x60 == bit_works_fill_in_between(0x90));

  g_assert(0x00 == bit_works_fill_in_between(0x01));
  g_assert(0x00 == bit_works_fill_in_between(0x02));
  g_assert(0x00 == bit_works_fill_in_between(0x04));
  g_assert(0x00 == bit_works_fill_in_between(0x08));
  g_assert(0x00 == bit_works_fill_in_between(0x10));
  g_assert(0x00 == bit_works_fill_in_between(0x20));
  g_assert(0x00 == bit_works_fill_in_between(0x40));
  g_assert(0x00 == bit_works_fill_in_between(0x80));
}

static void
bit_works_bitscanMS1B_8_test (void)
{
  g_assert(0 == bit_works_bitscanMS1B_8(0x01));
  g_assert(4 == bit_works_bitscanMS1B_8(0x10));
  g_assert(7 == bit_works_bitscanMS1B_8(0x80));
  g_assert(7 == bit_works_bitscanMS1B_8(0xFF));
}

static void
bit_works_bitscanMS1B_64_test (void)
{
  g_assert( 0 == bit_works_bitscanMS1B_64(0x0000000000000001));
  g_assert( 4 == bit_works_bitscanMS1B_64(0x0000000000000010));
  g_assert(63 == bit_works_bitscanMS1B_64(0x8000000000000000));
  g_assert(63 == bit_works_bitscanMS1B_64(0xFFFFFFFFFFFFFFFF));

  g_assert( 1 == bit_works_bitscanMS1B_64(0x0000000000000003));
}

static void
bit_works_bitscanLS1B_64_test (void)
{
  g_assert( 0 == bit_works_bitscanLS1B_64(0x0000000000000001));
  g_assert( 4 == bit_works_bitscanLS1B_64(0x0000000000000010));
  g_assert(63 == bit_works_bitscanLS1B_64(0x8000000000000000));
  g_assert( 0 == bit_works_bitscanLS1B_64(0xFFFFFFFFFFFFFFFF));

  g_assert( 0 == bit_works_bitscanLS1B_64(0x0000000000000003));
}

static void
bit_works_highest_bit_set_8_test (void)
{
  g_assert(0x00 == bit_works_highest_bit_set_8(0x00));
  g_assert(0x01 == bit_works_highest_bit_set_8(0x01));
  g_assert(0x02 == bit_works_highest_bit_set_8(0x02));
  g_assert(0x02 == bit_works_highest_bit_set_8(0x03));
  g_assert(0x80 == bit_works_highest_bit_set_8(0xFF));
}

static void
bit_works_highest_bit_set_32_test (void)
{
  g_assert(0x00000000 == bit_works_highest_bit_set_32(0x00000000));
  g_assert(0x00000001 == bit_works_highest_bit_set_32(0x00000001));
  g_assert(0x00000002 == bit_works_highest_bit_set_32(0x00000002));
  g_assert(0x00000002 == bit_works_highest_bit_set_32(0x00000003));
  g_assert(0x80000000 == bit_works_highest_bit_set_32(0xFFFFFFFF));
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
bit_works_bitcount_64_test (void)
{
  g_assert(0  == bit_works_bitcount_64_plain(0x00ULL));
  g_assert(1  == bit_works_bitcount_64_plain(0x01ULL));
  g_assert(1  == bit_works_bitcount_64_plain(0x02ULL));
  g_assert(2  == bit_works_bitcount_64_plain(0x03ULL));
  g_assert(64 == bit_works_bitcount_64_plain(0xFFFFFFFFFFFFFFFFULL));
}

static void
bit_works_type_size_test (void)
{
  g_assert(8 == sizeof(uint64_t));
  g_assert(4 == sizeof(uint32_t));
  g_assert(1 == sizeof(uint8_t));
}

static void
bit_works_ror_64_test (void)
{
  uint64_t bit_sequence, result;
  unsigned int shift;

  bit_sequence = 0ULL;
  shift = 0;
  result = bit_works_ror_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0xFFFFFFFFFFFFFFFF;
  shift = 0;
  result = bit_works_ror_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 0;
  result = bit_works_ror_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0ULL;
  shift = 64;
  result = bit_works_ror_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0xFFFFFFFFFFFFFFFF;
  shift = 64;
  result = bit_works_ror_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 64;
  result = bit_works_ror_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0x0000000000000001;
  shift = 1;
  result = bit_works_ror_64(bit_sequence, shift);
  g_assert(0x8000000000000000 == result);

  bit_sequence = 0x8000000000000000;
  shift = 1;
  result = bit_works_ror_64(bit_sequence, shift);
  g_assert(0x4000000000000000 == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 4;
  result = bit_works_ror_64(bit_sequence, shift);
  g_assert(0xF0F0F0F0F0F0F0F0 == result);

  bit_sequence = 0xF0F0F0F0F0F0F0F0;
  shift = 4;
  result = bit_works_ror_64(bit_sequence, shift);
  g_assert(0x0F0F0F0F0F0F0F0F == result);
}

static void
bit_works_rol_64_test (void)
{
  uint64_t bit_sequence, result;
  unsigned int shift;

  bit_sequence = 0ULL;
  shift = 0;
  result = bit_works_rol_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0xFFFFFFFFFFFFFFFF;
  shift = 0;
  result = bit_works_rol_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 0;
  result = bit_works_rol_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0ULL;
  shift = 64;
  result = bit_works_rol_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0xFFFFFFFFFFFFFFFF;
  shift = 64;
  result = bit_works_rol_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 64;
  result = bit_works_rol_64(bit_sequence, shift);
  g_assert(bit_sequence == result);

  bit_sequence = 0x0000000000000001;
  shift = 1;
  result = bit_works_rol_64(bit_sequence, shift);
  g_assert(0x0000000000000002 == result);

  bit_sequence = 0x8000000000000000;
  shift = 1;
  result = bit_works_rol_64(bit_sequence, shift);
  g_assert(0x0000000000000001 == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 4;
  result = bit_works_rol_64(bit_sequence, shift);
  g_assert(0xF0F0F0F0F0F0F0F0 == result);

  bit_sequence = 0xF0F0F0F0F0F0F0F0;
  shift = 4;
  result = bit_works_rol_64(bit_sequence, shift);
  g_assert(0x0F0F0F0F0F0F0F0F == result);
}
