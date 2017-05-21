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
bitw_type_size_checks (ut_test_t *const t)
{
  ut_assert(t, 8 == sizeof(uint64_t));
  ut_assert(t, 4 == sizeof(uint32_t));
  ut_assert(t, 2 == sizeof(uint16_t));
  ut_assert(t, 1 == sizeof(uint8_t));
}

static void
bitw_int_size_definition_checks (ut_test_t *const t)
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
bitw_bit_count_64_plain_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bitw_bit_count_64_plain(0x00ULL));
  ut_assert(t,  1 == bitw_bit_count_64_plain(0x01ULL));
  ut_assert(t,  1 == bitw_bit_count_64_plain(0x02ULL));
  ut_assert(t,  2 == bitw_bit_count_64_plain(0x03ULL));
  ut_assert(t, 64 == bitw_bit_count_64_plain(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t, 32 == bitw_bit_count_64_plain(0xAAAAAAAAAAAAAAAAULL));
  ut_assert(t, 32 == bitw_bit_count_64_plain(0x5555555555555555ULL));
  ut_assert(t,  4 == bitw_bit_count_64_plain(0xF000000000000000ULL));
  ut_assert(t,  4 == bitw_bit_count_64_plain(0x000000000000000FULL));
  ut_assert(t,  8 == bitw_bit_count_64_plain(0xFF00000000000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_plain(0x00FF000000000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_plain(0x0000FF0000000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_plain(0x000000FF00000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_plain(0x00000000FF000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_plain(0x0000000000FF0000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_plain(0x000000000000FF00ULL));
  ut_assert(t,  8 == bitw_bit_count_64_plain(0x00000000000000FFULL));
}

#ifdef __POPCNT__
static void
bitw_bit_count_64_popcnt_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bitw_bit_count_64_popcnt(0x00ULL));
  ut_assert(t,  1 == bitw_bit_count_64_popcnt(0x01ULL));
  ut_assert(t,  1 == bitw_bit_count_64_popcnt(0x02ULL));
  ut_assert(t,  2 == bitw_bit_count_64_popcnt(0x03ULL));
  ut_assert(t, 64 == bitw_bit_count_64_popcnt(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t, 32 == bitw_bit_count_64_popcnt(0xAAAAAAAAAAAAAAAAULL));
  ut_assert(t, 32 == bitw_bit_count_64_popcnt(0x5555555555555555ULL));
  ut_assert(t,  4 == bitw_bit_count_64_popcnt(0xF000000000000000ULL));
  ut_assert(t,  4 == bitw_bit_count_64_popcnt(0x000000000000000FULL));
  ut_assert(t,  8 == bitw_bit_count_64_popcnt(0xFF00000000000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_popcnt(0x00FF000000000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_popcnt(0x0000FF0000000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_popcnt(0x000000FF00000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_popcnt(0x00000000FF000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_popcnt(0x0000000000FF0000ULL));
  ut_assert(t,  8 == bitw_bit_count_64_popcnt(0x000000000000FF00ULL));
  ut_assert(t,  8 == bitw_bit_count_64_popcnt(0x00000000000000FFULL));
}
#endif

static void
bitw_bit_count_64_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bitw_bit_count_64(0x00ULL));
  ut_assert(t,  1 == bitw_bit_count_64(0x01ULL));
  ut_assert(t,  1 == bitw_bit_count_64(0x02ULL));
  ut_assert(t,  2 == bitw_bit_count_64(0x03ULL));
  ut_assert(t, 64 == bitw_bit_count_64(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t, 32 == bitw_bit_count_64(0xAAAAAAAAAAAAAAAAULL));
  ut_assert(t, 32 == bitw_bit_count_64(0x5555555555555555ULL));
  ut_assert(t,  4 == bitw_bit_count_64(0xF000000000000000ULL));
  ut_assert(t,  4 == bitw_bit_count_64(0x000000000000000FULL));
  ut_assert(t,  8 == bitw_bit_count_64(0xFF00000000000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64(0x00FF000000000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64(0x0000FF0000000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64(0x000000FF00000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64(0x00000000FF000000ULL));
  ut_assert(t,  8 == bitw_bit_count_64(0x0000000000FF0000ULL));
  ut_assert(t,  8 == bitw_bit_count_64(0x000000000000FF00ULL));
  ut_assert(t,  8 == bitw_bit_count_64(0x00000000000000FFULL));
}

static void
bitw_bit_scan_reverse_64_plain_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bitw_bit_scan_reverse_64_plain(0x0000000000000001));
  ut_assert(t,  4 == bitw_bit_scan_reverse_64_plain(0x0000000000000010));
  ut_assert(t, 63 == bitw_bit_scan_reverse_64_plain(0x8000000000000000));
  ut_assert(t, 63 == bitw_bit_scan_reverse_64_plain(0xFFFFFFFFFFFFFFFF));
  ut_assert(t,  1 == bitw_bit_scan_reverse_64_plain(0x0000000000000003));

  bitw_bit_scan_reverse_64_plain(0);
  ut_assert(t, true);
}

#ifdef __x86_64__
static void
bitw_bit_scan_reverse_64_bsr_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bitw_bit_scan_reverse_64_bsr(0x0000000000000001));
  ut_assert(t,  4 == bitw_bit_scan_reverse_64_bsr(0x0000000000000010));
  ut_assert(t, 63 == bitw_bit_scan_reverse_64_bsr(0x8000000000000000));
  ut_assert(t, 63 == bitw_bit_scan_reverse_64_bsr(0xFFFFFFFFFFFFFFFF));
  ut_assert(t,  1 == bitw_bit_scan_reverse_64_bsr(0x0000000000000003));

  bitw_bit_scan_reverse_64_bsr(0);
  ut_assert(t, true);
}
#endif

static void
bitw_bit_scan_reverse_64_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bitw_bit_scan_reverse_64(0x0000000000000001));
  ut_assert(t,  4 == bitw_bit_scan_reverse_64(0x0000000000000010));
  ut_assert(t, 63 == bitw_bit_scan_reverse_64(0x8000000000000000));
  ut_assert(t, 63 == bitw_bit_scan_reverse_64(0xFFFFFFFFFFFFFFFF));
  ut_assert(t,  1 == bitw_bit_scan_reverse_64(0x0000000000000003));

  bitw_bit_scan_reverse_64(0);
  ut_assert(t, true);
}

static void
bitw_bit_scan_forward_64_plain_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bitw_bit_scan_forward_64_plain(0x0000000000000001));
  ut_assert(t,  4 == bitw_bit_scan_forward_64_plain(0x0000000000000010));
  ut_assert(t, 63 == bitw_bit_scan_forward_64_plain(0x8000000000000000));
  ut_assert(t,  0 == bitw_bit_scan_forward_64_plain(0xFFFFFFFFFFFFFFFF));
  ut_assert(t,  0 == bitw_bit_scan_forward_64_plain(0x0000000000000003));

  bitw_bit_scan_forward_64_plain(0);
  ut_assert(t, true);
}

#ifdef __x86_64__
static void
bitw_bit_scan_forward_64_bsf_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bitw_bit_scan_forward_64_bsf(0x0000000000000001));
  ut_assert(t,  4 == bitw_bit_scan_forward_64_bsf(0x0000000000000010));
  ut_assert(t, 63 == bitw_bit_scan_forward_64_bsf(0x8000000000000000));
  ut_assert(t,  0 == bitw_bit_scan_forward_64_bsf(0xFFFFFFFFFFFFFFFF));
  ut_assert(t,  0 == bitw_bit_scan_forward_64_bsf(0x0000000000000003));

  bitw_bit_scan_forward_64_bsf(0);
  ut_assert(t, true);
}
#endif

static void
bitw_bit_scan_forward_64_t (ut_test_t *const t)
{
  ut_assert(t,  0 == bitw_bit_scan_forward_64(0x0000000000000001));
  ut_assert(t,  4 == bitw_bit_scan_forward_64(0x0000000000000010));
  ut_assert(t, 63 == bitw_bit_scan_forward_64(0x8000000000000000));
  ut_assert(t,  0 == bitw_bit_scan_forward_64(0xFFFFFFFFFFFFFFFF));
  ut_assert(t,  0 == bitw_bit_scan_forward_64(0x0000000000000003));

  bitw_bit_scan_forward_64(0);
  ut_assert(t, true);
}

static void
bitw_reset_lowest_set_bit_64_plain_t (ut_test_t *const t)
{
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64_plain(0x0000000000000001));
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64_plain(0x0000000000000002));
  ut_assert(t, 0x0000000000000002 == bitw_reset_lowest_set_bit_64_plain(0x0000000000000003));
  ut_assert(t, 0xFFFFFFFFFFFFFFFE == bitw_reset_lowest_set_bit_64_plain(0xFFFFFFFFFFFFFFFF));
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64_plain(0x8000000000000000));
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64_plain(0x0000000000000000));
}

#ifdef __x86_64__
static void
bitw_reset_lowest_set_bit_64_blsr_t (ut_test_t *const t)
{
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64_blsr(0x0000000000000001));
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64_blsr(0x0000000000000002));
  ut_assert(t, 0x0000000000000002 == bitw_reset_lowest_set_bit_64_blsr(0x0000000000000003));
  ut_assert(t, 0xFFFFFFFFFFFFFFFE == bitw_reset_lowest_set_bit_64_blsr(0xFFFFFFFFFFFFFFFF));
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64_blsr(0x8000000000000000));
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64_blsr(0x0000000000000000));
}
#endif

static void
bitw_reset_lowest_set_bit_64_t (ut_test_t *const t)
{
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64(0x0000000000000001));
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64(0x0000000000000002));
  ut_assert(t, 0x0000000000000002 == bitw_reset_lowest_set_bit_64(0x0000000000000003));
  ut_assert(t, 0xFFFFFFFFFFFFFFFE == bitw_reset_lowest_set_bit_64(0xFFFFFFFFFFFFFFFF));
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64(0x8000000000000000));
  ut_assert(t, 0x0000000000000000 == bitw_reset_lowest_set_bit_64(0x0000000000000000));
}

static void
bitw_lowest_set_bit_64_t (ut_test_t *const t)
{
 ut_assert(t, 0x01 == bitw_lowest_set_bit_64(0xFF));
 ut_assert(t, 0x02 == bitw_lowest_set_bit_64(0xFE));
 ut_assert(t, 0x80 == bitw_lowest_set_bit_64(0x80));
 ut_assert(t, 0x40 == bitw_lowest_set_bit_64(0xC0));

 ut_assert(t, 0x0000000000000000 == bitw_lowest_set_bit_64(0x0000000000000000));
 ut_assert(t, 0x1000000000000000 == bitw_lowest_set_bit_64(0xF000000000000000));
 ut_assert(t, 0x0000000000000001 == bitw_lowest_set_bit_64(0x000000000000000F));
 ut_assert(t, 0x0000000000000001 == bitw_lowest_set_bit_64(0xFFFFFFFFFFFFFFFF));
 ut_assert(t, 0x8000000000000000 == bitw_lowest_set_bit_64(0x8000000000000000));
}

static void
bitw_highest_set_bit_64_t (ut_test_t *const t)
{
 ut_assert(t, 0x80 == bitw_highest_set_bit_64(0xFF));
 ut_assert(t, 0x80 == bitw_highest_set_bit_64(0xFE));
 ut_assert(t, 0x80 == bitw_highest_set_bit_64(0x80));
 ut_assert(t, 0x40 == bitw_highest_set_bit_64(0x7F));

 ut_assert(t, 0x0000000000000000 == bitw_highest_set_bit_64(0x0000000000000000));
 ut_assert(t, 0x8000000000000000 == bitw_highest_set_bit_64(0xF000000000000000));
 ut_assert(t, 0x0000000000000008 == bitw_highest_set_bit_64(0x000000000000000F));
 ut_assert(t, 0x8000000000000000 == bitw_highest_set_bit_64(0xFFFFFFFFFFFFFFFF));
 ut_assert(t, 0x8000000000000000 == bitw_highest_set_bit_64(0x8000000000000000));
 ut_assert(t, 0x8000000000000000 == bitw_highest_set_bit_64(0x8000000000000001));
 ut_assert(t, 0x0000000000000001 == bitw_highest_set_bit_64(0x0000000000000001));
}

static void
bitw_ror_64_t (ut_test_t *const t)
{
  uint64_t bit_sequence, result;
  unsigned int shift;

  bit_sequence = 0ULL;
  shift = 0;
  result = bitw_ror_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0xFFFFFFFFFFFFFFFF;
  shift = 0;
  result = bitw_ror_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 0;
  result = bitw_ror_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0ULL;
  shift = 64;
  result = bitw_ror_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0xFFFFFFFFFFFFFFFF;
  shift = 64;
  result = bitw_ror_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 64;
  result = bitw_ror_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0x0000000000000001;
  shift = 1;
  result = bitw_ror_64(bit_sequence, shift);
  ut_assert(t, 0x8000000000000000 == result);

  bit_sequence = 0x8000000000000000;
  shift = 1;
  result = bitw_ror_64(bit_sequence, shift);
  ut_assert(t, 0x4000000000000000 == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 4;
  result = bitw_ror_64(bit_sequence, shift);
  ut_assert(t, 0xF0F0F0F0F0F0F0F0 == result);

  bit_sequence = 0xF0F0F0F0F0F0F0F0;
  shift = 4;
  result = bitw_ror_64(bit_sequence, shift);
  ut_assert(t, 0x0F0F0F0F0F0F0F0F == result);
}

static void
bitw_rol_64_t (ut_test_t *const t)
{
  uint64_t bit_sequence, result;
  unsigned int shift;

  bit_sequence = 0ULL;
  shift = 0;
  result = bitw_rol_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0xFFFFFFFFFFFFFFFF;
  shift = 0;
  result = bitw_rol_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 0;
  result = bitw_rol_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0ULL;
  shift = 64;
  result = bitw_rol_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0xFFFFFFFFFFFFFFFF;
  shift = 64;
  result = bitw_rol_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 64;
  result = bitw_rol_64(bit_sequence, shift);
  ut_assert(t, bit_sequence == result);

  bit_sequence = 0x0000000000000001;
  shift = 1;
  result = bitw_rol_64(bit_sequence, shift);
  ut_assert(t, 0x0000000000000002 == result);

  bit_sequence = 0x8000000000000000;
  shift = 1;
  result = bitw_rol_64(bit_sequence, shift);
  ut_assert(t, 0x0000000000000001 == result);

  bit_sequence = 0x0F0F0F0F0F0F0F0F;
  shift = 4;
  result = bitw_rol_64(bit_sequence, shift);
  ut_assert(t, 0xF0F0F0F0F0F0F0F0 == result);

  bit_sequence = 0xF0F0F0F0F0F0F0F0;
  shift = 4;
  result = bitw_rol_64(bit_sequence, shift);
  ut_assert(t, 0x0F0F0F0F0F0F0F0F == result);
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

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_type_size_checks", bitw_type_size_checks);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_int_size_definition_checks", bitw_int_size_definition_checks);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_count_64_plain", bitw_bit_count_64_plain_t);
#ifdef __POPCNT__
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_count_64_popcnt", bitw_bit_count_64_popcnt_t);
#endif
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_count_64", bitw_bit_count_64_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_scan_reverse_64_plain",  bitw_bit_scan_reverse_64_plain_t);
#ifdef __x86_64__
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_scan_reverse_64_bsr",  bitw_bit_scan_reverse_64_bsr_t);
#endif
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_scan_reverse_64",  bitw_bit_scan_reverse_64_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_scan_forward_64_plain",  bitw_bit_scan_forward_64_plain_t);
#ifdef __x86_64__
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_scan_forward_64_bsf",  bitw_bit_scan_forward_64_bsf_t);
#endif
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_scan_forward_64",  bitw_bit_scan_forward_64_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_reset_lowest_set_bit_64_plain",  bitw_reset_lowest_set_bit_64_plain_t);
#ifdef __x86_64__
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_reset_lowest_set_bit_64_blsr",  bitw_reset_lowest_set_bit_64_blsr_t);
#endif
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_reset_lowest_set_bit_64",  bitw_reset_lowest_set_bit_64_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_lowest_set_bit_64",  bitw_lowest_set_bit_64_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_highest_set_bit_64",  bitw_highest_set_bit_64_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_ror_64_t",  bitw_ror_64_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_rol_64_t",  bitw_rol_64_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
