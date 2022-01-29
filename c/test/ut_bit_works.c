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
 * @copyright 2017, 2019 Roberto Corradini. All rights reserved.
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
#include "bit_works.h"



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
bitw_lzcnt_64_plain_t (ut_test_t *const t)
{
  ut_assert(t, 64 == bitw_lzcnt_64_plain(0x00ULL));
  ut_assert(t, 63 == bitw_lzcnt_64_plain(0x01ULL));
  ut_assert(t, 62 == bitw_lzcnt_64_plain(0x02ULL));
  ut_assert(t, 62 == bitw_lzcnt_64_plain(0x03ULL));
  ut_assert(t,  0 == bitw_lzcnt_64_plain(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t,  7 == bitw_lzcnt_64_plain(0x0101010101010101ULL));
  ut_assert(t,  1 == bitw_lzcnt_64_plain(0x7000000000000000ULL));
}

#ifdef __LZCNT__
static void
bitw_lzcnt_64_lzcnt_t (ut_test_t *const t)
{
  ut_assert(t, 64 == bitw_lzcnt_64_lzcnt(0x00ULL));
  ut_assert(t, 63 == bitw_lzcnt_64_lzcnt(0x01ULL));
  ut_assert(t, 62 == bitw_lzcnt_64_lzcnt(0x02ULL));
  ut_assert(t, 62 == bitw_lzcnt_64_lzcnt(0x03ULL));
  ut_assert(t,  0 == bitw_lzcnt_64_lzcnt(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t,  7 == bitw_lzcnt_64_lzcnt(0x0101010101010101ULL));
  ut_assert(t,  1 == bitw_lzcnt_64_lzcnt(0x7000000000000000ULL));
}
#endif

static void
bitw_lzcnt_64_t (ut_test_t *const t)
{
  ut_assert(t, 64 == bitw_lzcnt_64(0x00ULL));
  ut_assert(t, 63 == bitw_lzcnt_64(0x01ULL));
  ut_assert(t, 62 == bitw_lzcnt_64(0x02ULL));
  ut_assert(t, 62 == bitw_lzcnt_64(0x03ULL));
  ut_assert(t,  0 == bitw_lzcnt_64(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t,  7 == bitw_lzcnt_64(0x0101010101010101ULL));
  ut_assert(t,  1 == bitw_lzcnt_64(0x7000000000000000ULL));
}

static void
bitw_tzcnt_64_plain_t (ut_test_t *const t)
{
  ut_assert(t, 64 == bitw_tzcnt_64_plain(0x00ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x01ULL));
  ut_assert(t,  1 == bitw_tzcnt_64_plain(0x02ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x03ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x0101010101010101ULL));
  ut_assert(t, 63 == bitw_tzcnt_64_plain(0x8000000000000000ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x8FFFFFFFFFFFFFFFULL));
  ut_assert(t,  4 == bitw_tzcnt_64_plain(0x0000000000000010ULL));
  ut_assert(t,  8 == bitw_tzcnt_64_plain(0x0000000000000100ULL));
}


#ifdef __BMI__
static void
bitw_tzcnt_64_tzcnt_t (ut_test_t *const t)
{
  ut_assert(t, 64 == bitw_tzcnt_64_plain(0x00ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x01ULL));
  ut_assert(t,  1 == bitw_tzcnt_64_plain(0x02ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x03ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x0101010101010101ULL));
  ut_assert(t, 63 == bitw_tzcnt_64_plain(0x8000000000000000ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x8FFFFFFFFFFFFFFFULL));
  ut_assert(t,  4 == bitw_tzcnt_64_plain(0x0000000000000010ULL));
  ut_assert(t,  8 == bitw_tzcnt_64_plain(0x0000000000000100ULL));
}
#endif

static void
bitw_tzcnt_64_t (ut_test_t *const t)
{
  ut_assert(t, 64 == bitw_tzcnt_64_plain(0x00ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x01ULL));
  ut_assert(t,  1 == bitw_tzcnt_64_plain(0x02ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x03ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0xFFFFFFFFFFFFFFFFULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x0101010101010101ULL));
  ut_assert(t, 63 == bitw_tzcnt_64_plain(0x8000000000000000ULL));
  ut_assert(t,  0 == bitw_tzcnt_64_plain(0x8FFFFFFFFFFFFFFFULL));
  ut_assert(t,  4 == bitw_tzcnt_64_plain(0x0000000000000010ULL));
  ut_assert(t,  8 == bitw_tzcnt_64_plain(0x0000000000000100ULL));
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

static void
bitw_pext_64_t (ut_test_t *const t)
{

  typedef uint64_t (*pext_f) (uint64_t bit_set, uint64_t mask);

  pext_f fs[] =
    {
     bitw_pext_64_plain,
#ifdef __BMI2__
     bitw_pext_64_pext,
#endif
     bitw_pext_64,
    };

  const size_t pext_fs_lenght = sizeof(fs) / sizeof(fs[0]);

  struct record
  {
    uint64_t expected;
    uint64_t value;
    uint64_t mask;
  };

  struct record data[] =
    {
     { 0x0000000000000000, 0xFFFFFFFFFFFFFFFF, 0x0000000000000000 },
     { 0x0000000000000001, 0xFFFFFFFFFFFFFFFF, 0x0000000000000001 },
     { 0x0000000000000001, 0xFFFFFFFFFFFFFFFF, 0x0000000000000002 },
     { 0x0000000000000000, 0xFFFFFFFFFFFFFF00, 0x0000000000000002 },
     { 0x0000000000000003, 0xFFFFFFFFFFFFFFFF, 0x0000000000000003 },
     { 0x0000000000000001, 0xFFFFFFFFFFFFFFFF, 0x8000000000000000 },
     { 0x0000000000000003, 0xFFFFFFFFFFFFFFFF, 0x8000000000000001 },
     { 0x0000000000000001, 0x00FFFFFFFFFFFFFF, 0x8000000000000001 },
     { 0x0000000000000002, 0xFFFFFFFFFFFFFF00, 0x8000000000000001 },
     { 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF },
    };

  const size_t data_lenght = sizeof(data) / sizeof(data[0]);

  ut_assert(t, 0xFFFFFFFFFFFFFFFF == bitw_pext_64_plain(0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF));
#ifdef __BMI2__
  ut_assert(t, 0x0000000000000000 == bitw_pext_64_pext(0xFFFFFFFFFFFFFFFF, 0x0000000000000000));
#endif
  ut_assert(t, 0x0000000000000000 == bitw_pext_64(0x0000000000000000, 0xFFFFFFFFFFFFFFFF));

  for (size_t j = 0; j < pext_fs_lenght; j++) {
    const pext_f pext = fs[j];
    for (size_t i = 0; i < data_lenght; i++) {
      const struct record r = data[i];
      const uint64_t result = pext(r.value, r.mask);
      if (result != r.expected) {
        fprintf(stderr, "Test failure!\n");
        fprintf(stderr,
                "i = %zu, expected = 0x%016lx, result = 0x%016lx, value = 0x%016lx, mask = 0x%016lx\n",
                i, r.expected, result, r.value, r.mask);
        ut_assert(t, false);
      }
    }
  }
}

static void
bitw_pdep_64_t (ut_test_t *const t)
{

  typedef uint64_t (*pdep_f) (uint64_t bit_set, uint64_t mask);

  pdep_f fs[] =
    {
     bitw_pdep_64_plain,
#ifdef __BMI2__
     bitw_pdep_64_pdep,
#endif
     bitw_pdep_64,
    };

  const size_t pdep_fs_lenght = sizeof(fs) / sizeof(fs[0]);

  struct record
  {
    uint64_t expected;
    uint64_t value;
    uint64_t mask;
  };

  struct record data[] =
    {
     { 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF },
     { 0x0000000000000000, 0x0000000000000000, 0xFFFFFFFFFFFFFFFF },
     { 0x0000000000000001, 0x0000000000000001, 0xFFFFFFFFFFFFFFFF },
     { 0x0000000000000003, 0x0000000000000003, 0xFFFFFFFFFFFFFFFF },
     { 0x8000000000000000, 0x8000000000000000, 0xFFFFFFFFFFFFFFFF },
     { 0xFF000000000000FF, 0xFF000000000000FF, 0xFFFFFFFFFFFFFFFF },
     { 0x0000000000000004, 0x0000000000000001, 0x0000000000000004 },
     { 0x0000000000000000, 0x0000000000000002, 0x0000000000000004 },
     { 0x0000000000000004, 0x0000000000000003, 0x0000000000000004 },
     { 0x0000000000005500, 0x0000000000000055, 0x000000000000FF00 },
     { 0x0000000000000000, 0x0000000000005500, 0x000000000000FF00 },
    };

  const size_t data_lenght = sizeof(data) / sizeof(data[0]);

  ut_assert(t, 0xFFFFFFFFFFFFFFFF == bitw_pdep_64_plain(0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF));
  ut_assert(t, 0x0000000000000000 == bitw_pdep_64_plain(0x0000000000000000, 0xFFFFFFFFFFFFFFFF));
#ifdef __BMI2__
  ut_assert(t, 0xFFFFFFFFFFFFFFFF == bitw_pdep_64_pdep(0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF));
  ut_assert(t, 0x0000000000000000 == bitw_pdep_64_pdep(0x0000000000000000, 0xFFFFFFFFFFFFFFFF));
#endif
  ut_assert(t, 0xFFFFFFFFFFFFFFFF == bitw_pdep_64(0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF));
  ut_assert(t, 0x0000000000000000 == bitw_pdep_64(0x0000000000000000, 0xFFFFFFFFFFFFFFFF));

  for (size_t j = 0; j < pdep_fs_lenght; j++) {
    const pdep_f pdep = fs[j];
    for (size_t i = 0; i < data_lenght; i++) {
      const struct record r = data[i];
      const uint64_t result = pdep(r.value, r.mask);
      if (result != r.expected) {
        fprintf(stderr, "Test failure!\n");
        fprintf(stderr,
                "j = %zu, i = %zu, expected = 0x%016lx, result = 0x%016lx, value = 0x%016lx, mask = 0x%016lx\n",
                j, i, r.expected, result, r.value, r.mask);
        ut_assert(t, false);
      }
    }
  }
}


/**
 * @brief Runs the test suite.
 */
int
main (int argc,
      char **argv)
{
  ut_prog_arg_config_t config;
  ut_init(&config, &argc, &argv);

  ut_suite_t *const s = ut_suite_new(&config, "bit_works");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_type_size_checks", bitw_type_size_checks);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_int_size_definition_checks", bitw_int_size_definition_checks);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_count_64_plain", bitw_bit_count_64_plain_t);
#ifdef __POPCNT__
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_count_64_popcnt", bitw_bit_count_64_popcnt_t);
#endif
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_bit_count_64", bitw_bit_count_64_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_lzcnt_64_plain", bitw_lzcnt_64_plain_t);
#ifdef __LZCNT__
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_lzcnt_64_lzcnt", bitw_lzcnt_64_lzcnt_t);
#endif
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_lzcnt_64", bitw_lzcnt_64_t);


  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_tzcnt_64_plain", bitw_tzcnt_64_plain_t);
#ifdef __BMI__
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_tzcnt_64_tzcnt", bitw_tzcnt_64_tzcnt_t);
#endif
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_tzcnt_64", bitw_tzcnt_64_t);


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

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_pext_64_t",  bitw_pext_64_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "bitw_pdep_64_t",  bitw_pdep_64_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
