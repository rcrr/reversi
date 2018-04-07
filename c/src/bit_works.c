/**
 * @file
 *
 * @brief Bit Works  module implementation.
 *
 * @par bit_works.c
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2013, 2014, 2016, 2017 Roberto Corradini. All rights reserved.
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

#include <stdio.h>
#include <stdlib.h>

#include "bit_works.h"

/**
 * @cond
 */

/*
 * INTERNAL constants.
 */

static const uint64_t m1  = 0x5555555555555555; // binary: 0101...
static const uint64_t m2  = 0x3333333333333333; // binary: 00110011..
static const uint64_t m4  = 0x0f0f0f0f0f0f0f0f; // binary:  4 zeros,  4 ones ...
static const uint64_t m8  = 0x00ff00ff00ff00ff; // binary:  8 zeros,  8 ones ...
static const uint64_t m16 = 0x0000ffff0000ffff; // binary: 16 zeros, 16 ones ...
static const uint64_t m32 = 0x00000000ffffffff; // binary: 32 zeros, 32 ones
static const uint64_t h01 = 0x0101010101010101; // the sum of 256 to the power of 0,1,2,3...

/* Log 2 array */
static const uint8_t log2_array[] = {
  0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
};

/* Array for de Bruijn multiplication. */
static const uint8_t debruijn_64_index[] = {
  63,  0, 58,  1, 59, 47, 53,  2,
  60, 39, 48, 27, 54, 33, 42,  3,
  61, 51, 37, 40, 49, 18, 28, 20,
  55, 30, 34, 11, 43, 14, 22,  4,
  62, 57, 46, 52, 38, 26, 32, 41,
  50, 36, 17, 19, 29, 10, 13, 21,
  56, 45, 25, 31, 35, 16,  9, 12,
  44, 24, 15,  8, 23,  7,  6,  5
};

/* 64 bit value for the de Bruijn's "magical" constant. */
static const uint64_t debruijn_64_magic_constant = 0x07EDD5E59A4E28C2ULL;

/* Right shift for the de Bruijn's algorithm. */
static const int debruijn_64_shift_value = 58;

/**
 * @endcond
 */



/*
 * Bit count functions.
 */

/**
 * @cond
 */

uint8_t
bitw_bit_count_64_plain (uint64_t bit_set)
{
  bit_set -= (bit_set >> 1) & m1;                    // puts count of each 2 bits into those 2 bits
  bit_set  = (bit_set & m2) + ((bit_set >> 2) & m2); // puts count of each 4 bits into those 4 bits
  bit_set  = (bit_set + (bit_set >> 4)) & m4;        // puts count of each 8 bits into those 8 bits
  return (bit_set * h01) >> 56;                      // returns left 8 bits of x + (x<<8) + (x<<16) + (x<<24) + ...
}

#ifdef __POPCNT__
extern uint8_t
bitw_bit_count_64_popcnt (uint64_t bit_set);
#endif

/**
 * @endcond
 */

/**
 * @brief Returns the count of the bit set to `1` in the `bit_set` argument.
 *
 * This function has two distinct implementations:
 * - `bitw_bit_count_64_plain`
 * - `bitw_bit_count_64_popcnt`
 *
 * Depending on the "compile time" value of the macro `__POPCNT__`, it
 * resolves to one of the two variants.
 *
 * The first implementation is plain `C` code, and should work on any
 * platform. It uses fewer arithmetic operations than any other known
 * implementation on machines with fast multiplication.
 * It uses 12 arithmetic operations, one of which is a multiply.
 *
 * The second implementation works on `x86` architecture that implements the `popcnt`
 * assembler instruction. The function call reduces to one ASM instruction
 * and it is always inlined.
 * This variant is always inlined.
 *
 * It is possible to call directly the two function underneath, this is usefull
 * for testing purposes, but it should be never done otherwise.
 *
 * @param [in] bit_set the set that has to be counted
 * @return             the count of bit set in the `bit_set` parameter
 */
extern uint8_t
bitw_bit_count_64 (uint64_t bit_set);



/*
 * Leading zero count functions.
 */

/**
 * @cond
 */

uint8_t
bitw_lzcnt_64_plain (const uint64_t bit_sequence)
{
  if (bit_sequence) return 63 - bitw_bit_scan_reverse_64_plain(bit_sequence);
  return 64;
}

#ifdef __ABM__
extern uint8_t
bitw_lzcnt_64_lzcnt (const uint64_t bit_sequence);
#endif

/**
 * @endcond
 */

/**
 * @brief Returns the count of leading zero bit set in the `bit_sequence` parameter.
 *
 * If the content of `bit_sequence` is 0, the result is `64`.
 *
 * This function has two distinct implementations:
 * - `bitw_lzcnt_64_plain`
 * - `bitw_lzcnt_64_lzcnt`
 *
 * Depending on the "compile time" value of the macro `__ABM__`, it
 * resolves to one of the two variants.
 *
 * The first implementation is plain `C` code, and should work on any
 * platform.
 *
 * The second implementation works on `x86` architecture that implements the `lzcnt`
 * assembler instruction. The function call reduces to one ASM instruction
 * and it is always inlined. This variant is always inlined.
 *
 * It is possible to call directly the two function underneath, this is usefull
 * for testing purposes, but it should be never done otherwise.
 *
 * @param bit_sequence value that is scanned
 * @return             the count `[0..64]` of leading zeros
 */
extern uint8_t
bitw_lzcnt_64 (const uint64_t bit_sequence);



/*
 * Bit scan reverse functions.
 */

/**
 * @cond
 */

uint8_t
bitw_bit_scan_reverse_64_plain (const uint64_t bit_sequence)
{
  uint64_t tmp = bit_sequence;
  uint8_t result = 0x00;
  if ((tmp & 0xFFFFFFFF00000000) != 0ULL) {
    tmp >>= 32;
    result  = 32;
  }
  if  (tmp > 0x000000000000FFFF) {
    tmp >>= 16;
    result |= 16;
  }
  if  (tmp > 0x00000000000000FF) {
    tmp >>=  8;
    result |=  8;
  }
  result |= log2_array[(int) tmp];
  return result;
}

#ifdef __x86_64__
extern uint8_t
bitw_bit_scan_reverse_64_bsr (const uint64_t bit_sequence);
#endif

/**
 * @endcond
 */

/**
 * @brief Returns the index of the most significant bit set in the `bit_sequence` parameter.
 *
 * If the content of `bit_sequence` is 0, the result is undefined.
 *
 * This function has two distinct implementations:
 * - `bitw_bit_scan_reverse_64_plain`
 * - `bitw_bit_scan_reverse_64_bsr`
 *
 * Depending on the "compile time" value of the macro `__x86_64__`, it
 * resolves to one of the two variants.
 *
 * The first implementation is plain `C` code, and should work on any
 * platform.
 *
 * The second implementation works on `x86` architecture that implements the `bsr`
 * assembler instruction. The function call reduces to one ASM instruction
 * and it is always inlined. This variant is always inlined.
 *
 * It is possible to call directly the two function underneath, this is usefull
 * for testing purposes, but it should be never done otherwise.
 *
 * @param bit_sequence value that is scanned
 * @return             the index `[0..63]` of the most significant bit set
 */
extern uint8_t
bitw_bit_scan_reverse_64 (const uint64_t bit_sequence);



/*
 * Bit scan forward functions.
 */

/**
 * @cond
 */

uint8_t
bitw_bit_scan_forward_64_plain (const uint64_t bit_sequence)
{
  const uint64_t mask = bit_sequence & (-bit_sequence);
  return debruijn_64_index[(mask * debruijn_64_magic_constant) >> debruijn_64_shift_value];
}

#ifdef __x86_64__
extern uint8_t
bitw_bit_scan_forward_64_bsf (const uint64_t bit_sequence);
#endif

/**
 * @endcond
 */

/**
 * @brief Returns the index of the least significant bit set in the `bit_sequence` parameter.
 *
 * If the content of `bit_sequence` is 0, the result is undefined.
 *
 * This function has two distinct implementations:
 * - `bitw_bit_scan_forward_64_plain`
 * - `bitw_bit_scan_forward_64_bsf`
 *
 * Depending on the "compile time" value of the macro `__x86_64__`, it
 * resolves to one of the two variants.
 *
 * The first implementation is plain `C` code, and should work on any
 * platform.
 *
 * The second implementation works on `x86` architecture that implements the `bsf`
 * assembler instruction. The function call reduces to one ASM instruction
 * and it is always inlined. This variant is always inlined.
 *
 * It is possible to call directly the two function underneath, this is usefull
 * for testing purposes, but it should be never done otherwise.
 *
 * @param bit_sequence value that is scanned
 * @return             the index `[0..63]` of the least significant bit set
 */
extern uint8_t
bitw_bit_scan_forward_64 (const uint64_t bit_sequence);



/*
 * Reset lowest set bit functions.
 */

/**
 * @cond
 */

uint64_t
bitw_reset_lowest_set_bit_64_plain (const uint64_t bit_sequence)
{
  return (bit_sequence - 1) & bit_sequence;
}

#ifdef __x86_64__
extern uint64_t
bitw_reset_lowest_set_bit_64_blsr (const uint64_t bit_sequence);
#endif

/**
 * @endcond
 */

/**
 * @brief Returns all bits from `bit_sequence`, and reset (set to 0)
 * the bit that corresponds to the lowest bit set.
 *
 * If the content of `bit_sequence` is 0, the result is undefined.
 *
 * This function has two distinct implementations:
 * - `bitw_reset_lowest_set_bit_64_plain`
 * - `bitw_reset_lowest_set_bit_64_blsr`
 *
 * Depending on the "compile time" value of the macro `__x86_64__`, it
 * resolves to one of the two variants.
 *
 * The first implementation is plain `C` code, and should work on any
 * platform.
 *
 * The second implementation works on `x86` architecture that implements the `blsr`
 * assembler instruction. The function call reduces to one ASM instruction
 * and it is always inlined. This variant is always inlined.
 *
 * It is possible to call directly the two function underneath, this is usefull
 * for testing purposes, but it should be never done otherwise.
 *
 * @param bit_sequence the input value
 * @return             the filtered sequence
 */
extern uint64_t
bitw_reset_lowest_set_bit_64 (const uint64_t bit_sequence);



/**
 * @brief Returns a bit sequence having one bit set, the lowest found
 * in the `bit_sequence` parameter.
 *
 * @param bit_sequence the input value
 * @return             the filtered sequence
 */
uint64_t
bitw_lowest_set_bit_64 (const uint64_t bit_sequence)
{
  return (bit_sequence & (bit_sequence - 1)) ^ bit_sequence;
}

/**
 * @brief Returns a value having all the bit set in `bit_sequence` turned to `0`
 * except the most significant one.
 *
 * When parameter `bit_sequence` is equal to `0` it returns `0`.
 *
 * @param bit_sequence the value analyzed
 * @return             an value having set the bit most significative found in bit_sequence
 */
uint64_t
bitw_highest_set_bit_64 (uint64_t bit_sequence)
{
  if (!bit_sequence) return 0;
  const uint8_t index = bitw_bit_scan_reverse_64(bit_sequence);
  return (uint64_t) 1 << index;
}

/**
 * @brief Returns a bit sequence by operating a circular right shift
 * on the `bit_sequence` parameter.
 *
 * The shift value has to be contained in the range `[0..64]`.
 * Unexpected results can happen when shift is out of the permitted range.
 *
 * @param bit_sequence the input value
 * @param shift        the amount to shift
 * @return             the rotaded sequence
 */
extern uint64_t
bitw_ror_64 (const uint64_t bit_sequence,
             const unsigned int shift);

/**
 * @brief Returns a bit sequence by operating a circular left shift
 * on the `bit_sequence` parameter.
 *
 * The shift value has to be contained in the range `[0..64]`.
 * Unexpected results can happen when shift is out of the permitted range.
 *
 * @param bit_sequence the input value
 * @param shift        the amount to shift
 * @return             the rotaded sequence
 */
extern uint64_t
bitw_rol_64 (const uint64_t bit_sequence,
             const unsigned int shift);

/**
 * @brief Unsigned integer based power function.
 *
 * @param b base for the operation
 * @param e exponent
 * @return  the value of `b^e`
 */
uint64_t
bitw_uipow (uint64_t b,
            uint64_t e)
{
  int r = 1;
  while (e) {
    if (e & 1) r *= b;
    e >>= 1;
    b *= b;
  }
  return r;
}
