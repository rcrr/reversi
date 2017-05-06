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

#include "board.h"
#include "bit_works.h"

/**
 * @cond
 */

/* When defined leverages the intel POPCNT instruction. */
#define X86_POPCNT



/*
 * Internal constants.
 */

static const uint64_t m1  = 0x5555555555555555; //binary: 0101...
static const uint64_t m2  = 0x3333333333333333; //binary: 00110011..
static const uint64_t m4  = 0x0f0f0f0f0f0f0f0f; //binary:  4 zeros,  4 ones ...
static const uint64_t m8  = 0x00ff00ff00ff00ff; //binary:  8 zeros,  8 ones ...
static const uint64_t m16 = 0x0000ffff0000ffff; //binary: 16 zeros, 16 ones ...
static const uint64_t m32 = 0x00000000ffffffff; //binary: 32 zeros, 32 ones
static const uint64_t hff = 0xffffffffffffffff; //binary: all ones
static const uint64_t h01 = 0x0101010101010101; //the sum of 256 to the power of 0,1,2,3...

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


/**
 * @cond
 */

unsigned int
bit_works_bitcount_64_plain (uint64_t x)
{
  x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
  x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits
  x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits
  return (x * h01) >> 56;         //returns left 8 bits of x + (x<<8) + (x<<16) + (x<<24) + ...
}

#ifdef __POPCNT__
extern unsigned int
bit_works_bitcount_64_popcnt (uint64_t x);
#endif

/**
 * @endcond
 */

/**
 * @brief Returns the count of the bit set to `1` in the `x` argument.
 *
 * This function has two distinct implementations:
 * - `bit_works_bitcount_64_plain`
 * - `bit_works_bitcount_64_popcnt`
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
 * @param [in] x the set that has to be counted
 * @return       the count of bit set in the `x` parameter
 */
extern unsigned int
bit_works_bitcount_64 (uint64_t x);

/**
 * @brief Returns a value computed shifting the `bit_sequence` parameter
 * to left by a signed amount given by the `shift` parameter.
 *
 * @param bit_sequence the value that will be shifted
 * @param shift        the number of position to shift
 * @return             the shifted value
 */
uint64_t
bit_works_signed_left_shift (uint64_t bit_sequence, int shift)
{
  return shift >= 0 ? bit_sequence << shift : bit_sequence >> -shift;
}

/**
 * @brief Returns an int value having all the bit set in `bit_sequence` turned to `0`
 * except the most significant one.
 *
 * When parameter `bit_sequence` is equal to `0` it returns `0`.
 *
 * @param bit_sequence the value analyzed
 * @return             an value having set the bit most significative found in bit_sequence
 */
uint32_t
bit_works_highest_bit_set_32 (uint32_t bit_sequence)
{
  if (bit_sequence == 0x00000000) {
    return 0x00000000;
  }
  uint32_t result = 0x00000001;
  uint32_t tmp = bit_sequence;
  if ((tmp & 0xFFFF0000) != 0x00000000) {
    tmp >>= 16;
    result = 0x00010000;
  }
  if  (tmp > 0x000000FF) {
    tmp >>=  8;
    result <<= 8;
  }
  result <<= log2_array[tmp];
  return result;
}

/**
 * @brief Returns an int value having all the bit set in `bit_sequence` turned to `0`
 * except the most significant one.
 *
 * When parameter `bit_sequence` is equal to `0` it returns `0`.
 *
 * @param bit_sequence the value analyzed
 * @return             an value having set the bit most significative found in bit_sequence
 */
uint8_t
bit_works_highest_bit_set_8 (uint8_t bit_sequence)
{
  if (bit_sequence == 0x00) {
    return 0x00;
  }
  uint8_t result = 0x01;
  result <<= log2_array[bit_sequence];
  return result;
}

/**
 * @brief The `bitsequence` parameter must have one or two bits set.
 * Returns a bit sequence having set the bits between the two, or zero
 * when only one bit is set.
 *
 * For example: `00100010` returns `00011100`.
 *
 * When the input data doesn't meet the requirements the result is unpredictable.
 *
 * @param [in] bit_sequence the value to be scanned
 * @return                  a bit sequence having the internal bits set
 */
uint8_t
bit_works_fill_in_between (uint8_t bit_sequence)
{
  return ((0x01 << bit_works_bitscanMS1B_8(bit_sequence)) - 0x01)
         & ((~bit_sequence & 0xFF) ^ (bit_sequence - 0x01));
}

/**
 * @brief Returns the index of the most significant bit set in the `bit_sequence` parameter.
 *
 * Parameter `bit_sequence` must be different from `0`.
 * If no bit set is found, meaning that `bit_sequence` is equal to `0`, `0` is
 * returned, that is clearly a wrong value.
 *
 * The proposed technique does three divide and conqueror steps, then makes a lookup in a table
 * hosting the log2 value for integers up to 255.
 *
 * So far it is the preferred choice for the reversi implementation.
 *
 * @param bit_sequence uint64_t value that is scanned
 * @return             the index `(0..63)` of the most significant bit set
 */
uint8_t
bit_works_bitscanMS1B_64 (const uint64_t bit_sequence)
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

/**
 * @brief Returns the index of the most significant bit set in the `bit_sequence` parameter.
 *
 * @param bit_sequence uint8_t value that is scanned
 * @return             the index (0..7) of the most significant bit set
 */
uint8_t
bit_works_bitscanMS1B_8 (const uint8_t bit_sequence)
{
  uint8_t result = 0x00;
  result |= log2_array[(int) bit_sequence];
  return result;
}

/**
 * @brief Returns the index of the least significant bit set in the `bit_sequence` parameter
 * via de Bruijn's perfect hashing.
 *
 * Parameter `bit_sequence` must be different from {@code 0L}.
 * If no bit set is found, meaning that `bit_sequence` is equal to `0ULL`, `63` is
 * returned, that is clearly a wrong value.
 *
 * See: <a href="https://chessprogramming.wikispaces.com/Bitscan#DeBruijnMultiplation" target="_blank">
 *      de Bruijn multiplication</a>
 *
 * @param bit_sequence value that is scanned
 * @return             the index of the least significant bit set
 */
uint8_t
bit_works_bitscanLS1B_64 (const uint64_t bit_sequence)
{
  /** mask isolates the least significant one bit (LS1B). */
  const uint64_t mask = bit_sequence & (-bit_sequence);
  return debruijn_64_index[(mask * debruijn_64_magic_constant) >> debruijn_64_shift_value];
}

/**
 *
 */
extern uint8_t
bit_works_bitscanLS1B_64_bsf (const uint64_t bit_sequence);

/**
 * @brief Returns all bits from `bit_sequence`, and reset (set to 0)
 * the bit that corresponds to the lowest bit set.
 *
 * @param bit_sequence the input value
 * @return             the filtered sequence
 */
uint64_t
bit_works_reset_lowest_bit_set_64 (const uint64_t bit_sequence)
{
  return (bit_sequence - 1) & bit_sequence;
}

/**
 *
 */
extern uint64_t
bit_works_reset_lowest_bit_set_64_blsr (const uint64_t bit_sequence);

/**
 * @brief Returns a bit sequence having one bit set, the lowest found
 * in the `bit_sequence` parameter.
 *
 * @param bit_sequence the input value
 * @return             the filtered sequence
 */
uint64_t
bit_works_lowest_bit_set_64 (const uint64_t bit_sequence)
{
  return (bit_sequence & (bit_sequence - 1)) ^ bit_sequence;
}

/**
 * @brief Returns a bit sequence having one bit set, the lowest found
 * in the `bit_sequence` parameter.
 *
 * @param bit_sequence the input value
 * @return             the filtered sequence
 */
uint32_t
bit_works_lowest_bit_set_32 (const uint32_t bit_sequence)
{
  return (bit_sequence & (bit_sequence - 1)) ^ bit_sequence;
}

/**
 * @brief Returns a bit sequence having one bit set, the lowest found
 * in the `bit_sequence` parameter.
 *
 * @param bit_sequence the input value
 * @return             the filtered sequence
 */
uint8_t
bit_works_lowest_bit_set_8 (const uint8_t bit_sequence)
{
  return (bit_sequence & (bit_sequence - 1)) ^ bit_sequence;
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
bit_works_ror_64 (const uint64_t bit_sequence,
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
bit_works_rol_64 (const uint64_t bit_sequence,
                  const unsigned int shift);
