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

#include "bit_works.h"

static const uint64 m1  = 0x5555555555555555; //binary: 0101...
static const uint64 m2  = 0x3333333333333333; //binary: 00110011..
static const uint64 m4  = 0x0f0f0f0f0f0f0f0f; //binary:  4 zeros,  4 ones ...
static const uint64 m8  = 0x00ff00ff00ff00ff; //binary:  8 zeros,  8 ones ...
static const uint64 m16 = 0x0000ffff0000ffff; //binary: 16 zeros, 16 ones ...
static const uint64 m32 = 0x00000000ffffffff; //binary: 32 zeros, 32 ones
static const uint64 hff = 0xffffffffffffffff; //binary: all ones
static const uint64 h01 = 0x0101010101010101; //the sum of 256 to the power of 0,1,2,3...

/* Log 2 array */
static const unsigned char log2_array[] = {
  0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
  7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
};

/*
 * This is a naive implementation, shown for comparison,
 * and to help in understanding the better functions.
 * It uses 24 arithmetic operations (shift, add, and).
 *
static int popcount_1(uint64 x) {
    x = (x & m1 ) + ((x >>  1) & m1 ); //put count of each  2 bits into those  2 bits 
    x = (x & m2 ) + ((x >>  2) & m2 ); //put count of each  4 bits into those  4 bits 
    x = (x & m4 ) + ((x >>  4) & m4 ); //put count of each  8 bits into those  8 bits 
    x = (x & m8 ) + ((x >>  8) & m8 ); //put count of each 16 bits into those 16 bits 
    x = (x & m16) + ((x >> 16) & m16); //put count of each 32 bits into those 32 bits 
    x = (x & m32) + ((x >> 32) & m32); //put count of each 64 bits into those 64 bits 
    return x;
}
*/

/*
 * This uses fewer arithmetic operations than any other known  
 * implementation on machines with slow multiplication.
 * It uses 17 arithmetic operations.
 *
static int popcount_2(uint64 x) {
    x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
    x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits 
    x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits 
    x += x >>  8;                   //put count of each 16 bits into their lowest 8 bits
    x += x >> 16;                   //put count of each 32 bits into their lowest 8 bits
    x += x >> 32;                   //put count of each 64 bits into their lowest 8 bits
    return x & 0x7f;
}
*/

/**
 * @brief Returns the count of the bit set to 1 in the `x` argument.
 *
 * This uses fewer arithmetic operations than any other known  
 * implementation on machines with fast multiplication.
 * It uses 12 arithmetic operations, one of which is a multiply.
 *
 * @param [in] x the set that has to be counted
 * @return       the count of bit set in the `x` parameter
 */
int popcount(uint64 x) {
    x -= (x >> 1) & m1;             //put count of each 2 bits into those 2 bits
    x = (x & m2) + ((x >> 2) & m2); //put count of each 4 bits into those 4 bits 
    x = (x + (x >> 4)) & m4;        //put count of each 8 bits into those 8 bits 
    return (x * h01) >> 56;         //returns left 8 bits of x + (x<<8) + (x<<16) + (x<<24) + ... 
}

/*
 * This is better when most bits in x are 0
 * It uses 3 arithmetic operations and one comparison/branch per "1" bit in x.
 *
static int popcount_4(uint64 x) {
    int count;
    for (count=0; x; count++)
        x &= x-1;
    return count;
}
*/

/**
 * Returns an HiLo struct of two int collecting the octal representation of the position of
 * the most significant bit set in the `bit_sequence` parameter.
 * The method does not verify that the `bit_sequence` parameter must be different from `0ULL`.
 * When this happens it returns a value equal to `{hi=0, lo=0}` that is the expected
 * value when the parameter is equal to `1ULL`.
 *
 * @param [out] result       a reference to the result struct
 * @param [in]  bit_sequence a long value different from 0L
 * @return                   the octal value of the position of the most significant bit set
 *                           collected in a two cells struct
 */
void
bitscan_MS1B_to_base8 (HiLo *result, uint64 bit_sequence)
{
  uint32 tmp;
  unsigned char hi;

  hi = 0;

  if ((bit_sequence & 0xFFFFFFFF00000000) != 0ULL) {
    tmp = (uint32) (bit_sequence >> 32);
    hi += 4;
  } else {
    tmp = (uint32) bit_sequence;
  }

  if ((tmp & 0xFFFF0000) != 0) { tmp >>= 16; hi += 2; }
  if ((tmp & 0x0000FF00) != 0) { tmp >>=  8; hi += 1; }
  
  result->hi = hi;
  result->lo = log2_array[tmp];
  return;
}
