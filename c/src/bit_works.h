/**
 * @file
 *
 * @brief Bit Works module definitions.
 * @details This module defines signed and unsigned
 * integer types, and some procedures processing the bits
 * of these objects.
 *
 * @par bit_works.h
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

#ifndef BIT_WORKS_H
#define BIT_WORKS_H

#include <stdint.h>



/*
 * Bit count functions - Section begin.
 */

/**
 * @cond
 */

extern unsigned int
bit_works_bitcount_64_plain (uint64_t bit_set);

#ifdef __POPCNT__
__attribute__((always_inline))
inline unsigned int
bit_works_bitcount_64_popcnt (uint64_t bit_set)
{
  uint64_t result;
  __asm__ __volatile__ ("popcnt %1, %0;" : "=r" (result) : "r" (bit_set));
  return (unsigned int) result;
}
#endif

/**
 * @endcond
 */

__attribute__((always_inline))
inline unsigned int
bit_works_bitcount_64 (uint64_t bit_set)
{
#ifdef __POPCNT__
  return bit_works_bitcount_64_popcnt(bit_set);
#else
  return bit_works_bitcount_64_plain(bit_set);
#endif
}

/* Bit count functions - Section end. */



/*
 * Bit scan reverse functions - Section begin.
 */

/**
 * @cond
 */

extern uint8_t
bit_works_bit_scan_reverse_64_plain (uint64_t bit_sequence);

#ifdef __x86_64__
__attribute__((always_inline))
inline uint8_t
bit_works_bit_scan_reverse_64_bsr (uint64_t bit_sequence)
{
  uint64_t result;
  __asm__ __volatile__ ("bsr %1, %0;" : "=r" (result) : "r" (bit_sequence));
  return (uint8_t) result;
}
#endif

/**
 * @endcond
 */

__attribute__((always_inline))
inline uint8_t
bit_works_bit_scan_reverse_64 (uint64_t bit_sequence)
{
#ifdef __x86_64__
  return bit_works_bit_scan_reverse_64_bsr(bit_sequence);
#else
  return bit_works_bit_scan_reverse_64_plain(bit_sequence);
#endif
}

/* Bit scan reverse functions - Section end. */



/*
 * Bit scan forward functions - Section begin.
 */

/**
 * @cond
 */

extern uint8_t
bit_works_bit_scan_forward_64_plain (uint64_t bit_sequence);

#ifdef __x86_64__
__attribute__((always_inline))
inline uint8_t
bit_works_bit_scan_forward_64_bsf (uint64_t bit_sequence)
{
  uint64_t result;
  __asm__ __volatile__ ("bsf %1, %0;" : "=r" (result) : "r" (bit_sequence));
  return (uint8_t) result;
}
#endif

/**
 * @endcond
 */

__attribute__((always_inline))
inline uint8_t
bit_works_bit_scan_forward_64 (uint64_t bit_sequence)
{
#ifdef __x86_64__
  return bit_works_bit_scan_forward_64_bsf(bit_sequence);
#else
  return bit_works_bit_scan_forward_64_plain(bit_sequence);
#endif
}

/* Bit scan forward functions - Secion end. */



/*
 * Reset lowest set bit functions - Section begin.
 */

/**
 * @cond
 */

extern uint64_t
bit_works_reset_lowest_set_bit_plain (uint64_t bit_sequence);

#ifdef __x86_64__
__attribute__((always_inline))
inline uint64_t
bit_works_reset_lowest_set_bit_blsr (uint64_t bit_sequence)
{
  uint64_t result;
  __asm__ __volatile__ ("blsr %1, %0;" : "=r" (result) : "r" (bit_sequence));
  return (uint64_t) result;
}
#endif

/**
 * @endcond
 */

__attribute__((always_inline))
inline uint64_t
bit_works_reset_lowest_set_bit (uint64_t bit_sequence)
{
#ifdef __x86_64__
  return bit_works_reset_lowest_set_bit_blsr(bit_sequence);
#else
  return bit_works_reset_lowest_set_bit_plain(bit_sequence);
#endif
}

/* Reset lowest set bit functions - Section end. */



extern uint64_t
bit_works_lowest_set_bit (const uint64_t bit_sequence);

extern uint64_t
bit_works_highest_set_bit (uint64_t bit_sequence);

extern uint8_t
bit_works_fill_in_between (uint8_t bit_sequence);

__attribute__((always_inline))
inline uint64_t
bit_works_ror_64 (const uint64_t bit_sequence,
                  const unsigned int shift)
{
  return (bit_sequence >> shift) | (bit_sequence << (64 - shift));
}

__attribute__((always_inline))
inline static uint64_t
bit_works_rol_64 (const uint64_t bit_sequence,
                  const unsigned int shift)
{
  return (bit_sequence << shift) | (bit_sequence >> (64 - shift));
}



#endif /* BIT_WORKS_H */
