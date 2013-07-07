/**
 * @file
 *
 * @brief Bit Works module definitions.
 * @details This module defines the #Player, #SquareState,
 * #Square, #SquareSet, #Board, #GamePosition, #Direction entities,
 * and the function prototypes that operate on them.
 *
 * @par bit_works.h
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

#ifndef BIT_WORKS_H
#define BIT_WORKS_H

/**
 * @typedef uint64
 * @brief Unsigned 64 bits integer.
 */
typedef unsigned long long int uint64;

/**
 * @typedef uint32
 * @brief Unsigned 32 bits integer.
 */
typedef unsigned long int uint32;

/**
 * @brief Used to return a two digit representation on a given base, for an integer.
 */
typedef struct {
  unsigned char lo;   /**< @brief The low digit. */
  unsigned char hi;   /**< @brief The high digit. */
} HiLo;

extern int
popcount (uint64 x);

extern void
bitscan_MS1B_to_base8 (HiLo *result, uint64 bit_sequence);

extern uint64
signed_left_shift (uint64 bit_sequence, int shift);

#endif /* BIT_WORKS_H */
