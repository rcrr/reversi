/**
 * @file
 *
 * @brief Kogge Stone module definitions.
 * @details This module defines ...
 *
 * @par kogge_stone.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2022 Roberto Corradini. All rights reserved.
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

#ifndef KOGGE_STONE_H
#define KOGGE_STONE_H

#include <stdint.h>

/**
 * @brief Returns the legal move set for the game position.
 *
 * @details The implementation is written in x86_64 ASM using AVX2 extensions.
 *          The three arguments and the returned value are all 64 bit unsigned integers,
 *          representing a bit field.
 *          The lower bit corresponds to A1 and the upper one to H8.
 *
 *          Arguments must fulfill two pre-conditions:
 *           - empties must be equal to ~(mover | opponent)
 *           - mover & opponent must be equal to 0ULL
 *
 * @param [in] mover    the square set belonging to the mover
 * @param [in] opponent the square set belonging to the opponent
 * @param [in] empties  the empty square set
 * @return              the legal move set
 */
extern uint64_t
kost_lms (uint64_t mover,
          uint64_t opponent,
          uint64_t empties);

/**
 * @brief Returns the flip square set for the given game position and move.
 *
 * @details The implementation is written in x86_64 ASM using AVX2 extensions.
 *          The three arguments and the returned value are all 64 bit unsigned integers,
 *          representing a bit field.
 *          The lower bit corresponds to A1 and the upper one to H8.
 *          When the move is 0ULL the returned value is always zero.
 *          When the move is legal the returned value is always different from zero.
 *          When the move is not legal the returned value is zero.
 *
 *          Arguments must fulfill three pre-conditions:
 *          - The move set must have zero or one bit set (not more)
 *          - (mover | opponent) & move must be equal to 0ULL
 *            It means that the move is on an empty square.
 *          - mover & opponent must be equal to 0ULL
 *            It means that there is no overlap between mover and opponent.
 *
 * @param [in] move     the move that mover is playing
 * @param [in] opponent the square set belonging to the opponent
 * @param [in] mover    the square set belonging to the mover
 * @return              the flipping square set (flips)
 */
extern uint64_t
kost_mm (uint64_t move,
         uint64_t opponent,
         uint64_t mover);

#endif /* KOGGE_STONE_H */
