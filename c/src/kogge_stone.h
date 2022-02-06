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

extern int64_t
kost_max_of_three (int64_t x,
                   int64_t y,
                   int64_t z);

extern uint64_t
kost_lms (uint64_t mover,
          uint64_t opponent,
          uint64_t empties);

#endif /* KOGGE_STONE_H */
