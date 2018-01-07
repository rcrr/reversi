/**
 * @file
 *
 * @brief Defines patterns and indexes for reversi boards.
 *
 * @details To be completed.
 *
 * The `EDGE` pattern has four instances ranging from `[0..3]`:
 *
 * @code
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 1 1     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 2  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 3  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 4  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 5  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 6  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 7  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 8  . . . . . . . .     . . . . . . . 1     1 1 1 1 1 1 1 1     1 . . . . . . .
 *
 * @endcode
 *
 *
 * @par board_patterns.h
 * <tt>
 * This file is part of the reversi program
 * http://github.com/rcrr/reversi
 * </tt>
 * @author Roberto Corradini mailto:rob_corradini@yahoo.it
 * @copyright 2018 Roberto Corradini. All rights reserved.
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

#ifndef BOARD_PATTERNS_H
#define BOARD_PATTERNS_H

#include "board.h"



/**
 * @brief TBD.
 *
 * @details TBD.
 *
 * @code
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 1 1     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 2  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 3  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 4  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 5  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 6  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 7  . . . . . . . .     . . . . . . . 1     . . . . . . . .     1 . . . . . . .
 * 8  . . . . . . . .     . . . . . . . 1     1 1 1 1 1 1 1 1     1 . . . . . . .
 *
 * @endcode
 *
 * @param [in] s any square set
 * @return       tbd
 */
extern SquareSet
board_patterns_abc (SquareSet s);



#endif
