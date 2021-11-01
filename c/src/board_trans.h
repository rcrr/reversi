/**
 * @file
 *
 * @brief Flipping, mirroring, and rotating square sets.
 *
 * @details Utilities for transforming the square set by flip, mirror, and rotation operations.
 *
 * Flip and mirror operations are summarized in the table below:
 *
 *   - function board_trans_flip_horizontal() flips horizontally
 *   - function board_trans_flip_vertical() flips vertically
 *   - function board_trans_flip_diag_h1a8() flips along the diagonal `h1-a8`
 *   - function board_trans_flip_diag_a1h8() flips along the diagonal `a1-h8`
 *
 * @code
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  . 1 1 1 1 . . .     . 1 1 1 1 . . .     . 1 1 1 1 . . .     . 1 1 1 1 . . .
 * 2  . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .
 * 3  . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .
 * 4  . 1 . . 1 . . .     . 1 . . 1 . . .     . 1 . . 1 . . .     . 1 . . 1 . . .
 * 5  . 1 1 1 . . . .     . 1 1 1 . . . .     . 1 1 1 . . . .     . 1 1 1 . . . .
 * 6  . 1 . 1 . . . .     . 1 . 1 . . . .     . 1 . 1 . . . .     . 1 . 1 . . . .
 * 7  . 1 . . 1 . . .     . 1 . . 1 . . .     . 1 . . 1 . . .     . 1 . . 1 . . .
 * 8  . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .
 *
 *           -                   |                    /               \
 *     flip_horizontal      flip_vertical     flip_diag_h1a8      flip_diag_a1h8
 *           -                   |                  /                   \
 *
 * 1  . 1 . . . 1 . .     . . . 1 1 1 1 .     . . . . . . . .     . . . . . . . .
 * 2  . 1 . . 1 . . .     . . 1 . . . 1 .     . . . . . . . .     1 1 1 1 1 1 1 1
 * 3  . 1 . 1 . . . .     . . 1 . . . 1 .     1 . . . . 1 1 .     1 . . . 1 . . .
 * 4  . 1 1 1 . . . .     . . . 1 . . 1 .     . 1 . . 1 . . 1     1 . . . 1 1 . .
 * 5  . 1 . . 1 . . .     . . . . 1 1 1 .     . . 1 1 . . . 1     1 . . 1 . . 1 .
 * 6  . 1 . . . 1 . .     . . . . 1 . 1 .     . . . 1 . . . 1     . 1 1 . . . . 1
 * 7  . 1 . . . 1 . .     . . . 1 . . 1 .     1 1 1 1 1 1 1 1     . . . . . . . .
 * 8  . 1 1 1 1 . . .     . . 1 . . . 1 .     . . . . . . . .     . . . . . . . .
 *
 * @endcode
 *
 * Three rotation functions are provided by the module:
 *
 *   - function board_trans_rotate_180() rotates the square set by 180 degrees
 *   - function board_trans_rotate_90c() rotates clockwise by 90 degrees
 *   - function board_trans_rotate_90a() rotates anti-clockwise by 90 degrees
 *
 * A rotation of zero degrees is obtained by calling:
 *
 *   - function board_trans_identity() leaves the square set as it is
 *
 * @code
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  . 1 1 1 1 . . .     . 1 1 1 1 . . .     . 1 1 1 1 . . .     . 1 1 1 1 . . .
 * 2  . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .
 * 3  . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .
 * 4  . 1 . . 1 . . .     . 1 . . 1 . . .     . 1 . . 1 . . .     . 1 . . 1 . . .
 * 5  . 1 1 1 . . . .     . 1 1 1 . . . .     . 1 1 1 . . . .     . 1 1 1 . . . .
 * 6  . 1 . 1 . . . .     . 1 . 1 . . . .     . 1 . 1 . . . .     . 1 . 1 . . . .
 * 7  . 1 . . 1 . . .     . 1 . . 1 . . .     . 1 . . 1 . . .     . 1 . . 1 . . .
 * 8  . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .     . 1 . . . 1 . .
 *
 *           .               --> + -->              -->                 <--
 *       identity           rorate_180          rorate_90c          rorate_90a
 *           .               <-- + <--              <--                 -->
 *
 * 1  . 1 1 1 1 . . .     . . 1 . . . 1 .     . . . . . . . .     . . . . . . . .
 * 2  . 1 . . . 1 . .     . . . 1 . . 1 .     1 1 1 1 1 1 1 1     . . . . . . . .
 * 3  . 1 . . . 1 . .     . . . . 1 . 1 .     . . . 1 . . . 1     . 1 1 . . . . 1
 * 4  . 1 . . 1 . . .     . . . . 1 1 1 .     . . 1 1 . . . 1     1 . . 1 . . 1 .
 * 5  . 1 1 1 . . . .     . . . 1 . . 1 .     . 1 . . 1 . . 1     1 . . . 1 1 . .
 * 6  . 1 . 1 . . . .     . . 1 . . . 1 .     1 . . . . 1 1 .     1 . . . 1 . . .
 * 7  . 1 . . 1 . . .     . . 1 . . . 1 .     . . . . . . . .     1 1 1 1 1 1 1 1
 * 8  . 1 . . . 1 . .     . . . 1 1 1 1 .     . . . . . . . .     . . . . . . . .
 *
 * @endcode
 *
 * @par board_trans.h
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

#ifndef BOARD_TRANS_H
#define BOARD_TRANS_H

#include "board.h"



typedef SquareSet (*board_trans_f) (SquareSet);



/**
 * @brief Flips a square set vertically.
 *
 * @details Row `1` is mapped to row `8` and vice versa.
 *
 * @code
 *
 *    ---- input ----    ---- output ---
 *
 *    a b c d e f g h    a b c d e f g h
 *
 * 1  . 1 1 1 1 . . .    . 1 . . . 1 . .
 * 2  . 1 . . . 1 . .    . 1 . . 1 . . .
 * 3  . 1 . . . 1 . .    . 1 . 1 . . . .
 * 4  . 1 . . 1 . . .    . 1 1 1 . . . .
 * 5  . 1 1 1 . . . .    . 1 . . 1 . . .
 * 6  . 1 . 1 . . . .    . 1 . . . 1 . .
 * 7  . 1 . . 1 . . .    . 1 . . . 1 . .
 * 8  . 1 . . . 1 . .    . 1 1 1 1 . . .
 *
 * @endcode
 *
 * @param [in] s any square set
 * @return       square set `s` flipped vertically
 */
extern SquareSet
board_trans_flip_horizontal (SquareSet s);

/**
 * @brief Mirrors a square set horizontally.
 *
 * @details Column `a` is mapped to column `h` and vice versa.
 *
 * @code
 *
 *    ---- input ----    ---- output ---
 *
 *    a b c d e f g h    a b c d e f g h
 *
 * 1  . 1 1 1 1 . . .    . . . 1 1 1 1 .
 * 2  . 1 . . . 1 . .    . . 1 . . . 1 .
 * 3  . 1 . . . 1 . .    . . 1 . . . 1 .
 * 4  . 1 . . 1 . . .    . . . 1 . . 1 .
 * 5  . 1 1 1 . . . .    . . . . 1 1 1 .
 * 6  . 1 . 1 . . . .    . . . . 1 . 1 .
 * 7  . 1 . . 1 . . .    . . . 1 . . 1 .
 * 8  . 1 . . . 1 . .    . . 1 . . . 1 .
 *
 * @endcode
 *
 * @param [in] s any square set
 * @return       square set `s` mirrored horizontally
 */
extern SquareSet
board_trans_flip_vertical (SquareSet s);

/**
 * @brief Flips a square set about the diagonal `h1-a8`.
 *
 * @details Square `a1` is mapped to `h8` and vice versa.
 *
 * @code
 *
 *    ---- input ----    ---- output ---
 *
 *    a b c d e f g h    a b c d e f g h
 *
 * 1  . 1 1 1 1 . . .    . . . . . . . .
 * 2  . 1 . . . 1 . .    . . . . . . . .
 * 3  . 1 . . . 1 . .    1 . . . . 1 1 .
 * 4  . 1 . . 1 . . .    . 1 . . 1 . . 1
 * 5  . 1 1 1 . . . .    . . 1 1 . . . 1
 * 6  . 1 . 1 . . . .    . . . 1 . . . 1
 * 7  . 1 . . 1 . . .    1 1 1 1 1 1 1 1
 * 8  . 1 . . . 1 . .    . . . . . . . .
 *
 * @endcode
 *
 * @param [in] s any square set
 * @return       square set `s` flipped about `h1-a8`
 */
extern SquareSet
board_trans_flip_diag_h1a8 (SquareSet s);

/**
 * @brief Flips a square set about the diagonal `a1-h8`.
 *
 * @details Square `h1` is mapped to `a8` and vice versa.
 *
 * @code
 *
 *    ---- input ----    ---- output ---
 *
 *    a b c d e f g h    a b c d e f g h
 *
 * 1  . 1 1 1 1 . . .    . . . . . . . .
 * 2  . 1 . . . 1 . .    1 1 1 1 1 1 1 1
 * 3  . 1 . . . 1 . .    1 . . . 1 . . .
 * 4  . 1 . . 1 . . .    1 . . . 1 1 . .
 * 5  . 1 1 1 . . . .    1 . . 1 . . 1 .
 * 6  . 1 . 1 . . . .    . 1 1 . . . . 1
 * 7  . 1 . . 1 . . .    . . . . . . . .
 * 8  . 1 . . . 1 . .    . . . . . . . .
 *
 * @endcode
 *
 * @param [in] s any square set
 * @return       square set `s` flipped about `a1-h8`
 */
extern SquareSet
board_trans_flip_diag_a1h8 (SquareSet s);

/**
 * @brief Rotate a square set by 180 degrees.
 *
 * @details Square `a1` is mapped to `h8`, and `b1` is mapped to `g8`.
 *
 * @code
 *
 *    ---- input ----    ---- output ---
 *
 *    a b c d e f g h    a b c d e f g h
 *
 * 1  . 1 1 1 1 . . .    . . 1 . . . 1 .
 * 2  . 1 . . . 1 . .    . . . 1 . . 1 .
 * 3  . 1 . . . 1 . .    . . . . 1 . 1 .
 * 4  . 1 . . 1 . . .    . . . . 1 1 1 .
 * 5  . 1 1 1 . . . .    . . . 1 . . 1 .
 * 6  . 1 . 1 . . . .    . . 1 . . . 1 .
 * 7  . 1 . . 1 . . .    . . 1 . . . 1 .
 * 8  . 1 . . . 1 . .    . . . 1 1 1 1 .
 *
 * @endcode
 *
 * @param [in] s any square set
 * @return       square set `s` rotated by 180 degrees
 */
extern SquareSet
board_trans_rotate_180 (SquareSet s);

/**
 * @brief Rotates a square set by 90 degrees clockwise.
 *
 * @details Square `a1` is mapped to `h1`, and `b1` is mapped to `h2`.
 *
 * @code
 *
 *    ---- input ----    ---- output ---
 *
 *    a b c d e f g h    a b c d e f g h
 *
 * 1  . 1 1 1 1 . . .    . . . . . . . .
 * 2  . 1 . . . 1 . .    1 1 1 1 1 1 1 1
 * 3  . 1 . . . 1 . .    . . . 1 . . . 1
 * 4  . 1 . . 1 . . .    . . 1 1 . . . 1
 * 5  . 1 1 1 . . . .    . 1 . . 1 . . 1
 * 6  . 1 . 1 . . . .    1 . . . . 1 1 .
 * 7  . 1 . . 1 . . .    . . . . . . . .
 * 8  . 1 . . . 1 . .    . . . . . . . .
 *
 * @endcode
 *
 * @param [in] s any square set
 * @return       square set `s` rotated 90 degrees clockwise
 */
extern SquareSet
board_trans_rotate_90c (SquareSet s);

/**
 * @brief Rotates a square set by 90 degrees anticlockwise.
 *
 * @details Square `a1` is mapped to `a8`, and `b1` is mapped to `a7`.
 *
 * @code
 *
 *    ---- input ----    ---- output ---
 *
 *    a b c d e f g h    a b c d e f g h
 *
 * 1  . 1 1 1 1 . . .    . . . . . . . .
 * 2  . 1 . . . 1 . .    . . . . . . . .
 * 3  . 1 . . . 1 . .    . 1 1 . . . . 1
 * 4  . 1 . . 1 . . .    1 . . 1 . . 1 .
 * 5  . 1 1 1 . . . .    1 . . . 1 1 . .
 * 6  . 1 . 1 . . . .    1 . . . 1 . . .
 * 7  . 1 . . 1 . . .    1 1 1 1 1 1 1 1
 * 8  . 1 . . . 1 . .    . . . . . . . .
 *
 * @endcode
 *
 * @param [in] s any square set
 * @return       square set `s` rotated 90 degrees anticlockwise
 */
extern SquareSet
board_trans_rotate_90a (SquareSet s);

/**
 * @brief Returns the square set as it is.
 *
 * @details Conceptually it applies a rotation of zero degrees.
 *
 * @code
 *
 *    ---- input ----    ---- output ---
 *
 *    a b c d e f g h    a b c d e f g h
 *
 * 1  . 1 1 1 1 . . .    . 1 1 1 1 . . .
 * 2  . 1 . . . 1 . .    . 1 . . . 1 . .
 * 3  . 1 . . . 1 . .    . 1 . . . 1 . .
 * 4  . 1 . . 1 . . .    . 1 . . 1 . . .
 * 5  . 1 1 1 . . . .    . 1 1 1 . . . .
 * 6  . 1 . 1 . . . .    . 1 . 1 . . . .
 * 7  . 1 . . 1 . . .    . 1 . . 1 . . .
 * 8  . 1 . . . 1 . .    . 1 . . . 1 . .
 *
 * @endcode
 *
 * @param [in] s any square set
 * @return       square set `s` rotated by zero degrees
 */
extern SquareSet
board_trans_identity (SquareSet s);

extern void
board_trans_flip_horizontal_vec (SquareSet *b,
                                 SquareSet *s);

extern void
board_trans_flip_vertical_vec (SquareSet *b,
                               SquareSet *s);

extern void
board_trans_flip_diag_a1h8_vec (SquareSet *b,
                                SquareSet *r);

extern void
board_trans_flip_diag_h1a8_vec (SquareSet *b,
                                SquareSet *r);

#endif
