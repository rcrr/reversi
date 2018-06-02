/**
 * @file
 *
 * @brief Board pattern module unit test suite.
 * @details Collects tests and helper methods for the board pattern module.
 *
 * @par ut_board_pattern.c
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

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

#include "unit_test.h"
#include "board_pattern.h"

/*
 * Square set definitions:
 *
 *
 *    empty               full
 *
 *    a b c d e f g h     a b c d e f g h
 *
 * 1  . . . . . . . .     1 1 1 1 1 1 1 1
 * 2  . . . . . . . .     1 1 1 1 1 1 1 1
 * 3  . . . . . . . .     1 1 1 1 1 1 1 1
 * 4  . . . . . . . .     1 1 1 1 1 1 1 1
 * 5  . . . . . . . .     1 1 1 1 1 1 1 1
 * 6  . . . . . . . .     1 1 1 1 1 1 1 1
 * 7  . . . . . . . .     1 1 1 1 1 1 1 1
 * 8  . . . . . . . .     1 1 1 1 1 1 1 1
 *
 *
 *    n_edge              e_edge              s_edge              w_edge
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
 *
 *    n_r2                e_r2                s_r2                w_r2
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  . . . . . . . .     . . . . . . 1 .     . . . . . . . .     . 1 . . . . . .
 * 2  1 1 1 1 1 1 1 1     . . . . . . 1 .     . . . . . . . .     . 1 . . . . . .
 * 3  . . . . . . . .     . . . . . . 1 .     . . . . . . . .     . 1 . . . . . .
 * 4  . . . . . . . .     . . . . . . 1 .     . . . . . . . .     . 1 . . . . . .
 * 5  . . . . . . . .     . . . . . . 1 .     . . . . . . . .     . 1 . . . . . .
 * 6  . . . . . . . .     . . . . . . 1 .     . . . . . . . .     . 1 . . . . . .
 * 7  . . . . . . . .     . . . . . . 1 .     1 1 1 1 1 1 1 1     . 1 . . . . . .
 * 8  . . . . . . . .     . . . . . . 1 .     . . . . . . . .     . 1 . . . . . .
 *
 *
 *    n_r3                e_r3                s_r3                w_r3
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  . . . . . . . .     . . . . . 1 . .     . . . . . . . .     . . 1 . . . . .
 * 2  . . . . . . . .     . . . . . 1 . .     . . . . . . . .     . . 1 . . . . .
 * 3  1 1 1 1 1 1 1 1     . . . . . 1 . .     . . . . . . . .     . . 1 . . . . .
 * 4  . . . . . . . .     . . . . . 1 . .     . . . . . . . .     . . 1 . . . . .
 * 5  . . . . . . . .     . . . . . 1 . .     . . . . . . . .     . . 1 . . . . .
 * 6  . . . . . . . .     . . . . . 1 . .     1 1 1 1 1 1 1 1     . . 1 . . . . .
 * 7  . . . . . . . .     . . . . . 1 . .     . . . . . . . .     . . 1 . . . . .
 * 8  . . . . . . . .     . . . . . 1 . .     . . . . . . . .     . . 1 . . . . .
 *
 *
 *    n_r4                e_r4                s_r4                w_r4
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  . . . . . . . .     . . . . 1 . . .     . . . . . . . .     . . . 1 . . . .
 * 2  . . . . . . . .     . . . . 1 . . .     . . . . . . . .     . . . 1 . . . .
 * 3  . . . . . . . .     . . . . 1 . . .     . . . . . . . .     . . . 1 . . . .
 * 4  1 1 1 1 1 1 1 1     . . . . 1 . . .     . . . . . . . .     . . . 1 . . . .
 * 5  . . . . . . . .     . . . . 1 . . .     1 1 1 1 1 1 1 1     . . . 1 . . . .
 * 6  . . . . . . . .     . . . . 1 . . .     . . . . . . . .     . . . 1 . . . .
 * 7  . . . . . . . .     . . . . 1 . . .     . . . . . . . .     . . . 1 . . . .
 * 8  . . . . . . . .     . . . . 1 . . .     . . . . . . . .     . . . 1 . . . .
 *
 *
 *    border              bord_1              bord_2              bord_3
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 1 1     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 2  1 . . . . . . 1     . 1 1 1 1 1 1 .     . . . . . . . .     . . . . . . . .
 * 3  1 . . . . . . 1     . 1 . . . . 1 .     . . 1 1 1 1 . .     . . . . . . . .
 * 4  1 . . . . . . 1     . 1 . . . . 1 .     . . 1 . . 1 . .     . . . 1 1 . . .
 * 5  1 . . . . . . 1     . 1 . . . . 1 .     . . 1 . . 1 . .     . . . 1 1 . . .
 * 6  1 . . . . . . 1     . 1 . . . . 1 .     . . 1 1 1 1 . .     . . . . . . . .
 * 7  1 . . . . . . 1     . 1 1 1 1 1 1 .     . . . . . . . .     . . . . . . . .
 * 8  1 1 1 1 1 1 1 1     . . . . . . . .     . . . . . . . .     . . . . . . . .
 *
 *
 *    zebra_o              zebra_e
 *
 *    a b c d e f g h     a b c d e f g h
 *
 * 1  . 1 . 1 . 1 . 1     1 . 1 . 1 . 1 .
 * 2  . . . . . . . .     . . . . . . . .
 * 3  . . . . . . . .     . . . . . . . .
 * 4  . . . . . . . .     . . . . . . . .
 * 5  . . . . . . . .     . . . . . . . .
 * 6  . . . . . . . .     . . . . . . . .
 * 7  . . . . . . . .     . . . . . . . .
 * 8  . . . . . . . .     . . . . . . . .
 *
 *
 *    nw_1x1              ne_1x1              se_1x1              sw_1x1
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 . . . . . . .     . . . . . . . 1     . . . . . . . .     . . . . . . . .
 * 2  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 3  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 4  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 5  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . . . . . 1     1 . . . . . . .
 *
 *
 *    nw_2x2              ne_2x2              se_2x2              sw_2x2
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 . . . . . .     . . . . . . 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 . . . . . .     . . . . . . 1 1     . . . . . . . .     . . . . . . . .
 * 3  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 4  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 5  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . . . . 1 1     1 1 . . . . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . . . . 1 1     1 1 . . . . . .
 *
 *
 *    nw_3x3              ne_3x3              se_3x3              sw_3x3
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 . . . . .     . . . . . 1 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 1 . . . . .     . . . . . 1 1 1     . . . . . . . .     . . . . . . . .
 * 3  1 1 1 . . . . .     . . . . . 1 1 1     . . . . . . . .     . . . . . . . .
 * 4  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 5  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . . . 1 1 1     1 1 1 . . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . . . 1 1 1     1 1 1 . . . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . . . 1 1 1     1 1 1 . . . . .
 *
 *
 *    nw_4x4              ne_4x4              se_4x4              sw_4x4
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 . . . .     . . . . 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 1 1 . . . .     . . . . 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 3  1 1 1 1 . . . .     . . . . 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 4  1 1 1 1 . . . .     . . . . 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 5  . . . . . . . .     . . . . . . . .     . . . . 1 1 1 1     1 1 1 1 . . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . . 1 1 1 1     1 1 1 1 . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . . 1 1 1 1     1 1 1 1 . . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . . 1 1 1 1     1 1 1 1 . . . .
 *
 *
 *    nw_5x5              ne_5x5              se_5x5              sw_5x5
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 . . .     . . . 1 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 1 1 1 . . .     . . . 1 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 3  1 1 1 1 1 . . .     . . . 1 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 4  1 1 1 1 1 . . .     . . . 1 1 1 1 1     . . . 1 1 1 1 1     1 1 1 1 1 . . .
 * 5  1 1 1 1 1 . . .     . . . 1 1 1 1 1     . . . 1 1 1 1 1     1 1 1 1 1 . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . 1 1 1 1 1     1 1 1 1 1 . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . 1 1 1 1 1     1 1 1 1 1 . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . 1 1 1 1 1     1 1 1 1 1 . . .
 *
 *
 *    nw_6x6              ne_6x6              se_6x6              sw_6x6
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 . .     . . 1 1 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 1 1 1 1 . .     . . 1 1 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 3  1 1 1 1 1 1 . .     . . 1 1 1 1 1 1     . . 1 1 1 1 1 1     1 1 1 1 1 1 . .
 * 4  1 1 1 1 1 1 . .     . . 1 1 1 1 1 1     . . 1 1 1 1 1 1     1 1 1 1 1 1 . .
 * 5  1 1 1 1 1 1 . .     . . 1 1 1 1 1 1     . . 1 1 1 1 1 1     1 1 1 1 1 1 . .
 * 6  1 1 1 1 1 1 . .     . . 1 1 1 1 1 1     . . 1 1 1 1 1 1     1 1 1 1 1 1 . .
 * 7  . . . . . . . .     . . . . . . . .     . . 1 1 1 1 1 1     1 1 1 1 1 1 . .
 * 8  . . . . . . . .     . . . . . . . .     . . 1 1 1 1 1 1     1 1 1 1 1 1 . .
 *
 *
 *    nw_7x7              ne_7x7              se_7x7              sw_7x7
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 1 .     . 1 1 1 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 1 1 1 1 1 .     . 1 1 1 1 1 1 1     . 1 1 1 1 1 1 1     1 1 1 1 1 1 1 .
 * 3  1 1 1 1 1 1 1 .     . 1 1 1 1 1 1 1     . 1 1 1 1 1 1 1     1 1 1 1 1 1 1 .
 * 4  1 1 1 1 1 1 1 .     . 1 1 1 1 1 1 1     . 1 1 1 1 1 1 1     1 1 1 1 1 1 1 .
 * 5  1 1 1 1 1 1 1 .     . 1 1 1 1 1 1 1     . 1 1 1 1 1 1 1     1 1 1 1 1 1 1 .
 * 6  1 1 1 1 1 1 1 .     . 1 1 1 1 1 1 1     . 1 1 1 1 1 1 1     1 1 1 1 1 1 1 .
 * 7  1 1 1 1 1 1 1 .     . 1 1 1 1 1 1 1     . 1 1 1 1 1 1 1     1 1 1 1 1 1 1 .
 * 8  . . . . . . . .     . . . . . . . .     . 1 1 1 1 1 1 1     1 1 1 1 1 1 1 .
 *
 *
 *    nw_tri_2            ne_tri_2            se_tri_2            sw_tri_2
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 . . . . . .     . . . . . . 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 . . . . . . .     . . . . . . . 1     . . . . . . . .     . . . . . . . .
 * 3  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 4  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 5  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . . . . . 1     1 . . . . . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . . . . 1 1     1 1 . . . . . .
 *
 *
 *    nw_tri_3            ne_tri_3            se_tri_3            sw_tri_3
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 . . . . .     . . . . . 1 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 . . . . . .     . . . . . . 1 1     . . . . . . . .     . . . . . . . .
 * 3  1 . . . . . . .     . . . . . . . 1     . . . . . . . .     . . . . . . . .
 * 4  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 5  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . . . . . 1     1 . . . . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . . . . 1 1     1 1 . . . . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . . . 1 1 1     1 1 1 . . . . .
 *
 *
 *    nw_tri_4            ne_tri_4            se_tri_4            sw_tri_4
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 . . . .     . . . . 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 1 . . . . .     . . . . . 1 1 1     . . . . . . . .     . . . . . . . .
 * 3  1 1 . . . . . .     . . . . . . 1 1     . . . . . . . .     . . . . . . . .
 * 4  1 . . . . . . .     . . . . . . . 1     . . . . . . . .     . . . . . . . .
 * 5  . . . . . . . .     . . . . . . . .     . . . . . . . 1     1 . . . . . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . . . . 1 1     1 1 . . . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . . . 1 1 1     1 1 1 . . . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . . 1 1 1 1     1 1 1 1 . . . .
 *
 *
 *    nw_tri_5            ne_tri_5            se_tri_5            sw_tri_5
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 . . .     . . . 1 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 1 1 . . . .     . . . . 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 3  1 1 1 . . . . .     . . . . . 1 1 1     . . . . . . . .     . . . . . . . .
 * 4  1 1 . . . . . .     . . . . . . 1 1     . . . . . . . 1     1 . . . . . . .
 * 5  1 . . . . . . .     . . . . . . . 1     . . . . . . 1 1     1 1 . . . . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . . . 1 1 1     1 1 1 . . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . . 1 1 1 1     1 1 1 1 . . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . 1 1 1 1 1     1 1 1 1 1 . . .
 *
 *
 *    nw_tri_6            ne_tri_6            se_tri_6            sw_tri_6
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 . .     . . 1 1 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 1 1 1 . . .     . . . 1 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 3  1 1 1 1 . . . .     . . . . 1 1 1 1     . . . . . . . 1     1 . . . . . . .
 * 4  1 1 1 . . . . .     . . . . . 1 1 1     . . . . . . 1 1     1 1 . . . . . .
 * 5  1 1 . . . . . .     . . . . . . 1 1     . . . . . 1 1 1     1 1 1 . . . . .
 * 6  1 . . . . . . .     . . . . . . . 1     . . . . 1 1 1 1     1 1 1 1 . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . 1 1 1 1 1     1 1 1 1 1 . . .
 * 8  . . . . . . . .     . . . . . . . .     . . 1 1 1 1 1 1     1 1 1 1 1 1 . .
 *
 *
 *    nw_tri_7            ne_tri_7            se_tri_7            sw_tri_7
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 1 .     . 1 1 1 1 1 1 1     . . . . . . . .     . . . . . . . .
 * 2  1 1 1 1 1 1 . .     . . 1 1 1 1 1 1     . . . . . . . 1     1 . . . . . . .
 * 3  1 1 1 1 1 . . .     . . . 1 1 1 1 1     . . . . . . 1 1     1 1 . . . . . .
 * 4  1 1 1 1 . . . .     . . . . 1 1 1 1     . . . . . 1 1 1     1 1 1 . . . . .
 * 5  1 1 1 . . . . .     . . . . . 1 1 1     . . . . 1 1 1 1     1 1 1 1 . . . .
 * 6  1 1 . . . . . .     . . . . . . 1 1     . . . 1 1 1 1 1     1 1 1 1 1 . . .
 * 7  1 . . . . . . .     . . . . . . . 1     . . 1 1 1 1 1 1     1 1 1 1 1 1 . .
 * 8  . . . . . . . .     . . . . . . . .     . 1 1 1 1 1 1 1     1 1 1 1 1 1 1 .
 *
 *
 *    nw_tri_8            ne_tri_8            se_tri_8            sw_tri_8
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1     . . . . . . . 1     1 . . . . . . .
 * 2  1 1 1 1 1 1 1 .     . 1 1 1 1 1 1 1     . . . . . . 1 1     1 1 . . . . . .
 * 3  1 1 1 1 1 1 . .     . . 1 1 1 1 1 1     . . . . . 1 1 1     1 1 1 . . . . .
 * 4  1 1 1 1 1 . . .     . . . 1 1 1 1 1     . . . . 1 1 1 1     1 1 1 1 . . . .
 * 5  1 1 1 1 . . . .     . . . . 1 1 1 1     . . . 1 1 1 1 1     1 1 1 1 1 . . .
 * 6  1 1 1 . . . . .     . . . . . 1 1 1     . . 1 1 1 1 1 1     1 1 1 1 1 1 . .
 * 7  1 1 . . . . . .     . . . . . . 1 1     . 1 1 1 1 1 1 1     1 1 1 1 1 1 1 .
 * 8  1 . . . . . . .     . . . . . . . 1     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1
 *
 */

static const SquareSet empty    = 0x0000000000000000;
static const SquareSet full     = 0xffffffffffffffff;

static const SquareSet n_edge   = 0x00000000000000ff;
static const SquareSet e_edge   = 0x8080808080808080;
static const SquareSet s_edge   = 0xff00000000000000;
static const SquareSet w_edge   = 0x0101010101010101;

static const SquareSet n_r2     = 0x000000000000ff00;
static const SquareSet e_r2     = 0x4040404040404040;
static const SquareSet s_r2     = 0x00ff000000000000;
static const SquareSet w_r2     = 0x0202020202020202;

static const SquareSet n_r3     = 0x0000000000ff0000;
static const SquareSet e_r3     = 0x2020202020202020;
static const SquareSet s_r3     = 0x0000ff0000000000;
static const SquareSet w_r3     = 0x0404040404040404;

static const SquareSet n_r4     = 0x00000000ff000000;
static const SquareSet e_r4     = 0x1010101010101010;
static const SquareSet s_r4     = 0x000000ff00000000;
static const SquareSet w_r4     = 0x0808080808080808;

static const SquareSet border   = 0xff818181818181ff;
static const SquareSet bord_1   = 0x007e424242427e00;
static const SquareSet bord_2   = 0x00003c24243c0000;
static const SquareSet bord_3   = 0x0000001818000000;

static const SquareSet zebra_o  = 0x00000000000000aa;
static const SquareSet zebra_e  = 0x0000000000000055;

static const SquareSet nw_1x1   = 0x0000000000000001;
static const SquareSet ne_1x1   = 0x0000000000000080;
static const SquareSet se_1x1   = 0x8000000000000000;
static const SquareSet sw_1x1   = 0x0100000000000000;

static const SquareSet nw_2x2   = 0x0000000000000303;
static const SquareSet ne_2x2   = 0x000000000000c0c0;
static const SquareSet se_2x2   = 0xc0c0000000000000;
static const SquareSet sw_2x2   = 0x0303000000000000;

static const SquareSet nw_3x3   = 0x0000000000070707;
static const SquareSet ne_3x3   = 0x0000000000e0e0e0;
static const SquareSet se_3x3   = 0xe0e0e00000000000;
static const SquareSet sw_3x3   = 0x0707070000000000;

static const SquareSet nw_4x4   = 0x000000000f0f0f0f;
static const SquareSet ne_4x4   = 0x00000000f0f0f0f0;
static const SquareSet se_4x4   = 0xf0f0f0f000000000;
static const SquareSet sw_4x4   = 0x0f0f0f0f00000000;

static const SquareSet nw_5x5   = 0x0000001f1f1f1f1f;
static const SquareSet ne_5x5   = 0x000000f8f8f8f8f8;
static const SquareSet se_5x5   = 0xf8f8f8f8f8000000;
static const SquareSet sw_5x5   = 0x1f1f1f1f1f000000;

static const SquareSet nw_6x6   = 0x00003f3f3f3f3f3f;
static const SquareSet ne_6x6   = 0x0000fcfcfcfcfcfc;
static const SquareSet se_6x6   = 0xfcfcfcfcfcfc0000;
static const SquareSet sw_6x6   = 0x3f3f3f3f3f3f0000;

static const SquareSet nw_7x7   = 0x007f7f7f7f7f7f7f;
static const SquareSet ne_7x7   = 0x00fefefefefefefe;
static const SquareSet se_7x7   = 0xfefefefefefefe00;
static const SquareSet sw_7x7   = 0x7f7f7f7f7f7f7f00;

static const SquareSet nw_tri_2 = 0x0000000000000103;
static const SquareSet ne_tri_2 = 0x00000000000080c0;
static const SquareSet se_tri_2 = 0xc080000000000000;
static const SquareSet sw_tri_2 = 0x0301000000000000;

static const SquareSet nw_tri_3 = 0x0000000000010307;
static const SquareSet ne_tri_3 = 0x000000000080c0e0;
static const SquareSet se_tri_3 = 0xe0c0800000000000;
static const SquareSet sw_tri_3 = 0x0703010000000000;

static const SquareSet nw_tri_4 = 0x000000000103070f;
static const SquareSet ne_tri_4 = 0x0000000080c0e0f0;
static const SquareSet se_tri_4 = 0xf0e0c08000000000;
static const SquareSet sw_tri_4 = 0x0f07030100000000;

static const SquareSet nw_tri_5 = 0x0000000103070f1f;
static const SquareSet ne_tri_5 = 0x00000080c0e0f0f8;
static const SquareSet se_tri_5 = 0xf8f0e0c080000000;
static const SquareSet sw_tri_5 = 0x1f0f070301000000;

static const SquareSet nw_tri_6 = 0x00000103070f1f3f;
static const SquareSet ne_tri_6 = 0x000080c0e0f0f8fc;
static const SquareSet se_tri_6 = 0xfcf8f0e0c0800000;
static const SquareSet sw_tri_6 = 0x3f1f0f0703010000;

static const SquareSet nw_tri_7 = 0x000103070f1f3f7f;
static const SquareSet ne_tri_7 = 0x0080c0e0f0f8fcfe;
static const SquareSet se_tri_7 = 0xfefcf8f0e0c08000;
static const SquareSet sw_tri_7 = 0x7f3f1f0f07030100;

static const SquareSet nw_tri_8 = 0x0103070f1f3f7fff;
static const SquareSet ne_tri_8 = 0x80c0e0f0f8fcfeff;
static const SquareSet se_tri_8 = 0xfffefcf8f0e0c080;
static const SquareSet sw_tri_8 = 0xff7f3f1f0f070301;

/*
 * Test structure.
 */
struct board_pattern_test_s {
  SquareSet mover;
  SquareSet opponent;
  board_pattern_index_t expected[8];
};



/*
 * Auxiliary functions.
 */

static void
aux_check_expected_indexes (ut_test_t *const t,
                            board_pattern_id_t p_id,
                            SquareSet m,
                            SquareSet o,
                            board_pattern_index_t *expected_indexes)
{
  const board_pattern_t *p;
  int n;
  board_t board;
  board_t *b;
  board_pattern_index_t computed_indexes[8];

  p = &board_patterns[p_id];
  n = p->n_instances;
  b = &board;

  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(computed_indexes, p, b);
  for (int i = 0; i < n; i++) ut_assert(t, expected_indexes[i] == computed_indexes[i]);
}

static void
aux_check_expected_indexes_array (ut_test_t *const t,
                                  board_pattern_id_t pattern_id,
                                  struct board_pattern_test_s test_data[],
                                  size_t number_of_tests)
{
  for (size_t i = 0; i < number_of_tests; i++) {
    aux_check_expected_indexes(t, pattern_id, test_data[i].mover, test_data[i].opponent, test_data[i].expected);
  }
}



/*
 * Test functions.
 */

static void
board_patterns_t (ut_test_t *const t)
{
  for (unsigned int bp = 0; bp < BOARD_PATTERN_INVALID; bp++) {
    ut_assert(t, bp == board_patterns[bp].id);
    const SquareSet base_mask = board_patterns[bp].masks[0];
    const unsigned int n_instances = board_patterns[bp].n_instances;
    const unsigned int n_squares = board_patterns[bp].n_squares;
    for (unsigned int i = 0; i < n_instances; i++) {
      const SquareSet mask = board_patterns[bp].masks[i];
      const board_trans_f ttp = board_patterns[bp].trans_to_principal_f[i];
      ut_assert(t, n_squares == bitw_bit_count_64(mask));
      ut_assert(t, base_mask == ttp(mask));
    }
  }
}

static void
board_pattern_compute_indexes_edge_t (ut_test_t *const t)
{
  /*
  char buf[512];
  square_set_print(buf, sw_tri_8);
  printf("\n\n%s\n", buf);
  */

  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,    0,    0,    0 }},
      {full,     empty,    { 3280, 3280, 3280, 3280 }},
      {empty,    full,     { 6560, 6560, 6560, 6560 }},
      {border,   empty,    { 3280, 3280, 3280, 3280 }},
      {empty,    border,   { 6560, 6560, 6560, 6560 }},
      {n_edge,   empty,    { 3280,    1,    0, 2187 }},
      {e_edge,   empty,    { 2187, 3280,    1,    0 }},
      {s_edge,   empty,    {    0, 2187, 3280,    1 }},
      {w_edge,   empty,    {    1,    0, 2187, 3280 }},
      {empty,    n_edge,   { 6560,    2,    0, 4374 }},
      {empty,    e_edge,   { 4374, 6560,    2,    0 }},
      {empty,    s_edge,   {    0, 4374, 6560,    2 }},
      {empty,    w_edge,   {    2,    0, 4374, 6560 }},
      {zebra_e,  empty,    {  820,    0,    0, 2187 }},
      {empty,    zebra_e,  { 1640,    0,    0, 4374 }},
      {zebra_o,  empty,    { 2460,    1,    0,    0 }},
      {empty,    zebra_o,  { 4920,    2,    0,    0 }},
      {zebra_e,  zebra_o,  { 5740,    2,    0, 2187 }},
      {zebra_o,  zebra_e,  { 4100,    1,    0, 4374 }},
      {n_r2,     empty,    {    0,    3,    0,  729 }},
      {n_r3,     empty,    {    0,    9,    0,  243 }},
      {n_r4,     empty,    {    0,   27,    0,   81 }},
      {nw_3x3,   e_r2,     { 1471,    0,    6, 3159 }},
      {empty,    ne_tri_5, { 6534,  242,    0,    0 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_EDGE, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_corner_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {     0,     0,     0,     0 }},
      {full,     empty,    {  9841,  9841,  9841,  9841 }},
      {empty,    full,     { 19682, 19682, 19682, 19682 }},
      {nw_5x5,   empty,    {  9841,     0,     0,     0 }},
      {nw_6x6,   empty,    {  9841,  9477,  6561,  6813 }},
      {nw_7x7,   empty,    {  9841,  9828,  9072,  9084 }},
      {empty,    nw_5x5,   { 19682,     0,     0,     0 }},
      {empty,    nw_6x6,   { 19682, 18954, 13122, 13626 }},
      {empty,    nw_7x7,   { 19682, 19656, 18144, 18168 }},
      {border,   bord_2,   { 13891, 13891, 13891, 13891 }},
      {bord_1,   bord_3,   {  2511,  2511,  2511,  2511 }},
      {nw_2x2,   se_tri_3, {   112,     0,  1700,     0 }},
      {ne_tri_8, empty,    {  6898,  9841,  9586,     0 }},
      {ne_tri_7, sw_tri_7, {  6141,  9841,  3453, 19682 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_CORNER, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_xedge_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {     0,     0,     0,     0 }},
      {full,     empty,    { 29524, 29524, 29524, 29524 }},
      {empty,    full,     { 59048, 59048, 59048, 59048 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_XEDGE, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_r2_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {     0,     0,     0,     0}},
      {full,     empty,    {  3280,  3280,  3280,  3280}},
      {empty,    full,     {  6560,  6560,  6560,  6560}},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_R2, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_r3_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {     0,     0,     0,     0 }},
      {full,     empty,    {  3280,  3280,  3280,  3280 }},
      {empty,    full,     {  6560,  6560,  6560,  6560 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_R3, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_r4_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,     0,     0,     0}},
      {full,     empty,    { 3280,  3280,  3280,  3280}},
      {empty,    full,     { 6560,  6560,  6560,  6560}},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_R4, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag4_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,     0,     0,     0 }},
      {full,     empty,    {   40,    40,    40,    40 }},
      {empty,    full,     {   80,    80,    80,    80 }},
      {n_edge,   empty,    {   27,     1,     0,     0 }},
      {e_edge,   empty,    {    0,    27,     1,     0 }},
      {s_edge,   empty,    {    0,     0,    27,     1 }},
      {w_edge,   empty,    {    1,     0,     0,    27 }},
      {empty,    n_r2,     {   18,     6,     0,     0 }},
      {empty,    e_r2,     {    0,    18,     6,     0 }},
      {empty,    s_r2,     {    0,     0,    18,     6 }},
      {empty,    w_r2,     {    6,     0,     0,    18 }},
      {n_r3,     n_r4,     {    5,    63,     0,     0 }},
      {e_r3,     e_r4,     {    0,     5,    63,     0 }},
      {s_r3,     s_r4,     {    0,     0,     5,    63 }},
      {w_r3,     w_r4,     {   63,     0,     0,     5 }},
      {border,   empty,    {   28,    28,    28,    28 }},
      {empty,    border,   {   56,    56,    56,    56 }},
      {border,   empty,    {   28,    28,    28,    28 }},
      {empty,    border,   {   56,    56,    56,    56 }},
      {bord_1,   empty,    {   12,    12,    12,    12 }},
      {empty,    bord_1,   {   24,    24,    24,    24 }},
      {bord_2,   bord_3,   {    0,     0,     0,     0 }},
      {border,   bord_1,   {   52,    52,    52,    52 }},
      {bord_1,   border,   {   68,    68,    68,    68 }},
      {nw_3x3,   ne_3x3,   {   12,    24,     0,     0 }},
      {ne_3x3,   se_3x3,   {    0,    12,    24,     0 }},
      {se_3x3,   sw_3x3,   {    0,     0,    12,    24 }},
      {sw_3x3,   nw_3x3,   {   24,     0,     0,    12 }},
      {nw_7x7,   empty,    {   40,    13,    12,    39 }},
      {ne_7x7,   empty,    {   39,    40,    13,    12 }},
      {se_7x7,   empty,    {   12,    39,    40,    13 }},
      {sw_7x7,   empty,    {   13,    12,    39,    40 }},
      {empty,    nw_7x7,   {   80,    26,    24,    78 }},
      {empty,    ne_7x7,   {   78,    80,    26,    24 }},
      {empty,    se_7x7,   {   24,    78,    80,    26 }},
      {empty,    sw_7x7,   {   26,    24,    78,    80 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG4, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag5_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,     0,     0,     0}},
      {full,     empty,    {  121,   121,   121,   121}},
      {empty,    full,     {  242,   242,   242,   242}},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG5, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag6_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,     0,     0,     0 }},
      {full,     empty,    {  364,   364,   364,   364 }},
      {empty,    full,     {  728,   728,   728,   728 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG6, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag7_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,     0,     0,     0 }},
      {full,     empty,    { 1093,  1093,  1093,  1093 }},
      {empty,    full,     { 2186,  2186,  2186,  2186 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG7, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag8_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,     0 }},
      {full,     empty,    { 3280,  3280 }},
      {empty,    full,     { 6560,  6560 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG8, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_2x5cor_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {     0,     0,     0,     0,    0,      0,     0,     0 }},
      {full,     empty,    { 29524, 29524, 29524, 29524, 29524, 29524, 29524, 29524 }},
      {empty,    full,     { 59048, 59048, 59048, 59048, 59048, 59048, 59048, 59048 }},
      {n_edge,   empty,    {   121,   244,     0,     0,   121,     0,     0,   244 }}
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_2X5COR, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
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

  ut_suite_t *const s = ut_suite_new(&config, "board_pattern");

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_patterns", board_patterns_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_edge", board_pattern_compute_indexes_edge_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_corner", board_pattern_compute_indexes_corner_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_xedge", board_pattern_compute_indexes_xedge_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_r2", board_pattern_compute_indexes_r2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_r3", board_pattern_compute_indexes_r3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_r4", board_pattern_compute_indexes_r4_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag4", board_pattern_compute_indexes_diag4_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag5", board_pattern_compute_indexes_diag5_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag6", board_pattern_compute_indexes_diag6_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag7", board_pattern_compute_indexes_diag7_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag8", board_pattern_compute_indexes_diag8_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_2x5cor", board_pattern_compute_indexes_2x5cor_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
