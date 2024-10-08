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
 * @copyright 2018, 2019, 2023 Roberto Corradini. All rights reserved.
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
 *
 *    packed_03           packed_04           packed_05            packed_06
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 . . . . .     1 1 1 1 . . . .     1 1 1 1 1 . . .     1 1 1 1 1 1 . .
 * 2  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 3  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 4  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 5  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 *
 *
 *    packed_07           packed_08           packed_09            packed_10
 *
 *    a b c d e f g h     a b c d e f g h     a b c d e f g h     a b c d e f g h
 *
 * 1  1 1 1 1 1 1 1 .     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1     1 1 1 1 1 1 1 1
 * 2  . . . . . . . .     . . . . . . . .     1 . . . . . . .     1 1 . . . . . .
 * 3  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 4  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 5  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 6  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 7  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 * 8  . . . . . . . .     . . . . . . . .     . . . . . . . .     . . . . . . . .
 *
 */

static const SquareSet empty         = 0x0000000000000000;
static const SquareSet full          = 0xffffffffffffffff;

static const SquareSet n_edge        = 0x00000000000000ff;
static const SquareSet e_edge        = 0x8080808080808080;
static const SquareSet s_edge        = 0xff00000000000000;
static const SquareSet w_edge        = 0x0101010101010101;

static const SquareSet n_r2          = 0x000000000000ff00;
static const SquareSet e_r2          = 0x4040404040404040;
static const SquareSet s_r2          = 0x00ff000000000000;
static const SquareSet w_r2          = 0x0202020202020202;

static const SquareSet n_r3          = 0x0000000000ff0000;
static const SquareSet e_r3          = 0x2020202020202020;
static const SquareSet s_r3          = 0x0000ff0000000000;
static const SquareSet w_r3          = 0x0404040404040404;

static const SquareSet n_r4          = 0x00000000ff000000;
static const SquareSet e_r4          = 0x1010101010101010;
static const SquareSet s_r4          = 0x000000ff00000000;
static const SquareSet w_r4          = 0x0808080808080808;

static const SquareSet border        = 0xff818181818181ff;
static const SquareSet bord_1        = 0x007e424242427e00;
static const SquareSet bord_2        = 0x00003c24243c0000;
static const SquareSet bord_3        = 0x0000001818000000;

static const SquareSet zebra_o       = 0x00000000000000aa;
static const SquareSet zebra_e       = 0x0000000000000055;

static const SquareSet nw_2x2        = 0x0000000000000303;

static const SquareSet nw_3x3        = 0x0000000000070707;
static const SquareSet ne_3x3        = 0x0000000000e0e0e0;
static const SquareSet se_3x3        = 0xe0e0e00000000000;
static const SquareSet sw_3x3        = 0x0707070000000000;

static const SquareSet nw_5x5        = 0x0000001f1f1f1f1f;

static const SquareSet nw_6x6        = 0x00003f3f3f3f3f3f;

static const SquareSet nw_7x7        = 0x007f7f7f7f7f7f7f;
static const SquareSet ne_7x7        = 0x00fefefefefefefe;
static const SquareSet se_7x7        = 0xfefefefefefefe00;
static const SquareSet sw_7x7        = 0x7f7f7f7f7f7f7f00;

static const SquareSet nw_tri_2      = 0x0000000000000103;
static const SquareSet ne_tri_2      = 0x00000000000080c0;
static const SquareSet se_tri_2      = 0xc080000000000000;
static const SquareSet sw_tri_2      = 0x0301000000000000;

static const SquareSet nw_tri_3      = 0x0000000000010307;
static const SquareSet ne_tri_3      = 0x000000000080c0e0;
static const SquareSet se_tri_3      = 0xe0c0800000000000;
static const SquareSet sw_tri_3      = 0x0703010000000000;

static const SquareSet nw_tri_4      = 0x000000000103070f;
static const SquareSet ne_tri_4      = 0x0000000080c0e0f0;
static const SquareSet se_tri_4      = 0xf0e0c08000000000;
static const SquareSet sw_tri_4      = 0x0f07030100000000;

static const SquareSet nw_tri_5      = 0x0000000103070f1f;
static const SquareSet ne_tri_5      = 0x00000080c0e0f0f8;
static const SquareSet se_tri_5      = 0xf8f0e0c080000000;
static const SquareSet sw_tri_5      = 0x1f0f070301000000;

static const SquareSet ne_tri_7      = 0x0080c0e0f0f8fcfe;
static const SquareSet sw_tri_7      = 0x7f3f1f0f07030100;

static const SquareSet ne_tri_8      = 0x80c0e0f0f8fcfeff;

static const SquareSet packed_03     = 0x0000000000000007;
static const SquareSet packed_04     = 0x000000000000000f;
static const SquareSet packed_05     = 0x000000000000001f;
static const SquareSet packed_06     = 0x000000000000003f;

static const SquareSet packed_07     = 0x000000000000007f;
static const SquareSet packed_08     = 0x00000000000000ff;
static const SquareSet packed_09     = 0x00000000000001ff;
static const SquareSet packed_10     = 0x00000000000003ff;

static const SquareSet packed_12     = 0x0000000000000fff;

static const SquareSet mask_edge_0   = 0x00000000000000ff;
static const SquareSet mask_corner_0 = 0x0000000000070707;
static const SquareSet mask_xedge_0  = 0x00000000000042ff;
static const SquareSet mask_r2_0     = 0x000000000000ff00;
static const SquareSet mask_r3_0     = 0x0000000000ff0000;
static const SquareSet mask_r4_0     = 0x00000000ff000000;
static const SquareSet mask_diag3_0  = 0x0000000000010204;
static const SquareSet mask_diag4_0  = 0x0000000001020408;
static const SquareSet mask_diag5_0  = 0x0000000102040810;
static const SquareSet mask_diag6_0  = 0x0000010204081020;
static const SquareSet mask_diag7_0  = 0x0001020408102040;
static const SquareSet mask_diag8_0  = 0x0102040810204080;
static const SquareSet mask_2x5cor_0 = 0x0000000000001f1f;
static const SquareSet mask_2x6cor_0 = 0x0000000000003f3f;

/*
 * Test structure.
 */
struct board_pattern_test_s {
  SquareSet mover;
  SquareSet opponent;
  board_pattern_index_t expected[8];
  board_pattern_index_t expected_principal[8];
};



/*
 * Auxiliary functions.
 */

static void
aux_check_expected_indexes (ut_test_t *const t,
                            board_pattern_id_t p_id,
                            SquareSet m,
                            SquareSet o,
                            board_pattern_index_t *expected_indexes,
                            board_pattern_index_t *expected_principal_indexes)
{
  const board_pattern_t *p;
  int n;
  board_t board;
  board_t *b;
  board_pattern_index_t computed_indexes[8];
  board_pattern_index_t computed_principal_indexes[8];

  p = &board_patterns[p_id];
  n = p->n_instances;
  b = &board;

  ut_assert(t, (m & o) == empty);

  board_set_square_sets(b, m, o);
  board_pattern_compute_indexes(computed_indexes, p, b);
  for (int i = 0; i < n; i++) {
    if (expected_indexes[i] != computed_indexes[i]) {
      printf("\n");
      printf("  i = %d\n", i);
      printf("  expected_indexes[i] = %d\n", expected_indexes[i]);
      printf("  computed_indexes[i] = %d\n", computed_indexes[i]);
      ut_assert(t, false);
    }
  }
  board_pattern_compute_principal_indexes(computed_principal_indexes, computed_indexes, p, false);
  for (int i = 0; i < n; i++) {
    if (expected_principal_indexes[i] != computed_principal_indexes[i]) {
      printf("\n");
      printf("  i = %d\n", i);
      printf("  expected_principal_indexes[i] = %d\n", expected_principal_indexes[i]);
      printf("  computed_principal_indexes[i] = %d\n", computed_principal_indexes[i]);
      ut_assert(t, false);
    }
  }
}

static void
aux_check_expected_indexes_array (ut_test_t *const t,
                                  board_pattern_id_t pattern_id,
                                  struct board_pattern_test_s test_data[],
                                  size_t number_of_tests)
{
  for (size_t i = 0; i < number_of_tests; i++) {
    aux_check_expected_indexes(t, pattern_id, test_data[i].mover, test_data[i].opponent, test_data[i].expected, test_data[i].expected_principal);
  }
}



/*
 * Test functions.
 */

static void
board_pattern_compute_principal_indexes_one_value_t (ut_test_t *const t)
{
  const bool one_value = true;

  const board_pattern_t *p;
  board_pattern_index_t index;
  board_pattern_index_t computed_principal_index;

  p = &board_patterns[BOARD_PATTERN_DIAG3];
  index = 24;
  board_pattern_compute_principal_indexes(&computed_principal_index, &index, p, one_value);
  ut_assert(t, computed_principal_index == 8);
}

static void
board_pattern_structure_sizes_t (ut_test_t *const t)
{
  /*
   * In case of changes to the definition of the struct board_pattern_t this test may fail.
   * In case of failure sizes have to be adjusted, and accordingly the ctypes interface in python
   * has to be updated.
   */
  ut_assert(t, 1 == sizeof(char));
  ut_assert(t, 4 == sizeof(board_pattern_id_t));
  ut_assert(t, 7 == sizeof(char[7]));
  ut_assert(t, 4 == sizeof(int));
  ut_assert(t, 8 == sizeof(unsigned long int));
  ut_assert(t, 8 == sizeof(SquareSet));
  ut_assert(t, 8 == sizeof(board_trans_f));
  ut_assert(t, 8 == sizeof( SquareSet (*) (SquareSet)));
  ut_assert(t, 184 == sizeof(board_pattern_t));
}

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
board_pattern_get_id_by_name_t (ut_test_t *const t)
{
  board_pattern_id_t idp;
  bool is_valid_pattern;
  char *pattern;

  pattern = "EDGE";
  is_valid_pattern = board_pattern_get_id_by_name(&idp, pattern);
  ut_assert(t, is_valid_pattern == true);
  ut_assert(t, idp == BOARD_PATTERN_EDGE);

  pattern = "EDGE";
  is_valid_pattern = board_pattern_get_id_by_name(NULL, pattern);
  ut_assert(t, is_valid_pattern == true);

  pattern = "ABCD";
  is_valid_pattern = board_pattern_get_id_by_name(&idp, pattern);
  ut_assert(t, is_valid_pattern == false);
}

static void
board_pattern_pack_edge_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_edge(empty));
  ut_assert(t, packed_08 == board_pattern_pack_edge(mask_edge_0));
  ut_assert(t, packed_08 == board_pattern_pack_edge(full));
}

static void
board_pattern_unpack_edge_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_edge(empty));
  ut_assert(t, mask_edge_0 == board_pattern_unpack_edge(packed_08));
}

static void
board_pattern_pack_corner_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_corner(empty));
  ut_assert(t, packed_09 == board_pattern_pack_corner(mask_corner_0));
  ut_assert(t, packed_09 == board_pattern_pack_corner(full));
}

static void
board_pattern_unpack_corner_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_corner(empty));
  ut_assert(t, mask_corner_0 == board_pattern_unpack_corner(packed_09));
}

static void
board_pattern_pack_xedge_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_xedge(empty));
  ut_assert(t, packed_10 == board_pattern_pack_xedge(mask_xedge_0));
  ut_assert(t, packed_10 == board_pattern_pack_xedge(full));
}

static void
board_pattern_unpack_xedge_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_xedge(empty));
  ut_assert(t, mask_xedge_0 == board_pattern_unpack_xedge(packed_10));
}

static void
board_pattern_pack_r2_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_r2(empty));
  ut_assert(t, packed_08 == board_pattern_pack_r2(mask_r2_0));
  ut_assert(t, packed_08 == board_pattern_pack_r2(full));
}

static void
board_pattern_unpack_r2_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_r2(empty));
  ut_assert(t, mask_r2_0 == board_pattern_unpack_r2(packed_08));
}

static void
board_pattern_pack_r3_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_r3(empty));
  ut_assert(t, packed_08 == board_pattern_pack_r3(mask_r3_0));
  ut_assert(t, packed_08 == board_pattern_pack_r3(full));
}

static void
board_pattern_unpack_r3_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_r3(empty));
  ut_assert(t, mask_r3_0 == board_pattern_unpack_r3(packed_08));
}

static void
board_pattern_pack_r4_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_r4(empty));
  ut_assert(t, packed_08 == board_pattern_pack_r4(mask_r4_0));
  ut_assert(t, packed_08 == board_pattern_pack_r4(full));
}

static void
board_pattern_unpack_r4_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_r4(empty));
  ut_assert(t, mask_r4_0 == board_pattern_unpack_r4(packed_08));
}

static void
board_pattern_pack_diag4_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_diag4(empty));
  ut_assert(t, packed_04 == board_pattern_pack_diag4(mask_diag4_0));
  ut_assert(t, packed_04 == board_pattern_pack_diag4(full));
  ut_assert(t, 0x0000000000000008 == board_pattern_pack_diag4(0x0000000000000008));
  ut_assert(t, 0x0000000000000004 == board_pattern_pack_diag4(0x0000000000000400));
  ut_assert(t, 0x0000000000000002 == board_pattern_pack_diag4(0x0000000000020000));
  ut_assert(t, 0x0000000000000001 == board_pattern_pack_diag4(0x0000000001000000));
  ut_assert(t, empty == board_pattern_pack_diag4(full & ~mask_diag4_0));
}

static void
board_pattern_unpack_diag4_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_diag4(empty));
  ut_assert(t, mask_diag4_0 == board_pattern_unpack_diag4(packed_04));
  ut_assert(t, mask_diag4_0 == board_pattern_unpack_diag4(full));
  ut_assert(t, 0x0000000000000008 == board_pattern_unpack_diag4(0x0000000000000008));
  ut_assert(t, 0x0000000000000400 == board_pattern_unpack_diag4(0x0000000000000004));
  ut_assert(t, 0x0000000000020000 == board_pattern_unpack_diag4(0x0000000000000002));
  ut_assert(t, 0x0000000001000000 == board_pattern_unpack_diag4(0x0000000000000001));
  ut_assert(t, empty == board_pattern_unpack_diag4(full & ~packed_04));
}

static void
board_pattern_pack_diag5_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_diag5(empty));
  ut_assert(t, packed_05 == board_pattern_pack_diag5(mask_diag5_0));
  ut_assert(t, packed_05 == board_pattern_pack_diag5(full));
  ut_assert(t, 0x0000000000000010 == board_pattern_pack_diag5(0x0000000000000010));
  ut_assert(t, 0x0000000000000008 == board_pattern_pack_diag5(0x0000000000000800));
  ut_assert(t, 0x0000000000000004 == board_pattern_pack_diag5(0x0000000000040000));
  ut_assert(t, 0x0000000000000002 == board_pattern_pack_diag5(0x0000000002000000));
  ut_assert(t, 0x0000000000000001 == board_pattern_pack_diag5(0x0000000100000000));
  ut_assert(t, empty == board_pattern_pack_diag5(full & ~mask_diag5_0));
}

static void
board_pattern_unpack_diag5_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_diag5(empty));
  ut_assert(t, mask_diag5_0 == board_pattern_unpack_diag5(packed_05));
  ut_assert(t, mask_diag5_0 == board_pattern_unpack_diag5(full));
  ut_assert(t, 0x0000000000000010 == board_pattern_unpack_diag5(0x0000000000000010));
  ut_assert(t, 0x0000000000000800 == board_pattern_unpack_diag5(0x0000000000000008));
  ut_assert(t, 0x0000000000040000 == board_pattern_unpack_diag5(0x0000000000000004));
  ut_assert(t, 0x0000000002000000 == board_pattern_unpack_diag5(0x0000000000000002));
  ut_assert(t, 0x0000000100000000 == board_pattern_unpack_diag5(0x0000000000000001));
  ut_assert(t, empty == board_pattern_unpack_diag5(full & ~packed_05));
}

static void
board_pattern_pack_diag6_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_diag6(empty));
  ut_assert(t, packed_06 == board_pattern_pack_diag6(mask_diag6_0));
  ut_assert(t, packed_06 == board_pattern_pack_diag6(full));
  ut_assert(t, 0x0000000000000020 == board_pattern_pack_diag6(0x0000000000000020));
  ut_assert(t, 0x0000000000000010 == board_pattern_pack_diag6(0x0000000000001000));
  ut_assert(t, 0x0000000000000008 == board_pattern_pack_diag6(0x0000000000080000));
  ut_assert(t, 0x0000000000000004 == board_pattern_pack_diag6(0x0000000004000000));
  ut_assert(t, 0x0000000000000002 == board_pattern_pack_diag6(0x0000000200000000));
  ut_assert(t, 0x0000000000000001 == board_pattern_pack_diag6(0x0000010000000000));
  ut_assert(t, empty == board_pattern_pack_diag6(full & ~mask_diag6_0));
}

static void
board_pattern_unpack_diag6_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_diag6(empty));
  ut_assert(t, mask_diag6_0 == board_pattern_unpack_diag6(packed_06));
  ut_assert(t, mask_diag6_0 == board_pattern_unpack_diag6(full));
  ut_assert(t, 0x0000000000000020 == board_pattern_unpack_diag6(0x0000000000000020));
  ut_assert(t, 0x0000000000001000 == board_pattern_unpack_diag6(0x0000000000000010));
  ut_assert(t, 0x0000000000080000 == board_pattern_unpack_diag6(0x0000000000000008));
  ut_assert(t, 0x0000000004000000 == board_pattern_unpack_diag6(0x0000000000000004));
  ut_assert(t, 0x0000000200000000 == board_pattern_unpack_diag6(0x0000000000000002));
  ut_assert(t, 0x0000010000000000 == board_pattern_unpack_diag6(0x0000000000000001));
  ut_assert(t, empty == board_pattern_unpack_diag6(full & ~packed_06));
}

static void
board_pattern_pack_diag7_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_diag7(empty));
  ut_assert(t, packed_07 == board_pattern_pack_diag7(mask_diag7_0));
  ut_assert(t, packed_07 == board_pattern_pack_diag7(full));
  ut_assert(t, 0x0000000000000040 == board_pattern_pack_diag7(0x0000000000000040));
  ut_assert(t, 0x0000000000000020 == board_pattern_pack_diag7(0x0000000000002000));
  ut_assert(t, 0x0000000000000010 == board_pattern_pack_diag7(0x0000000000100000));
  ut_assert(t, 0x0000000000000008 == board_pattern_pack_diag7(0x0000000008000000));
  ut_assert(t, 0x0000000000000004 == board_pattern_pack_diag7(0x0000000400000000));
  ut_assert(t, 0x0000000000000002 == board_pattern_pack_diag7(0x0000020000000000));
  ut_assert(t, 0x0000000000000001 == board_pattern_pack_diag7(0x0001000000000000));
  ut_assert(t, empty == board_pattern_pack_diag7(full & ~mask_diag7_0));
}

static void
board_pattern_unpack_diag7_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_diag7(empty));
  ut_assert(t, mask_diag7_0 == board_pattern_unpack_diag7(packed_07));
  ut_assert(t, mask_diag7_0 == board_pattern_unpack_diag7(full));
  ut_assert(t, 0x0000000000000040 == board_pattern_unpack_diag7(0x0000000000000040));
  ut_assert(t, 0x0000000000002000 == board_pattern_unpack_diag7(0x0000000000000020));
  ut_assert(t, 0x0000000000100000 == board_pattern_unpack_diag7(0x0000000000000010));
  ut_assert(t, 0x0000000008000000 == board_pattern_unpack_diag7(0x0000000000000008));
  ut_assert(t, 0x0000000400000000 == board_pattern_unpack_diag7(0x0000000000000004));
  ut_assert(t, 0x0000020000000000 == board_pattern_unpack_diag7(0x0000000000000002));
  ut_assert(t, 0x0001000000000000 == board_pattern_unpack_diag7(0x0000000000000001));
  ut_assert(t, empty == board_pattern_unpack_diag7(full & ~packed_07));
}

static void
board_pattern_pack_diag8_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_diag8(empty));
  ut_assert(t, packed_08 == board_pattern_pack_diag8(mask_diag8_0));
  ut_assert(t, packed_08 == board_pattern_pack_diag8(full));
  ut_assert(t, 0x0000000000000080 == board_pattern_pack_diag8(0x0000000000000080));
  ut_assert(t, 0x0000000000000040 == board_pattern_pack_diag8(0x0000000000004000));
  ut_assert(t, 0x0000000000000020 == board_pattern_pack_diag8(0x0000000000200000));
  ut_assert(t, 0x0000000000000010 == board_pattern_pack_diag8(0x0000000010000000));
  ut_assert(t, 0x0000000000000008 == board_pattern_pack_diag8(0x0000000800000000));
  ut_assert(t, 0x0000000000000004 == board_pattern_pack_diag8(0x0000040000000000));
  ut_assert(t, 0x0000000000000002 == board_pattern_pack_diag8(0x0002000000000000));
  ut_assert(t, 0x0000000000000001 == board_pattern_pack_diag8(0x0100000000000000));
  ut_assert(t, empty == board_pattern_pack_diag8(full & ~mask_diag8_0));
}

static void
board_pattern_unpack_diag8_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_diag8(empty));
  ut_assert(t, mask_diag8_0 == board_pattern_unpack_diag8(packed_08));
  ut_assert(t, mask_diag8_0 == board_pattern_unpack_diag8(full));
  ut_assert(t, 0x0000000000000080 == board_pattern_unpack_diag8(0x0000000000000080));
  ut_assert(t, 0x0000000000004000 == board_pattern_unpack_diag8(0x0000000000000040));
  ut_assert(t, 0x0000000000200000 == board_pattern_unpack_diag8(0x0000000000000020));
  ut_assert(t, 0x0000000010000000 == board_pattern_unpack_diag8(0x0000000000000010));
  ut_assert(t, 0x0000000800000000 == board_pattern_unpack_diag8(0x0000000000000008));
  ut_assert(t, 0x0000040000000000 == board_pattern_unpack_diag8(0x0000000000000004));
  ut_assert(t, 0x0002000000000000 == board_pattern_unpack_diag8(0x0000000000000002));
  ut_assert(t, 0x0100000000000000 == board_pattern_unpack_diag8(0x0000000000000001));
  ut_assert(t, empty == board_pattern_unpack_diag8(full & ~packed_08));
}

static void
board_pattern_pack_2x5cor_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_2x5cor(empty));
  ut_assert(t, packed_10 == board_pattern_pack_2x5cor(mask_2x5cor_0));
  ut_assert(t, packed_10 == board_pattern_pack_2x5cor(full));
}

static void
board_pattern_unpack_2x5cor_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_2x5cor(empty));
  ut_assert(t, mask_2x5cor_0 == board_pattern_unpack_2x5cor(packed_10));
}

static void
board_pattern_pack_diag3_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_diag3(empty));
  ut_assert(t, packed_03 == board_pattern_pack_diag3(mask_diag3_0));
  ut_assert(t, packed_03 == board_pattern_pack_diag3(full));
  ut_assert(t, 0x0000000000000004 == board_pattern_pack_diag3(0x0000000000000004));
  ut_assert(t, 0x0000000000000002 == board_pattern_pack_diag3(0x0000000000000200));
  ut_assert(t, 0x0000000000000001 == board_pattern_pack_diag3(0x0000000000010000));
  ut_assert(t, empty == board_pattern_pack_diag3(full & ~mask_diag3_0));
}

static void
board_pattern_unpack_diag3_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_diag3(empty));
  ut_assert(t, mask_diag3_0 == board_pattern_unpack_diag3(packed_03));
  ut_assert(t, mask_diag3_0 == board_pattern_unpack_diag3(full));
  ut_assert(t, 0x0000000000000004 == board_pattern_unpack_diag3(0x0000000000000004));
  ut_assert(t, 0x0000000000000200 == board_pattern_unpack_diag3(0x0000000000000002));
  ut_assert(t, 0x0000000000010000 == board_pattern_unpack_diag3(0x0000000000000001));
  ut_assert(t, empty == board_pattern_unpack_diag3(full & ~packed_03));
}

static void
board_pattern_pack_2x6cor_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_pack_2x6cor(empty));
  ut_assert(t, packed_12 == board_pattern_pack_2x6cor(mask_2x6cor_0));
  ut_assert(t, packed_12 == board_pattern_pack_2x6cor(full));
}

static void
board_pattern_unpack_2x6cor_t (ut_test_t *const t)
{
  ut_assert(t, empty == board_pattern_unpack_2x6cor(empty));
  ut_assert(t, mask_2x6cor_0 == board_pattern_unpack_2x6cor(packed_12));
}

static void
board_pattern_packed_to_index_aux (ut_test_t *const t,
                                   board_pattern_index_t (*fp) (board_t *, unsigned int))
{
  board_t b;

  /* Test edge 00000000 */
  board_set_square_sets(&b, 0x0000, 0x0000);
  ut_assert(t, fp(&b, 8) == 0);

  /* Test edge 11111111 */
  board_set_square_sets(&b, 0x00ff, 0x0000);
  ut_assert(t, fp(&b, 8) == 3280);

  /* Test edge 22222222 */
  board_set_square_sets(&b, 0x0000, 0x00ff);
  ut_assert(t, fp(&b, 8) == 6560);

  /* Test xedge 0022212021 */
  board_set_square_sets(&b, 0x0220, 0x015c);
  ut_assert(t, fp(&b, 10) == 34740);

  /* Test corner 001021222 */
  board_set_square_sets(&b, 0x0024, 0x01d0);
  ut_assert(t, fp(&b, 9) == 19368);

  /* Test R2 12222221 */
  board_set_square_sets(&b, 0x0081, 0x007e);
  ut_assert(t, fp(&b, 8) == 4372);

  /* Test diag3 211 */
  board_set_square_sets(&b, 0x0006, 0x0001);
  ut_assert(t, fp(&b, 3) == 14);

  /* Test diag4 0122 */
  board_set_square_sets(&b, 0x0002, 0x000c);
  ut_assert(t, fp(&b, 4) == 75);

  /* Test diag5 21122 */
  board_set_square_sets(&b, 0x0006, 0x0019);
  ut_assert(t, fp(&b, 5) == 230);

  /* Test diag6 111201 */
  board_set_square_sets(&b, 0x0027, 0x0008);
  ut_assert(t, fp(&b, 6) == 310);

  /* Test diag7 0122212 */
  board_set_square_sets(&b, 0x0022, 0x005c);
  ut_assert(t, fp(&b, 7) == 1938);

  /* Test diag8 00121111 */
  board_set_square_sets(&b, 0x00f4, 0x0008);
  ut_assert(t, fp(&b, 8) == 3303);

  /* Test 2x5cor 2222222220 */
  board_set_square_sets(&b, 0x0000, 0x01ff);
  ut_assert(t, fp(&b, 10) == 19682);

  /* Test 2x5cor 2222222222 */
  board_set_square_sets(&b, 0x0000, 0x03ff);
  ut_assert(t, fp(&b, 10) == 59048);

  /* Test diag3 222 */
  board_set_square_sets(&b, 0x0000, 0x0007);
  ut_assert(t, fp(&b, 3) == 26);

  /* Test 2x6cor 222222222222 */
  board_set_square_sets(&b, 0x0000, 0x0fff);
  ut_assert(t, fp(&b, 12) == 531440);

  /* Test 2x6cor 222222222220 */
  board_set_square_sets(&b, 0x0000, 0x07ff);
  ut_assert(t, fp(&b, 12) == 177146);
}

static void
board_pattern_packed_to_index_t (ut_test_t *const t)
{
  for (int i = 0; i < 1; i++)
    board_pattern_packed_to_index_aux(t, board_pattern_packed_to_index);
}

static void
board_pattern_packed_to_index_vec_t (ut_test_t *const t)
{
  for (int i = 0; i < 1; i++)
    board_pattern_packed_to_index_aux(t, board_pattern_packed_to_index_vec);
}

static void
board_pattern_index_to_packed_t (ut_test_t *const t)
{
  board_t result;
  SquareSet me, oe;
  board_pattern_index_t index;

  /* Test edge.a */
  index = 0;
  me = 0x0000000000000000;
  oe = 0x0000000000000000;
  board_pattern_index_to_packed(&result, index);
  ut_assert(t, board_get_mover_square_set(&result) == me);
  ut_assert(t, board_get_opponent_square_set(&result) == oe);

  /* Test edge.b */
  index = 6560;
  me = 0x0000000000000000;
  oe = 0x00000000000000ff;
  board_pattern_index_to_packed(&result, index);
  ut_assert(t, board_get_mover_square_set(&result) == me);
  ut_assert(t, board_get_opponent_square_set(&result) == oe);

  /* Test edge.c */
  index = 2329;
  me = 0x0000000000000091;
  oe = 0x000000000000000a;
  board_pattern_index_to_packed(&result, index);
  ut_assert(t, board_get_mover_square_set(&result) == me);
  ut_assert(t, board_get_opponent_square_set(&result) == oe);
}

static void
board_pattern_compute_indexes_edge_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,    0,    0,    0 }, {    0,    0,    0,    0 }},
      {full,     empty,    { 3280, 3280, 3280, 3280 }, { 3280, 3280, 3280, 3280 }},
      {empty,    full,     { 6560, 6560, 6560, 6560 }, { 6560, 6560, 6560, 6560 }},
      {border,   empty,    { 3280, 3280, 3280, 3280 }, { 3280, 3280, 3280, 3280 }},
      {empty,    border,   { 6560, 6560, 6560, 6560 }, { 6560, 6560, 6560, 6560 }},
      {n_edge,   empty,    { 3280,    1,    0, 2187 }, { 3280,    1,    0,    1 }},
      {e_edge,   empty,    { 2187, 3280,    1,    0 }, {    1, 3280,    1,    0 }},
      {s_edge,   empty,    {    0, 2187, 3280,    1 }, {    0,    1, 3280,    1 }},
      {w_edge,   empty,    {    1,    0, 2187, 3280 }, {    1,    0,    1, 3280 }},
      {empty,    n_edge,   { 6560,    2,    0, 4374 }, { 6560,    2,    0,    2 }},
      {empty,    e_edge,   { 4374, 6560,    2,    0 }, {    2, 6560,    2,    0 }},
      {empty,    s_edge,   {    0, 4374, 6560,    2 }, {    0,    2, 6560,    2 }},
      {empty,    w_edge,   {    2,    0, 4374, 6560 }, {    2,    0,    2, 6560 }},
      {zebra_e,  empty,    {  820,    0,    0, 2187 }, {  820,    0,    0,    1 }},
      {empty,    zebra_e,  { 1640,    0,    0, 4374 }, { 1640,    0,    0,    2 }},
      {zebra_o,  empty,    { 2460,    1,    0,    0 }, {  820,    1,    0,    0 }},
      {empty,    zebra_o,  { 4920,    2,    0,    0 }, { 1640,    2,    0,    0 }},
      {zebra_e,  zebra_o,  { 5740,    2,    0, 2187 }, { 4100,    2,    0,    1 }},
      {zebra_o,  zebra_e,  { 4100,    1,    0, 4374 }, { 4100,    1,    0,    2 }},
      {n_r2,     empty,    {    0,    3,    0,  729 }, {    0,    3,    0,    3 }},
      {n_r3,     empty,    {    0,    9,    0,  243 }, {    0,    9,    0,    9 }},
      {n_r4,     empty,    {    0,   27,    0,   81 }, {    0,   27,    0,   27 }},
      {nw_3x3,   e_r2,     { 1471,    0,    6, 3159 }, { 1471,    0,    6,   13 }},
      {empty,    ne_tri_5, { 6534,  242,    0,    0 }, {  242,  242,    0,    0 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_EDGE, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_corner_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {     0,     0,     0,     0 }, {     0,     0,     0,     0 }},
      {full,     empty,    {  9841,  9841,  9841,  9841 }, {  9841,  9841,  9841,  9841 }},
      {empty,    full,     { 19682, 19682, 19682, 19682 }, { 19682, 19682, 19682, 19682 }},
      {nw_5x5,   empty,    {  9841,     0,     0,     0 }, {  9841,     0,     0,     0 }},
      {nw_6x6,   empty,    {  9841,  9477,  6561,  6813 }, {  9841,  6813,  6561,  6813 }},
      {nw_7x7,   empty,    {  9841,  9828,  9072,  9084 }, {  9841,  9084,  9072,  9084 }},
      {empty,    nw_5x5,   { 19682,     0,     0,     0 }, { 19682,     0,     0,     0 }},
      {empty,    nw_6x6,   { 19682, 18954, 13122, 13626 }, { 19682, 13626, 13122, 13626 }},
      {empty,    nw_7x7,   { 19682, 19656, 18144, 18168 }, { 19682, 18168, 18144, 18168 }},
      {border,   bord_2,   { 13891, 13891, 13891, 13891 }, { 13891, 13891, 13891, 13891 }},
      {bord_1,   bord_3,   {  2511,  2511,  2511,  2511 }, {  2511,  2511,  2511,  2511 }},
      {nw_2x2,   se_tri_3, {   112,     0,  1700,     0 }, {   112,     0,  1700,     0 }},
      {ne_tri_8, empty,    {  6898,  9841,  9586,     0 }, {  6898,  9841,  6898,     0 }},
      {ne_tri_7, sw_tri_7, {  6141,  9841,  3453, 19682 }, {  3453,  9841,  3453, 19682 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_CORNER, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_xedge_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {     0,     0,     0,     0 }, {     0,     0,     0,     0 }},
      {full,     empty,    { 29524, 29524, 29524, 29524 }, { 29524, 29524, 29524, 29524 }},
      {empty,    full,     { 59048, 59048, 59048, 59048 }, { 59048, 59048, 59048, 59048 }},
      {n_edge,   empty,    {  3280,     1,     0,  2187 }, {  3280,     1,     0,     1 }},
      {e_edge,   empty,    {  2187,  3280,     1,     0 }, {     1,  3280,     1,     0 }},
      {s_edge,   empty,    {     0,  2187,  3280,     1 }, {     0,     1,  3280,     1 }},
      {w_edge,   empty,    {     1,     0,  2187,  3280 }, {     1,     0,     1,  3280 }},
      {empty,    n_r2,     { 52488, 13128,     0, 40824 }, { 52488, 13128,     0, 13128 }},
      {empty,    e_r2,     { 40824, 52488, 13128,     0 }, { 13128, 52488, 13128,     0 }},
      {empty,    s_r2,     {     0, 40824, 52488, 13128 }, {     0, 13128, 52488, 13128 }},
      {empty,    w_r2,     { 13128,     0, 40824, 52488 }, { 13128,     0, 13128, 52488 }},
      {n_r3,     n_r4,     {     0,    63,     0,   405 }, {     0,    63,     0,    63 }},
      {e_r3,     e_r4,     {   405,     0,    63,     0 }, {    63,     0,    63,     0 }},
      {s_r3,     s_r4,     {     0,   405,     0,    63 }, {     0,    63,     0,    63 }},
      {w_r3,     w_r4,     {    63,     0,   405,     0 }, {    63,     0,    63,     0 }},
      {border,   empty,    {  3280,  3280,  3280,  3280 }, {  3280,  3280,  3280,  3280 }},
      {empty,    border,   {  6560,  6560,  6560,  6560 }, {  6560,  6560,  6560,  6560 }},
      {bord_1,   empty,    { 26244, 26244, 26244, 26244 }, { 26244, 26244, 26244, 26244 }},
      {empty,    bord_1,   { 52488, 52488, 52488, 52488 }, { 52488, 52488, 52488, 52488 }},
      {bord_2,   bord_3,   {     0,     0,     0,     0 }, {     0,     0,     0,     0 }},
      {border,   bord_1,   { 55768, 55768, 55768, 55768 }, { 55768, 55768, 55768, 55768 }},
      {bord_1,   border,   { 32804, 32804, 32804, 32804 }, { 32804, 32804, 32804, 32804 }},
      {nw_3x3,   ne_3x3,   { 52258, 13148,     0, 22842 }, { 35990, 13148,     0,  6574 }},
      {ne_3x3,   se_3x3,   { 22842, 52258, 13148,     0 }, {  6574, 35990, 13148,     0 }},
      {se_3x3,   sw_3x3,   {     0, 22842, 52258, 13148 }, {     0,  6574, 35990, 13148 }},
      {sw_3x3,   nw_3x3,   { 13148,     0, 22842, 52258 }, { 13148,     0,  6574, 35990 }},
      {nw_7x7,   empty,    { 27337, 26244, 26244, 29523 }, { 27337, 26244, 26244, 27337 }},
      {ne_7x7,   empty,    { 29523, 27337, 26244, 26244 }, { 27337, 27337, 26244, 26244 }},
      {se_7x7,   empty,    { 26244, 29523, 27337, 26244 }, { 26244, 27337, 27337, 26244 }},
      {sw_7x7,   empty,    { 26244, 26244, 29523, 27337 }, { 26244, 26244, 27337, 27337 }},
      {empty,    nw_7x7,   { 54674, 52488, 52488, 59046 }, { 54674, 52488, 52488, 54674 }},
      {empty,    ne_7x7,   { 59046, 54674, 52488, 52488 }, { 54674, 54674, 52488, 52488 }},
      {empty,    se_7x7,   { 52488, 59046, 54674, 52488 }, { 52488, 54674, 54674, 52488 }},
      {empty,    sw_7x7,   { 52488, 52488, 59046, 54674 }, { 52488, 52488, 54674, 54674 }},
      {nw_tri_2, ne_tri_3, { 45688, 13148,     0,  2916 }, { 16064, 13148,     0,     4 }},
      {ne_tri_2, se_tri_3, {  2916, 45688, 13148,     0 }, {     4, 16064, 13148,     0 }},
      {se_tri_2, sw_tri_3, {     0,  2916, 45688, 13148 }, {     0,     4, 16064, 13148 }},
      {sw_tri_2, nw_tri_3, { 13148,     0,  2916, 45688 }, { 13148,     0,     4, 16064 }},
      {nw_tri_4, se_tri_5, {  6601, 45900, 13364, 22923 }, {  6601, 13364, 13364,  6601 }},
      {ne_tri_4, sw_tri_5, { 22923,  6601, 45900, 13364 }, {  6601,  6601, 13364, 13364 }},
      {se_tri_4, nw_tri_5, { 13364, 22923,  6601, 45900 }, { 13364,  6601,  6601, 13364 }},
      {sw_tri_4, ne_tri_5, { 45900, 13364, 22923,  6601 }, { 13364, 13364,  6601,  6601 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_XEDGE, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_r2_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,    0,    0,    0 }, {    0,    0,    0,    0 }},
      {full,     empty,    { 3280, 3280, 3280, 3280 }, { 3280, 3280, 3280, 3280 }},
      {empty,    full,     { 6560, 6560, 6560, 6560 }, { 6560, 6560, 6560, 6560 }},
      {n_edge,   empty,    {    0,    1,    0, 2187 }, {    0,    1,    0,    1 }},
      {e_edge,   empty,    { 2187,    0,    1,    0 }, {    1,    0,    1,    0 }},
      {s_edge,   empty,    {    0, 2187,    0,    1 }, {    0,    1,    0,    1 }},
      {w_edge,   empty,    {    1,    0, 2187,    0 }, {    1,    0,    1,    0 }},
      {empty,    n_r2,     { 6560,    6,    0, 1458 }, { 6560,    6,    0,    6 }},
      {empty,    e_r2,     { 1458, 6560,    6,    0 }, {    6, 6560,    6,    0 }},
      {empty,    s_r2,     {    0, 1458, 6560,    6 }, {    0,    6, 6560,    6 }},
      {empty,    w_r2,     {    6,    0, 1458, 6560 }, {    6,    0,    6, 6560 }},
      {n_r3,     n_r4,     {    0,   63,    0,  405 }, {    0,   63,    0,   63 }},
      {e_r3,     e_r4,     {  405,    0,   63,    0 }, {   63,    0,   63,    0 }},
      {s_r3,     s_r4,     {    0,  405,    0,   63 }, {    0,   63,    0,   63 }},
      {w_r3,     w_r4,     {   63,    0,  405,    0 }, {   63,    0,   63,    0 }},
      {border,   empty,    { 2188, 2188, 2188, 2188 }, { 2188, 2188, 2188, 2188 }},
      {empty,    border,   { 4376, 4376, 4376, 4376 }, { 4376, 4376, 4376, 4376 }},
      {bord_1,   empty,    { 1092, 1092, 1092, 1092 }, { 1092, 1092, 1092, 1092 }},
      {empty,    bord_1,   { 2184, 2184, 2184, 2184 }, { 2184, 2184, 2184, 2184 }},
      {bord_2,   bord_3,   {    0,    0,    0,    0 }, {    0,    0,    0,    0 }},
      {border,   bord_1,   { 4372, 4372, 4372, 4372 }, { 4372, 4372, 4372, 4372 }},
      {bord_1,   border,   { 5468, 5468, 5468, 5468 }, { 5468, 5468, 5468, 5468 }},
      {nw_3x3,   ne_3x3,   { 6331,   26,    0, 3159 }, { 3185,   26,    0,   13 }},
      {ne_3x3,   se_3x3,   { 3159, 6331,   26,    0 }, {   13, 3185,   26,    0 }},
      {se_3x3,   sw_3x3,   {    0, 3159, 6331,   26 }, {    0,   13, 3185,   26 }},
      {sw_3x3,   nw_3x3,   {   26,    0, 3159, 6331 }, {   26,    0,   13, 3185 }},
      {nw_7x7,   empty,    { 1093, 1093, 3279, 3279 }, { 1093, 1093, 1093, 1093 }},
      {ne_7x7,   empty,    { 3279, 1093, 1093, 3279 }, { 1093, 1093, 1093, 1093 }},
      {se_7x7,   empty,    { 3279, 3279, 1093, 1093 }, { 1093, 1093, 1093, 1093 }},
      {sw_7x7,   empty,    { 1093, 3279, 3279, 1093 }, { 1093, 1093, 1093, 1093 }},
      {empty,    nw_7x7,   { 2186, 2186, 6558, 6558 }, { 2186, 2186, 2186, 2186 }},
      {empty,    ne_7x7,   { 6558, 2186, 2186, 6558 }, { 2186, 2186, 2186, 2186 }},
      {empty,    se_7x7,   { 6558, 6558, 2186, 2186 }, { 2186, 2186, 2186, 2186 }},
      {empty,    sw_7x7,   { 2186, 6558, 6558, 2186 }, { 2186, 2186, 2186, 2186 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_R2, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_r3_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,    0,    0,    0 }, {    0,    0,    0,    0 }},
      {full,     empty,    { 3280, 3280, 3280, 3280 }, { 3280, 3280, 3280, 3280 }},
      {empty,    full,     { 6560, 6560, 6560, 6560 }, { 6560, 6560, 6560, 6560 }},
      {n_edge,   empty,    {    0,    1,    0, 2187 }, {    0,    1,    0,    1 }},
      {e_edge,   empty,    { 2187,    0,    1,    0 }, {    1,    0,    1,    0 }},
      {s_edge,   empty,    {    0, 2187,    0,    1 }, {    0,    1,    0,    1 }},
      {w_edge,   empty,    {    1,    0, 2187,    0 }, {    1,    0,    1,    0 }},
      {empty,    n_r2,     {    0,    6,    0, 1458 }, {    0,    6,    0,    6 }},
      {empty,    e_r2,     { 1458,    0,    6,    0 }, {    6,    0,    6,    0 }},
      {empty,    s_r2,     {    0, 1458,    0,    6 }, {    0,    6,    0,    6 }},
      {empty,    w_r2,     {    6,    0, 1458,    0 }, {    6,    0,    6,    0 }},
      {n_r3,     n_r4,     { 3280,   63,    0,  405 }, { 3280,   63,    0,   63 }},
      {e_r3,     e_r4,     {  405, 3280,   63,    0 }, {   63, 3280,   63,    0 }},
      {s_r3,     s_r4,     {    0,  405, 3280,   63 }, {    0,   63, 3280,   63 }},
      {w_r3,     w_r4,     {   63,    0,  405, 3280 }, {   63,    0,   63, 3280 }},
      {border,   empty,    { 2188, 2188, 2188, 2188 }, { 2188, 2188, 2188, 2188 }},
      {empty,    border,   { 4376, 4376, 4376, 4376 }, { 4376, 4376, 4376, 4376 }},
      {bord_1,   empty,    {  732,  732,  732,  732 }, {  732,  732,  732,  732 }},
      {empty,    bord_1,   { 1464, 1464, 1464, 1464 }, { 1464, 1464, 1464, 1464 }},
      {bord_2,   bord_3,   {  360,  360,  360,  360 }, {  360,  360,  360,  360 }},
      {border,   bord_1,   { 3652, 3652, 3652, 3652 }, { 3652, 3652, 3652, 3652 }},
      {bord_1,   border,   { 5108, 5108, 5108, 5108 }, { 5108, 5108, 5108, 5108 }},
      {nw_3x3,   ne_3x3,   { 6331,   26,    0, 3159 }, { 3185,   26,    0,   13 }},
      {ne_3x3,   se_3x3,   { 3159, 6331,   26,    0 }, {   13, 3185,   26,    0 }},
      {se_3x3,   sw_3x3,   {    0, 3159, 6331,   26 }, {    0,   13, 3185,   26 }},
      {sw_3x3,   nw_3x3,   {   26,    0, 3159, 6331 }, {   26,    0,   13, 3185 }},
      {nw_7x7,   empty,    { 1093, 1093, 3279, 3279 }, { 1093, 1093, 1093, 1093 }},
      {ne_7x7,   empty,    { 3279, 1093, 1093, 3279 }, { 1093, 1093, 1093, 1093 }},
      {se_7x7,   empty,    { 3279, 3279, 1093, 1093 }, { 1093, 1093, 1093, 1093 }},
      {sw_7x7,   empty,    { 1093, 3279, 3279, 1093 }, { 1093, 1093, 1093, 1093 }},
      {empty,    nw_7x7,   { 2186, 2186, 6558, 6558 }, { 2186, 2186, 2186, 2186 }},
      {empty,    ne_7x7,   { 6558, 2186, 2186, 6558 }, { 2186, 2186, 2186, 2186 }},
      {empty,    se_7x7,   { 6558, 6558, 2186, 2186 }, { 2186, 2186, 2186, 2186 }},
      {empty,    sw_7x7,   { 2186, 6558, 6558, 2186 }, { 2186, 2186, 2186, 2186 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_R3, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_r4_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,    0,    0,    0 }, {    0,    0,    0,    0 }},
      {full,     empty,    { 3280, 3280, 3280, 3280 }, { 3280, 3280, 3280, 3280 }},
      {empty,    full,     { 6560, 6560, 6560, 6560 }, { 6560, 6560, 6560, 6560 }},
      {n_edge,   empty,    {    0,    1,    0, 2187 }, {    0,    1,    0,    1 }},
      {e_edge,   empty,    { 2187,    0,    1,    0 }, {    1,    0,    1,    0 }},
      {s_edge,   empty,    {    0, 2187,    0,    1 }, {    0,    1,    0,    1 }},
      {w_edge,   empty,    {    1,    0, 2187,    0 }, {    1,    0,    1,    0 }},
      {empty,    n_r2,     {    0,    6,    0, 1458 }, {    0,    6,    0,    6 }},
      {empty,    e_r2,     { 1458,    0,    6,    0 }, {    6,    0,    6,    0 }},
      {empty,    s_r2,     {    0, 1458,    0,    6 }, {    0,    6,    0,    6 }},
      {empty,    w_r2,     {    6,    0, 1458,    0 }, {    6,    0,    6,    0 }},
      {n_r3,     n_r4,     { 6560,   63,    0,  405 }, { 6560,   63,    0,   63 }},
      {e_r3,     e_r4,     {  405, 6560,   63,    0 }, {   63, 6560,   63,    0 }},
      {s_r3,     s_r4,     {    0,  405, 6560,   63 }, {    0,   63, 6560,   63 }},
      {w_r3,     w_r4,     {   63,    0,  405, 6560 }, {   63,    0,   63, 6560 }},
      {border,   empty,    { 2188, 2188, 2188, 2188 }, { 2188, 2188, 2188, 2188 }},
      {empty,    border,   { 4376, 4376, 4376, 4376 }, { 4376, 4376, 4376, 4376 }},
      {bord_1,   empty,    {  732,  732,  732,  732 }, {  732,  732,  732,  732 }},
      {empty,    bord_1,   { 1464, 1464, 1464, 1464 }, { 1464, 1464, 1464, 1464 }},
      {bord_2,   bord_3,   {  468,  468,  468,  468 }, {  468,  468,  468,  468 }},
      {border,   bord_1,   { 3652, 3652, 3652, 3652 }, { 3652, 3652, 3652, 3652 }},
      {bord_1,   border,   { 5108, 5108, 5108, 5108 }, { 5108, 5108, 5108, 5108 }},
      {nw_3x3,   ne_3x3,   {    0,    0,    0,    0 }, {    0,    0,    0,    0 }},
      {ne_3x3,   se_3x3,   {    0,    0,    0,    0 }, {    0,    0,    0,    0 }},
      {se_3x3,   sw_3x3,   {    0,    0,    0,    0 }, {    0,    0,    0,    0 }},
      {sw_3x3,   nw_3x3,   {    0,    0,    0,    0 }, {    0,    0,    0,    0 }},
      {nw_7x7,   empty,    { 1093, 1093, 3279, 3279 }, { 1093, 1093, 1093, 1093 }},
      {ne_7x7,   empty,    { 3279, 1093, 1093, 3279 }, { 1093, 1093, 1093, 1093 }},
      {se_7x7,   empty,    { 3279, 3279, 1093, 1093 }, { 1093, 1093, 1093, 1093 }},
      {sw_7x7,   empty,    { 1093, 3279, 3279, 1093 }, { 1093, 1093, 1093, 1093 }},
      {empty,    nw_7x7,   { 2186, 2186, 6558, 6558 }, { 2186, 2186, 2186, 2186 }},
      {empty,    ne_7x7,   { 6558, 2186, 2186, 6558 }, { 2186, 2186, 2186, 2186 }},
      {empty,    se_7x7,   { 6558, 6558, 2186, 2186 }, { 2186, 2186, 2186, 2186 }},
      {empty,    sw_7x7,   { 2186, 6558, 6558, 2186 }, { 2186, 2186, 2186, 2186 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_R4, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag4_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {  0,  0,  0,  0 }, {  0,  0,  0,  0 }},
      {full,     empty,    { 40, 40, 40, 40 }, { 40, 40, 40, 40 }},
      {empty,    full,     { 80, 80, 80, 80 }, { 80, 80, 80, 80 }},
      {n_edge,   empty,    { 27,  1,  0,  0 }, {  1,  1,  0,  0 }},
      {e_edge,   empty,    {  0, 27,  1,  0 }, {  0,  1,  1,  0 }},
      {s_edge,   empty,    {  0,  0, 27,  1 }, {  0,  0,  1,  1 }},
      {w_edge,   empty,    {  1,  0,  0, 27 }, {  1,  0,  0,  1 }},
      {empty,    n_r2,     { 18,  6,  0,  0 }, {  6,  6,  0,  0 }},
      {empty,    e_r2,     {  0, 18,  6,  0 }, {  0,  6,  6,  0 }},
      {empty,    s_r2,     {  0,  0, 18,  6 }, {  0,  0,  6,  6 }},
      {empty,    w_r2,     {  6,  0,  0, 18 }, {  6,  0,  0,  6 }},
      {n_r3,     n_r4,     {  5, 63,  0,  0 }, {  5,  5,  0,  0 }},
      {e_r3,     e_r4,     {  0,  5, 63,  0 }, {  0,  5,  5,  0 }},
      {s_r3,     s_r4,     {  0,  0,  5, 63 }, {  0,  0,  5,  5 }},
      {w_r3,     w_r4,     { 63,  0,  0,  5 }, {  5,  0,  0,  5 }},
      {border,   empty,    { 28, 28, 28, 28 }, { 28, 28, 28, 28 }},
      {empty,    border,   { 56, 56, 56, 56 }, { 56, 56, 56, 56 }},
      {bord_1,   empty,    { 12, 12, 12, 12 }, { 12, 12, 12, 12 }},
      {empty,    bord_1,   { 24, 24, 24, 24 }, { 24, 24, 24, 24 }},
      {bord_2,   bord_3,   {  0,  0,  0,  0 }, {  0,  0,  0,  0 }},
      {border,   bord_1,   { 52, 52, 52, 52 }, { 52, 52, 52, 52 }},
      {bord_1,   border,   { 68, 68, 68, 68 }, { 68, 68, 68, 68 }},
      {nw_3x3,   ne_3x3,   { 12, 24,  0,  0 }, { 12, 24,  0,  0 }},
      {ne_3x3,   se_3x3,   {  0, 12, 24,  0 }, {  0, 12, 24,  0 }},
      {se_3x3,   sw_3x3,   {  0,  0, 12, 24 }, {  0,  0, 12, 24 }},
      {sw_3x3,   nw_3x3,   { 24,  0,  0, 12 }, { 24,  0,  0, 12 }},
      {nw_7x7,   empty,    { 40, 13, 12, 39 }, { 40, 13, 12, 13 }},
      {ne_7x7,   empty,    { 39, 40, 13, 12 }, { 13, 40, 13, 12 }},
      {se_7x7,   empty,    { 12, 39, 40, 13 }, { 12, 13, 40, 13 }},
      {sw_7x7,   empty,    { 13, 12, 39, 40 }, { 13, 12, 13, 40 }},
      {empty,    nw_7x7,   { 80, 26, 24, 78 }, { 80, 26, 24, 26 }},
      {empty,    ne_7x7,   { 78, 80, 26, 24 }, { 26, 80, 26, 24 }},
      {empty,    se_7x7,   { 24, 78, 80, 26 }, { 24, 26, 80, 26 }},
      {empty,    sw_7x7,   { 26, 24, 78, 80 }, { 26, 24, 26, 80 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG4, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag5_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {   0,   0,   0,   0 }, {   0,   0,   0,   0 }},
      {full,     empty,    { 121, 121, 121, 121 }, { 121, 121, 121, 121 }},
      {empty,    full,     { 242, 242, 242, 242 }, { 242, 242, 242, 242 }},
      {n_edge,   empty,    {  81,   1,   0,   0 }, {   1,   1,   0,   0 }},
      {e_edge,   empty,    {   0,  81,   1,   0 }, {   0,   1,   1,   0 }},
      {s_edge,   empty,    {   0,   0,  81,   1 }, {   0,   0,   1,   1 }},
      {w_edge,   empty,    {   1,   0,   0,  81 }, {   1,   0,   0,   1 }},
      {empty,    n_r2,     {  54,   6,   0,   0 }, {   6,   6,   0,   0 }},
      {empty,    e_r2,     {   0,  54,   6,   0 }, {   0,   6,   6,   0 }},
      {empty,    s_r2,     {   0,   0,  54,   6 }, {   0,   0,   6,   6 }},
      {empty,    w_r2,     {   6,   0,   0,  54 }, {   6,   0,   0,   6 }},
      {n_r3,     n_r4,     {  15,  63,   2, 162 }, {  15,  15,   2,   2 }},
      {e_r3,     e_r4,     { 162,  15,  63,   2 }, {   2,  15,  15,   2 }},
      {s_r3,     s_r4,     {   2, 162,  15,  63 }, {   2,   2,  15,  15 }},
      {w_r3,     w_r4,     {  63,   2, 162,  15 }, {  15,   2,   2,  15 }},
      {border,   empty,    {  82,  82,  82,  82 }, {  82,  82,  82,  82 }},
      {empty,    border,   { 164, 164, 164, 164 }, { 164, 164, 164, 164 }},
      {bord_1,   empty,    {  30,  30,  30,  30 }, {  30,  30,  30,  30 }},
      {empty,    bord_1,   {  60,  60,  60,  60 }, {  60,  60,  60,  60 }},
      {bord_2,   bord_3,   {   9,   9,   9,   9 }, {   9,   9,   9,   9 }},
      {border,   bord_1,   { 142, 142, 142, 142 }, { 142, 142, 142, 142 }},
      {bord_1,   border,   { 194, 194, 194, 194 }, { 194, 194, 194, 194 }},
      {nw_3x3,   ne_3x3,   {   9,  18,   0,   0 }, {   9,  18,   0,   0 }},
      {ne_3x3,   se_3x3,   {   0,   9,  18,   0 }, {   0,   9,  18,   0 }},
      {se_3x3,   sw_3x3,   {   0,   0,   9,  18 }, {   0,   0,   9,  18 }},
      {sw_3x3,   nw_3x3,   {  18,   0,   0,   9 }, {  18,   0,   0,   9 }},
      {nw_7x7,   empty,    { 121,  40,  39, 120 }, { 121,  40,  39,  40 }},
      {ne_7x7,   empty,    { 120, 121,  40,  39 }, {  40, 121,  40,  39 }},
      {se_7x7,   empty,    {  39, 120, 121,  40 }, {  39,  40, 121,  40 }},
      {sw_7x7,   empty,    {  40,  39, 120, 121 }, {  40,  39,  40, 121 }},
      {empty,    nw_7x7,   { 242,  80,  78, 240 }, { 242,  80,  78,  80 }},
      {empty,    ne_7x7,   { 240, 242,  80,  78 }, {  80, 242,  80,  78 }},
      {empty,    se_7x7,   {  78, 240, 242,  80 }, {  78,  80, 242,  80 }},
      {empty,    sw_7x7,   {  80,  78, 240, 242 }, {  80,  78,  80, 242 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG5, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag6_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {   0,   0,   0,   0 }, {   0,   0,   0,   0 }},
      {full,     empty,    { 364, 364, 364, 364 }, { 364, 364, 364, 364 }},
      {empty,    full,     { 728, 728, 728, 728 }, { 728, 728, 728, 728 }},
      {n_edge,   empty,    { 243,   1,   0,   0 }, {   1,   1,   0,   0 }},
      {e_edge,   empty,    {   0, 243,   1,   0 }, {   0,   1,   1,   0 }},
      {s_edge,   empty,    {   0,   0, 243,   1 }, {   0,   0,   1,   1 }},
      {w_edge,   empty,    {   1,   0,   0, 243 }, {   1,   0,   0,   1 }},
      {empty,    n_r2,     { 162,   6,   0,   0 }, {   6,   6,   0,   0 }},
      {empty,    e_r2,     {   0, 162,   6,   0 }, {   0,   6,   6,   0 }},
      {empty,    s_r2,     {   0,   0, 162,   6 }, {   0,   0,   6,   6 }},
      {empty,    w_r2,     {   6,   0,   0, 162 }, {   6,   0,   0,   6 }},
      {n_r3,     n_r4,     {  45,  63,   7, 405 }, {  45,  45,   7,   7 }},
      {e_r3,     e_r4,     { 405,  45,  63,   7 }, {   7,  45,  45,   7 }},
      {s_r3,     s_r4,     {   7, 405,  45,  63 }, {   7,   7,  45,  45 }},
      {w_r3,     w_r4,     {  63,   7, 405,  45 }, {  45,   7,   7,  45 }},
      {border,   empty,    { 244, 244, 244, 244 }, { 244, 244, 244, 244 }},
      {empty,    border,   { 488, 488, 488, 488 }, { 488, 488, 488, 488 }},
      {bord_1,   empty,    {  84,  84,  84,  84 }, {  84,  84,  84,  84 }},
      {empty,    bord_1,   { 168, 168, 168, 168 }, { 168, 168, 168, 168 }},
      {bord_2,   bord_3,   {  36,  36,  36,  36 }, {  36,  36,  36,  36 }},
      {border,   bord_1,   { 412, 412, 412, 412 }, { 412, 412, 412, 412 }},
      {bord_1,   border,   { 572, 572, 572, 572 }, { 572, 572, 572, 572 }},
      {nw_3x3,   ne_3x3,   { 486,   1,   2, 243 }, {   2,   1,   2,   1 }},
      {ne_3x3,   se_3x3,   { 243, 486,   1,   2 }, {   1,   2,   1,   2 }},
      {se_3x3,   sw_3x3,   {   2, 243, 486,   1 }, {   2,   1,   2,   1 }},
      {sw_3x3,   nw_3x3,   {   1,   2, 243, 486 }, {   1,   2,   1,   2 }},
      {nw_7x7,   empty,    { 364, 121, 120, 363 }, { 364, 121, 120, 121 }},
      {ne_7x7,   empty,    { 363, 364, 121, 120 }, { 121, 364, 121, 120 }},
      {se_7x7,   empty,    { 120, 363, 364, 121 }, { 120, 121, 364, 121 }},
      {sw_7x7,   empty,    { 121, 120, 363, 364 }, { 121, 120, 121, 364 }},
      {empty,    nw_7x7,   { 728, 242, 240, 726 }, { 728, 242, 240, 242 }},
      {empty,    ne_7x7,   { 726, 728, 242, 240 }, { 242, 728, 242, 240 }},
      {empty,    se_7x7,   { 240, 726, 728, 242 }, { 240, 242, 728, 242 }},
      {empty,    sw_7x7,   { 242, 240, 726, 728 }, { 242, 240, 242, 728 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG6, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag7_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,    0,    0,    0 }, {    0,    0,    0,    0 }},
      {full,     empty,    { 1093, 1093, 1093, 1093 }, { 1093, 1093, 1093, 1093 }},
      {empty,    full,     { 2186, 2186, 2186, 2186 }, { 2186, 2186, 2186, 2186 }},
      {n_edge,   empty,    {  729,    1,    0,    0 }, {    1,    1,    0,    0 }},
      {e_edge,   empty,    {    0,  729,    1,    0 }, {    0,    1,    1,    0 }},
      {s_edge,   empty,    {    0,    0,  729,    1 }, {    0,    0,    1,    1 }},
      {w_edge,   empty,    {    1,    0,    0,  729 }, {    1,    0,    0,    1 }},
      {empty,    n_r2,     {  486,    6,    2, 1458 }, {    6,    6,    2,    2 }},
      {empty,    e_r2,     { 1458,  486,    6,    2 }, {    2,    6,    6,    2 }},
      {empty,    s_r2,     {    2, 1458,  486,    6 }, {    2,    2,    6,    6 }},
      {empty,    w_r2,     {    6,    2, 1458,  486 }, {    6,    2,    2,    6 }},
      {n_r3,     n_r4,     {  135,   63,   21,  405 }, {   63,   63,   21,   21 }},
      {e_r3,     e_r4,     {  405,  135,   63,   21 }, {   21,   63,   63,   21 }},
      {s_r3,     s_r4,     {   21,  405,  135,   63 }, {   21,   21,   63,   63 }},
      {w_r3,     w_r4,     {   63,   21,  405,  135 }, {   63,   21,   21,   63 }},
      {border,   empty,    {  730,  730,  730,  730 }, {  730,  730,  730,  730 }},
      {empty,    border,   { 1460, 1460, 1460, 1460 }, { 1460, 1460, 1460, 1460 }},
      {bord_1,   empty,    {  246,  246,  246,  246 }, {  246,  246,  246,  246 }},
      {empty,    bord_1,   {  492,  492,  492,  492 }, {  492,  492,  492,  492 }},
      {bord_2,   bord_3,   {  144,  144,  144,  144 }, {  144,  144,  144,  144 }},
      {border,   bord_1,   { 1222, 1222, 1222, 1222 }, { 1222, 1222, 1222, 1222 }},
      {bord_1,   border,   { 1706, 1706, 1706, 1706 }, { 1706, 1706, 1706, 1706 }},
      {nw_3x3,   ne_3x3,   { 1944,    4,    8,  972 }, {    8,    4,    8,    4 }},
      {ne_3x3,   se_3x3,   {  972, 1944,    4,    8 }, {    4,    8,    4,    8 }},
      {se_3x3,   sw_3x3,   {    8,  972, 1944,    4 }, {    8,    4,    8,    4 }},
      {sw_3x3,   nw_3x3,   {    4,    8,  972, 1944 }, {    4,    8,    4,    8 }},
      {nw_7x7,   empty,    { 1093,  364,  363, 1092 }, { 1093,  364,  363,  364 }},
      {ne_7x7,   empty,    { 1092, 1093,  364,  363 }, {  364, 1093,  364,  363 }},
      {se_7x7,   empty,    {  363, 1092, 1093,  364 }, {  363,  364, 1093,  364 }},
      {sw_7x7,   empty,    {  364,  363, 1092, 1093 }, {  364,  363,  364, 1093 }},
      {empty,    nw_7x7,   { 2186,  728,  726, 2184 }, { 2186,  728,  726,  728 }},
      {empty,    ne_7x7,   { 2184, 2186,  728,  726 }, {  728, 2186,  728,  726 }},
      {empty,    se_7x7,   {  726, 2184, 2186,  728 }, {  726,  728, 2186,  728 }},
      {empty,    sw_7x7,   {  728,  726, 2184, 2186 }, {  728,  726,  728, 2186 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG7, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag8_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {    0,    0 }, {    0,    0 }},
      {full,     empty,    { 3280, 3280 }, { 3280, 3280 }},
      {empty,    full,     { 6560, 6560 }, { 6560, 6560 }},
      {n_edge,   empty,    { 2187,    1 }, {    1,    1 }},
      {e_edge,   empty,    { 2187, 2187 }, {    1,    1 }},
      {s_edge,   empty,    {    1, 2187 }, {    1,    1 }},
      {w_edge,   empty,    {    1,    1 }, {    1,    1 }},
      {empty,    n_r2,     { 1458,    6 }, {    6,    6 }},
      {empty,    e_r2,     { 1458, 1458 }, {    6,    6 }},
      {empty,    s_r2,     {    6, 1458 }, {    6,    6 }},
      {empty,    w_r2,     {    6,    6 }, {    6,    6 }},
      {n_r3,     n_r4,     {  405,   63 }, {   63,   63 }},
      {e_r3,     e_r4,     {  405,  405 }, {   63,   63 }},
      {s_r3,     s_r4,     {   63,  405 }, {   63,   63 }},
      {w_r3,     w_r4,     {   63,   63 }, {   63,   63 }},
      {border,   empty,    { 2188, 2188 }, { 2188, 2188 }},
      {empty,    border,   { 4376, 4376 }, { 4376, 4376 }},
      {bord_1,   empty,    {  732,  732 }, {  732,  732 }},
      {empty,    bord_1,   { 1464, 1464 }, { 1464, 1464 }},
      {bord_2,   bord_3,   {  468,  468 }, {  468,  468 }},
      {border,   bord_1,   { 3652, 3652 }, { 3652, 3652 }},
      {bord_1,   border,   { 5108, 5108 }, { 5108, 5108 }},
      {nw_3x3,   ne_3x3,   { 6318,   13 }, {   26,   13 }},
      {ne_3x3,   se_3x3,   { 3159, 6318 }, {   13,   26 }},
      {se_3x3,   sw_3x3,   {   26, 3159 }, {   26,   13 }},
      {sw_3x3,   nw_3x3,   {   13,   26 }, {   13,   26 }},
      {nw_7x7,   empty,    { 1092, 1093 }, { 1092, 1093 }},
      {ne_7x7,   empty,    { 3279, 1092 }, { 1093, 1092 }},
      {se_7x7,   empty,    { 1092, 3279 }, { 1092, 1093 }},
      {sw_7x7,   empty,    { 1093, 1092 }, { 1093, 1092 }},
      {empty,    nw_7x7,   { 2184, 2186 }, { 2184, 2186 }},
      {empty,    ne_7x7,   { 6558, 2184 }, { 2186, 2184 }},
      {empty,    se_7x7,   { 2184, 6558 }, { 2184, 2186 }},
      {empty,    sw_7x7,   { 2186, 2184 }, { 2186, 2184 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG8, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_2x5cor_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {     0,     0,     0,     0,    0,      0,     0,     0 }, {     0,     0,     0,     0,    0,      0,     0,     0 }},
      {full,     empty,    { 29524, 29524, 29524, 29524, 29524, 29524, 29524, 29524 }, { 29524, 29524, 29524, 29524, 29524, 29524, 29524, 29524 }},
      {empty,    full,     { 59048, 59048, 59048, 59048, 59048, 59048, 59048, 59048 }, { 59048, 59048, 59048, 59048, 59048, 59048, 59048, 59048 }},
      {n_edge,   empty,    {   121,   244,     0,     0,   121,     0,     0,   244 }, {   121,   244,     0,     0,   121,     0,     0,   244 }},
      {e_edge,   empty,    {     0,   121,   244,     0,   244,   121,     0,     0 }, {     0,   121,   244,     0,   244,   121,     0,     0 }},
      {s_edge,   empty,    {     0,     0,   121,   244,     0,   244,   121,     0 }, {     0,     0,   121,   244,     0,   244,   121,     0 }},
      {w_edge,   empty,    {   244,     0,     0,   121,     0,     0,   244,   121 }, {   244,     0,     0,   121,     0,     0,   244,   121 }},
      {empty,    n_r2,     { 58806,  1464,     0,     0, 58806,     0,     0,  1464 }, { 58806,  1464,     0,     0, 58806,     0,     0,  1464 }},
      {empty,    e_r2,     {     0, 58806,  1464,     0,  1464, 58806,     0,     0 }, {     0, 58806,  1464,     0,  1464, 58806,     0,     0 }},
      {empty,    s_r2,     {     0,     0, 58806,  1464,     0,  1464, 58806,     0 }, {     0,     0, 58806,  1464,     0,  1464, 58806,     0 }},
      {empty,    w_r2,     {  1464,     0,     0, 58806,     0,     0,  1464, 58806 }, {  1464,     0,     0, 58806,     0,     0,  1464, 58806 }},
      {n_r3,     n_r4,     {     0, 15372,     0, 39528,     0, 39528,     0, 15372 }, {     0, 15372,     0, 39528,     0, 39528,     0, 15372 }},
      {e_r3,     e_r4,     { 39528,     0, 15372,     0, 15372,     0, 39528,     0 }, { 39528,     0, 15372,     0, 15372,     0, 39528,     0 }},
      {s_r3,     s_r4,     {     0, 39528,     0, 15372,     0, 15372,     0, 39528 }, {     0, 39528,     0, 15372,     0, 15372,     0, 39528 }},
      {w_r3,     w_r4,     { 15372,     0, 39528,     0, 39528,     0, 15372,     0 }, { 15372,     0, 39528,     0, 39528,     0, 15372,     0 }},
      {border,   empty,    {   364,   364,   364,   364,   364,   364,   364,   364 }, {   364,   364,   364,   364,   364,   364,   364,   364 }},
      {empty,    border,   {   728,   728,   728,   728,   728,   728,   728,   728 }, {   728,   728,   728,   728,   728,   728,   728,   728 }},
      {bord_1,   empty,    { 29160, 29160, 29160, 29160, 29160, 29160, 29160, 29160 }, { 29160, 29160, 29160, 29160, 29160, 29160, 29160, 29160 }},
      {empty,    bord_1,   { 58320, 58320, 58320, 58320, 58320, 58320, 58320, 58320 }, { 58320, 58320, 58320, 58320, 58320, 58320, 58320, 58320 }},
      {bord_2,   bord_3,   {     0,     0,     0,     0,     0,     0,     0,     0 }, {     0,     0,     0,     0,     0,     0,     0,     0 }},
      {border,   bord_1,   { 58684, 58684, 58684, 58684, 58684, 58684, 58684, 58684 }, { 58684, 58684, 58684, 58684, 58684, 58684, 58684, 58684 }},
      {bord_1,   border,   { 29888, 29888, 29888, 29888, 29888, 29888, 29888, 29888 }, { 29888, 29888, 29888, 29888, 29888, 29888, 29888, 29888 }},
      {nw_3x3,   ne_3x3,   {  3172,  6344,     0,     0,  6344,     0,     0,  3172 }, {  3172,  6344,     0,     0,  6344,     0,     0,  3172 }},
      {ne_3x3,   se_3x3,   {     0,  3172,  6344,     0,  3172,  6344,     0,     0 }, {     0,  3172,  6344,     0,  3172,  6344,     0,     0 }},
      {se_3x3,   sw_3x3,   {     0,     0,  3172,  6344,     0,  3172,  6344,     0 }, {     0,     0,  3172,  6344,     0,  3172,  6344,     0 }},
      {sw_3x3,   nw_3x3,   {  6344,     0,     0,  3172,     0,     0,  3172,  6344 }, {  6344,     0,     0,  3172,     0,     0,  3172,  6344 }},
      {nw_7x7,   empty,    { 29524, 29403, 29160, 29280, 29280, 29160, 29403, 29524 }, { 29524, 29403, 29160, 29280, 29280, 29160, 29403, 29524 }},
      {ne_7x7,   empty,    { 29280, 29524, 29403, 29160, 29524, 29280, 29160, 29403 }, { 29280, 29524, 29403, 29160, 29524, 29280, 29160, 29403 }},
      {se_7x7,   empty,    { 29160, 29280, 29524, 29403, 29403, 29524, 29280, 29160 }, { 29160, 29280, 29524, 29403, 29403, 29524, 29280, 29160 }},
      {sw_7x7,   empty,    { 29403, 29160, 29280, 29524, 29160, 29403, 29524, 29280 }, { 29403, 29160, 29280, 29524, 29160, 29403, 29524, 29280 }},
      {empty,    nw_7x7,   { 59048, 58806, 58320, 58560, 58560, 58320, 58806, 59048 }, { 59048, 58806, 58320, 58560, 58560, 58320, 58806, 59048 }},
      {empty,    ne_7x7,   { 58560, 59048, 58806, 58320, 59048, 58560, 58320, 58806 }, { 58560, 59048, 58806, 58320, 59048, 58560, 58320, 58806 }},
      {empty,    se_7x7,   { 58320, 58560, 59048, 58806, 58806, 59048, 58560, 58320 }, { 58320, 58560, 59048, 58806, 58806, 59048, 58560, 58320 }},
      {empty,    sw_7x7,   { 58806, 58320, 58560, 59048, 58320, 58806, 59048, 58560 }, { 58806, 58320, 58560, 59048, 58320, 58806, 59048, 58560 }},
      {nw_tri_2, ne_tri_3, {   247,  1970,     0,     0,  1970,     0,     0,   247 }, {   247,  1970,     0,     0,  1970,     0,     0,   247 }},
      {ne_tri_2, se_tri_3, {     0,   247,  1970,     0,   247,  1970,     0,     0 }, {     0,   247,  1970,     0,   247,  1970,     0,     0 }},
      {se_tri_2, sw_tri_3, {     0,     0,   247,  1970,     0,   247,  1970,     0 }, {     0,     0,   247,  1970,     0,   247,  1970,     0 }},
      {sw_tri_2, nw_tri_3, {  1970,     0,     0,   247,     0,     0,   247,  1970 }, {  1970,     0,     0,   247,     0,     0,   247,  1970 }},
      {nw_tri_4, se_tri_5, {  3199, 39582, 19682,    81,    81, 19682, 39582,  3199 }, {  3199, 39582, 19682,    81,    81, 19682, 39582,  3199 }},
      {ne_tri_4, sw_tri_5, {    81,  3199, 39582, 19682,  3199,    81, 19682, 39582 }, {    81,  3199, 39582, 19682,  3199,    81, 19682, 39582 }},
      {se_tri_4, nw_tri_5, { 19682,    81,  3199, 39582, 39582,  3199,    81, 19682 }, { 19682,    81,  3199, 39582, 39582,  3199,    81, 19682 }},
      {sw_tri_4, ne_tri_5, { 39582, 19682,    81,  3199, 19682, 39582,  3199,    81 }, { 39582, 19682,    81,  3199, 19682, 39582,  3199,    81 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_2X5COR, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_diag3_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {  0,  0,  0,  0 }, {  0,  0,  0,  0 }},
      {full,     empty,    { 13, 13, 13, 13 }, { 13, 13, 13, 13 }},
      {empty,    full,     { 26, 26, 26, 26 }, { 26, 26, 26, 26 }},
      {n_edge,   empty,    {  9,  1,  0,  0 }, {  1,  1,  0,  0 }},
      {e_edge,   empty,    {  0,  9,  1,  0 }, {  0,  1,  1,  0 }},
      {s_edge,   empty,    {  0,  0,  9,  1 }, {  0,  0,  1,  1 }},
      {w_edge,   empty,    {  1,  0,  0,  9 }, {  1,  0,  0,  1 }},
      {empty,    n_r2,     {  6,  6,  0,  0 }, {  6,  6,  0,  0 }},
      {empty,    e_r2,     {  0,  6,  6,  0 }, {  0,  6,  6,  0 }},
      {empty,    s_r2,     {  0,  0,  6,  6 }, {  0,  0,  6,  6 }},
      {empty,    w_r2,     {  6,  0,  0,  6 }, {  6,  0,  0,  6 }},
      {n_r3,     n_r4,     {  1,  9,  0,  0 }, {  1,  1,  0,  0 }},
      {e_r3,     e_r4,     {  0,  1,  9,  0 }, {  0,  1,  1,  0 }},
      {s_r3,     s_r4,     {  0,  0,  1,  9 }, {  0,  0,  1,  1 }},
      {w_r3,     w_r4,     {  9,  0,  0,  1 }, {  1,  0,  0,  1 }},
      {border,   empty,    { 10, 10, 10, 10 }, { 10, 10, 10, 10 }},
      {empty,    border,   { 20, 20, 20, 20 }, { 20, 20, 20, 20 }},
      {bord_1,   empty,    {  3,  3,  3,  3 }, {  3,  3,  3,  3 }},
      {empty,    bord_1,   {  6,  6,  6,  6 }, {  6,  6,  6,  6 }},
      {bord_2,   bord_3,   {  0,  0,  0,  0 }, {  0,  0,  0,  0 }},
      {border,   bord_1,   { 16, 16, 16, 16 }, { 16, 16, 16, 16 }},
      {bord_1,   border,   { 23, 23, 23, 23 }, { 23, 23, 23, 23 }},
      {nw_3x3,   ne_3x3,   { 13, 26,  0,  0 }, { 13, 26,  0,  0 }},
      {ne_3x3,   se_3x3,   {  0, 13, 26,  0 }, {  0, 13, 26,  0 }},
      {se_3x3,   sw_3x3,   {  0,  0, 13, 26 }, {  0,  0, 13, 26 }},
      {sw_3x3,   nw_3x3,   { 26,  0,  0, 13 }, { 26,  0,  0, 13 }},
      {nw_7x7,   empty,    { 13,  4,  3, 12 }, { 13,  4,  3,  4 }},
      {ne_7x7,   empty,    { 12, 13,  4,  3 }, {  4, 13,  4,  3 }},
      {se_7x7,   empty,    {  3, 12, 13,  4 }, {  3,  4, 13,  4 }},
      {sw_7x7,   empty,    {  4,  3, 12, 13 }, {  4,  3,  4, 13 }},
      {empty,    nw_7x7,   { 26,  8,  6, 24 }, { 26,  8,  6,  8 }},
      {empty,    ne_7x7,   { 24, 26,  8,  6 }, {  8, 26,  8,  6 }},
      {empty,    se_7x7,   {  6, 24, 26,  8 }, {  6,  8, 26,  8 }},
      {empty,    sw_7x7,   {  8,  6, 24, 26 }, {  8,  6,  8, 26 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_DIAG3, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
}

static void
board_pattern_compute_indexes_2x6cor_t (ut_test_t *const t)
{
  struct board_pattern_test_s test_data[] =
    {
      {empty,    empty,    {      0,      0,      0,      0,      0,      0,      0,      0 }, {      0,      0,      0,      0,      0,      0,      0,      0 }},
      {full,     empty,    { 265720, 265720, 265720, 265720, 265720, 265720, 265720, 265720 }, { 265720, 265720, 265720, 265720, 265720, 265720, 265720, 265720 }},
      {empty,    full,     { 531440, 531440, 531440, 531440, 531440, 531440, 531440, 531440 }, { 531440, 531440, 531440, 531440, 531440, 531440, 531440, 531440 }},
      {n_edge,   empty,    {    364,    730,      0,      0,    364,      0,      0,    730 }, {    364,    730,      0,      0,    364,      0,      0,    730 }},
      {e_edge,   empty,    {      0,    364,    730,      0,    730,    364,      0,      0 }, {      0,    364,    730,      0,    730,    364,      0,      0 }},
      {s_edge,   empty,    {      0,      0,    364,    730,      0,    730,    364,      0 }, {      0,      0,    364,    730,      0,    730,    364,      0 }},
      {w_edge,   empty,    {    730,      0,      0,    364,      0,      0,    730,    364 }, {    730,      0,      0,    364,      0,      0,    730,    364 }},
      {empty,    n_r2,     { 530712,   4380,      0,      0, 530712,      0,      0,   4380 }, { 530712,   4380,      0,      0, 530712,      0,      0,   4380 }},
      {empty,    e_r2,     {      0, 530712,   4380,      0,   4380, 530712,      0,      0 }, {      0, 530712,   4380,      0,   4380, 530712,      0,      0 }},
      {empty,    s_r2,     {      0,      0, 530712,   4380,      0,   4380, 530712,      0 }, {      0,      0, 530712,   4380,      0,   4380, 530712,      0 }},
      {empty,    w_r2,     {   4380,      0,      0, 530712,      0,      0,   4380, 530712 }, {   4380,      0,      0, 530712,      0,      0,   4380, 530712 }},
      {n_r3,     n_r4,     {      0,  45990,      0, 295650,      0, 295650,      0,  45990 }, {      0,  45990,      0, 295650,      0, 295650,      0,  45990 }},
      {e_r3,     e_r4,     { 295650,      0,  45990,      0,  45990,      0, 295650,      0 }, { 295650,      0,  45990,      0,  45990,      0, 295650,      0 }},
      {s_r3,     s_r4,     {      0, 295650,      0,  45990,      0,  45990,      0, 295650 }, {      0, 295650,      0,  45990,      0,  45990,      0, 295650 }},
      {w_r3,     w_r4,     {  45990,      0, 295650,      0, 295650,      0,  45990,      0 }, {  45990,      0, 295650,      0, 295650,      0,  45990,      0 }},
      {border,   empty,    {   1093,   1093,   1093,   1093,   1093,   1093,   1093,   1093 }, {   1093,   1093,   1093,   1093,   1093,   1093,   1093,   1093 }},
      {empty,    border,   {   2186,   2186,   2186,   2186,   2186,   2186,   2186,   2186 }, {   2186,   2186,   2186,   2186,   2186,   2186,   2186,   2186 }},
      {bord_1,   empty,    { 264627, 264627, 264627, 264627, 264627, 264627, 264627, 264627 }, { 264627, 264627, 264627, 264627, 264627, 264627, 264627, 264627 }},
      {empty,    bord_1,   { 529254, 529254, 529254, 529254, 529254, 529254, 529254, 529254 }, { 529254, 529254, 529254, 529254, 529254, 529254, 529254, 529254 }},
      {bord_2,   bord_3,   {      0,      0,      0,      0,      0,      0,      0,      0 }, {      0,      0,      0,      0,      0,      0,      0,      0 }},
      {border,   bord_1,   { 530347, 530347, 530347, 530347, 530347, 530347, 530347, 530347 }, { 530347, 530347, 530347, 530347, 530347, 530347, 530347, 530347 }},
      {bord_1,   border,   { 266813, 266813, 266813, 266813, 266813, 266813, 266813, 266813 }, { 266813, 266813, 266813, 266813, 266813, 266813, 266813, 266813 }},
      {nw_3x3,   ne_3x3,   { 364270,  18980,      0, 177390, 196370, 354780,      0,   9490 }, { 364270,  18980,      0, 177390, 196370, 354780,      0,   9490 }},
      {ne_3x3,   se_3x3,   { 177390, 364270,  18980,      0,   9490, 196370, 354780,      0 }, { 177390, 364270,  18980,      0,   9490, 196370, 354780,      0 }},
      {se_3x3,   sw_3x3,   {      0, 177390, 364270,  18980,      0,   9490, 196370, 354780 }, {      0, 177390, 364270,  18980,      0,   9490, 196370, 354780 }},
      {sw_3x3,   nw_3x3,   {  18980,      0, 177390, 364270, 354780,      0,   9490, 196370 }, {  18980,      0, 177390, 364270, 354780,      0,   9490, 196370 }},
      {nw_7x7,   empty,    { 265720, 265356, 264627, 264990, 264990, 264627, 265356, 265720 }, { 265720, 265356, 264627, 264990, 264990, 264627, 265356, 265720 }},
      {ne_7x7,   empty,    { 264990, 265720, 265356, 264627, 265720, 264990, 264627, 265356 }, { 264990, 265720, 265356, 264627, 265720, 264990, 264627, 265356 }},
      {se_7x7,   empty,    { 264627, 264990, 265720, 265356, 265356, 265720, 264990, 264627 }, { 264627, 264990, 265720, 265356, 265356, 265720, 264990, 264627 }},
      {sw_7x7,   empty,    { 265356, 264627, 264990, 265720, 264627, 265356, 265720, 264990 }, { 265356, 264627, 264990, 265720, 264627, 265356, 265720, 264990 }},
      {empty,    nw_7x7,   { 531440, 530712, 529254, 529980, 529980, 529254, 530712, 531440 }, { 531440, 530712, 529254, 529980, 529980, 529254, 530712, 531440 }},
      {empty,    ne_7x7,   { 529980, 531440, 530712, 529254, 531440, 529980, 529254, 530712 }, { 529980, 531440, 530712, 529254, 531440, 529980, 529254, 530712 }},
      {empty,    se_7x7,   { 529254, 529980, 531440, 530712, 530712, 531440, 529980, 529254 }, { 529254, 529980, 531440, 530712, 530712, 531440, 529980, 529254 }},
      {empty,    sw_7x7,   { 530712, 529254, 529980, 531440, 529254, 530712, 531440, 529980 }, { 530712, 529254, 529980, 531440, 529254, 530712, 531440, 529980 }},
      {nw_tri_2, ne_tri_3, {   1219,   5858,      0,      0,   5858,    486,      0,    733 }, {   1219,   5858,      0,      0,   5858,    486,      0,    733 }},
      {ne_tri_2, se_tri_3, {      0,   1219,   5858,      0,    733,   5858,    486,      0 }, {      0,   1219,   5858,      0,    733,   5858,    486,      0 }},
      {se_tri_2, sw_tri_3, {      0,      0,   1219,   5858,      0,    733,   5858,    486 }, {      0,      0,   1219,   5858,      0,    733,   5858,    486 }},
      {sw_tri_2, nw_tri_3, {   5858,      0,      0,   1219,    486,      0,    733,   5858 }, {   5858,      0,      0,   1219,    486,      0,    733,   5858 }},
      {nw_tri_4, se_tri_5, {   9517, 473094,  58562, 177471, 177471,  58562, 473094,   9517 }, {   9517, 473094,  58562, 177471, 177471,  58562, 473094,   9517 }},
      {ne_tri_4, sw_tri_5, { 177471,   9517, 473094,  58562,   9517, 177471,  58562, 473094 }, { 177471,   9517, 473094,  58562,   9517,  177471, 58562, 473094 }},
      {se_tri_4, nw_tri_5, {  58562, 177471,   9517, 473094, 473094,   9517, 177471,  58562 }, {  58562, 177471,   9517, 473094, 473094,   9517, 177471,  58562 }},
      {sw_tri_4, ne_tri_5, { 473094,  58562, 177471,   9517,  58562, 473094,   9517, 177471 }, { 473094,  58562, 177471,   9517,  58562, 473094,   9517, 177471 }},
    };

  aux_check_expected_indexes_array(t, BOARD_PATTERN_2X6COR, test_data, sizeof(test_data) / sizeof(struct board_pattern_test_s));
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

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_structure_sizes_t", board_pattern_structure_sizes_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_patterns", board_patterns_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_get_id_by_name", board_pattern_get_id_by_name_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_edge", board_pattern_pack_edge_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_edge", board_pattern_unpack_edge_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_corner", board_pattern_pack_corner_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_corner", board_pattern_unpack_corner_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_xedge", board_pattern_pack_xedge_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_xedge", board_pattern_unpack_xedge_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_r2", board_pattern_pack_r2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_r2", board_pattern_unpack_r2_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_r3", board_pattern_pack_r3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_r3", board_pattern_unpack_r3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_r4", board_pattern_pack_r4_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_r4", board_pattern_unpack_r4_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_diag4", board_pattern_pack_diag4_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_diag4", board_pattern_unpack_diag4_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_diag5", board_pattern_pack_diag5_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_diag5", board_pattern_unpack_diag5_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_diag6", board_pattern_pack_diag6_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_diag6", board_pattern_unpack_diag6_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_diag7", board_pattern_pack_diag7_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_diag7", board_pattern_unpack_diag7_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_diag8", board_pattern_pack_diag8_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_diag8", board_pattern_unpack_diag8_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_2x5cor", board_pattern_pack_2x5cor_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_2x5cor", board_pattern_unpack_2x5cor_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_diag3", board_pattern_pack_diag3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_diag3", board_pattern_unpack_diag3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_pack_2x6cor", board_pattern_pack_2x6cor_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_unpack_2x6cor", board_pattern_unpack_2x6cor_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_packed_to_index", board_pattern_packed_to_index_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_index_to_packed", board_pattern_index_to_packed_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_packed_to_index_vec", board_pattern_packed_to_index_vec_t);

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
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_diag3", board_pattern_compute_indexes_diag3_t);
  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_indexes_2x6cor", board_pattern_compute_indexes_2x6cor_t);

  ut_suite_add_simple_test(s, UT_MODE_STND, UT_QUICKNESS_0001, "board_pattern_compute_principal_indexes_one_value", board_pattern_compute_principal_indexes_one_value_t);

  int failure_count = ut_suite_run(s);
  ut_suite_free(s);
  return failure_count;
}
