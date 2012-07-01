/*
 *  Line.java
 *
 *  Copyright (c) 2012 Roberto Corradini. All rights reserved.
 *
 *  This file is part of the reversi program
 *  http://github.com/rcrr/reversi
 *
 *  This program is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License as published by the
 *  Free Software Foundation; either version 3, or (at your option) any
 *  later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 *  or visit the site <http://www.gnu.org/licenses/>.
 */

package rcrr.reversi.board;

import java.util.ArrayList;
import java.util.List;

/**
 * The {@code Line} enum defines a line of the board game.
 */
public enum Line {
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
    A6_C8,
    A5_D8,
    A4_E8,
    A3_F8,
    A2_G8,
    A1_H8,
    B1_H7,
    C1_H6,
    D1_H5,
    E1_H4,
    F1_H3,
    C1_A3,
    D1_A4,
    E1_A5,
    F1_A6,
    G1_A7,
    H1_A8,
    H2_B8,
    H3_C8,
    H4_D8,
    H5_E8,
    H6_F8;

    /** The null instance. */
    public static final Line NULL = null;

    /** The number of lines. */
    private static final int NUMBER_OF = values().length;

}
