/*
 *  DiagonalLR.java
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
 * The {@code DiagonalLR} enum defines a diagonal left-up to right-down of the board game.
 */
public enum DiagonalLR implements File {

    /**
     * Diagonal from A6 to C8.
     */
    A6_C8("A6-C8"),

    /**
     * Diagonal from A5 to D8.
     */
     A5_D8("A5-D8"),

    /**
     * Diagonal from A4 to E8.
     */
     A4_E8("A4-E8"),

    /**
     * Diagonal from A3 to F8.
     */
     A3_F8("A3-F8"),

    /**
     * Diagonal from A2 to G8.
     */
     A2_G8("A2-G8"),

    /**
     * Diagonal from A1 to H8.
     */
     A1_H8("A1-H8"),

    /**
     * Diagonal from B1 to H7.
     */
     B1_H7("B1-H7"),

    /**
     * Diagonal from C1 to H6.
     */
     C1_H6("C1-H6"),

    /**
     * Diagonal from D1 to H5.
     */
     D1_H5("D1-H5"),

    /**
     * Diagonal from E1 to H4.
     */
     E1_H4("E1-H4"),

    /**
     * Diagonal from F1 to H3.
     */
     F1_H3("F1-H3");

    /** The null instance. */
    public static final DiagonalLR NULL = null;

    /** The number of rows. */
    private static final int NUMBER_OF = values().length;

    /** The row label. */
    private final String label;

    /**
     * Enum constructor.
     *
     * @param label the tow's label
     */
    private DiagonalLR(final String label) {
        this.label = label;
    }

    /**
     * Returns a {@code String} that represents the diagonal's label.
     *
     * @return the diagonal's label
     */
    public String label() { return label; }

}
