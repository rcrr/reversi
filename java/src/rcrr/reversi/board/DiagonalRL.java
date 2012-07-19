/*
 *  DiagonalRL.java
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
 * The {@code DiagonalRL} enum defines a diagonal right to left of the board game.
 */
public enum DiagonalRL implements File {

    /**
     * Diagonal from C1 to A3.
     */
    C1_A3("C1-A3"),

    /**
     * Diagonal from D1 to A4.
     */
     D1_A4("D1-A4"),

    /**
     * Diagonal from E1 to A5.
     */
     E1_A5("E1-A5"),

    /**
     * Diagonal from F1 to A6.
     */
     F1_A6("F1-A6"),

    /**
     * Diagonal from G1 to A7.
     */
     G1_A7("G1-A7"),

    /**
     * Diagonal from H1 to A8.
     */
     H1_A8("H1-A8"),

    /**
     * Diagonal from H2 to B8.
     */
    H2_B8("H2-B8"),

    /**
     * Diagonal from H3 to C8.
     */
     H3_C8("H3-C8"),

    /**
     * Diagonal from H4 to D8.
     */
     H4_D8("H4-D8"),

    /**
     * Diagonal from H5 to E8.
     */
     H5_E8("H5-E8"),

    /**
     * Diagonal from H6 to F8.
     */
     H6_F8("H6-F8");

    /** The null instance. */
    public static final DiagonalRL NULL = null;

    /** The number of diagonals. */
    private static final int NUMBER_OF = values().length;

    /** The diagonal label. */
    private final String label;

    /**
     * Enum constructor.
     *
     * @param label the diagonal's label
     */
    private DiagonalRL(final String label) {
        this.label = label;
    }

    /**
     * Returns a {@code String} that represents the diagonal's label.
     *
     * @return the diagonal's label
     */
    public String label() { return label; }

    /**
     * {@inheritDoc}
     */
    @Override
    public Axis axis() {
        return Axis.DIAGONAL_RL;
    }

}
