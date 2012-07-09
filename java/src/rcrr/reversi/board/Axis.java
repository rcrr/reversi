/*
 *  Axis.java
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

/**
 * The axes are the lines that pass throw a square. A general
 * square has four axes.
 */
public enum Axis {

    /**
     * Horizontal axis (W-E).
     */
    HORIZONTAL(Row.class),

    /**
     * Vertical axis (N-S).
     */
    VERTICAL(Column.class),

    /**
     * Diagonal left to right axis (NW-SE).
     */
    DIAGONAL_LR(DiagonalLR.class),

    /**
     * Diagonal right to left axis (NE-SW).
     */
    DIAGONAL_RL(DiagonalRL.class);

    /** The null instance. */
    public static final Axis NULL = null;

    /** The number of axes. */
    public static final int NUMBER_OF = values().length;

    /** The axis relatedEnumFile field. */
    private final Class relatedEnum;

    /**
     * Enum constructor.
     */
    private Axis(final Class relatedEnum) {
        this.relatedEnum = relatedEnum;
    }

    /**
     * Returns a {@code Class} that represents the axis' releted enum.
     *
     * @return the axis' relatedEnum
     */
    public Class relatedEnum() { return this.relatedEnum; }

}
