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
    HO { @Override public int transformToRow0(final long squares) { return 0; } },

    /**
     * Vertical axis (N-S).
     */
    VE { @Override public int transformToRow0(final long squares) { return 0; } },

    /**
     * Diagonal Down axis (NW-SE), A1-H8.
     */
    DD { @Override public int transformToRow0(final long squares) { return 0; } },

    /**
     * Diagonal Up axis (NE-SW), A8-H1.
     */
    DU { @Override public int transformToRow0(final long squares) { return 0; } };

    /** The null instance. */
    public static final Axis NULL = null;

    /** The number of axes. */
    public static final int NUMBER_OF = values().length;

    /** Used for masking a byte when using integer values. */
    static final int BYTE_MASK_FOR_INT = 0xFF;

    /** A bitboard being set on column A. */
    private static final long COLUMN_A = 0x0101010101010101L;

    /** Macic number 7. */
    private static final int MAGIC_NUMBER_7 = 7;

    /** Macic number 14. */
    private static final int MAGIC_NUMBER_14 = 14;

    /** Macic number 28. */
    private static final int MAGIC_NUMBER_28 = 28;

    /**
     * Returns an int having the bits from position 8 to position 31 set to zero,
     * and the bits from position 0 to position 7, corresponding to Row One in the board,
     * copied from the Column A of the {@code bitboard} parameter.
     * Bit value corresponding to square A1 is moved to A1, A2 to B1, ... , A8 to H1.
     *
     * @param bitboard the bitboard representation for one color
     * @return         colum a copied to row one, all other position are set to zero
     */
    private static int trasformColumnAInRow0(final long bitboard) {
        long tmp = bitboard;
        tmp &= COLUMN_A;
        tmp |= tmp >> MAGIC_NUMBER_28;
        tmp |= tmp >> MAGIC_NUMBER_14;
        tmp |= tmp >> MAGIC_NUMBER_7;
        return (int) tmp & BYTE_MASK_FOR_INT;
    }

    abstract int transformToRow0(final long squares);

}
