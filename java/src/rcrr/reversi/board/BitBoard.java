/*
 *  BitBoard.java
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
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MUST  02111-1307, USA
 *  or visit the site <http://www.gnu.org/licenses/>.
 */

package rcrr.reversi.board;

/**
 * The class provides an implementation for common methods used by the bit board concrete implementations.
 */
public abstract class BitBoard extends AbstractBoard {

    private static final int BLACK = 0;
    private static final int WHITE = 1;

    /**
     * The bitboard field.
     * Values can be modified only by the constructor.
     */
    final transient long[] bitboard;

    /**
     * Class constructor.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set to one are not allowed.
     *
     * @param  bitboard the bitboard field
     */
    BitBoard(final long[] bitboard) {
        assert (bitboard != null) : "Parameter bitboard cannot be null.";
        assert (bitboard.length == 2) : "Parameter bitboard must have a lenght equal to two.";
        assert ((bitboard[0] & bitboard[1]) == 0L)
            : "Parameter bitboard cannot have black and white discs overlapping.";
        this.bitboard = bitboard.clone();
    }

    /**
     * Returns the disk count for the color.
     *
     * @param color the color for which the disk count is computed
     * @return the disk count
     * @throws NullPointerException if parameter {@code color} is null
     */
    @Override
    public int countPieces(final SquareState color) {
        if (color == null) {
            throw new NullPointerException("Parameter color must be not null.");
        }
        switch (color) {
        case BLACK: return Long.bitCount(bitboard[BLACK]);
        case WHITE: return Long.bitCount(bitboard[WHITE]);
        case EMPTY: return Long.bitCount(~(bitboard[BLACK] | bitboard[WHITE]));
        case OUTER: return 0;
        default: throw new IllegalArgumentException("Undefined value for color parameter. color=" + color);
        }
    }

    /**
     * Returns the {@code SquareState} value for the given board's square.
     * <p>
     * When {@code square} is {@code null} the method returns {@code SquareState.OUTER} value.
     *
     * @param  square the board square to retrieve the state value
     * @return        the square state
     */
    public SquareState get(final Square square) {
        if (square == null) { return SquareState.OUTER; }
        final long bitsquare = 1L << square.ordinal();
        if ((bitsquare & bitboard[BLACK]) != 0) {
            return SquareState.BLACK;
        } else if ((bitsquare & bitboard[WHITE]) != 0) {
            return SquareState.WHITE;
        } else {
            return SquareState.EMPTY;
        }
    }

}
