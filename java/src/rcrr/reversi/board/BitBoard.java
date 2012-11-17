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

    /** Integer value for the black player. It is equal to Player.BLACK.ordinal(). */
    static final int BLACK = 0;

    /** Integer value for the white player. It is equal to Player.WHITE.ordinal(). */
    static final int WHITE = 1;

    /** Used for masking a byte when using integer values. */
    static final int BYTE_MASK_FOR_INT = 0xFF;

    /**
     * Returns the opponent of {@code player} parameter.
     *
     * @param player a player
     * @return       the opponent player
     */
    static final int opponent(final int player) { return player ^ WHITE; }

    /**
     * The bitboard field.
     * Values can be modified only by the constructor.
     */
    private final transient long[] bitboard;

    /**
     * Class constructor.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set to one are not allowed.
     * Invariants are not enforced.
     *
     * @param  bitboard the bitboard field
     */
    BitBoard(final long[] bitboard) {
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
        case EMPTY: return Long.bitCount(empties());
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

    /**
     * Returns the empty set of squares in the board.
     *
     * @return the empy set of squares
     */
    final long empties() { return ~(bitboard[BLACK] | bitboard[WHITE]); }

    /**
     * Acessor method for the bitboard field.
     * <p>
     * Be carefull! Do not change the array values.
     * It should return a copy of the field, but for
     * performance reasons a reference of the array is returned.
     *
     * @return the bitboard field
     */
    final long[] bitboard() { return this.bitboard; }

    /**
     * Acessor method for the bitboard field.
     *
     * @param  player the player whom bitboard will be retrieved
     * @return the bitboard field for the palyer
     */
    final long bitboard(final int player) { return this.bitboard[player]; }

}
