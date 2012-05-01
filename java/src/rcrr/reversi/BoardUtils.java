/*
 *  BoardUtils.java
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

package rcrr.reversi;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;

public final class BoardUtils {

    /** The number of squares hosted by the board. */
    private static final int NUMBER_OF_SQUARES = Square.values().length;

    /**
     * The MASKS array has sixtyfour long value entries.
     * Each entry has all bit set to zero ecept one. The one turned on
     * is positioned according to the index position of the entry into the array.
     */
    private static final long[] MASKS = new long[NUMBER_OF_SQUARES];

    /**
     * Prepares the precompiled mask array.
     */
    static {
        for (int position = 0; position < NUMBER_OF_SQUARES; position++) {
            MASKS[position] = 1L << position;
        }
    }

    /**
     * Returns a new squares map being filled by empty values.
     *
     * @return a new map having and empty square state value for each
     *         square in the board
     */
    public static Map<Square, SquareState> emptyBoardSquares() {
        Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(Square.class);
        for (Square sq : Square.values()) {
            sm.put(sq, SquareState.EMPTY);
        }
        return sm;
    }

    /**
     * Receives the squares map and returns the bitboard representation.
     *
     * @param squares the squares map
     * @return        the bitboard representation
     */
    protected static long[] mapToBitboard(final Map<Square, SquareState> squares) {
        assert (squares != null) : "Parameter squares cannot be null.";
        assert (squares.size() == Square.values().length) : "Parameter squares size is not consistent."
            + " squares.size()=" + squares.size()
            + " expected value: " + Square.values().length;
        assert (!squares.containsKey(null)) : "Parameter squares cannot contains null keys.";
        assert (!squares.containsValue(null)) : "Parameter squares cannot contains null values.";
        final long[] bitboard = {0L, 0L};
        final Square[] keys = Square.values();
        for (int position = 0; position < NUMBER_OF_SQUARES; position++) {
            Square key = keys[position];
            switch (squares.get(key)) {
            case EMPTY: break;
            case BLACK: bitboard[0] += MASKS[position]; break;
            case WHITE: bitboard[1] += MASKS[position]; break;
            default: throw new IllegalArgumentException("Parameter squares contains an unexpected value.");
            }
        }
        return bitboard;
    }

    /**
     * Receives the bitboard representation and returns the squares map.
     *
     * @param bitboard the bitboard representation
     * @return         the squares map
     */
    protected static Map<Square, SquareState> bitboardToMap(final long[] bitboard) {
        assert (bitboard.length == 2) : "Parameter bitboard must be an array of length two.";
        final Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(Square.class);
        final Square[] keys = Square.values();
        for (int position = 0; position < NUMBER_OF_SQUARES; position++) {
            Square key = keys[position];
            SquareState value;
            if ((bitboard[0] & MASKS[position]) != 0L) {
                value = SquareState.BLACK;
            } else if ((bitboard[1] & MASKS[position]) != 0L) {
                value = SquareState.WHITE;
            } else {
                value = SquareState.EMPTY;
            }
            sm.put(key, value);
        }
        return sm;
    }

    protected static Map<Square, SquareState> squares(final Board board) {
        final Map<Square, SquareState> squares = new EnumMap<Square, SquareState>(Square.class);
        for (final Square sq : Square.values()) {
            squares.put(sq, board.get(sq));
        }
        return squares;
    }

}
