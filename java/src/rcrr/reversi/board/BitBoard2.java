/*
 *  BitBoard2.java
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

import java.math.BigInteger;

import java.util.Arrays;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * A board concrete implementation in the bitboard family.
 * <p>
 * A {@code BitBoard2} object holds the information of the state of each board's square.
 * The board state is kept into a long array having a length equal two.
 * The first entry keeps the black squares, the second the whites.
 * <p>
 * The board applyes some variants to the {@code BitBoard1} implementation ....
 * <p>
 * {@code BitBoard2} is immutable.
 * <p>
 * @see Square
 */
public final class BitBoard2 extends BitBoard1 {

    private static final boolean LOG = true;
    private static int callsTolegalMoves = 0;
    private static int callsToMakeMove = 0;
    private static int callsToConstructor = 0;

    private static final Direction[] DIRECTION_VALUES = Direction.values();

    private static final long ALL_SQUARES_EXCEPT_COLUMN_A = 0x7F7F7F7F7F7F7F7FL;
    private static final long ALL_SQUARES_EXCEPT_COLUMN_H = 0xFEFEFEFEFEFEFEFEL;

    public static String printLog() {
        String ret = "callsTolegalMoves=" + callsTolegalMoves + ", callsToMakeMove=" + callsToMakeMove + ", callsToConstructor=" + callsToConstructor;
        return ret;
    }

    /**
     * Base static factory for the class.
     * <p>
     * {@code squares} must be not null, and must have an entry for every board square.
     * Given that the map cannot have duplicate keys, its size must be equal to the number
     * of class instances defined by the {@code Square} enum.
     *
     * @param  squares the map of squares
     * @return         a new board having as state the given square map
     * @throws NullPointerException     if parameter {@code squares} is null
     * @throws IllegalArgumentException if the {@code squares} is not complete
     */
    static Board valueOf(final Map<Square, SquareState> squares) {
        BoardUtils.checkForConsistencyTheSquareMap(squares);
        return new BitBoard2(BoardUtils.mapToBitboard(squares));
    }

    /**
     * Static factory for the class.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set are not valid.
     * Precondition on the {@code bitboard} parameter are not enforced.
     *
     * @param  bitboard the bitboard field
     */
    static Board valueOf(final long[] bitboard) {
        return new BitBoard2(bitboard);
    }

    private static long neighbor(final long square, final Direction dir, final int amount) {
        long result = square;
        for (int i = 0; i < amount; i++) {
            result = neighbor(result, dir);
        }
        return result;
    }

    private static long neighbor(final long square, final Direction dir) {
        switch (dir) {
        case NW: return (square >>> 9) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case N:  return (square >>> 8);
        case NE: return (square >>> 7) & ALL_SQUARES_EXCEPT_COLUMN_H;
        case W:  return (square >>> 1) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case E:  return (square <<  1) & ALL_SQUARES_EXCEPT_COLUMN_H;
        case SW: return (square <<  7) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case S:  return (square <<  8);
        case SE: return (square <<  9) & ALL_SQUARES_EXCEPT_COLUMN_H;
        default: throw new IllegalArgumentException("Undefined value for direction. dir=" + dir);
        }
    }

    private static long neighbors(final long squares) {
        long neighbors = squares;
        neighbors |= (neighbors >>> 8);
        neighbors |= (neighbors >>> 1) & ALL_SQUARES_EXCEPT_COLUMN_A;
        neighbors |= (neighbors <<  1) & ALL_SQUARES_EXCEPT_COLUMN_H;
        neighbors |= (neighbors <<  8);
        return neighbors;
    }

    /**
     * Class constructor.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set to one are not allowed.
     *
     * @param  bitboard the bitboard field
     */
    private BitBoard2(final long[] bitboard) {
        super(bitboard);
        if (LOG) callsToConstructor++;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Square> legalMoves(final Player player) {

        if (LOG) callsTolegalMoves++;

        if (player == null) { throw new NullPointerException("Parameter player must be not null."); }

        return new SquareList(legalMoves(player.ordinal()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Board makeMove(final Square move, final Player player) {

        if (LOG) callsToMakeMove++;

        if (player == null) {
            throw new NullPointerException("Parameter player must be not null.");
        }
        if (move == null) {
            if (hasAnyLegalMove(player)) {
                throw new NullPointerException("Parameter move must be not null when a legal one is available.");
            } else {
                return this;
            }
        }
        if (!isLegal(move, player)) {
            throw new IllegalArgumentException("The move<"
                                               + move + "> by player<"
                                               + player + "> is illegal.");
        }

        return valueOf(makeMoveImpl(move, player));
    }

    private long legalMoves(final int player) {
        final int opponent = player ^ WHITE;
        final long empties = ~(bitboard[BLACK] | bitboard[WHITE]);

        long lm = 0L;
        for (final Direction dir : DIRECTION_VALUES) {
            long wave = neighbor(empties, dir) & bitboard[opponent];
            for (int shift = 2; shift < 8; shift++) {
                wave = neighbor(wave, dir);
                lm |= neighbor((wave & bitboard[player]), dir.opposite(), shift);
                wave &= bitboard[opponent];
            }
        }

        return lm;
    }

}
