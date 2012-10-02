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
 * The board applyes some variants to the {@code BitBoard1} implementation,
 * it differ from the parent implementation because the legalMoves implementation doesn't
 * relay on the isLegal method. The makeMove method instead works by looping on directions and
 * finding legal moves by mean of shifting the board as a whole.
 * <p>
 * {@code BitBoard2} is immutable.
 * <p>
 * @see Square
 */
public final class BitBoard2 extends BitBoard1 {

    /** It turns on or off the class logging for performances. */
    private static final boolean LOG = true;

    /** Collects the number of call to legalMoves method. */
    private static int callsTolegalMoves = 0;

    /** Collects the number of call to makeMove method. */
    private static int callsToMakeMove = 0;
 
    /** Collects the number of call to the class constructor. */
    private static int callsToConstructor = 0;

    /** Caches the direction enum values in a local array. */
    private static final Direction[] DIRECTION_VALUES = Direction.values();

    /**
     * Returns info for performance statistics.
     *
     * @return a string having class performance statistics
     */
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
        makeMoveInvariantsAreSatisfied(move, player);
        return valueOf(makeMoveImpl(move, player));
    }

    /**
     * The core method of this class. Implements the legal moves call by waveing the potential
     * legal moves up to the bracketing pieces. Directions are computed one by one, squares work
     * in parallel.
     *
     * @param player the player that has to move
     * @return       legal moves for the player
     */
    private long legalMoves(final int player) {
        final int opponent = player ^ WHITE;
        final long empties = ~(bitboard[BLACK] | bitboard[WHITE]);

        long lm = 0L;
        for (final Direction dir : DIRECTION_VALUES) {
            final Direction opp = dir.opposite();
            long wave = shift(empties, dir) & bitboard[opponent];
            for (int shift = 2; shift < 8; shift++) {
                wave = shift(wave, dir);
                lm |= shift((wave & bitboard[player]), opp, shift);
                wave &= bitboard[opponent];
                if (wave == 0L) { break; }
            }
        }

        return lm;
    }

}
