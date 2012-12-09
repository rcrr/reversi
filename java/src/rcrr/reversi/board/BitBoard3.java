/*
 *  BitBoard3.java
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

import java.util.Map;
import java.util.List;

/**
 * A board concrete implementation in the bitboard family.
 * <p>
 * The board applyes some variants to the {@code BitBoard1} implementation,
 * it differ from the parent implementation because the legalMoves implementation doesn't
 * relay on the isLegal method.
 * <p>
 * {@code BitBoard3} is immutable.
 * <p>
 * @see Square
 */
public final class BitBoard3 extends BitBoard1 {

    /** It turns on or off the class logging for performances. */
    private static final boolean LOG = true;

    /** Caches the direction enum values in a local array. */
    private static final Line2[] LINE2_VALUES = Line2.values();

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
        String ret = "callsTolegalMoves=" + callsTolegalMoves + ", callsToMakeMove=" + callsToMakeMove
            + ", callsToConstructor=" + callsToConstructor;
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
        return new BitBoard3(BoardUtils.mapToBitboard(squares));
    }

    /**
     * Static factory for the class.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set are not valid.
     * Precondition on the {@code bitboard} parameter are not enforced.
     *
     * @param  bitboard the bitboard field
     * @return          a new board having as state the given bitboard array
     */
    static Board valueOf(final long[] bitboard) {
        return new BitBoard3(bitboard);
    }

    /**
     * Class constructor.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set to one are not allowed.
     *
     * @param  bitboard the bitboard field
     */
    private BitBoard3(final long[] bitboard) {
        super(bitboard);
        if (LOG) { callsToConstructor++; }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Square> legalMoves(final Player player) {
        if (LOG) { callsTolegalMoves++; }
        if (player == null) { throw new NullPointerException("Parameter player must be not null."); }
        return new SquareList(legalMoves(player.ordinal()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Board makeMove(final Square move, final Player player) {
        if (LOG) { callsToMakeMove++; }
        makeMoveInvariantsAreSatisfied(move, player);
        return valueOf(makeMoveImpl(move, player.ordinal()));
    }

    /**
     * The core method of this class. Implements the legal moves call by looping on the thirtyeigth lines.
     *
     * @param player the player that has to move
     * @return       legal moves for the player
     */
    private long legalMoves(final int player) {
        long result = 0L;
        if (hasLegalMovesBeenComputed(player)) {
            result = legalMovesCache(player);
        } else {
            final int opponent = opponent(player);
            final long empties = empties();
            final long pBitboard = bitboard(player);
            final long oBitboard = bitboard(opponent);
            for (final Line2 line : LINE2_VALUES) {
                final int lineIndex = line.index(pBitboard, oBitboard);
                final long lm = line.legalMoves(lineIndex);
                result |= lm;
            }
            setLegalMovesCache(player, result);
            setHasLegalMovesBeenComputed(player, true);
        }
        return result;
    }

}
