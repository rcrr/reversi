/*
 *  BitBoard0.java
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

/**
 * A board concrete implementation in the bitboard family.
 * <p>
 * A {@code BitBoard0} object holds the information of the state of each board's square.
 * The board state is kept into a long array having a length equal two.
 * The first entry keeps the black squares, the second the whites.
 * <p>
 * The board uses wouldFlip and findBracketingPiece ideas as described in the PAIP book.
 * <p>
 * {@code BitBoard0} is immutable.
 * <p>
 * @see Square
 */
public final class BitBoard0 extends BitBoard {

    /** It turns on or off the class logging for performances. */
    private static final boolean LOG = true;

    /** Collects the number of call to legalMoves method. */
    private static int callsTolegalMoves = 0;

    /** Collects the number of call to makeMove method. */
    private static int callsToMakeMove = 0;

    /** Collects the number of call to the class constructor. */
    private static int callsToConstructor = 0;

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
        return new BitBoard0(BoardUtils.mapToBitboard(squares));
    }

    /**
     * Static factory for the class.
     *
     * @param  bitboard the board configuration as a pair of long values
     * @return          a new board having as state the given bitboard parameter
     */
    static Board valueOf(final long[] bitboard) {
        return new BitBoard0(bitboard);
    }

    /**
     * Class constructor.
     * <p>
     * {@code bitboard} must be not null, and must have a length equal to
     * two. Overlapping bit set to one are not allowed.
     *
     * @param  bitboard the bitboard field
     */
    private BitBoard0(final long[] bitboard) {
        super(bitboard);
        if (LOG) { callsToConstructor++; }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isLegal(final Square move, final Player player) {
        isLegalInvariantsAreSatisfied(move, player);
        final long bitmove = 1L << move.ordinal();
        if ((bitmove & (bitboard()[0] | bitboard()[1])) != 0) {
            return false;
        }
        for (final Direction dir : move.capableToFlipDirections()) {
            if (wouldFlip(bitmove, player, dir) != 0L) {
                return true;
            }
        }
        return false;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Board makeMove(final Square move, final Player player) {

        if (LOG) { callsToMakeMove++; }

        makeMoveInvariantsAreSatisfied(move, player);

        final long[] newbitboard = bitboard().clone();
        final int p = player.ordinal();
        final int o = p ^ WHITE;
        final int m = move.ordinal();
        final long bitmove = 1L << m;
        newbitboard[p] |= bitmove; // set the move
        for (final Direction dir : move.capableToFlipDirections()) {
            final long bracketer = wouldFlip(bitmove, player, dir);
            if (bracketer != 0L) {
                for (long c = shift(bitmove, dir); true; c = shift(c, dir)) {
                    if (c == bracketer) { break; }
                    newbitboard[p] |= c;
                    newbitboard[o] &= ~c;
                }
            }
        }
        return valueOf(newbitboard);
    }

    /**
     * Returns the bracketing square or 0L if it is missing.
     * The method does not check that the move is legal and that the square parameter
     * is one step from move in the given direction.
     *
     * @param square the square obtained moving by one from the move in the given direction
     * @param player the player
     * @param dir    the direction
     * @return       the bracketing square, or 0L if it is not found
     */
    private long findBracketingPiece(final long square, final int player, final Direction dir) {
        if ((square & bitboard()[player]) != 0L) {
            return square;
        } else if ((square & bitboard()[opponent(player)]) != 0L) {
            long next = shift(square, dir);
            if (next != 0L) {
                return findBracketingPiece(next, player, dir);
            }
        }
        return 0L;
    }

    /**
     * Returns the bracketing square or null if it is not found.
     * The method does not check that the move is legal.
     * <p>
     * Parameters player, and dir must be not null.
     * Parameter move must have only one bit set.
     * <p>
     * Assertion add a 4.56% time increase in the ReversiRoundRobin2Perf benchmark.
     *
     * @param move   the square where to move
     * @param player the player
     * @param dir    the direction
     * @return       the bracketing square, or 0L if it is not found
     */
    private long wouldFlip(final long move, final Player player, final Direction dir) {
        long bracketing = 0L;
        long neighbor = shift(move, dir);
        final int intPlayer = player.ordinal();
        final int intOpponent = intPlayer ^ WHITE;
        if ((neighbor & bitboard()[intOpponent]) != 0L) {
            long next = shift(neighbor, dir);
            if (next != 0L) {
                bracketing = findBracketingPiece(next, intPlayer, dir);
            }
        }
        return bracketing;
    }

}
