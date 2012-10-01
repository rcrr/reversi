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

import java.math.BigInteger;

import java.util.Arrays;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * A board concrete implementation in the bitboard family.
 *
 * To do:
 * replace neighbor with shift. Directions have to be represented by mean of the Direction Enum instead of the integer constants.
 * <p>
 * A {@code BitBoard0} object holds the information of the state of each board's square.
 * The board state is kept into a long array having a length equal two.
 * The first entry keeps the black squares, the second the whites.
 * <p>
 * The board uses wouldFlip ....
 * <p>
 * {@code BitBoard0} is immutable.
 * <p>
 * @see Square
 */
public final class BitBoard0 extends BitBoard {

    private static final boolean LOG = true;
    private static int callsTolegalMoves = 0;
    private static int callsToMakeMove = 0;
    private static int callsToConstructor = 0;

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
        return new BitBoard0(BoardUtils.mapToBitboard(squares));
    }

    static Board valueOf(final long[] bitboard) {
        return new BitBoard0(bitboard);
    }
    /*
    private static int[] asArray(final List<Integer> list) {
        final int[] array = new int[list.size()];
        int index = 0;
        for (final Integer element : list) {
            array[index] = element.intValue();
            index++;
        }
        return array;
    }

    private static List<Integer> asList(final int[] array) {
        final List<Integer> list = new ArrayList<Integer>(array.length);
        for (int i = 0; i < array.length; i++) { list.add(array[i]); }
        return list;
    }
    */
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
        if (LOG) callsToConstructor++;
    }

    /**
     * Returns the boolean value telling if the move, done by the
     * specified player, is legal.
     * <p>
     * Parameter {@code move} must be not {@code null}.
     * Parameter {@code player} must be not {@code null}.
     *
     * @param move   the square where to put the new disk
     * @param player the player moving
     * @return       true if the move is legal, otherwise false
     * @throws NullPointerException if parameter {@code move} or {@code player} is null
     */
    public boolean isLegal(final Square move, final Player player) {
        if (move == null) {
            throw new NullPointerException("Parameter move must be not null.");
        }
        if (player == null) {
            throw new NullPointerException("Parameter player must be not null.");
        }
        final long bitmove = 1L << move.ordinal();
        if ((bitmove & (bitboard[0] | bitboard[1])) != 0) {
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

        if (LOG) callsToMakeMove++;

        makeMoveInvariantAreSatisfied(move, player);

        final long[] newbitboard = bitboard.clone();
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

    private long wouldFlip(final long move, final Player player, final Direction dir) {
        //assert (Long.bitCount(move) == 1) : "Argument move must be have one and only one bit set.";
        //assert (player != null) : "Argument player must be not null.";
        long bracketing = 0L;
        long neighbor = shift(move, dir);
        final int intPlayer = player.ordinal(); 
        final int intOpponent = intPlayer ^ WHITE;
        if ((neighbor & bitboard[intOpponent]) != 0L) {
            long next = shift(neighbor, dir);
            if (next != 0L) {
                bracketing = findBracketingPiece(next, intPlayer, dir);
            }
        }
        return bracketing;
    }

    private long findBracketingPiece(final long square, final int intPlayer, final Direction dir) {
        final int intOpponent = intPlayer ^ WHITE;
        if ((square & bitboard[intPlayer]) != 0L) {
            return square;
        } else if ((square & bitboard[intOpponent]) != 0L) {
            long next = shift(square, dir);
            if (next != 0L) {
                return findBracketingPiece(next, intPlayer, dir);
            }
        }
        return 0L;
    }

}
