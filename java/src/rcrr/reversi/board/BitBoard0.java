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
public final class BitBoard0 extends AbstractBoard {

    private static final boolean LOG = true;
    private static int callsTolegalMoves = 0;
    private static int callsToMakeMove = 0;
    private static int callsToConstructor = 0;

    private static final int BLACK = 0;
    private static final int WHITE = 1;

    private static final int DIR_NW = -9;
    private static final int DIR_NN = -8;
    private static final int DIR_NE = -7;
    private static final int DIR_WW = -1;
    private static final int DIR_EE = +1;
    private static final int DIR_SW = +7;
    private static final int DIR_SS = +8;
    private static final int DIR_SE = +9;

    private static final int NUMBER_OF_DIRECTIONS = 8;
    private static final int[] DIRECTIONS = new int [] {DIR_NW, DIR_NN, DIR_NE, DIR_WW, DIR_EE, DIR_SW, DIR_SS, DIR_SE};
    private static final int[][] FLIPPING_DIRECTIONS = new int[64][];

    private static final long CORE_SQUARES                = 0x007E7E7E7E7E7E00L;
    private static final long EDGES_SQUARES               = 0xFF818181818181FFL;
    private static final long ALL_SQUARES_EXCEPT_COLUMN_A = 0x7F7F7F7F7F7F7F7FL;
    private static final long ALL_SQUARES_EXCEPT_COLUMN_H = 0xFEFEFEFEFEFEFEFEL;

    private static final int EDGE_N = 0;
    private static final int EDGE_E = 1;
    private static final int EDGE_S = 2;
    private static final int EDGE_W = 3;

    private static final int[] EDGES = new int[] {EDGE_N, EDGE_E, EDGE_S, EDGE_W};

    private static final int ROW_1 = 0;
    private static final int ROW_2 = 1;
    private static final int ROW_3 = 2;
    private static final int ROW_4 = 3;
    private static final int ROW_5 = 4;
    private static final int ROW_6 = 5;
    private static final int ROW_7 = 6;
    private static final int ROW_8 = 7;

    private static final int COL_A = 0;
    private static final int COL_B = 1;
    private static final int COL_C = 2;
    private static final int COL_D = 3;
    private static final int COL_E = 4;
    private static final int COL_F = 5;
    private static final int COL_G = 6;
    private static final int COL_H = 7;

    /** Used for masking a byte when using integer values. */
    private static final int BYTE_MASK_FOR_INT = 0xFF;

    static {

        for (int sq = 0; sq < 64; sq++) {
            final List<Integer> directions = asList(DIRECTIONS);
            if (squareBelongsToEdge(sq, EDGE_N)) {
                directions.remove(Integer.valueOf(DIR_NW));
                directions.remove(Integer.valueOf(DIR_NN));
                directions.remove(Integer.valueOf(DIR_NE));
            }
            if (squareBelongsToEdge(sq, EDGE_E)) {
                directions.remove(Integer.valueOf(DIR_NE));
                directions.remove(Integer.valueOf(DIR_EE));
                directions.remove(Integer.valueOf(DIR_SE));
            }
            if (squareBelongsToEdge(sq, EDGE_S)) {
                directions.remove(Integer.valueOf(DIR_SW));
                directions.remove(Integer.valueOf(DIR_SS));
                directions.remove(Integer.valueOf(DIR_SE));
            }
            if (squareBelongsToEdge(sq, EDGE_W)) {
                directions.remove(Integer.valueOf(DIR_NW));
                directions.remove(Integer.valueOf(DIR_WW));
                directions.remove(Integer.valueOf(DIR_SW));
            }
            FLIPPING_DIRECTIONS[sq] = asArray(directions);
        }

    }

    public static String printLog() {
        String ret = "callsTolegalMoves=" + callsTolegalMoves + ", callsToMakeMove=" + callsToMakeMove + ", callsToConstructor=" + callsToConstructor;
        return ret;
    }

    private static boolean squareBelongsToEdge(final int square, final int edge) {
        assert (Arrays.binarySearch(EDGES, edge) >= 0) : "Argument edge must be contained in the EDGES array.";
        final int col = square % 8;
        final int row = square / 8;
        switch (edge) {
        case EDGE_N: if (row < ROW_2) { return true; }
            break;
        case EDGE_E: if (col > COL_G) { return true; }
            break;
        case EDGE_S: if (row > ROW_7) { return true; }
            break;
        case EDGE_W: if (col < COL_B) { return true; }
            break;
        default: throw new IllegalArgumentException("Parameter edge out of range. edge = " + edge);
        }
        return false;
    }

    private static List<Integer> asList(final int[] array) {
        final List<Integer> list = new ArrayList<Integer>(array.length);
        for (int i = 0; i < array.length; i++) { list.add(array[i]); }
        return list;
    }

    private static int[] asArray(final List<Integer> list) {
        final int[] array = new int[list.size()];
        int index = 0;
        for (final Integer element : list) {
            array[index] = element.intValue();
            index++;
        }
        return array;
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

    /**
     * The bitboard field.
     */
    private final long[] bitboard;

    /**
     * Class constructor.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set to one are not allowed.
     *
     * @param  bitboard the bitboard field
     */
    private BitBoard0(final long[] bitboard) {
        assert (bitboard != null) : "Parameter bitboard cannot be null.";
        assert (bitboard.length == 2) : "Parameter bitboard must have a lenght equal to two.";
        assert ((bitboard[0] & bitboard[1]) == 0L)
            : "Parameter bitboard cannot have black and white discs overlapping.";
        if (LOG) callsToConstructor++;
        this.bitboard = bitboard.clone();
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
        for (int dir : FLIPPING_DIRECTIONS[move.ordinal()]) {
            if (wouldFlip(bitmove, player, dir) != 0L) {
                return true;
            }
        }
        return false;
    }

    private long wouldFlip(final long move, final Player player, final int dir) {
        assert (Long.bitCount(move) == 1) : "Argument move must be have one and only one bit set.";
        assert (player != null) : "Argument player must be not null.";
        long bracketing = 0L;
        long neighbor = neighbor(move, dir);
        final int intPlayer = player.ordinal(); 
        final int intOpponent = intPlayer ^ WHITE;
        if ((neighbor & bitboard[intOpponent]) != 0L) {
            long next = neighbor(neighbor, dir);
            if (next != 0L) {
                bracketing = findBracketingPiece(next, intPlayer, dir);
            }
        }
        return bracketing;
    }

    private long findBracketingPiece(final long square, final int intPlayer, final int dir) {
        final int intOpponent = intPlayer ^ WHITE;
        if ((square & bitboard[intPlayer]) != 0L) {
            return square;
        } else if ((square & bitboard[intOpponent]) != 0L) {
            long next = neighbor(square, dir);
            if (next != 0L) {
                return findBracketingPiece(next, intPlayer, dir);
            }
        }
        return 0L;
    }

    static long neighbor(final long square, final int dir) {
        switch (dir) {
        case DIR_NW: return (square >>> 9) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case DIR_NN: return (square >>> 8);
        case DIR_NE: return (square >>> 7) & ALL_SQUARES_EXCEPT_COLUMN_H;
        case DIR_WW: return (square >>> 1) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case DIR_EE: return (square <<  1) & ALL_SQUARES_EXCEPT_COLUMN_H;
        case DIR_SW: return (square <<  7) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case DIR_SS: return (square <<  8);
        case DIR_SE: return (square <<  9) & ALL_SQUARES_EXCEPT_COLUMN_H;
        default: throw new IllegalArgumentException("Undefined value for dir parameter. dir=" + dir);
        }
    }

    /**
     * Returns a new updated board to reflect move by player. This static
     * factory executes a game move to the board and returns a new one,
     * reflecting the move. The original board is not modified.
     * <p>
     * A null value for player is not allowed, a {@code NullPointerException}
     * is thrown in such a case.
     * <p>
     * A null value for move is allowed, and moreover is the only valid value
     * acceptable by the method, when the player has not any legal move.
     * Otherwise a null move is forbidden, and a {@code NullPointerException}
     * is risen.
     * <p>
     * The method does check if the move is legal. It throws an
     * {@code IllegalArgumentException} in case it is not.
     *
     * @param  move   the board square where to put the disk
     * @param  player the disk color to put on the board
     * @return        a new {@code Board} reflecting the move made
     * @throws NullPointerException     if parameter {@code move}
     *                                  or {@code player} is null
     * @throws IllegalArgumentException if the {@code move}
     *                                  by {@code player} is illegal
     */
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
        final long[] newbitboard = bitboard.clone();
        final int p = player.ordinal(); 
        final int o = p ^ WHITE;
        final int m = move.ordinal();
        final long bitmove = 1L << m;
        newbitboard[p] |= bitmove; // set the move
        for (final int dir : FLIPPING_DIRECTIONS[m]) {
            final long bracketer = wouldFlip(bitmove, player, dir);
            if (bracketer != 0L) {
                for (long c = neighbor(bitmove, dir); true; c = neighbor(c, dir)) {
                    if (c == bracketer) { break; }
                    newbitboard[p] |= c;
                    newbitboard[o] &= ~c;
                }
            }
        }
        final Board result = valueOf(newbitboard);
        return result;
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

}
