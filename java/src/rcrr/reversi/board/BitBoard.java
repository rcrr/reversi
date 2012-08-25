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

import java.math.BigInteger;

import java.util.Arrays;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * A board concrete implementation.
 * <p>
 * A {@code BitBoard} object holds the information of the state of each board's square.
 * The board state is kept into a long array having a length equal two.
 * The first entry keeps the black squares, the second the whites.
 * <p>
 * {@code BitBoard} is immutable.
 * <p>
 * @see Square
 */
public final class BitBoard extends AbstractBoard {

    private static final boolean LOG = true;
    private static int callsTolegalMoves = 0;
    private static int callsToMakeMove = 0;
    private static int callsToConstructor = 0;

    private static final int BLACK = 0;
    private static final int WHITE = 1;

    private static final int FILE_MAX_LENGTH = 8;

    private static final int SQUARE_SIZE = 64;

    static final int DIR_NW = -9;
    static final int DIR_NN = -8;
    static final int DIR_NE = -7;
    static final int DIR_WW = -1;
    static final int DIR_EE = +1;
    static final int DIR_SW = +7;
    static final int DIR_SS = +8;
    static final int DIR_SE = +9;

    static final int NUMBER_OF_DIRECTIONS = 8;
    static final int[] DIRECTIONS = new int [] {DIR_NW, DIR_NN, DIR_NE, DIR_WW, DIR_EE, DIR_SW, DIR_SS, DIR_SE};
    static final int[][] FLIPPING_DIRECTIONS = new int[SQUARE_SIZE][];

    private static final long CORE_SQUARES                = 0x007E7E7E7E7E7E00L;
    private static final long EDGES_SQUARES               = 0xFF818181818181FFL;
    private static final long ALL_SQUARES_EXCEPT_COLUMN_A = 0x7F7F7F7F7F7F7F7FL;
    private static final long ALL_SQUARES_EXCEPT_COLUMN_H = 0xFEFEFEFEFEFEFEFEL;

    public static String printLog() {
        String ret = "callsTolegalMoves=" + callsTolegalMoves + ", callsToMakeMove=" + callsToMakeMove + ", callsToConstructor=" + callsToConstructor;
        return ret;
    }

    /** Square index literal. */
    static final int A1 =  0;
    static final int B1 =  1;
    static final int C1 =  2;
    static final int D1 =  3;
    static final int E1 =  4;
    static final int F1 =  5;
    static final int G1 =  6;
    static final int H1 =  7;

    static final int A2 =  8;
    static final int B2 =  9;
    static final int C2 = 10;
    static final int D2 = 11;
    static final int E2 = 12;
    static final int F2 = 13;
    static final int G2 = 14;
    static final int H2 = 15;

    static final int A3 = 16;
    static final int B3 = 17;
    static final int C3 = 18;
    static final int D3 = 19;
    static final int E3 = 20;
    static final int F3 = 21;
    static final int G3 = 22;
    static final int H3 = 23;

    static final int A4 = 24;
    static final int B4 = 25;
    static final int C4 = 26;
    static final int D4 = 27;
    static final int E4 = 28;
    static final int F4 = 29;
    static final int G4 = 30;
    static final int H4 = 31;

    static final int A5 = 32;
    static final int B5 = 33;
    static final int C5 = 34;
    static final int D5 = 35;
    static final int E5 = 36;
    static final int F5 = 37;
    static final int G5 = 38;
    static final int H5 = 39;

    static final int A6 = 40;
    static final int B6 = 41;
    static final int C6 = 42;
    static final int D6 = 43;
    static final int E6 = 44;
    static final int F6 = 45;
    static final int G6 = 46;
    static final int H6 = 47;

    static final int A7 = 48;
    static final int B7 = 49;
    static final int C7 = 50;
    static final int D7 = 51;
    static final int E7 = 52;
    static final int F7 = 53;
    static final int G7 = 54;
    static final int H7 = 55;

    static final int A8 = 56;
    static final int B8 = 57;
    static final int C8 = 58;
    static final int D8 = 59;
    static final int E8 = 60;
    static final int F8 = 61;
    static final int G8 = 62;
    static final int H8 = 63;

    /**
     * Returns an int value corresponding to the ordinal position of the only bit set in the {@code square} parameter.
     * <p>
     * The {@code square} parameter must have one and only one bit set to {@code 1}, this assumption is not verified.
     * <p>
     * The method is equivalent to the following loop:
     * <pre>
     *   for (int i = 0; i < 64; i++) {
     *       if (1L == (square >>> i)) { return i; }
     *   }
     *   throw new IllegalArgumentException("Parameter square is invalid.");
     * </pre>
     * Another inplementation follows the "divide and conqueror" approach instead of the linear access proposed.
     * The extimation of the iterations is 64/2 = 32 for the linear implementation,
     * compared with the logarithm base 2 of 64 that is equal to 6.
     * Here the technique:
     * <pre>
     *   int low = 0;
     *   int high = 64;
     *   while (true) {
     *       final int bit = (low + high) >>> 1;
     *       final long window = square >>> bit;
     *       if (window == 1L) {
     *           return bit; // value found
     *       } else if (window == 0L) {
     *           high = bit;
     *       } else {
     *           low = bit;
     *       }
     *   }
     * </pre>
     * The "real implementation" applyes bitshifts and passes the result to a precomputed table
     * composed of 256 entries that maps integers to their logarithm. 
     *
     * @param square the long value for the square
     * @return       the ordinal position of the bit set in the {@code square} parameter
     */
    static int squareIntValue(final long square) {
        long tmp = square;
        int result = 0;
        if((tmp & 0xFFFFFFFF00000000L) != 0) { tmp >>>= 32; result  = 32; }
        if(tmp > 0x000000000000FFFFL)        { tmp >>>= 16; result |= 16; }
        if(tmp > 0x00000000000000FFL)        { tmp >>>=  8; result |=  8; }
        result |= LOG2_ARRAY[(int)tmp];
        return result;
    }

    static private int LOG2_ARRAY[] = new int[] {
        0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    };

    static String longToString(final long value) {
        final StringBuilder sb = new StringBuilder();
        final String toBinaryString = Long.toBinaryString(value);
        final int len = toBinaryString.length();
        final int missingZeroes = SQUARE_SIZE - len;
        for (int i = SQUARE_SIZE; i > 0; i--) {
            if ((i != SQUARE_SIZE) && (i % FILE_MAX_LENGTH == 0)) { sb.append("."); }
            if ((missingZeroes - (SQUARE_SIZE - i)) > 0) {
                sb.append('0');
            } else {
                sb.append(toBinaryString.charAt(len - i));
            }
        }
        return sb.toString();
    }

    static final int EDGE_N = 0;
    static final int EDGE_E = 1;
    static final int EDGE_S = 2;
    static final int EDGE_W = 3;

    static final int[] EDGES = new int[] {EDGE_N, EDGE_E, EDGE_S, EDGE_W};

    static final int ROW_1 = 0;
    static final int ROW_2 = 1;
    static final int ROW_3 = 2;
    static final int ROW_4 = 3;
    static final int ROW_5 = 4;
    static final int ROW_6 = 5;
    static final int ROW_7 = 6;
    static final int ROW_8 = 7;

    static final int COL_A = 0;
    static final int COL_B = 1;
    static final int COL_C = 2;
    static final int COL_D = 3;
    static final int COL_E = 4;
    static final int COL_F = 5;
    static final int COL_G = 6;
    static final int COL_H = 7;

    static boolean squareBelongsToEdge(final int square, final int edge) {
        assert (Arrays.binarySearch(EDGES, edge) >= 0) : "Argument edge must be contained in the EDGES array.";
        final int col = squareColumn(square);
        final int row = squareRow(square);
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

    private static int squareRow(final int square) {
        return square / FILE_MAX_LENGTH;
    }

    private static int squareColumn(final int square) {
        return square % FILE_MAX_LENGTH;
    }

    static {

        for (int sq = 0; sq < SQUARE_SIZE; sq++) {
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
        return new BitBoard(BoardUtils.mapToBitboard(squares));
    }

    static Board valueOf(final Board board) {
        if (board == null) { throw new NullPointerException("Parameter board cannot be null."); }
        int i = 0;
        final long[] bitboard = new long[2];
        for (final Square sq : Square.values()) {
            long mask = 1L << i;
            SquareState color = board.get(sq);
            if (color == SquareState.BLACK) {
                bitboard[0] |= mask;
            } else if (color == SquareState.WHITE) {
                bitboard[1] |= mask;
            }
            i++;
        }
        return new BitBoard(bitboard);
    }

    static Board valueOf(final long[] bitboard) {
        return new BitBoard(bitboard);
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
    private BitBoard(final long[] bitboard) {
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
    public boolean isLegal_(final Square move, final Player player) {
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

    /**
     * A few more optimizations are possible:
     * Precompute the shiftDistance value, remake signedShift using a shift.
     * Eliminate the check for diagonals shorter than 3 pieces.
     * Verify that the square is empty. !!!! Eureka! -15%
     */
    public boolean isLegal(final Square move, final Player player) {
        if (move == null) {
            throw new NullPointerException("Parameter move must be not null.");
        }
        if (player == null) {
            throw new NullPointerException("Parameter player must be not null.");
        }

        /** Are almost the same. */
        //final long bitmove = bitMove(move);
        final long bitmove = 1L << move.ordinal();
        if ((bitmove & (bitboard[0] | bitboard[1])) != 0) {
            return false;
        }

        final int intPlayer = (player == Player.BLACK) ? BLACK : WHITE;

        /** The two alternative seams to be equal for performances. The second is much more clear. */
        final int x = move.ordinal() % 8;
        final int y = move.ordinal() / 8;
        /*
        final int x = move.column().ordinal();
        final int y = move.row().ordinal();
        */

        final long playerBitboard;
        final long opponentBitboard;

        if (intPlayer == WHITE) {
            playerBitboard = bitboard[1];
            opponentBitboard = bitboard[0];
        } else {
            playerBitboard = bitboard[0];
            opponentBitboard = bitboard[1];
        }

        int playerBitrow;
        int opponentBitrow;

        /** Check for flipping on row. */
        playerBitrow = (int)(playerBitboard >>> (8 * y)) & 0xFF;
        opponentBitrow = (int)(opponentBitboard >>> (8 * y)) & 0xFF;
        if (bitrowChangesForPlayer(playerBitrow, opponentBitrow, x) != playerBitrow) {
            return true;
        }

        /** Check for flipping on column. */
        playerBitrow = trasformColumnAInRow0(playerBitboard >>> x);
        opponentBitrow = trasformColumnAInRow0(opponentBitboard >>> x);
        if (bitrowChangesForPlayer(playerBitrow, opponentBitrow, y) != playerBitrow) {
            return true;
        }

        /** Check for flipping on diagonal having direction H1-A8. */
        byte shiftDistance = (byte)((x - y) << 3);
        playerBitrow = trasformDiagonalH1A8InRow0(signedLeftShift(playerBitboard, shiftDistance));
        opponentBitrow = trasformDiagonalH1A8InRow0(signedLeftShift(opponentBitboard, shiftDistance));
        if (bitrowChangesForPlayer(playerBitrow, opponentBitrow, x) != playerBitrow) {
            return true;
        }

        /** Check for flipping on diagonal having direction A1-H8. */
        shiftDistance = (byte)((7 - x - y) << 3);
        playerBitrow = trasformDiagonalA1H8InRow0(signedLeftShift(playerBitboard, shiftDistance));
        opponentBitrow = trasformDiagonalA1H8InRow0(signedLeftShift(opponentBitboard, shiftDistance));
        if (bitrowChangesForPlayer(playerBitrow, opponentBitrow, x) != playerBitrow) {
            return true;
        }

        /** If no capture on the four directions happens, return false. */
        return false;
    }

    private static int trasformColumnAInRow0(long x) {
        x &= 0x0101010101010101L;
        x |= x >> 28;
        x |= x >> 14;
        x |= x >> 7;
        return (int)x & 0xFF;
    }

    private static int trasformDiagonalH1A8InRow0(long x) {
        x &= 0x8040201008040201L;
        x |= x >> 32;
        x |= x >> 16;
        x |= x >> 8;
        return (int)x & 0xFF;
    }

    private static int trasformDiagonalA1H8InRow0(long x) {
        x &= 0x0102040810204080L;
        x |= x >> 32;
        x |= x >> 16;
        x |= x >> 8;
        return (int)x & 0xFF;
    }

    static long signedLeftShift(long x, byte signedAmount) {
        return signedAmount >= 0 ? x << signedAmount : x >>> -signedAmount;
    }

    static long lowestSquareInBoard(final long bitboard) {
        return (bitboard & (bitboard - 1)) ^ bitboard;
    }

    static int lowestSquareInRow(final int bitrow) {
        return (bitrow & (bitrow - 1)) ^ bitrow;
    }

    static int highestSquareInRow(final int bitrow) {
        if (bitrow == 0) return 0;
        int result = 1;
        int tmp = bitrow;
        if ((tmp & 0xFFFF0000) != 0) { tmp >>>= 16; result = 0x10000; }
        if (tmp > 0x000000FF) { tmp >>>= 8; result <<= 8; }
        result <<= LOG2_ARRAY[tmp];
        return result;
    }

    static int fillInBetween(final int x) {
        return ((1 << squareIntValue(x)) - 1) & ((~x & 0xFF) ^ (x - 1));
    }

    /**
     * Returns an 8-bit row representation of the player pieces after applying the move.
     * 
     * @param playerRowIndex   8-bit bitboard corrosponding to player pieces
     * @param opponentRowIndex 8-bit bitboard corrosponding to opponent pieces
     * @param movePosition     square to move
     * @return                 the new player's row index after making the move
     */
    private int bitrowChangesForPlayer(final int playerRowIndex, final int opponentRowIndex, final int movePosition) {
        final int arrayIndex = playerRowIndex | (opponentRowIndex << 8) | (movePosition << 16);
        return (int)BITROW_CHANGES_FOR_PLAYER_ARRAY[arrayIndex] & BYTE_MASK_FOR_INT;
    }

    /** Used for masking a byte when using integer values. */
    private static final int BYTE_MASK_FOR_INT = 0xFF;

    /**
     * This array is an implementation of the precomputed table that contains the effects of moving
     * a piece in any of the eigth squares in a row.
     * The size is so computed:
     *  - there are 256 arrangments of player discs,
     *  - and 256 arrangements of opponent pieces,
     *  - the potential moves are 8.
     * So the array size is 256 * 256 * 8 = 524,288 Bytes = 512kB.
     * Not all the entries are legal! The first set of eigth bits and the second one (opponent row)
     * must not set the same position. 
     * 
     * The index of the array is computed by this formula:
     * index = playerRowIndex | (opponentRowIndex << 8) | (movePosition << 16);
     */
    public static byte[] BITROW_CHANGES_FOR_PLAYER_ARRAY = initializeBitrowChangesForPlayerArray();

    /** Used to initialize the BITROW_CHANGES_FOR_PLAYER_ARRAY. */
    private static byte[] initializeBitrowChangesForPlayerArray() {

        final byte[] arrayResult = new byte[256 * 256 * 8];
        for (int playerRowIndex = 0; playerRowIndex < 256; playerRowIndex++) {
            for (int opponentRowIndex = 0; opponentRowIndex < 256; opponentRowIndex++) {
                final int notEmptyRowIndex = playerRowIndex | opponentRowIndex;
                final int emptyRowIndex = ~(notEmptyRowIndex) & BYTE_MASK_FOR_INT;
                for (int movePosition = 0; movePosition < 8; movePosition++) {
                    final int moveIndex = 1 << movePosition;
                    final int arrayResultIndex = playerRowIndex | (opponentRowIndex << 8) | (movePosition << 16);

                    int playerRowAfterMoveIndex;

                    /**
                     * It checks two conditions that cannot happen because are illegal.
                     * First player and opponent cannot have overlapping discs.
                     * Second the move cannot overlap existing discs.
                     * When either one of the two condition applys the result is set being equal to the player row index.
                     * Otherwise when black and white do not overlap, and the move is on an empy square it procede with the else block.
                     **/
                    if (((playerRowIndex & opponentRowIndex) != 0) || ((moveIndex & notEmptyRowIndex) != 0)) {
                        playerRowAfterMoveIndex = playerRowIndex;
                    } else {

                        /** The square of the move is added to the player configuration of the row after the move. */
                        playerRowAfterMoveIndex = playerRowIndex | moveIndex;

                        /**
                         * The potential bracketing disc on the right is the first player disc found moving
                         * on the left starting from the square of the move.
                         */
                        final int potentialBracketingDiscOnTheLeftIndex = highestSquareInRow(playerRowIndex & (moveIndex - 1));

                        /**
                         * The left rank is the sequence of adiacent discs that start from the bracketing disc and end
                         * with the move disc. */
                        final int leftRankIndex = fillInBetween(potentialBracketingDiscOnTheLeftIndex | moveIndex);

                        /**
                         * If the rank contains empy squares, this is a fake flip, and it doesn't do anything.
                         * If the rank is full, it cannot be full of anything different than opponent discs, so
                         * it adds the discs to the after move player configuration.
                         */
                        if ((leftRankIndex & emptyRowIndex) == 0) {
                            playerRowAfterMoveIndex |= leftRankIndex;
                        }

                        /** Here it does the same computed on the left also on the right. */
                        final int potentialBracketingDiscOnTheRightIndex = lowestSquareInRow(playerRowIndex & ~(moveIndex - 1));
                        final int rightRankIndex = fillInBetween(potentialBracketingDiscOnTheRightIndex | moveIndex);
                        if ((rightRankIndex & emptyRowIndex) == 0) {
                            playerRowAfterMoveIndex |= rightRankIndex;
                        }

                        /**
                         * It checks that the after move configuration is different from the starting one for the player.
                         * This case can happen because it never checked that the bracketing piece was not adjacent to the move disc,
                         * on such a case, on both side, the move is illegal, and it is recorded setting
                         * the result configuation appropriately.
                         */
                        if (playerRowAfterMoveIndex == (playerRowIndex | moveIndex)) {
                            playerRowAfterMoveIndex = playerRowIndex;
                        }
                    }

                    /** Asignes the computed player row index to the proper array position. */
                    arrayResult[arrayResultIndex] = (byte) playerRowAfterMoveIndex;

                }
            }
        }

        return arrayResult;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Square> legalMoves(final Player player) {

        if (LOG) callsTolegalMoves++;

        //final List<Square> legalMoves = super.legalMoves(player); 

        if (player == null) { throw new NullPointerException("Parameter player must be not null."); }
        final List<Square> legalMoves = new ArrayList<Square>(); 
        // The loop modifies likelyMoves removing the less significative bit set on each iteration.
        for (long likelyMoves = likelyMoves(player); likelyMoves != 0; likelyMoves &= likelyMoves - 1) {
            final int iSquare = squareIntValue(lowestSquareInBoard(likelyMoves));
            final Square square = Square.values()[iSquare];
            if (isLegal(square, player)) {
                legalMoves.add(square);
            }
        }
        
        /*
        final long lm = legalMoves(player.ordinal());

        final List<Square> lmSquares = new ArrayList<Square>();

        long lmEroding = lm;
        while (lmEroding != 0L) { 
            final int movePosition = BitWorks.bitscanLS1B(lmEroding);
            final Square move = Square.values()[movePosition];
            lmSquares.add(move);
            lmEroding &= ~(1L << movePosition);
        }
        */

        /*
        if (!legalMoves.equals(lmSquares)) {
            System.out.println("legalMoves=" + legalMoves + ", lmSquares=" + lmSquares);
        }
        */

        return legalMoves;
    }

    public long likelyMoves(final Player player) {
        final int intPlayer = player.ordinal(); 
        final int intOpponent = intPlayer ^ WHITE;
        final long empties = ~(bitboard[BLACK] | bitboard[WHITE]);
        // Case 1 is simple. Case 2 removes the likely moves that are on the "second crown" and are neighbor of only edge squares.
        return neighbors(bitboard[intOpponent]) & empties;
        //return ((neighbors(bitboard[intOpponent]) & EDGES_SQUARES) | neighbors(bitboard[intOpponent] & CORE_SQUARES) & empties);
    }

    private long neighbors(final long squares) {
        long neighbors = squares;
        neighbors |= (neighbors >>> 8);
        neighbors |= (neighbors >>> 1) & ALL_SQUARES_EXCEPT_COLUMN_A;
        neighbors |= (neighbors <<  1) & ALL_SQUARES_EXCEPT_COLUMN_H;
        neighbors |= (neighbors <<  8);
        return neighbors;
    }

    private long legalMoves(final int player) {
        long lm = 0L;
        int opponent = player ^ WHITE;
        long empties = ~(bitboard[BLACK] | bitboard[WHITE]);

        for (final int dir : DIRECTIONS) {
            long wave = neighbor(empties, dir) & bitboard[opponent];
            for (int shift = 2; shift < 8; shift++) {
                wave = neighbor(wave, dir);
                lm |= neighbor((wave & bitboard[player]), -dir, shift);
                wave &= bitboard[opponent];
            }
        }

        return lm;
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

    static long neighbor(final long square, final int dir, final int amount) {
        long result = square;
        for (int i = 0; i < amount; i++) {
            result = neighbor(result, dir);
        }
        return result;
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

    @Override
    Object writeReplace() {
        return super.writeReplace();
    }

}
