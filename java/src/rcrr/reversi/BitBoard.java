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

package rcrr.reversi;

import java.util.Arrays;
import java.util.Map;
import java.math.BigInteger;

public final class BitBoard extends AbstractBoard {

    private static final int BITBOARD_BLACK_INDEX = 0;
    private static final int BITBOARD_WHITE_INDEX = 1;

    private static final int FILE_MAX_LENGTH = 8;

    private static final int SQUARE_SIZE = 64;

    private static final long[] SQUARE_LONG_VALUE = new long[SQUARE_SIZE];

    private static final int[] FILE_INDEX_COEFFICIENT = new int[FILE_MAX_LENGTH]; 

    //rivate static final int NUMBER_OF_DIRECTIONS = 8;
    final static int[] DIRECTION =  {-9, -8, -7, -1, +1, +7, +8, +9};

    /**
     * Is the number of files that are managed.
     * There are 8 rows, 8 columns, 4 diagonals having 3 cels, 4 ones having 4 cels,
     * another four ones having respectively 4, 6, and 7 cels. Finally there are the two
     * main diagonals. Summing up 8 + 8 + 4 * 5 + 2 = 38. 
     */
    private static final int FILE_SIZE = 38;

    /**
     * The array contains the mask for each file. Values are assigned in the static class block.
     * Values are assigned as a long value. In the same line as a comment there is the binary representation
     * that would require JDK 1.7 or higher.
     * The long value is calculated with the help of a calc sheet mantained in the file: Reversi-BITBOARD-design.ods
     */
    private static final long[] FILE_MASK_ARRAY = new long[FILE_SIZE];

    /**
     * The array contains the length of each file (the number of squares).
     */
    private static final int[] FILE_LENGTH_ARRAY = new int[FILE_SIZE];


    /**
     * It MUST BE TURNED INTO PRIVATE .....
     */
    public static int index(final byte[] file) {
        assert (file != null) : "Parameter file cannot be null.";
        assert (file.length == 2) : "Parameter file must have a lenght equal to two.";
        assert ((file[0] & file[1]) == (byte) 0) : "Parameter file cannot have black and white discs overlapping.";

        int index = 0;
        for (int i = 0; i < FILE_MAX_LENGTH; i++) {
            int isBlack = (file[BITBOARD_BLACK_INDEX] >>> i) & 1;
            int isWhite = (file[BITBOARD_WHITE_INDEX] >>> i) & 1;
            index += (isBlack + 2 * isWhite) * FILE_INDEX_COEFFICIENT[i];
        }
        return index;
    }

    /**
     * It MUST BE TURNED INTO PRIVATE .....
     */
    public final byte[] file(final int index) {
        assert (index >= 0 || index < FILE_SIZE) : "Parameter index must be in the range 0...FILE_SIZE.";
        byte[] file = new byte[2];
        long mask = FILE_MASK_ARRAY[index];
        int j = 0;
        file_completed:
        for (int i = 0; i < SQUARE_SIZE; i++) {
            if (j > 7) break file_completed;
            long bit = 1L << i;
            byte pointInFile = (byte) (1 << j);
            if ((mask & bit) != 0L) {
                if ((bitboard[0] & bit) != 0L) { file[0] |= pointInFile; }
                if ((bitboard[1] & bit) != 0L) { file[1] |= pointInFile; }
                j++;
            }
        }
        return file;
    }

    public static final String fileToString(final byte file[]) {
        assert (file != null) : "Parameter file cannot be null.";
        assert (file.length == 2) : "Parameter file must have a lenght equal to two.";
        assert ((file[0] & file[1]) == (byte) 0) : "Parameter file cannot have black and white discs overlapping.";

        final StringBuffer sb = new StringBuffer();
        sb.append("[ ");
        for (int i = 0; i < FILE_MAX_LENGTH; i++) {
            byte pointInFile = (byte) (1 << i);
            if ((file[0] & pointInFile) != 0 ) {
                sb.append('@');
            } else if ((file[1] & pointInFile) != 0 ) {
                sb.append('O');
            } else {
                sb.append('.');
            }
            sb.append(' ');
        }
        sb.append(']');
        return sb.toString();
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

    /** Rows are numbered from 1 to 8. */
    static final int FIRST_ROW   = 0;
    static final int SECOND_ROW  = 1;
    static final int THIRD_ROW   = 2;
    static final int FOURTH_ROW  = 3;
    static final int FIFTH_ROW   = 4;
    static final int SIXTH_ROW   = 5;
    static final int SEVENTH_ROW = 6;
    static final int EIGHTH_ROW  = 7;

    /** Columns are numbered from A to H. */
    static final int FIRST_COLUMN   =  8;
    static final int SECOND_COLUMN  =  9;
    static final int THIRD_COLUMN   = 10;
    static final int FOURTH_COLUMN  = 11;
    static final int FIFTH_COLUMN   = 12;
    static final int SIXTH_COLUMN   = 13;
    static final int SEVENTH_COLUMN = 14;
    static final int EIGHTH_COLUMN  = 15;

    /** Diagonals are identified by first and last squares. */
    /** Diagonals having three cels. */
    static final int A3_C1_DIAG = 16;
    static final int F1_H3_DIAG = 17;
    static final int H6_F8_DIAG = 18;
    static final int C8_A6_DIAG = 19;

    /** Diagonals having four cels. */
    static final int A4_D1_DIAG = 20;
    static final int E1_H4_DIAG = 21;
    static final int H5_E8_DIAG = 22;
    static final int D8_A5_DIAG = 23;

    /** Diagonals having five cels. */
    static final int A5_E1_DIAG = 24;
    static final int D1_H5_DIAG = 25;
    static final int H4_D8_DIAG = 26;
    static final int E8_A4_DIAG = 27;

    /** Diagonals having six cels. */
    static final int A6_F1_DIAG = 28;
    static final int C1_H6_DIAG = 29;
    static final int H3_C8_DIAG = 30;
    static final int F8_A3_DIAG = 31;

    /** Diagonals having seven cels. */
    static final int A7_G1_DIAG = 32;
    static final int B1_H7_DIAG = 33;
    static final int H2_B8_DIAG = 34;
    static final int G8_A2_DIAG = 35;

    /** Diagonals having eight cels. */
    static final int A8_H1_DIAG = 36;
    static final int A1_H8_DIAG = 37;

    static final long fileMaskArray(final int file) {
        if (file < 0 || file > FILE_SIZE) { throw new IllegalArgumentException("Parameter file out of range."); }
        return FILE_MASK_ARRAY[file];
    }

    static final long squareLongValue(final int square) {
        if (square < 0 || square > SQUARE_SIZE) { throw new IllegalArgumentException("Parameter square out of range."); }
        return SQUARE_LONG_VALUE[square];
    }

    static final String longToString(final long value) {
        final StringBuilder sb = new StringBuilder();
        final String toBinaryString = Long.toBinaryString(value);
        final int len = toBinaryString.length();
        final int missingZeroes = SQUARE_SIZE - len;
        for (int i = SQUARE_SIZE; i > 0; i--) {
            if ((i != SQUARE_SIZE) && (i % FILE_MAX_LENGTH == 0)) sb.append(".");
            if ((missingZeroes - (SQUARE_SIZE - i)) > 0) {
                sb.append('0');
            } else {
                sb.append(toBinaryString.charAt(len - i));
            }
        }
        return sb.toString();
    }

    static final boolean hasDuplicates(final int ... numbers) {
        boolean result = false;
        for (int i = 0; i < numbers.length; i++) {
            for (int j = i + 1; j < numbers.length; j++) {
                if (numbers[i] == numbers[j]) { result = true;}
            }
        }
        return result;
    }

    static final long sumOfSquareLongValue(final int ... squares) {
        if (hasDuplicates(squares)) {
            throw new IllegalArgumentException("Parameter squares has duplicates.");
        }
        long total = 0L;
        for (int i = 0; i < squares.length; i++) {
            final int square = squares[i];
            if (square < 0 || square > SQUARE_SIZE) {
                throw new IllegalArgumentException("Parameter squares has a wrong member: " + square);
            }
            total += SQUARE_LONG_VALUE[square];
        }
        return total;
    }

    private static final long[] BIT_MOVE = new long[SQUARE_SIZE];

    static final long bitMove(final Square move) {
        return BIT_MOVE[move.ordinal()];
    }

    static {

        for (int i = 0; i < SQUARE_SIZE; i++) {
            BIT_MOVE[i] = 1L << i;
        }

        for (int i = 0; i < FILE_MAX_LENGTH; i++) {
            FILE_INDEX_COEFFICIENT[i] = BigInteger.valueOf(3).pow(i).intValue();
        }

        for (int i = 0; i < SQUARE_SIZE; i++) {
            SQUARE_LONG_VALUE[i] = BigInteger.valueOf(2).pow(i).longValue();
        }

        FILE_MASK_ARRAY[FIRST_ROW]   = sumOfSquareLongValue(A1, B1, C1, D1, E1, F1, G1, H1);
        FILE_MASK_ARRAY[SECOND_ROW]  = sumOfSquareLongValue(A2, B2, C2, D2, E2, F2, G2, H2);
        FILE_MASK_ARRAY[THIRD_ROW]   = sumOfSquareLongValue(A3, B3, C3, D3, E3, F3, G3, H3);
        FILE_MASK_ARRAY[FOURTH_ROW]  = sumOfSquareLongValue(A4, B4, C4, D4, E4, F4, G4, H4);
        FILE_MASK_ARRAY[FIFTH_ROW]   = sumOfSquareLongValue(A5, B5, C5, D5, E5, F5, G5, H5);
        FILE_MASK_ARRAY[SIXTH_ROW]   = sumOfSquareLongValue(A6, B6, C6, D6, E6, F6, G6, H6);
        FILE_MASK_ARRAY[SEVENTH_ROW] = sumOfSquareLongValue(A7, B7, C7, D7, E7, F7, G7, H7);
        FILE_MASK_ARRAY[EIGHTH_ROW]  = sumOfSquareLongValue(A8, B8, C8, D8, E8, F8, G8, H8);

        FILE_MASK_ARRAY[FIRST_COLUMN]   = sumOfSquareLongValue(A1, A2, A3, A4, A5, A6, A7, A8);
        FILE_MASK_ARRAY[SECOND_COLUMN]  = sumOfSquareLongValue(B1, B2, B3, B4, B5, B6, B7, B8);
        FILE_MASK_ARRAY[THIRD_COLUMN]   = sumOfSquareLongValue(C1, C2, C3, C4, C5, C6, C7, C8);
        FILE_MASK_ARRAY[FOURTH_COLUMN]  = sumOfSquareLongValue(D1, D2, D3, D4, D5, D6, D7, D8);
        FILE_MASK_ARRAY[FIFTH_COLUMN]   = sumOfSquareLongValue(E1, E2, E3, E4, E5, E6, E7, E8); 
        FILE_MASK_ARRAY[SIXTH_COLUMN]   = sumOfSquareLongValue(F1, F2, F3, F4, F5, F6, F7, F8);
        FILE_MASK_ARRAY[SEVENTH_COLUMN] = sumOfSquareLongValue(G1, G2, G3, G4, G5, G6, G7, G8);
        FILE_MASK_ARRAY[EIGHTH_COLUMN]  = sumOfSquareLongValue(H1, H2, H3, H4, H5, H6, H7, H8);

        FILE_MASK_ARRAY[A3_C1_DIAG] = sumOfSquareLongValue(A3, B2, C1);
        FILE_MASK_ARRAY[F1_H3_DIAG] = sumOfSquareLongValue(F1, G2, H3);
        FILE_MASK_ARRAY[H6_F8_DIAG] = sumOfSquareLongValue(H6, G7, F8);
        FILE_MASK_ARRAY[C8_A6_DIAG] = sumOfSquareLongValue(C8, B7, A6);

        FILE_MASK_ARRAY[A4_D1_DIAG] = sumOfSquareLongValue(A4, B3, C2, D1);
        FILE_MASK_ARRAY[E1_H4_DIAG] = sumOfSquareLongValue(E1, F2, G3, H4);
        FILE_MASK_ARRAY[H5_E8_DIAG] = sumOfSquareLongValue(H5, G6, F7, E8);
        FILE_MASK_ARRAY[D8_A5_DIAG] = sumOfSquareLongValue(D8, C7, B6, A5);

        FILE_MASK_ARRAY[A5_E1_DIAG] = sumOfSquareLongValue(A5, B4, C3, D2, E1);
        FILE_MASK_ARRAY[D1_H5_DIAG] = sumOfSquareLongValue(D1, E2, F3, G4, H5);
        FILE_MASK_ARRAY[H4_D8_DIAG] = sumOfSquareLongValue(H4, G5, F6, E7, D8);
        FILE_MASK_ARRAY[E8_A4_DIAG] = sumOfSquareLongValue(E8, D7, C6, B5, A4);

        FILE_MASK_ARRAY[A6_F1_DIAG] = sumOfSquareLongValue(A6, B5, C4, D3, E2, F1);
        FILE_MASK_ARRAY[C1_H6_DIAG] = sumOfSquareLongValue(C1, D2, E3, F4, G5, H6);
        FILE_MASK_ARRAY[H3_C8_DIAG] = sumOfSquareLongValue(H3, G4, F5, E6, D7, C8);
        FILE_MASK_ARRAY[F8_A3_DIAG] = sumOfSquareLongValue(F8, E7, D6, C5, B4, A3);

        FILE_MASK_ARRAY[A7_G1_DIAG] = sumOfSquareLongValue(A7, B6, C5, D4, E3, F2, G1);
        FILE_MASK_ARRAY[B1_H7_DIAG] = sumOfSquareLongValue(B1, C2, D3, E4, F5, G6, H7);
        FILE_MASK_ARRAY[H2_B8_DIAG] = sumOfSquareLongValue(H2, G3, F4, E5, D6, C7, B8);
        FILE_MASK_ARRAY[G8_A2_DIAG] = sumOfSquareLongValue(G8, F7, E6, D5, C4, B3, A2);

        FILE_MASK_ARRAY[A8_H1_DIAG] = sumOfSquareLongValue(A8, B7, C6, D5, E4, F3, G2, H1);
        FILE_MASK_ARRAY[A1_H8_DIAG] = sumOfSquareLongValue(A1, B2, C3, D4, E5, F6, G7, H8);

        for (int i = 0; i < FILE_SIZE; i++) {
            FILE_LENGTH_ARRAY[i] = Long.bitCount(FILE_MASK_ARRAY[i]);
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
        if (squares == null) { throw new NullPointerException("Parameter squares cannot be null."); }
        if (squares.size() != Square.values().length) {
            throw new IllegalArgumentException("Parameter squares size is not consistent."
                                               + " squares.size()=" + squares.size()
                                               + " expected value: " + Square.values().length);
        }
        if (squares.containsKey(null)) {
            throw new NullPointerException("Parameter squares cannot have null keys. squares=" + squares);
        }
        if (squares.containsValue(null)) {
            throw new NullPointerException("Parameter squares cannot have null values. squares=" + squares);
        }
        return new BitBoard(BoardUtils.mapToBitboard(squares));
    }

    /**
     * The bitboard field.
     */
    private final long[] bitboard;

    /** The squares field ***MUST BE REMOVED****. */
    private final transient Map<Square, SquareState> squares;

    /** The squares field ***MUST BE REMOVED****. */
    private final transient Board transientBoard;

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
        assert ((bitboard[0] & bitboard[1]) == 0L) : "Parameter bitboard cannot have black and white discs overlapping.";
        this.bitboard = bitboard.clone();
        this.squares = BoardUtils.bitboardToMap(this.bitboard);
        this.transientBoard = EnumMapBoard.valueOf(this.squares);
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
        return (square == null) ? SquareState.OUTER : squares.get(square);
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
        /** MUST BE REWRITTEN! */
        final long bitmove = bitMove(move);
        if ((bitmove & (bitboard[0] & bitboard[1])) != 0) { return false; }

        for (int dir : DIRECTION) {
            if(wouldFlip(bitmove, player, dir) != 0L) { return true; }
        }
        /*
        for (Direction dir : Direction.values()) {
            if (wouldFlip(move, player, dir) != null) { return true; }
        }
        */
        final Board transientBoard = EnumMapBoard.valueOf(BoardUtils.bitboardToMap(this.bitboard));
        return transientBoard.isLegal(move, player);
    }

    private long wouldFlip(final long move, final Player player, final int dir) {
        assert (Long.bitCount(move) == 1) : "Argument move must be have one and only one bit set";
        assert (player != null) : "Argument player must be not null";
        assert (Arrays.binarySearch(DIRECTION, dir) >= 0) : "Argument dir must be contained in the DIRECTION array";
        long bracketing = 0L;
        /*
        Square neighbor = move.neighbors().get(dir);
        Square bracketing = null;
        if (get(neighbor) == player.opponent().color()) {
            Square next = neighbor.neighbors().get(dir);
            if (next != null) { bracketing = findBracketingPiece(next, player, dir); }
        }
        */
        return bracketing;
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
        return transientBoard.makeMove(move, player);
    }

    /**
     * Returns the disk count for the color.
     *
     * @param color the color for which the disk count is computed
     * @return the disk count
     * @throws NullPointerException if parameter {@code color} is null
     */
    public int countPieces(final SquareState color) {
        if (color == null) {
            throw new NullPointerException("parameter color must be not null.");
        }
        switch (color) {
        case BLACK: return Long.bitCount(bitboard[BITBOARD_BLACK_INDEX]);
        case WHITE: return Long.bitCount(bitboard[BITBOARD_WHITE_INDEX]);
        case EMPTY: return 64 - Long.bitCount(bitboard[BITBOARD_BLACK_INDEX] | bitboard[BITBOARD_WHITE_INDEX]);
        case OUTER: return 0;
        default: throw new IllegalArgumentException("Undefined value for color parameter. color=" + color);
        }
    }

}
