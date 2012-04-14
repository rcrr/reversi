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
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 *  or visit the site <http://www.gnu.org/licenses/>.
 */

package rcrr.reversi;

import java.util.Map;
import java.math.BigInteger;

public final class BitBoard extends AbstractBoard {

    private static final int BITBOARD_BLACK_INDEX = 0;
    private static final int BITBOARD_WHITE_INDEX = 1;

    private static final int[] FILE_INDEX_COEFFICIENT = new int[8]; 

    /**
     * Is the number of files that are managed.
     * There are 8 rows, 8 columns, 4 diagonals having 3 cels, 4 ones having 4 cels,
     * another four ones having respectively 4, 6, and 7 cels. Finally there are the two
     * main diagonals. Summing up 8 + 8 + 4 * 5 + 2 = 38. 
     */
    private static final int FILE_SIZE = 38;

    /**
     * The array contains the mask for each file.
     */
    private static final long[] FILE_MASK_ARRAY = new long[FILE_SIZE];

    /**
     * It MUST BE TURNED INTO PRIVATE .....
     */
    public static int index(final byte[] file) {
        assert (file != null) : "Parameter file cannot be null.";
        assert (file.length == 2) : "Parameter file must have a lenght equal to two.";
        assert ((file[0] & file[1]) == (byte) 0) : "Parameter file cannot have black and white discs overlapping.";

        // System.out.println("file[0]=" + file[0]);
        // System.out.println("file[1]=" + file[1]);

        int index = 0;
        for (int i = 0; i < 8; i++) {
            int isBlack = (file[BITBOARD_BLACK_INDEX] >>> i) & 1;
            int isWhite = (file[BITBOARD_WHITE_INDEX] >>> i) & 1;
            index += (isBlack + 2 * isWhite) * FILE_INDEX_COEFFICIENT[i];
            // System.out.println("i=" + i + ", isBlack=" + isBlack + ", isWhite=" + isWhite);
        }
        return index;
    }

    /**
     * It MUST BE TURNED INTO PRIVATE .....
     */
    public byte[] file(final int index) {
        byte[] file = new byte[2];
        // IMPLEMENTATION HERE! 
        long mask = FILE_MASK_ARRAY[index];
        return file;
    }

    /** Rows are numbered from 1 to 8. */
    private static final int FIRST_ROW   = 0;
    private static final int SECOND_ROW  = 1;
    private static final int THIRD_ROW   = 2;
    private static final int FOURTH_ROW  = 3;
    private static final int FIFTH_ROW   = 4;
    private static final int SIXTH_ROW   = 5;
    private static final int SEVENTH_ROW = 6;
    private static final int EIGHTH_ROW  = 7;

    /** Columns are numbered from A to H. */
    private static final int FIRST_COLUMN   =  8;
    private static final int SECOND_COLUMN  =  9;
    private static final int THIRD_COLUMN   = 10;
    private static final int FOURTH_COLUMN  = 11;
    private static final int FIFTH_COLUMN   = 12;
    private static final int SIXTH_COLUMN   = 13;
    private static final int SEVENTH_COLUMN = 14;
    private static final int EIGHTH_COLUMN  = 15;

    /** Diagonals are identified by first and last squares. */
    /** Diagonals having three cels. */
    private static final int A3_C1_DIAG = 16;
    private static final int F1_H3_DIAG = 17;
    private static final int H6_F8_DIAG = 18;
    private static final int C8_A6_DIAG = 19;

    /** Diagonals having four cels. */
    private static final int A4_D1_DIAG = 20;
    private static final int E1_H4_DIAG = 21;
    private static final int H5_E8_DIAG = 22;
    private static final int D8_A5_DIAG = 23;

    /** Diagonals having five cels. */
    private static final int A5_E1_DIAG = 24;
    private static final int D1_H5_DIAG = 25;
    private static final int H4_D8_DIAG = 26;
    private static final int E8_A4_DIAG = 27;

    /** Diagonals having six cels. */
    private static final int A6_F1_DIAG = 28;
    private static final int C1_H6_DIAG = 29;
    private static final int H3_C8_DIAG = 30;
    private static final int F8_A3_DIAG = 31;

    /** Diagonals having seven cels. */
    private static final int A7_G1_DIAG = 32;
    private static final int B1_H7_DIAG = 33;
    private static final int H2_B8_DIAG = 34;
    private static final int G8_A2_DIAG = 35;

    /** Diagonals having eight cels. */
    private static final int A8_H1_DIAG = 36;
    private static final int A1_H8_DIAG = 37;

    static {

        for (int i = 0; i < 8; i++) {
            FILE_INDEX_COEFFICIENT[i] = BigInteger.valueOf(3).pow(i).intValue();
        }

        FILE_MASK_ARRAY[FIRST_ROW]      =                  255L; // 0b11111111_00000000_00000000_00000000_00000000_00000000_00000000_00000000
        FILE_MASK_ARRAY[SECOND_ROW]     =                65280L; // 0b00000000_11111111_00000000_00000000_00000000_00000000_00000000_00000000
        FILE_MASK_ARRAY[THIRD_ROW]      =             16711680L; // 0b00000000_00000000_11111111_00000000_00000000_00000000_00000000_00000000
        FILE_MASK_ARRAY[FOURTH_ROW]     =           4278190080L; // 0b00000000_00000000_00000000_11111111_00000000_00000000_00000000_00000000
        FILE_MASK_ARRAY[FIFTH_ROW]      =        1095216660480L; // 0b00000000_00000000_00000000_00000000_11111111_00000000_00000000_00000000
        FILE_MASK_ARRAY[SIXTH_ROW]      =      280375465082880L; // 0b00000000_00000000_00000000_00000000_00000000_11111111_00000000_00000000
        FILE_MASK_ARRAY[SEVENTH_ROW]    =    71776119061217300L; // 0b00000000_00000000_00000000_00000000_00000000_00000000_11111111_00000000
        FILE_MASK_ARRAY[EIGHTH_ROW]     =   -72057594037927900L; // 0b00000000_00000000_00000000_00000000_00000000_00000000_00000000_11111111

        FILE_MASK_ARRAY[FIRST_COLUMN]   =    72340172838076700L; // 0b10000000_10000000_10000000_10000000_10000000_10000000_10000000_10000000
        FILE_MASK_ARRAY[SECOND_COLUMN]  =   144680345676153000L; // 0b01000000_01000000_01000000_01000000_01000000_01000000_01000000_01000000
        FILE_MASK_ARRAY[THIRD_COLUMN]   =   289360691352307000L; // 0b00100000_00100000_00100000_00100000_00100000_00100000_00100000_00100000
        FILE_MASK_ARRAY[FOURTH_COLUMN]  =   578721382704613000L; // 0b00010000_00010000_00010000_00010000_00010000_00010000_00010000_00010000
        FILE_MASK_ARRAY[FIFTH_COLUMN]   =  1157442765409230000L; // 0b00001000_00001000_00001000_00001000_00001000_00001000_00001000_00001000
        FILE_MASK_ARRAY[SIXTH_COLUMN]   =  2314885530818450000L; // 0b00000100_00000100_00000100_00000100_00000100_00000100_00000100_00000100
        FILE_MASK_ARRAY[SEVENTH_COLUMN] =  4629771061636910000L; // 0b00000010_00000010_00000010_00000010_00000010_00000010_00000010_00000010
        FILE_MASK_ARRAY[EIGHTH_COLUMN]  = -9187201950435740000L; // 0b00000001_00000001_00000001_00000001_00000001_00000001_00000001_00000001

        FILE_MASK_ARRAY[A3_C1_DIAG]     =                66052L; // 0b00100000_01000000_10000000_00000000_00000000_00000000_00000000_00000000
        FILE_MASK_ARRAY[F1_H3_DIAG]     =              8405024L; // 0b00000100_00000010_00000001_00000000_00000000_00000000_00000000_00000000
        FILE_MASK_ARRAY[H6_F8_DIAG]     =  2323998145211530000L; // 0b00000000_00000000_00000000_00000000_00000000_00000001_00000010_00000100
        FILE_MASK_ARRAY[C8_A6_DIAG]     =   288794425616761000L; // 0b00000000_00000000_00000000_00000000_00000000_10000000_01000000_00100000

        FILE_MASK_ARRAY[A4_D1_DIAG]     =             16909320L; // 0b00010000_00100000_01000000_10000000_00000000_00000000_00000000_00000000
        FILE_MASK_ARRAY[E1_H4_DIAG]     =           2151686160L; // 0b00001000_00000100_00000010_00000001_00000000_00000000_00000000_00000000
        FILE_MASK_ARRAY[H5_E8_DIAG]     =  1161999622361580000L; // 0b00000000_00000000_00000000_00000000_00000001_00000010_00000100_00001000
        FILE_MASK_ARRAY[D8_A5_DIAG]     =   577588855528489000L; // 0b00000000_00000000_00000000_00000000_10000000_01000000_00100000_00010000

        FILE_MASK_ARRAY[A5_E1_DIAG]     =           4328785936L; // 0b00001000_00010000_00100000_01000000_10000000_00000000_00000000_00000000
        FILE_MASK_ARRAY[D1_H5_DIAG]     =         550831656968L; // 0b00010000_00001000_00000100_00000010_00000001_00000000_00000000_00000000
        FILE_MASK_ARRAY[H4_D8_DIAG]     =   580999813328273000L; // 0b00000000_00000000_00000000_00000001_00000010_00000100_00001000_00010000
        FILE_MASK_ARRAY[E8_A4_DIAG]     =  1155177711073760000L; // 0b00000000_00000000_00000000_10000000_01000000_00100000_00010000_00001000

        FILE_MASK_ARRAY[A6_F1_DIAG]     =        1108169199648L; // 0b00000100_00001000_00010000_00100000_01000000_10000000_00000000_00000000
        FILE_MASK_ARRAY[C1_H6_DIAG]     =      141012904183812L; // 0b00100000_00010000_00001000_00000100_00000010_10000000_00000000_00000000
        FILE_MASK_ARRAY[H3_C8_DIAG]     =   290499906672525000L; // 0b00000000_00000000_00000001_00000010_00000100_00001000_00010000_00100000
        FILE_MASK_ARRAY[F8_A3_DIAG]     =  2310355422147580000L; // 0b00000000_00000000_10000000_01000000_00100000_00010000_00001000_00000100

        FILE_MASK_ARRAY[A7_G1_DIAG]     =      283691315109952L; // 0b00000010_00000100_00001000_00010000_00100000_01000000_10000000_00000000
        FILE_MASK_ARRAY[B1_H7_DIAG]     =    36099303471055900L; // 0b01000000_00100000_00010000_00001000_00000100_01000000_10000000_00000000
        FILE_MASK_ARRAY[H2_B8_DIAG]     =   145249953336295000L; // 0b00000000_00000001_00000010_00000100_00001000_00010000_00100000_01000000
        FILE_MASK_ARRAY[G8_A2_DIAG]     =  4620710844295150000L; // 0b00000000_10000000_01000000_00100000_00010000_00001000_00000100_00000010

        FILE_MASK_ARRAY[A8_H1_DIAG]     =    72624976668147800L; // 0b00000001_00000010_00000100_00001000_00010000_00100000_01000000_10000000
        FILE_MASK_ARRAY[A1_H8_DIAG]     = -9205322385119250000L; // 0b10000000_01000000_00100000_00010000_00001000_00000100_00000010_00000001

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
        final Board transientBoard = EnumMapBoard.valueOf(BoardUtils.bitboardToMap(this.bitboard));
        return transientBoard.isLegal(move, player);
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
