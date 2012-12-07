/*
 *  Line2.java
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

package rcrr.reversi.board;

import java.math.BigInteger;

import java.util.EnumSet;
import java.util.Map;
import java.util.EnumMap;
import java.util.Arrays;
import java.util.Collections;
import java.util.ArrayList;
import java.util.List;

import static rcrr.reversi.board.Axis.*;
import static rcrr.reversi.board.Square.*;

/**
 * The {@code Line} enum defines a line of the board game.
 */
public enum Line2 {
    A1_H1(HO,   0, squaresAsList(A1, B1, C1, D1, E1, F1, G1, H1)),
    A2_H2(HO,  -8, squaresAsList(A2, B2, C2, D2, E2, F2, G2, H2)),
    A3_H3(HO, -16, squaresAsList(A3, B3, C3, D3, E3, F3, G3, H3)),
    A4_H4(HO, -24, squaresAsList(A4, B4, C4, D4, E4, F4, G4, H4)),
    A5_H5(HO, -32, squaresAsList(A5, B5, C5, D5, E5, F5, G5, H5)),
    A6_H6(HO, -40, squaresAsList(A6, B6, C6, D6, E6, F6, G6, H6)),
    A7_H7(HO, -48, squaresAsList(A7, B7, C7, D7, E7, F7, G7, H7)),
    A8_H8(HO, -56, squaresAsList(A8, B8, C8, D8, E8, F8, G8, H8)),
    A1_A8(VE,   0, squaresAsList(A1, A2, A3, A4, A5, A6, A7, A8)),
    B1_B8(VE,  -1, squaresAsList(B1, B2, B3, B4, B5, B6, B7, B8)),
    C1_C8(VE,  -2, squaresAsList(C1, C2, C3, C4, C5, C6, C7, C8)),
    D1_D8(VE,  -3, squaresAsList(D1, D2, D3, D4, D5, D6, D7, D8)),
    E1_E8(VE,  -4, squaresAsList(E1, E2, E3, E4, E5, E6, E7, E8)),
    F1_F8(VE,  -5, squaresAsList(F1, F2, F3, F4, F5, F6, F7, F8)),
    G1_G8(VE,  -6, squaresAsList(G1, G2, G3, G4, G5, G6, G7, G8)),
    H1_H8(VE,  -7, squaresAsList(H1, H2, H3, H4, H5, H6, H7, H8)),
    A6_C8(DD, -40, squaresAsList(A6, B7, C8)),
    A5_D8(DD, -32, squaresAsList(A5, B6, C7, D8)),
    A4_E8(DD, -24, squaresAsList(A4, B5, C6, D7, E8)),
    A3_F8(DD, -16, squaresAsList(A3, B4, C5, D6, E7, F8)),
    A2_G8(DD,  -8, squaresAsList(A2, B3, C4, D5, E6, F7, G8)),
    A1_H8(DD,   0, squaresAsList(A1, B2, C3, D4, E5, F6, G7, H8)),
    B1_H7(DD,   8, squaresAsList(B1, C2, D3, E4, F5, G6, H7)),
    C1_H6(DD,  16, squaresAsList(C1, D2, E3, F4, G5, H6)),
    D1_H5(DD,  24, squaresAsList(D1, E2, F3, G4, H5)),
    E1_H4(DD,  32, squaresAsList(E1, F2, G3, H4)),
    F1_H3(DD,  40, squaresAsList(F1, G2, H3)),
    A3_C1(DU,  40, squaresAsList(A3, B2, C1)),
    A4_D1(DU,  32, squaresAsList(A4, B3, C2, D1)),
    A5_E1(DU,  24, squaresAsList(A5, B4, C3, D2, E1)),
    A6_F1(DU,  16, squaresAsList(A6, B5, C4, D3, E2, F1)),
    A7_G1(DU,   8, squaresAsList(A7, B6, C5, D4, E3, F2, G1)),
    A8_H1(DU,   0, squaresAsList(A8, B7, C6, D5, E4, F3, G2, H1)),
    B8_H2(DU,  -8, squaresAsList(B8, C7, D6, E5, F4, G3, H2)),
    C8_H3(DU, -16, squaresAsList(C8, D7, E6, F5, G4, H3)),
    D8_H4(DU, -24, squaresAsList(D8, E7, F6, G5, H4)),
    E8_H5(DU, -32, squaresAsList(E8, F7, G6, H5)),
    F8_H6(DU, -40, squaresAsList(F8, G7, H6));

    /** The null instance. */
    public static final Line2 NULL = null;

    /** The number of lines. */
    public static final int NUMBER_OF = values().length;

    /** A generic line instance. */
    public static final Line2 AN_INSTANCE = B8_H2;

    /** Used for masking a byte when using integer values. */
    static final int BYTE_MASK_FOR_INT = 0xFF;

    /** Macic number 8. */
    private static final int MAGIC_NUMBER_8 = 8;

    /** Macic number 256. */
    private static final int MAGIC_NUMBER_256 = 256;

    /** The list of the four corners. */
    private static List<Square> squaresAsList(Square... squares) {
        return Collections.unmodifiableList(Arrays.asList(squares));
    }

    /** The axis field. */
    private final Axis axis;

    /** The order field. */
    private final int order;

    /** The mask field. */
    private final long mask;

    /** The shift field. */
    private final int shift;

    /** The squares field. */
    private final List<Square> squares;

    /**
     * Enum constructor.
     *
     * @param squares the list of squares
     */
    private Line2(final Axis axis, final int shift, final List<Square> squares) {
        this.axis = axis;
        this.shift = shift;
        this.squares = squares;
        this.order = this.squares.size();
        this.mask = Square.toBitmask(squares);

        /* TESTS
        Map<Square, SquareState> mSquares = new EnumMap<Square, SquareState>(Square.class);
        for (final Square sq : Square.values()) {
            if (squares.contains(sq)) {
                mSquares.put(sq, SquareState.BLACK);
            } else {
                mSquares.put(sq, SquareState.EMPTY);
            }
        }
        final long[] bitboard = new long[] {mask, 0L};

        System.out.printf("Line2(%2d): %s\n", this.ordinal(), this);
        //System.out.printf("%s", BoardFactoryHolder.getInstance().boardFactory().valueOf(mSquares).printBoard());
        System.out.printf("%s", BitBoard3.valueOf(bitboard).printBoard());
        System.out.printf("\n");
        */
    }

    /** TO BE IMPLEMENTED. */
    public long legalMoves(final int index) {
        return (int) BITROW_LEGAL_MOVES_FOR_PLAYER_ARRAY[index] & BYTE_MASK_FOR_INT;
    }

    /** TO BE IMPLEMENTED. */
    public int index(final long playerBitboard, final long opponentBitboard) {
        int playerRow    = axis().transformToRowOne(BitWorks.signedLeftShift(playerBitboard,   shift()));
        int opponentRow  = axis().transformToRowOne(BitWorks.signedLeftShift(opponentBitboard, shift()));
        // insert the set of outer bits as (1, 1)
        return playerRow | (opponentRow << MAGIC_NUMBER_8);
    }

    public Axis axis() {
        return this.axis;
    }

    public int shift() {
        return this.shift;
    }

    public int order() {
        return this.order;
    }

    public List<Square> squares() {
        return this.squares;
    }

    /**
     * This array is an implementation of the precomputed table that contains the legal moves for the line.
     * The size is so computed:
     *  - there are 256 arrangments of player discs,
     *  - and 256 arrangements of opponent pieces.
     * So the array size is 256 * 256 = 65,536 Bytes = 64kB.
     * The lines shorter than 8 have the outer squares marked by having the bit set either for black and white.
     *
     * The index of the array is computed by this formula:
     * index = playerRow | (opponentRow << 8);
     */
    private static final byte[] BITROW_LEGAL_MOVES_FOR_PLAYER_ARRAY = initializeBitrowLegalMovesForPlayerArray();

    private static byte[] initializeBitrowLegalMovesForPlayerArray() {

        final byte[] arrayResult = new byte[MAGIC_NUMBER_256 * MAGIC_NUMBER_256];
        for (int playerRow = 0; playerRow < MAGIC_NUMBER_256; playerRow++) {
            for (int opponentRow = 0; opponentRow < MAGIC_NUMBER_256; opponentRow++) {
                // MUST BE COMPLETED .....
                ;
            }
        }

        return arrayResult;
    }

}
