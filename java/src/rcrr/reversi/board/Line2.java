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
    A1_H1(HO,   0, 0x00, squaresAsList(A1, B1, C1, D1, E1, F1, G1, H1)),
    A2_H2(HO,  -8, 0x00, squaresAsList(A2, B2, C2, D2, E2, F2, G2, H2)),
    A3_H3(HO, -16, 0x00, squaresAsList(A3, B3, C3, D3, E3, F3, G3, H3)),
    A4_H4(HO, -24, 0x00, squaresAsList(A4, B4, C4, D4, E4, F4, G4, H4)),
    A5_H5(HO, -32, 0x00, squaresAsList(A5, B5, C5, D5, E5, F5, G5, H5)),
    A6_H6(HO, -40, 0x00, squaresAsList(A6, B6, C6, D6, E6, F6, G6, H6)),
    A7_H7(HO, -48, 0x00, squaresAsList(A7, B7, C7, D7, E7, F7, G7, H7)),
    A8_H8(HO, -56, 0x00, squaresAsList(A8, B8, C8, D8, E8, F8, G8, H8)),
    A1_A8(VE,   0, 0x00, squaresAsList(A1, A2, A3, A4, A5, A6, A7, A8)),
    B1_B8(VE,  -1, 0x00, squaresAsList(B1, B2, B3, B4, B5, B6, B7, B8)),
    C1_C8(VE,  -2, 0x00, squaresAsList(C1, C2, C3, C4, C5, C6, C7, C8)),
    D1_D8(VE,  -3, 0x00, squaresAsList(D1, D2, D3, D4, D5, D6, D7, D8)),
    E1_E8(VE,  -4, 0x00, squaresAsList(E1, E2, E3, E4, E5, E6, E7, E8)),
    F1_F8(VE,  -5, 0x00, squaresAsList(F1, F2, F3, F4, F5, F6, F7, F8)),
    G1_G8(VE,  -6, 0x00, squaresAsList(G1, G2, G3, G4, G5, G6, G7, G8)),
    H1_H8(VE,  -7, 0x00, squaresAsList(H1, H2, H3, H4, H5, H6, H7, H8)),
    A6_C8(DD, -40, 0xF8, squaresAsList(A6, B7, C8)),
    A5_D8(DD, -32, 0xF0, squaresAsList(A5, B6, C7, D8)),
    A4_E8(DD, -24, 0xE0, squaresAsList(A4, B5, C6, D7, E8)),
    A3_F8(DD, -16, 0xC0, squaresAsList(A3, B4, C5, D6, E7, F8)),
    A2_G8(DD,  -8, 0x80, squaresAsList(A2, B3, C4, D5, E6, F7, G8)),
    A1_H8(DD,   0, 0x00, squaresAsList(A1, B2, C3, D4, E5, F6, G7, H8)),
    B1_H7(DD,   8, 0x01, squaresAsList(B1, C2, D3, E4, F5, G6, H7)),
    C1_H6(DD,  16, 0x03, squaresAsList(C1, D2, E3, F4, G5, H6)),
    D1_H5(DD,  24, 0x07, squaresAsList(D1, E2, F3, G4, H5)),
    E1_H4(DD,  32, 0x0F, squaresAsList(E1, F2, G3, H4)),
    F1_H3(DD,  40, 0x1F, squaresAsList(F1, G2, H3)),
    A3_C1(DU,  40, 0xF8, squaresAsList(A3, B2, C1)),
    A4_D1(DU,  32, 0xF0, squaresAsList(A4, B3, C2, D1)),
    A5_E1(DU,  24, 0xE0, squaresAsList(A5, B4, C3, D2, E1)),
    A6_F1(DU,  16, 0xC0, squaresAsList(A6, B5, C4, D3, E2, F1)),
    A7_G1(DU,   8, 0x80, squaresAsList(A7, B6, C5, D4, E3, F2, G1)),
    A8_H1(DU,   0, 0x00, squaresAsList(A8, B7, C6, D5, E4, F3, G2, H1)),
    B8_H2(DU,  -8, 0x01, squaresAsList(B8, C7, D6, E5, F4, G3, H2)),
    C8_H3(DU, -16, 0x03, squaresAsList(C8, D7, E6, F5, G4, H3)),
    D8_H4(DU, -24, 0x07, squaresAsList(D8, E7, F6, G5, H4)),
    E8_H5(DU, -32, 0x0F, squaresAsList(E8, F7, G6, H5)),
    F8_H6(DU, -40, 0x1F, squaresAsList(F8, G7, H6));

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

    /** The list of the four corners. */
    private static List<Square> squaresAsList(Square... squares) {
        return Collections.unmodifiableList(Arrays.asList(squares));
    }

    private static byte[] initializeBitrowLegalMovesForPlayerArray() {
        final byte[] arrayResult = new byte[MAGIC_NUMBER_256 * MAGIC_NUMBER_256];
        for (int playerRow = 0; playerRow < MAGIC_NUMBER_256; playerRow++) {
            for (int opponentRow = 0; opponentRow < MAGIC_NUMBER_256; opponentRow++) {
                final int index = playerRow | (opponentRow << MAGIC_NUMBER_8);
                byte legalMoves = 0;
                for (int movePosition = 0; movePosition < MAGIC_NUMBER_8; movePosition++) {
                    final int move = 1 << movePosition;
                    legalMoves |= isLegal(playerRow, opponentRow, move);
                }
                arrayResult[index] = legalMoves;
            }
        }
        return arrayResult;
    }

    /**
     * Returns a byte set to zero when the move is not legal, returns the parameter {@code move}
     * when the move is legal.
     * The parameter {@code move} must be a power of two.
     * The parameters {@code playerRow} and {@code opponentRow} fully define the file configuration.
     * These two parameters have the first eigth bits (0 to 7 from the less significative) representing
     * the file squares, the other three bytes are meaningles. When a position has no bits set is empty,
     * when has one bit set is occupied by the respective player, when both bits are set the square is outer.
     * Files having outer squares can be real or not, real cases are files having order minor than eight
     * with outer square packed on one of the two sides. 
     * 
     *
     */
    private static byte isLegal(final int playerRow, final int opponentRow, final int move) {
        if (Integer.bitCount(move) != 1) { throw new IllegalArgumentException("Parameter move is illegal. move=" + move); }
        final byte notLegal = (byte) 0;
        final int po = playerRow   | ~BYTE_MASK_FOR_INT;
        final int oo = opponentRow | ~BYTE_MASK_FOR_INT;
        final int m = move;
        final int filled = po | oo;
        final int empties = ~filled;
        final int p = po & ~oo;
        final int o = oo & ~po;
        if ((empties & move) == 0) return notLegal; // the square is filled or outer
        int b;
        // move right
        b = (move >>> 1) & o;
        while (b > 0) {
            if ((b & p) != 0) return (byte) move;
            if ((b & o) == 0) break;
            b = b >>> 1;
        }
        // move left
        b = (move << 1) & o;
        while ((b > 0) && (b < 256)) {
            if ((b & p) != 0) return (byte) move;
            if ((b & o) == 0) break;
            b = b << 1;
        }
        return notLegal;
    }

    /** The axis field. */
    private final Axis axis;

    /**
     * The outerMask field.
     * Bits from 0 to 7 (the first byte) that are not part of the file after the transformation to row one, are set to.
     */
    private final int outerMask;

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
    private Line2(final Axis axis, final int shift, final int outerMask, final List<Square> squares) {
        this.axis = axis;
        this.shift = shift;
        this.outerMask = outerMask;
        this.squares = squares;
        this.order = this.squares.size();
        this.mask = Square.toBitmask(squares);
    }

    /** Completed. */
    public long legalMoves(final int index) {
        final int legalMovesBitrow = (int) BITROW_LEGAL_MOVES_FOR_PLAYER_ARRAY[index] & BYTE_MASK_FOR_INT;
        return BitWorks.signedLeftShift(axis().transformBackFromRowOne(legalMovesBitrow), -shift());
    }

    /** Completed. */
    public int index(final long playerBitboard, final long opponentBitboard) {
        int playerRow    = axis().transformToRowOne(BitWorks.signedLeftShift(playerBitboard,   shift())) | outerMask();
        int opponentRow  = axis().transformToRowOne(BitWorks.signedLeftShift(opponentBitboard, shift())) | outerMask();
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

    public long mask() {
        return this.mask;
    }

    public List<Square> squares() {
        return this.squares;
    }

    private int outerMask() {
        return this.outerMask;
    }

}
