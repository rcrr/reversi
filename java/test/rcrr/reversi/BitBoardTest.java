/*
 *  BitBoardTest.java
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
import java.util.HashMap;

import org.junit.Test;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for {@code BitBoard} class.
 */
public class BitBoardTest {

    /** Class constructor. */
    public BitBoardTest() { }

    @Test
    public final void testDummy() {

        int shift = 9;
        long bitmove = BitBoard.squareLongValue(BitBoard.A1);
        long result = bitmove << shift;

        System.out.println("shift=" + shift);
        System.out.println("bitmove = " + BitBoard.longToString(bitmove));
        System.out.println("result  = " + BitBoard.longToString(result));

        assertThat("To be developed.",
                   true,
                   is(true));

    }

    @Test
    public final void testIsLegal() {

        final Map<Square, SquareState> squares = new HashMap<Square, SquareState>();
        for (final Square sq : Square.values()) {
            squares.put(sq, BoardFixtures.BLACK_HAS_TO_PASS.get(sq));
        }
        final Board board = BitBoard.valueOf(squares);
        final BitBoard bitboard = (BitBoard) board;

        assertThat("bitboard.isLegal(Square.H7, Player.WHITE) is true.",
                   bitboard.isLegal(Square.H7, Player.WHITE),
                   is(true));

        assertThat("bitboard.isLegal(Square.A4, Player.WHITE) is true.",
                   bitboard.isLegal(Square.A4, Player.WHITE),
                   is(true));

        assertThat("bitboard.isLegal(Square.A1, Player.WHITE) is true.",
                   bitboard.isLegal(Square.A1, Player.WHITE),
                   is(false));

        assertThat("bitboard.isLegal(Square.A1, Player.BLACK) is true.",
                   bitboard.isLegal(Square.A1, Player.BLACK),
                   is(false));

    }

    @Test
    public final void testNeighbor() {

        assertThat("BitBoard.neighbor(BitBoard.squareLongValue(BitBoard.A1), BitBoard.DIR_EE) is BitBoard.squareLongValue(BitBoard.B1)).",
                   BitBoard.neighbor(BitBoard.squareLongValue(BitBoard.A1), BitBoard.DIR_EE),
                   is(BitBoard.squareLongValue(BitBoard.B1)));

        assertThat("BitBoard.neighbor(BitBoard.squareLongValue(BitBoard.A1), BitBoard.DIR_NN) is 0L.",
                   BitBoard.neighbor(BitBoard.squareLongValue(BitBoard.A1), BitBoard.DIR_NN),
                   is(0L));

        assertThat("BitBoard.neighbor(BitBoard.squareLongValue(BitBoard.C4), BitBoard.DIR_NW) is BitBoard.squareLongValue(BitBoard.B3)).",
                   BitBoard.neighbor(BitBoard.squareLongValue(BitBoard.C4), BitBoard.DIR_NW),
                   is(BitBoard.squareLongValue(BitBoard.B3)));

    }

    @Test
    public final void testSquareRow() {

        assertThat("BitBoard.squareRow(BitBoard.A1) is BitBoard.ROW_1.",
                   BitBoard.squareRow(BitBoard.A1),
                   is(BitBoard.ROW_1));

        assertThat("BitBoard.squareRow(BitBoard.B3) is BitBoard.ROW_3.",
                   BitBoard.squareRow(BitBoard.B3),
                   is(BitBoard.ROW_3));

        assertThat("BitBoard.squareRow(BitBoard.H8) is BitBoard.ROW_8.",
                   BitBoard.squareRow(BitBoard.H8),
                   is(BitBoard.ROW_8));

    }

    @Test
    public final void testSquareColumn() {

        assertThat("BitBoard.squareColumn(BitBoard.A1) is BitBoard.COL_A.",
                   BitBoard.squareColumn(BitBoard.A1),
                   is(BitBoard.COL_A));

        assertThat("BitBoard.squareColumn(BitBoard.B3) is BitBoard.COL_B.",
                   BitBoard.squareColumn(BitBoard.B3),
                   is(BitBoard.COL_B));

        assertThat("BitBoard.squareColumn(BitBoard.H8) is BitBoard.COL_H.",
                   BitBoard.squareColumn(BitBoard.H8),
                   is(BitBoard.COL_H));

    }


    @Test
    public final void testSquareBelongsToEdge() {

        assertThat("BitBoard.squareBelongsToEdge(BitBoard.A1, BitBoard.EDGE_N) is true.",
                   BitBoard.squareBelongsToEdge(BitBoard.A1, BitBoard.EDGE_N),
                   is(true));

        assertThat("BitBoard.squareBelongsToEdge(BitBoard.A1, BitBoard.EDGE_W) is true.",
                   BitBoard.squareBelongsToEdge(BitBoard.A1, BitBoard.EDGE_W),
                   is(true));

        assertThat("BitBoard.squareBelongsToEdge(BitBoard.A1, BitBoard.EDGE_S) is false.",
                   BitBoard.squareBelongsToEdge(BitBoard.A1, BitBoard.EDGE_S),
                   is(false));

        assertThat("BitBoard.squareBelongsToEdge(BitBoard.H4, BitBoard.EDGE_S) is false.",
                   BitBoard.squareBelongsToEdge(BitBoard.H4, BitBoard.EDGE_S),
                   is(false));

        assertThat("BitBoard.squareBelongsToEdge(BitBoard.H4, BitBoard.EDGE_E) is true.",
                   BitBoard.squareBelongsToEdge(BitBoard.H4, BitBoard.EDGE_E),
                   is(true));
    }


    @Test
    public final void testFile() {

        final Map<Square, SquareState> squares = new HashMap<Square, SquareState>();
        for (final Square sq : Square.values()) {
            squares.put(sq, BoardFixtures.BLACK_HAS_TO_PASS.get(sq));
        }
        final Board board = BitBoard.valueOf(squares);
        final BitBoard bitboard = (BitBoard) board;

        assertThat("BoardFixtures.BLACK_HAS_TO_PASS FIRST_ROW fileToString is [ O @ . @ . O . . ].",
                   BitBoard.fileToString(bitboard.file(BitBoard.FIRST_ROW)),
                   is("[ O @ . @ . O . . ]"));

        assertThat("BoardFixtures.BLACK_HAS_TO_PASS SECOND_ROW fileToString is [ @ @ @ @ @ @ @ O ].",
                   BitBoard.fileToString(bitboard.file(BitBoard.SECOND_ROW)),
                   is("[ @ @ @ @ @ @ @ O ]"));

        assertThat("BoardFixtures.BLACK_HAS_TO_PASS FIRST_ROW fileToString is [ . @ @ O @ O @ O ].",
                   BitBoard.fileToString(bitboard.file(BitBoard.A8_H1_DIAG)),
                   is("[ . @ @ O @ O @ O ]"));

    }

    @Test
    public final void testLongToString() {

        assertThat("BitBoard.longToString(1L) is 00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(0L),
                   is("00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(1L) is 00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000001.",
                   BitBoard.longToString(1L),
                   is("00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000001"));

        assertThat("BitBoard.longToString(2L) is 00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000010.",
                   BitBoard.longToString(2L),
                   is("00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000010"));

        assertThat("BitBoard.longToString(1L) is 11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111.",
                   BitBoard.longToString(-1L),
                   is("11111111.11111111.11111111.11111111.11111111.11111111.11111111.11111111"));

        assertThat("BitBoard.longToString(1L) is 10000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(-9223372036854775808L),
                   is("10000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000"));

    }

    @Test
    public final void testIndex() {

        assertThat("BitBoard.index(new byte[] {16, 65}) is 1541.",
                   BitBoard.index(new byte[] {16, 65}),
                   is(1541));

        assertThat("BitBoard.index(new byte[] {0, -1}) is 6560.",
                   BitBoard.index(new byte[] {0, -1}),
                   is(6560));

    }

    @Test
    public final void testFileMaskArray() {

        /**
         *             8        7        6        5        4        3        2        1
         *             HGFEDCBA.HGFEDCBA.HGFEDCBA.HGFEDCBA.HGFEDCBA.HGFEDCBA.HGFEDCBA.HGFEDCBA
         */

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FIRST_ROW)) is"
                   + " 00000000.00000000.00000000.00000000.00000000.00000000.00000000.11111111.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FIRST_ROW)),
                   is("00000000.00000000.00000000.00000000.00000000.00000000.00000000.11111111"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SECOND_ROW)) is"
                   + " 00000000.00000000.00000000.00000000.00000000.00000000.11111111.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SECOND_ROW)),
                   is("00000000.00000000.00000000.00000000.00000000.00000000.11111111.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.THIRD_ROW)) is"
                   + " 00000000.00000000.00000000.00000000.00000000.11111111.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.THIRD_ROW)),
                   is("00000000.00000000.00000000.00000000.00000000.11111111.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FOURTH_ROW)) is "
                   + " 00000000.00000000.00000000.00000000.11111111.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FOURTH_ROW)),
                   is("00000000.00000000.00000000.00000000.11111111.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FIFTH_ROW)) is"
                   + " 00000000.00000000.00000000.11111111.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FIFTH_ROW)),
                   is("00000000.00000000.00000000.11111111.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SIXTH_ROW)) is"
                   + " 00000000.00000000.11111111.00000000.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SIXTH_ROW)),
                   is("00000000.00000000.11111111.00000000.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SEVENTH_ROW)) is"
                   + " 00000000.11111111.00000000.00000000.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SEVENTH_ROW)),
                   is("00000000.11111111.00000000.00000000.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.EIGHTH_ROW)) is"
                   + " 11111111.00000000.00000000.00000000.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.EIGHTH_ROW)),
                   is("11111111.00000000.00000000.00000000.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FIRST_COLUMN)) is"
                   + " 00000001.00000001.00000001.00000001.00000001.00000001.00000001.00000001.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FIRST_COLUMN)),
                   is("00000001.00000001.00000001.00000001.00000001.00000001.00000001.00000001"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SECOND_COLUMN)) is"
                   + " 00000010.00000010.00000010.00000010.00000010.00000010.00000010.00000010.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SECOND_COLUMN)),
                   is("00000010.00000010.00000010.00000010.00000010.00000010.00000010.00000010"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.THIRD_COLUMN)) is"
                   + " 00000100.00000100.00000100.00000100.00000100.00000100.00000100.00000100.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.THIRD_COLUMN)),
                   is("00000100.00000100.00000100.00000100.00000100.00000100.00000100.00000100"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FOURTH_COLUMN)) is"
                   + " 00001000.00001000.00001000.00001000.00001000.00001000.00001000.00001000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FOURTH_COLUMN)),
                   is("00001000.00001000.00001000.00001000.00001000.00001000.00001000.00001000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FIFTH_COLUMN)) is"
                   + " 00010000.00010000.00010000.00010000.00010000.00010000.00010000.00010000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.FIFTH_COLUMN)),
                   is("00010000.00010000.00010000.00010000.00010000.00010000.00010000.00010000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SIXTH_COLUMN)) is"
                   + " 00100000.00100000.00100000.00100000.00100000.00100000.00100000.00100000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SIXTH_COLUMN)),
                   is("00100000.00100000.00100000.00100000.00100000.00100000.00100000.00100000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SEVENTH_COLUMN)) is"
                   + " 01000000.01000000.01000000.01000000.01000000.01000000.01000000.01000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.SEVENTH_COLUMN)),
                   is("01000000.01000000.01000000.01000000.01000000.01000000.01000000.01000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.EIGHTH_COLUMN)) is"
                   + " 10000000.10000000.10000000.10000000.10000000.10000000.10000000.10000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.EIGHTH_COLUMN)),
                   is("10000000.10000000.10000000.10000000.10000000.10000000.10000000.10000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A3_C1_DIAG)) is"
                   + " 00000000.00000000.00000000.00000000.00000000.00000001.00000010.00000100.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A3_C1_DIAG)),
                   is("00000000.00000000.00000000.00000000.00000000.00000001.00000010.00000100"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.F1_H3_DIAG)) is"
                   + " 00000000.00000000.00000000.00000000.00000000.10000000.01000000.00100000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.F1_H3_DIAG)),
                   is("00000000.00000000.00000000.00000000.00000000.10000000.01000000.00100000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.H6_F8_DIAG)) is"
                   + " 00100000.01000000.10000000.00000000.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.H6_F8_DIAG)),
                   is("00100000.01000000.10000000.00000000.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.C8_A6_DIAG)) is"
                   + " 00000100.00000010.00000001.00000000.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.C8_A6_DIAG)),
                   is("00000100.00000010.00000001.00000000.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A4_D1_DIAG)) is"
                   + " 00000000.00000000.00000000.00000000.00000001.00000010.00000100.00001000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A4_D1_DIAG)),
                   is("00000000.00000000.00000000.00000000.00000001.00000010.00000100.00001000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.E1_H4_DIAG)) is"
                   + " 00000000.00000000.00000000.00000000.10000000.01000000.00100000.00010000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.E1_H4_DIAG)),
                   is("00000000.00000000.00000000.00000000.10000000.01000000.00100000.00010000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.H5_E8_DIAG)) is"
                   + " 00010000.00100000.01000000.10000000.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.H5_E8_DIAG)),
                   is("00010000.00100000.01000000.10000000.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.D8_A5_DIAG)) is"
                   + " 00001000.00000100.00000010.00000001.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.D8_A5_DIAG)),
                   is("00001000.00000100.00000010.00000001.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A5_E1_DIAG)) is"
                   + " 00000000.00000000.00000000.00000001.00000010.00000100.00001000.00010000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A5_E1_DIAG)),
                   is("00000000.00000000.00000000.00000001.00000010.00000100.00001000.00010000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.D1_H5_DIAG)) is"
                   + " 00000000.00000000.00000000.10000000.01000000.00100000.00010000.00001000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.D1_H5_DIAG)),
                   is("00000000.00000000.00000000.10000000.01000000.00100000.00010000.00001000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.H4_D8_DIAG)) is"
                   + " 00001000.00010000.00100000.01000000.10000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.H4_D8_DIAG)),
                   is("00001000.00010000.00100000.01000000.10000000.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.E8_A4_DIAG)) is"
                   + " 00010000.00001000.00000100.00000010.00000001.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.E8_A4_DIAG)),
                   is("00010000.00001000.00000100.00000010.00000001.00000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A6_F1_DIAG)) is"
                   + " 00000000.00000000.00000001.00000010.00000100.00001000.00010000.00100000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A6_F1_DIAG)),
                   is("00000000.00000000.00000001.00000010.00000100.00001000.00010000.00100000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.C1_H6_DIAG)) is"
                   + " 00000000.00000000.10000000.01000000.00100000.00010000.00001000.00000100.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.C1_H6_DIAG)),
                   is("00000000.00000000.10000000.01000000.00100000.00010000.00001000.00000100"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.H3_C8_DIAG)) is"
                   + " 00000100.00001000.00010000.00100000.01000000.10000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.H3_C8_DIAG)),
                   is("00000100.00001000.00010000.00100000.01000000.10000000.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.F8_A3_DIAG)) is"
                   + " 00100000.00010000.00001000.00000100.00000010.00000001.00000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.F8_A3_DIAG)),
                   is("00100000.00010000.00001000.00000100.00000010.00000001.00000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A7_G1_DIAG)) is"
                   + " 00000000.00000001.00000010.00000100.00001000.00010000.00100000.01000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A7_G1_DIAG)),
                   is("00000000.00000001.00000010.00000100.00001000.00010000.00100000.01000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.B1_H7_DIAG)) is"
                   + " 00000000.10000000.01000000.00100000.00010000.00001000.00000100.00000010.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.B1_H7_DIAG)),
                   is("00000000.10000000.01000000.00100000.00010000.00001000.00000100.00000010"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.H2_B8_DIAG)) is"
                   + " 00000010.00000100.00001000.00010000.00100000.01000000.10000000.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.H2_B8_DIAG)),
                   is("00000010.00000100.00001000.00010000.00100000.01000000.10000000.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.G8_A2_DIAG)) is"
                   + " 01000000.00100000.00010000.00001000.00000100.00000010.00000001.00000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.G8_A2_DIAG)),
                   is("01000000.00100000.00010000.00001000.00000100.00000010.00000001.00000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A8_H1_DIAG)) is"
                   + " 00000001.00000010.00000100.00001000.00010000.00100000.01000000.10000000.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A8_H1_DIAG)),
                   is("00000001.00000010.00000100.00001000.00010000.00100000.01000000.10000000"));

        assertThat("BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A1_H8_DIAG)) is"
                   + " 10000000.01000000.00100000.00010000.00001000.00000100.00000010.00000001.",
                   BitBoard.longToString(BitBoard.fileMaskArray(BitBoard.A1_H8_DIAG)),
                   is("10000000.01000000.00100000.00010000.00001000.00000100.00000010.00000001"));

    }

    @Test
    public final void testSquareLongValue() {

        assertThat("BitBoard.longToString(BitBoard.squareLongValue(BitBoard.A1)) is"
                   + " 00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000001.",
                   BitBoard.longToString(BitBoard.squareLongValue(BitBoard.A1)),
                   is("00000000.00000000.00000000.00000000.00000000.00000000.00000000.00000001"));

        assertThat("BitBoard.squareLongValue(BitBoard.A1) is 1L.",
                   BitBoard.squareLongValue(BitBoard.A1),
                   is(1L));

        assertThat("BitBoard.longToString(BitBoard.squareLongValue(BitBoard.G8)) is"
                   + " 01000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.squareLongValue(BitBoard.G8)),
                   is("01000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.squareLongValue(BitBoard.G8) is 4611686018427387904L.",
                   BitBoard.squareLongValue(BitBoard.G8),
                   is(4611686018427387904L));

        assertThat("BitBoard.longToString(BitBoard.squareLongValue(BitBoard.H8)) is"
                   + " 10000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000.",
                   BitBoard.longToString(BitBoard.squareLongValue(BitBoard.H8)),
                   is("10000000.00000000.00000000.00000000.00000000.00000000.00000000.00000000"));

        assertThat("BitBoard.squareLongValue(BitBoard.H8) is -9223372036854775808L.",
                   BitBoard.squareLongValue(BitBoard.H8),
                   is(-9223372036854775808L));

    }

}
