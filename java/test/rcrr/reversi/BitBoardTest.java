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

        assertThat("To be developed.",
                   true,
                   is(true));

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

        /**
         *             8        7        6        5        4        3        2        1
         *             HGFEDCBA.HGFEDCBA.HGFEDCBA.HGFEDCBA.HGFEDCBA.HGFEDCBA.HGFEDCBA.HGFEDCBA
         */


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
