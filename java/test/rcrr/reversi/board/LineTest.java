/*
 *  LineTest.java
 *
 *  Copyright (c) 2010, 2011, 2012 Roberto Corradini. All rights reserved.
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

import org.junit.Test;

import java.util.Arrays;

import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code Line} class.
 */
public class LineTest {

    /** Class constructor. */
    public LineTest() { }

    /**
     * Tests the {@code axis()} method.
     *
     * @see Line#axis()
     */
    @Test
    public final void testAxis() {
        assertThat("Line.A1_A8.axis() is VE.",
                   Line.A1_A8.axis(),
                   is(Axis.VE));
    }

    /**
     * Tests the {@code index()} method.
     *
     * @see Line#index(long, long)
     */
    @Test
    public final void testIndex() {
        assertThat("A board having the ROW 8 filled by player discs has the index E8_H5 equal to 3855+16=3871.",
                   Line.E8_H5.index(0xFF00000000000000L,
                                     0x0000000000000000L),
                   is(3871));
        assertThat("F1_H3 filled by player discs has index equal to 7967+224x256=57344.",
                   Line.F1_H3.index(0x0000000000000000L,
                                     0x0000000000804020L),
                   is(65311));
        assertThat("F1_H3 filled by player discs has index equal to 7967+224=8191.",
                   Line.F1_H3.index(0xFFFFFFFFFFFFFFFFL,
                                     0x0000000000000000L),
                   is(8191));
        assertThat("F1_H3 filled by player discs has index equal to 7967+224=8191.",
                   Line.F1_H3.index(0x0000000000804020L,
                                     0x0000000000000000L),
                   is(8191));
    }

    /**
     * Tests the {@code legalMoves()} method.
     *
     * @see Line#legalMoves(int)
     */
    @Test
    public final void testLegalMoves() {

        assertThat("On line F1_H3, player has H3, opponent has G2, expected result is F1.",
                   Line.F1_H3.legalMoves(Line.F1_H3.index(0x0000000000800000L,
                                                            0x0000000000004000L)),
                   is(0x0000000000000020L));

        assertThat("On line F1_H3, player has F1, opponent has G2, expected result is H3.",
                   Line.F1_H3.legalMoves(Line.F1_H3.index(0x0000000000000020L,
                                                            0x0000000000004000L)),
                   is(0x0000000000800000L));

        assertThat("On line F1_H3, player has F1, G2, H3, expected result is empty.",
                   Line.F1_H3.legalMoves(Line.F1_H3.index(0x0000000000804020L,
                                                            0x0000000000000000L)),
                   is(0x0000000000000000L));

        assertThat("On line F1_H3, player has F1, G2, expected result is empty.",
                   Line.F1_H3.legalMoves(Line.F1_H3.index(0x0000000000004020L,
                                                            0x0000000000000000L)),
                   is(0x0000000000000000L));

        assertThat("On line F1_H3, all squares are empty, expected result is empty.",
                   Line.F1_H3.legalMoves(Line.F1_H3.index(0x0000000000000000L,
                                                            0x0000000000000000L)),
                   is(0x0000000000000000L));

        assertThat("On line F1_H3, player has no squares, opponent has F1, G2, expected result is empty.",
                   Line.F1_H3.legalMoves(Line.F1_H3.index(0x0000000000000000L,
                                                            0x0000000000004020L)),
                   is(0x0000000000000000L));
    }

    /**
     * Tests the {@code mask()} method.
     *
     * @see Line#mask()
     */
    @Test
    public final void testMask() {
        assertThat("Line.F1_H3.mask() is 0x0000000000804020L.",
                   Line.F1_H3.mask(),
                   is(0x0000000000804020L));
    }

    /**
     * Tests the {@code order()} method.
     *
     * @see Line#order()
     */
    @Test
    public final void testOrder() {
        assertThat("Line.A1_A8.order() is 8.",
                   Line.A1_A8.order(),
                   is(8));
        assertThat("Line.F1_H3.order() is 3.",
                   Line.F1_H3.order(),
                   is(3));
    }

    /**
     * Tests the {@code shift()} method.
     *
     * @see Line#shift()
     */
    @Test
    public final void testShift() {
        assertThat("Line.A1_A8.shift() is 0.",
                   Line.A1_A8.shift(),
                   is(0));
        assertThat("Line.B1_B8.shift() is -1.",
                   Line.B1_B8.shift(),
                   is(-1));
    }

    /**
     * Tests the {@code squares()} method.
     *
     * @see Line#squares()
     */
    @Test
    public final void testSquares() {
        assertThat("Line.A1_A8.squares() is A1...A8.",
                   Line.A1_A8.squares(),
                   is(Arrays.asList(Square.A1, Square.A2, Square.A3, Square.A4,
                                    Square.A5, Square.A6, Square.A7, Square.A8)));
    }

}
