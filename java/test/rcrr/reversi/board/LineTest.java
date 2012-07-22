/*
 *  LineTest.java
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

import org.junit.Test;

import static org.junit.Assert.assertThat;

import static org.junit.matchers.JUnitMatchers.hasItems;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code Line} enum.
 */
public class LineTest {

    /** Class constructor. */
    public LineTest() { }

    @Test(expected = NullPointerException.class)
    public final void testCross_boundaryConditions_checkNullParameter_cross() {
        Line.C.cross(Line.NULL);
    }

    @Test(expected = IllegalArgumentException.class)
    public final void testCross_boundaryConditions_checkValidParameter_cross() {
        Line.C.cross(Line.C);
    }

    /**
     * Tests the {@code cross(Line)} method.
     *
     * @see Line#cross(Line)
     */
    @Test
    public final void testCross() {
        assertThat("Line.C.cross(Line.R2) is Square.C2.",
                   Line.C.cross(Line.R2),
                   is(Square.C2));
        assertThat("Line.H1_A8.cross(Line.A1_H8 is Square.NULL.",
                   Line.H1_A8.cross(Line.A1_H8),
                   is(Square.NULL));
        assertThat("Line.H1_A8.cross(Line.A1_H8 is Square.E5.",
                   Line.A1_H8.cross(Line.H2_B8),
                   is(Square.E5));
    }

    /**
     * Tests the {@code file()} method.
     *
     * @see Line#file()
     */
    @Test
    public final void testFile() {
        assertThat("Line.C.file() is Column.C.",
                   Line.C.file(),
                   is((File) Column.C));
    }

    /**
     * Tests the {@code getInstance(File)} method.
     *
     * @see Line#getInstance(File)
     */
    @Test
    public final void testGetInstance() {
        assertThat("Line.getInstance((File) DiagonalRL.H2_B8) is Line.H2_B8.",
                   Line.getInstance((File) DiagonalRL.H2_B8),
                   is(Line.H2_B8));
    }

    /**
     * Tests the {@code linesForSquare(Square)} method.
     *
     * @see Line#linesForSquare(Square)
     */
    @Test
    public final void testLinesForSquare() {
        assertThat("Line.linesForSquare(Square.C4) has items: C, R3, A2_G8, F1_A6.",
                   Line.linesForSquare(Square.C4),
                   hasItems(Line.C,
                            Line.R4,
                            Line.A2_G8,
                            Line.F1_A6));
    }


}
