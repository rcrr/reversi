/*
 *  SquareTest.java
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

import java.util.Map;

import org.junit.Test;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import static org.junit.matchers.JUnitMatchers.hasItems;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;


/**
 * Test Suite for {@code Square} enum.
 */
public class SquareTest {

    /** The null string. */
    private static final String NULL_STRING = null;

    /** Class constructor. */
    public SquareTest() { }

    /**
     * Test the {@code files()} method.
     *
     * @see FileUtils#files()
     */
    @Test
    public final void testFiles() {

        assertThat("Square A1 is crossed by three files.",
                   Square.A1.files().size(),
                   is(4));

        assertThat("Square A1 is crossed by column A, row, R1, and diagonal left-up to right-down A1_H8.",
                   Square.A1.files().values(),
                   hasItems((File) Column.A,
                            (File) Row.R1,
                            (File) DiagonalLR.A1_H8,
                            (File) DiagonalRL.NULL));

        assertThat("Square A1 has three active axes: VERTICAL, HORIZONTAL, DIAGONAL_LR.",
                   Square.A1.files().keySet(),
                   hasItems(Axis.VERTICAL,
                            Axis.HORIZONTAL,
                            Axis.DIAGONAL_LR,
                            Axis.DIAGONAL_RL));

        assertThat("Square E7 is crossed by column E, row, R7, iagonals A3_F8 and H4_D8.",
                   Square.E7.files().values(),
                   hasItems((File) Column.E,
                            (File) Row.R7,
                            (File) DiagonalLR.A3_F8,
                            (File) DiagonalRL.H4_D8));
    }

     /**
     * Tests the {@code capableToFlipDirections()} method.
     *
     * @see Square#capableToFlipDirections()
     */
    @Test
    public final void testCapableToFlipDirections() {

        assertThat("Square.A1.capableToFlipDirections() is E, S, SE",
                   Square.A1.capableToFlipDirections(),
                   hasItems(Direction.E,
                            Direction.S,
                            Direction.SE));

        assertThat("Square.B2.capableToFlipDirections() is E, S, SE",
                   Square.B2.capableToFlipDirections(),
                   hasItems(Direction.E,
                            Direction.S,
                            Direction.SE));

        assertThat("Square.B2.capableToFlipDirections() is E, SE, S, SW, W, NW, N, NE",
                   Square.C3.capableToFlipDirections(),
                   hasItems(Direction.E,
                            Direction.SE,
                            Direction.S,
                            Direction.SW,
                            Direction.W,
                            Direction.NW,
                            Direction.N,
                            Direction.NE));

    }

    /**
     * Tests the {@code column()} method.
     *
     * @see Square#column()
     */
    @Test
    public final void testColumn() {
        assertThat("Square.A1.column() is Column.A.",
                   Square.A1.column(),
                   is(Column.A));
        assertThat("Square.C6.column() is Column.C.",
                   Square.C6.column(),
                   is(Column.C));
        assertThat("Square.H7.column() is Column.H.",
                   Square.H7.column(),
                   is(Column.H));
    }

    /**
     * Tests the {@code cornerFor()} method.
     *
     * @see Square#cornerFor()
     */
    @Test
    public final void testCornerFor() {
        assertThat("Square.A3.cornerFor() is null.",
                   Square.A3.cornerFor(),
                   is(Square.NULL));
        assertThat("Square.B2.cornerFor() is A1.",
                   Square.B2.cornerFor(),
                   is(Square.A1));
        assertThat("Square.G7.cornerFor() is H8.",
                   Square.G7.cornerFor(),
                   is(Square.H8));
    }

    /**
     * Tests the {@code corners()} method.
     *
     * @see Square#corners()
     */
    @Test
    public final void testCorners() {
        assertThat("Square.corners() has items: A1, A8, H1, H8.",
                   Square.corners(),
                   hasItems(Square.A1,
                            Square.A8,
                            Square.H1,
                            Square.H8));

        assertThat("Square.corners() has 4 elements.",
                   Square.corners().size(),
                   is(4));
    }

    /**
     * Tests the {@code getHasegawaLabel()} method.
     *
     * @see Square#getHasegawaLabel()
     */
    @Test
    public final void testGetHasegawaLabel() {
        assertThat("Square.A2.getHasegawaLabel() is the C char.",
                   Square.A2.getHasegawaLabel(),
                   is('C'));
        assertThat("Square.B2.getHasegawaLabel() is the X char.",
                   Square.B2.getHasegawaLabel(),
                   is('X'));
        assertThat("Square.C3.getHasegawaLabel() is the blank space char.",
                   Square.C3.getHasegawaLabel(),
                   is(' '));
    }

    /**
     * Tests the {@code getInstance(String)} method when parameter
     * {@code label} is {@code null}.
     *
     * @see Square#getInstance(String)
     */
    @Test(expected = NullPointerException.class)
    public final void testGetInstance_byLabel_boundaryConditions_checkNullParameter_label() {
        Square.getInstance(NULL_STRING);
    }

    /**
     * Tests the {@code getInstance(String)} method when parameter
     * {@code label} is not a string representing a valid label.
     *
     * @see Square#getInstance(String)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testGetInstance_byLabel_boundaryConditions_checkIllegalArgument_label() {
        Square.getInstance("wrong label");
    }

    /**
     * Tests the {@code getInstance(String)} method.
     *
     * @see Square#getInstance(String)
     */
    @Test
    public final void testGetInstance_byLabel() {
        assertThat("Square.getInstance(\"a1\") is Square.A1.",
                   Square.getInstance("a1"),
                   is(Square.A1));
        assertThat("Square.getInstance(\"c6\") is Square.C6.",
                   Square.getInstance("c6"),
                   is(Square.C6));
        assertThat("Square.getInstance(\"h7\") is Square.H7.",
                   Square.getInstance("h7"),
                   is(Square.H7));
    }

    /**
     * Tests the {@code getInstance(Row, Column)} method when one between the
     * two parameter is {@code null}.
     *
     * @see Square#getInstance(Row, Column)
     */
    @Test
    public final void testGetInstance_byRowAndColumn_whenOneParameterIsNull() {
        assertThat("Square.getInstance(Row.NULL, Column.H) must return a null value.",
                   Square.getInstance(Row.NULL, Column.H),
                   nullValue());
        assertThat("Square.getInstance(Row.R7, Column.NULL must return a null value.",
                   Square.getInstance(Row.R7, Column.NULL),
                   nullValue());
    }

    /**
     * Tests the {@code getInstance(Row, Column)} method.
     *
     * @see Square#getInstance(Row, Column)
     */
    @Test
    public final void testGetInstance_byRowAndColumn() {
        assertThat("Square.getInstance(Row.R1, Column.A) is A1.",
                   Square.getInstance(Row.R1, Column.A),
                   is(Square.A1));
        assertThat("Square.getInstance(Row.R6, Column.C) is C6.",
                   Square.getInstance(Row.R6, Column.C),
                   is(Square.C6));
    }

    /**
     * Tests the {@code isCorner()} method.
     *
     * @see Square#isCorner()
     */
    @Test
    public final void testIsCorner() {
        assertTrue("Square.A1 is a corner.",
                   Square.A1.isCorner());
        assertTrue("Square.H1 is a corner.",
                   Square.H1.isCorner());
        assertTrue("Square.A8 is a corner.",
                   Square.A8.isCorner());
        assertTrue("Square.H8 is a corner.",
                   Square.H8.isCorner());

        assertFalse("Square.C2 is not a corner.",
                    Square.C2.isCorner());
    }

    /**
     * Tests the {@code isXSquare()} method.
     *
     * @see Square#isXSquare()
     */
    @Test
    public final void testIsXSquare() {
        assertTrue("Square.B2 is an X square.",
                   Square.B2.isXSquare());
        assertFalse("Square.C2 is not an X square.",
                    Square.C2.isXSquare());
    }

    /**
     * Tests the {@code label()} method.
     *
     * @see Square#label()
     */
    @Test
    public final void testLabel() {
        assertThat("Square.A1.label() is a1.",
                   Square.A1.label(),
                   is("a1"));
        assertThat("Square.C6.label() is c6.",
                   Square.C6.label(),
                   is("c6"));
        assertThat("Square.H7.label() is h7.",
                   Square.H7.label(),
                   is("h7"));
    }

    /**
     * Tests the {@code neighbors()} method.
     * <p>
     * The test verifies for three board squares that the returned
     * map from the {@code neighbors(}) method has the six appropriate
     * entries.
     * <p>
     * The test applies the assertThat assertion, it would be more "elegant" if
     * the {@code org.hamcrest.collection.IsMapContaining.hasEntry} would have been used
     * instead. It would have beed required to add a new jar library to the
     * unit test dependencies.
     *
     * @see Square#neighbors()
     */
    @Test
    public final void testNeighbors() {

        /** Testing the Upper-Left corner (A1) neighbor table. */
        assertThat("Square.A1.neighbors().get(Direction.N) is Square.NULL",
                   Square.A1.neighbors().get(Direction.N),
                   is(Square.NULL));
        assertThat("Square.A1.neighbors().get(Direction.NE) is Square.NULL",
                   Square.A1.neighbors().get(Direction.NE),
                   is(Square.NULL));
        assertThat("Square.A1.neighbors().get(Direction.E) is Square.B1",
                   Square.A1.neighbors().get(Direction.E),
                   is(Square.B1));
        assertThat("Square.A1.neighbors().get(Direction.SE) is Square.B2",
                   Square.A1.neighbors().get(Direction.SE),
                   is(Square.B2));
        assertThat("Square.A1.neighbors().get(Direction.S) is Square.A2",
                   Square.A1.neighbors().get(Direction.S),
                   is(Square.A2));
        assertThat("Square.A1.neighbors().get(Direction.SW) is Square.NULL",
                   Square.A1.neighbors().get(Direction.SW),
                   is(Square.NULL));
        assertThat("Square.A1.neighbors().get(Direction.W) is Square.NULL",
                   Square.A1.neighbors().get(Direction.W),
                   is(Square.NULL));
        assertThat("Square.A1.neighbors().get(Direction.NW) is Square.NULL",
                   Square.A1.neighbors().get(Direction.NW),
                   is(Square.NULL));

        /** Testing a regular square (D6) neighbor table. */
        Map<Direction, Square> d6Neighbors = Square.D6.neighbors();
        assertThat("d6Neighbors.get(Direction.N) is D5.",
                   d6Neighbors.get(Direction.N),
                   is(Square.D5));
        assertThat("d6Neighbors.get(Direction.NE) is E5.",
                   d6Neighbors.get(Direction.NE),
                   is(Square.E5));
        assertThat("d6Neighbors.get(Direction.E) is E6.",
                   d6Neighbors.get(Direction.E),
                   is(Square.E6));
        assertThat("d6Neighbors.get(Direction.SE) is E7.",
                   d6Neighbors.get(Direction.SE),
                   is(Square.E7));
        assertThat("d6Neighbors.get(Direction.S) is D7.",
                   d6Neighbors.get(Direction.S),
                   is(Square.D7));
        assertThat("d6Neighbors.get(Direction.SW) is C7.",
                   d6Neighbors.get(Direction.SW),
                   is(Square.C7));
        assertThat("d6Neighbors.get(Direction.W) is C6.",
                   d6Neighbors.get(Direction.W),
                   is(Square.C6));
        assertThat("d6Neighbors.get(Direction.NW) is C5.",
                   d6Neighbors.get(Direction.NW),
                   is(Square.C5));

        /** Testing the border square (B8) neighbor table. */
        Map<Direction, Square> b8Neighbors = Square.B8.neighbors();
        assertThat("b8Neighbors.get(Direction.N) is B7.",
                   b8Neighbors.get(Direction.N),
                   is(Square.B7));
        assertThat("b8Neighbors.get(Direction.NE) is C7.",
                   b8Neighbors.get(Direction.NE),
                   is(Square.C7));
        assertThat("b8Neighbors.get(Direction.E) is C8.",
                   b8Neighbors.get(Direction.E),
                   is(Square.C8));
        assertThat("b8Neighbors.get(Direction.SE) is null.",
                   b8Neighbors.get(Direction.SE),
                   is(Square.NULL));
        assertThat("b8Neighbors.get(Direction.S) is null.",
                   b8Neighbors.get(Direction.S),
                   is(Square.NULL));
        assertThat("b8Neighbors.get(Direction.SW) is null.",
                   b8Neighbors.get(Direction.SW),
                   is(Square.NULL));
        assertThat("b8Neighbors.get(Direction.W) is A8.",
                   b8Neighbors.get(Direction.W),
                   is(Square.A8));
        assertThat("b8Neighbors.get(Direction.NW) is A7.",
                   b8Neighbors.get(Direction.NW),
                   is(Square.A7));
    }

     /**
     * Tests the {@code row()} method.
     *
     * @see Square#row()
     */
    @Test
    public final void testRow() {
        assertThat("Square.A1.row() is Row.R1.",
                   Square.A1.row(),
                   is(Row.R1));
        assertThat("Square.C6.row() is Row.R6.",
                   Square.C6.row(),
                   is(Row.R6));
        assertThat("Square.H7.row() is Row.R7.",
                   Square.H7.row(),
                   is(Row.R7));
    }

     /**
     * Tests the {@code xSquareFor()} method.
     *
     * @see Square#xSquareFor()
     */
    @Test
    public final void testXSquareFor() {
        assertThat("Square.A1.xSquareFor() is Square.B2.",
                   Square.A1.xSquareFor(),
                   is(Square.B2));
        assertThat("Square.C4.xSquareFor() is null.",
                   Square.C4.xSquareFor(),
                   is(Square.NULL));
    }

    /**
     * Tests the {@code ordinalPositionInFile(Axis)} method when parameter
     * {@code axis} is {@code null}.
     *
     * @see Square#ordinalPositionInFile(Axis)
     */
    @Test(expected = NullPointerException.class)
    public final void testOrdinalPositionInFile_boundaryConditions_checkNullParameter_axis() {
        Square.A1.ordinalPositionInFile(Axis.NULL);
    }

     /**
     * Tests the {@code ordinalPositionInFile(Axis)} method.
     *
     * @see Square#ordinalPositionInFile(Axis)
     */
    @Test
    public final void testOrdinalPositionInFile_axisParameter() {
        assertThat("Square.A1.ordinalPositionInFile(Axis.VERTICAL) is 0.",
                   Square.A1.ordinalPositionInFile(Axis.VERTICAL),
                   is(0));
        assertThat("Square.A1.ordinalPositionInFile(Axis.HORIZONTAL) is 0.",
                   Square.A1.ordinalPositionInFile(Axis.HORIZONTAL),
                   is(0));
        assertThat("Square.A1.ordinalPositionInFile(Axis.DIAGONAL_LR) is 0.",
                   Square.A1.ordinalPositionInFile(Axis.DIAGONAL_LR),
                   is(0));
        assertThat("Square.A1.ordinalPositionInFile(Axis.DIAGONAL_RL) is -1.",
                   Square.A1.ordinalPositionInFile(Axis.DIAGONAL_RL),
                   is(-1));
        assertThat("Square.A8.ordinalPositionInFile(Axis.VERTICAL) is 7.",
                   Square.A8.ordinalPositionInFile(Axis.VERTICAL),
                   is(7));
        assertThat("Square.A3.ordinalPositionInFile(Axis.DIAGONAL_RL) is 2.",
                   Square.A3.ordinalPositionInFile(Axis.DIAGONAL_RL),
                   is(2));
        assertThat("Square.E7.ordinalPositionInFile(Axis.DIAGONAL_LR) is 4.",
                   Square.E7.ordinalPositionInFile(Axis.DIAGONAL_LR),
                   is(4));

        assertThat("Square.F1.ordinalPositionInFile(Axis.DIAGONAL_LR) is 0.",
                   Square.F1.ordinalPositionInFile(Axis.DIAGONAL_LR),
                   is(0));

        assertThat("Square.G2.ordinalPositionInFile(Axis.DIAGONAL_LR) is 1.",
                   Square.G2.ordinalPositionInFile(Axis.DIAGONAL_LR),
                   is(1));

        assertThat("Square.H3.ordinalPositionInFile(Axis.DIAGONAL_LR) is 2.",
                   Square.H3.ordinalPositionInFile(Axis.DIAGONAL_LR),
                   is(2));
    }

    /**
     * Tests the {@code ordinalPositionInFile(File)} method when parameter
     * {@code file} is {@code null}.
     *
     * @see Square#ordinalPositionInFile(File)
     */
    @Test(expected = NullPointerException.class)
    public final void testOrdinalPositionInFile_boundaryConditions_checkNullParameter_file() {
        Square.A1.ordinalPositionInFile(FileUtils.NULL_FILE);
    }


     /**
     * Tests the {@code ordinalPositionInFile(File)} method.
     *
     * @see Square#ordinalPositionInFile(File)
     */
    @Test
    public final void testOrdinalPositionInFile_fileParameter() {
        assertThat("Square.A1.ordinalPositionInFile((File) Column.A) is 0.",
                   Square.A1.ordinalPositionInFile((File) Column.A),
                   is(0));
        assertThat("Square.C4.ordinalPositionInFile((File) Column.C) is 3.",
                   Square.C4.ordinalPositionInFile((File) Column.C),
                   is(3));
        assertThat("Square.C4.ordinalPositionInFile((File) Column.A) is -1.",
                   Square.C4.ordinalPositionInFile((File) Column.A),
                   is(-1));
    }

}
