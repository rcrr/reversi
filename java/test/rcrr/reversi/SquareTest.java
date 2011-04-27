/*
 *  SquareTest.java
 *
 *  Copyright (c) 2010, 2011 Roberto Corradini. All rights reserved.
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

import org.junit.Test;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import static org.junit.matchers.JUnitMatchers.hasItems;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code Square} enum.
 */
public class SquareTest {

    /** Class constructor. */
    public SquareTest() { }

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

    @Test
    public final void testNeighbors() {
        Map n;

        /* Testing the Upper-Left corner (A1) neighbor table. */
        n = Square.A1.neighbors();
        assertEquals(null, n.get(Direction.N));
        assertEquals(null, n.get(Direction.NE));
        assertEquals(Square.B1, n.get(Direction.E));
        assertEquals(Square.B2, n.get(Direction.SE));
        assertEquals(Square.A2, n.get(Direction.S));
        assertEquals(null, n.get(Direction.SW));
        assertEquals(null, n.get(Direction.W));
        assertEquals(null, n.get(Direction.NW));

        /* Testing a common square (D6) neighbor table. */
        n = Square.D6.neighbors();
        assertEquals(Square.D5, n.get(Direction.N));
        assertEquals(Square.E5, n.get(Direction.NE));
        assertEquals(Square.E6, n.get(Direction.E));
        assertEquals(Square.E7, n.get(Direction.SE));
        assertEquals(Square.D7, n.get(Direction.S));
        assertEquals(Square.C7, n.get(Direction.SW));
        assertEquals(Square.C6, n.get(Direction.W));
        assertEquals(Square.C5, n.get(Direction.NW));

        /* Testing the border square (B8) neighbor table. */
        n = Square.B8.neighbors();
        assertEquals(Square.B7, n.get(Direction.N));
        assertEquals(Square.C7, n.get(Direction.NE));
        assertEquals(Square.C8, n.get(Direction.E));
        assertEquals(null, n.get(Direction.SE));
        assertEquals(null, n.get(Direction.S));
        assertEquals(null, n.get(Direction.SW));
        assertEquals(Square.A8, n.get(Direction.W));
        assertEquals(Square.A7, n.get(Direction.NW));
    }

    @Test
    public final void testGetInstance() {

        assertEquals(Square.A1, Square.getInstance(0));
        assertEquals(Square.C6, Square.getInstance(42));
        assertEquals(Square.H7, Square.getInstance(55));

        try {
            Square.getInstance(64);
            fail("An exception must be risen.");
        } catch (IndexOutOfBoundsException ioobe) {
            assertTrue(true);
        }

        assertEquals(Square.A1, Square.getInstance("a1"));
        assertEquals(Square.C6, Square.getInstance("c6"));
        assertEquals(Square.H7, Square.getInstance("h7"));

        try {
            Square.getInstance("w0");
            fail("An exception must be risen.");
        } catch (IllegalArgumentException iae) {
            assertTrue(true);
        }
    }

    @Test
    public final void testGetInstanceRC() {
        assertEquals(Square.A1, Square.getInstance(Row.R1, Column.A));
        assertEquals(Square.C6, Square.getInstance(Row.R6, Column.C));
        assertEquals(Square.H7, Square.getInstance(Row.R7, Column.H));
        assertEquals(Square.E8, Square.getInstance(Row.R8, Column.E));
        assertEquals(Square.H8, Square.getInstance(Row.R8, Column.H));

        assertEquals(null, Square.getInstance(null, Column.H));
        assertEquals(null, Square.getInstance(Row.R7, null));
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

}
