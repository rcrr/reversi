/*
 *  SquareTest.java
 *
 *  Copyright (c) 2010 Roberto Corradini. All rights reserved.
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

import java.util.List;
import java.util.Map;

import org.junit.*;
import static org.junit.Assert.*;

public class SquareTest {

    @Test
    public void testInstanceOf() {
	assertEquals(Square.A1, Square.instanceOf(Row.R1, Column.A));
	assertEquals(Square.C6, Square.instanceOf(Row.R6, Column.C));
	assertEquals(Square.H7, Square.instanceOf(Row.R7, Column.H));
	assertEquals(Square.E8, Square.instanceOf(Row.R8, Column.E));
	assertEquals(Square.H8, Square.instanceOf(Row.R8, Column.H));

	assertEquals(null, Square.instanceOf(null, Column.H));
	assertEquals(null, Square.instanceOf(Row.R7, null));
    }

    @Test
    public void testRow() {
	assertEquals(Row.R1, Square.A1.row());
	assertEquals(Row.R6, Square.C6.row());
	assertEquals(Row.R7, Square.H7.row());
    }

    @Test
    public void testColumn() {
	assertEquals(Column.A, Square.A1.column());
	assertEquals(Column.C, Square.C6.column());
	assertEquals(Column.H, Square.H7.column());
    }

    @Test
    public void testLabel() {
	assertEquals("a1", Square.A1.label());
	assertEquals("c6", Square.C6.label());
	assertEquals("h7", Square.H7.label());
    }

    @Test
    public void testNeighbors() {
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
    public void testGetInstance() {

	boolean thrown;

	assertEquals(Square.A1, Square.getInstance(0));
	assertEquals(Square.C6, Square.getInstance(42));
	assertEquals(Square.H7, Square.getInstance(55));

	thrown = false;
	try {
	    Square.getInstance(64);
	} catch (IndexOutOfBoundsException ioobe) {
	    thrown = true;
	}
	assertTrue(thrown);

	assertEquals(Square.A1, Square.getInstance("a1"));
	assertEquals(Square.C6, Square.getInstance("c6"));
	assertEquals(Square.H7, Square.getInstance("h7"));

	thrown = false;
	try {
	    Square.getInstance("w0");
	} catch (IllegalArgumentException iae) {
	    thrown = true;
	}
	assertTrue(thrown);
    }

    @Test
    public void testGetHasegawaLabel() {
	assertEquals(Character.valueOf('C'), Square.A2.getHasegawaLabel());
	assertEquals(Character.valueOf('X'), Square.B2.getHasegawaLabel());
	assertEquals(null, Square.C3.getHasegawaLabel());
    }

    @Test
    public void testCorners() {
	List<Square> corners = Square.corners();
	assertEquals(true, corners.contains(Square.A1));
	assertEquals(true, corners.contains(Square.A8));
	assertEquals(true, corners.contains(Square.H1));
	assertEquals(true, corners.contains(Square.H8));
	assertEquals(4, corners.size());
    }

    @Test
    public void testIsCorner() {
	assertEquals(true, Square.A1.isCorner());
	assertEquals(true, Square.H1.isCorner());
	assertEquals(true, Square.A8.isCorner());
	assertEquals(true, Square.H8.isCorner());
	assertEquals(false, Square.C2.isCorner());
    }

    @Test
    public void testNumerosity() {
	assertEquals(64, Square.numerosity());
    }

   
}