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

import java.util.Map;

import org.junit.*;
import static org.junit.Assert.*;

public class SquareTest {

    @Test
    public void testBase() {
	assertTrue(true);
    }

    @Test
    public void testNeighbors() {
	Map n;
	
	/* Testing the Upper-Left corner (A1) neighbor table. */
	n = Square.neighbors(Square.A1);
	assertEquals(null, n.get(Direction.N));
	assertEquals(null, n.get(Direction.NE));
	assertEquals(Square.B1, n.get(Direction.E));
	assertEquals(Square.B2, n.get(Direction.SE));
	assertEquals(Square.A2, n.get(Direction.S));
	assertEquals(null, n.get(Direction.SW));
	assertEquals(null, n.get(Direction.W));
	assertEquals(null, n.get(Direction.NW));

	/* Testing a common square (D6) neighbor table. */
	n = Square.neighbors(Square.D6);
	assertEquals(Square.D5, n.get(Direction.N));
	assertEquals(Square.E5, n.get(Direction.NE));
	assertEquals(Square.E6, n.get(Direction.E));
	assertEquals(Square.E7, n.get(Direction.SE));
	assertEquals(Square.D7, n.get(Direction.S));
	assertEquals(Square.C7, n.get(Direction.SW));
	assertEquals(Square.C6, n.get(Direction.W));
	assertEquals(Square.C5, n.get(Direction.NW));

	/* Testing the border square (B8) neighbor table. */
	n = Square.neighbors(Square.B8);
	assertEquals(Square.B7, n.get(Direction.N));
	assertEquals(Square.C7, n.get(Direction.NE));
	assertEquals(Square.C8, n.get(Direction.E));
	assertEquals(null, n.get(Direction.SE));
	assertEquals(null, n.get(Direction.S));
	assertEquals(null, n.get(Direction.SW));
	assertEquals(Square.A8, n.get(Direction.W));
	assertEquals(Square.A7, n.get(Direction.NW));
    }
   
}