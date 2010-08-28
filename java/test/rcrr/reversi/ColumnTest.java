/*
 *  ColumnTest.java
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

public class ColumnTest {

    @Test
    public void testLabel() {
	assertEquals("e", Column.E.label());
    }

    @Test
    public void testSize() {
	assertEquals(8, Column.size());
	assertEquals(Column.values().length, Column.size());
    }

    @Test
    public void testGetInstance() {
	assertEquals(Column.A, Column.getInstance(0));
	assertEquals(Column.C, Column.getInstance(2));
	assertEquals(Column.H, Column.getInstance(7));

	boolean thrown = false;
	try {
	    Row.getInstance(8);
	} catch (IndexOutOfBoundsException e) {
	    thrown = true;
	}
	assertTrue(thrown);
    }

    @Test
    public void testShift() {
	assertEquals(null, Column.A.shift(-1));
	assertEquals(Column.A, Column.A.shift(0));
	assertEquals(Column.B, Column.A.shift(+1));
	assertEquals(Column.D, Column.E.shift(-1));
	assertEquals(Column.E, Column.E.shift(0));
	assertEquals(Column.F, Column.E.shift(+1));
	assertEquals(Column.G, Column.H.shift(-1));
	assertEquals(Column.H, Column.H.shift(0));
	assertEquals(null, Column.H.shift(+1));
    }

}