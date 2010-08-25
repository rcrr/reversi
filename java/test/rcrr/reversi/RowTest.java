/*
 *  RowTest.java
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

public class RowTest {

    @Test
    public void testLabel() {
	assertEquals("5", Row.R5.label());
    }

    @Test
    public void testSize() {
	assertEquals(8, Row.size());
    }

    @Test
    public void testGetInstance() {
	assertEquals(Row.R3, Row.getInstance(2));
    }

    @Test
    public void testShift() {
	assertEquals(Row.R3, Row.R2.shift(1));
	assertEquals(Row.R4, Row.R2.shift(2));
	assertEquals(Row.R8, Row.R2.shift(6));
	assertEquals(null, Row.R2.shift(7));
	assertEquals(null, Row.R2.shift(-2));
    }

}


