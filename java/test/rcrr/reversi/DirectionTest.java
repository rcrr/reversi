/*
 *  DirectionTest.java
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

import org.junit.Test;
import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code Direction} enum.
 */
public class DirectionTest {

    /** Class constructor. */
    public DirectionTest() { }

    /**
     * Tests the {@code deltaColumn()} method.
     */
    @Test
    public final void testDeltaColumn() {
        assertThat("Direction.N.deltaColumn() must return 0.",
                   Direction.N.deltaColumn(),
                   is(0));
        assertThat("Direction.S.deltaColumn() must return 0.",
                   Direction.S.deltaColumn(),
                   is(0));
        assertThat("Direction.E.deltaColumn() must return +1.",
                   Direction.E.deltaColumn(),
                   is(+1));
        assertThat("Direction.SE.deltaColumn() must return +1.",
                   Direction.SE.deltaColumn(),
                   is(+1));
        assertThat("Direction.NE.deltaColumn() must return +1.",
                   Direction.NE.deltaColumn(),
                   is(+1));
        assertThat("Direction.W.deltaColumn() must return -1.",
                   Direction.W.deltaColumn(),
                   is(-1));
        assertThat("Direction.SW.deltaColumn() must return -1.",
                   Direction.SW.deltaColumn(),
                   is(-1));
        assertThat("Direction.NW.deltaColumn() must return -1.",
                   Direction.NW.deltaColumn(),
                   is(-1));
    }

    /**
     * Tests the {@code deltaRow()} method.
     */
    @Test
    public final void testDeltaRow() {
        assertThat("Direction.N.deltaRow() must return -1.",
                   Direction.N.deltaRow(),
                   is(-1));
        assertThat("Direction.NE.deltaRow() must return -1.",
                   Direction.NE.deltaRow(),
                   is(-1));
        assertThat("Direction.NW.deltaRow() must return -1.",
                   Direction.NW.deltaRow(),
                   is(-1));
        assertThat("Direction.S.deltaRow() must return +1.",
                   Direction.S.deltaRow(),
                   is(+1));
        assertThat("Direction.SE.deltaRow() must return +1.",
                   Direction.SE.deltaRow(),
                   is(+1));
        assertThat("Direction.SW.deltaRow() must return +1.",
                   Direction.SW.deltaRow(),
                   is(+1));
        assertThat("Direction.E.deltaRow() must return 0.",
                   Direction.E.deltaRow(),
                   is(0));
        assertThat("Direction.W.deltaRow() must return 0.",
                   Direction.W.deltaRow(),
                   is(0));
    }

    /**
     * Tests the {@code getDescription()} method.
     */
    @Test
    public final void testGetDescription() {
        assertThat("Direction.N.description() must return North.",
                   Direction.N.description(),
                   is("North"));
        assertThat("Direction.S.description() must return South.",
                   Direction.S.description(),
                   is("South"));
        assertThat("Direction.E.description() must return East.",
                   Direction.E.description(),
                   is("East"));
        assertThat("Direction.W.description() must return West.",
                   Direction.W.description(),
                   is("West"));
        assertThat("Direction.NW.description() must return North-West.",
                   Direction.NW.description(),
                   is("North-West"));
        assertThat("Direction.SW.description() must return South-West.",
                   Direction.SW.description(),
                   is("South-West"));
        assertThat("Direction.NE.description() must return North-East.",
                   Direction.NE.description(),
                   is("North-East"));
        assertThat("Direction.SE.description() must return South-East.",
                   Direction.SE.description(),
                   is("South-East"));
    }

}
