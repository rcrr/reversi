/*
 *  ColumnTest.java
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

import static org.junit.Assert.assertThat;

import static org.junit.matchers.JUnitMatchers.hasItems;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code Column} enum.
 */
public class ColumnTest {

    /** Class constructor. */
    public ColumnTest() { }

    /**
     * Tests the {@code label()} method.
     *
     * @see Column#label()
     */
    @Test
    public final void testLabel() {
        assertThat("Column.E.label() must return e.",
                   Column.E.label(),
                   is("e"));
    }

    /**
     * Tests the {@code neighbor(Direction)} method.
     *
     * @see Column#neighbor(Direction)
     */
    @Test
    public final void testNeighbor() {
        assertThat("Column.A.neighbor(Direction.W) must return Column.NULL.",
                   Column.A.neighbor(Direction.W),
                   is(Column.NULL));
        assertThat("Column.A.neighbor(Direction.E) must return Column.B.",
                   Column.A.neighbor(Direction.E),
                   is(Column.B));
        assertThat("Column.E.neighbor(Direction.SW) must return Column.D.",
                   Column.E.neighbor(Direction.SW),
                   is(Column.D));
        assertThat("Column.E.neighbor(Direction.NE) must return Column.F.",
                   Column.E.neighbor(Direction.NE),
                   is(Column.F));
        assertThat("Column.H.neighbor(Direction.NW) must return Column.G.",
                   Column.H.neighbor(Direction.NW),
                   is(Column.G));
        assertThat("Column.H.neighbor(Direction.N) must return Column.G.",
                   Column.H.neighbor(Direction.N),
                   is(Column.H));
        assertThat("Column.H.neighbor(Direction.S) must return Column.G.",
                   Column.H.neighbor(Direction.S),
                   is(Column.H));
        assertThat("Column.H.neighbor(Direction.E) must return Column.NULL.",
                   Column.H.neighbor(Direction.E),
                   is(Column.NULL));
    }

}
