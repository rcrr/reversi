/*
 *  RowTest.java
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
 * Test Suite for {@code Row} enum.
 */
public class RowTest {

    /** Class constructor. */
    public RowTest() { }

    /**
     * Tests the {@code label()} method.
     *
     * @see Row#label()
     */
    @Test
    public final void testLabel() {
        assertThat("Row.R5.label() must return 5.",
                     Row.R5.label(),
                     is("5"));
    }

    /**
     * Tests the {@code neighbor(Direction)} method.
     *
     * @see Row#neighbor(Direction)
     */
    @Test
    public final void testNeighbor() {
        assertThat("Row.R1.neighbor(Direction.W) must return Row.R1",
                   Row.R1.neighbor(Direction.W),
                   is(Row.R1));
        assertThat("Row.R1.neighbor(Direction.E) must return Row.R1.",
                   Row.R1.neighbor(Direction.E),
                   is(Row.R1));
        assertThat("Row.R5.neighbor(Direction.SW) must return Row.R6.",
                   Row.R5.neighbor(Direction.SW),
                   is(Row.R6));
        assertThat("Row.R5.neighbor(Direction.NE) must return Row.R4.",
                   Row.R5.neighbor(Direction.NE),
                   is(Row.R4));
        assertThat("Row.R8.neighbor(Direction.NW) must return Row.R7.",
                   Row.R8.neighbor(Direction.NW),
                   is(Row.R7));
        assertThat("Row.R8.neighbor(Direction.N) must return Row.R7.",
                   Row.R8.neighbor(Direction.N),
                   is(Row.R7));
        assertThat("Row.R8.neighbor(Direction.S) must return Row.NULL.",
                   Row.R8.neighbor(Direction.S),
                   is(Row.NULL));
        assertThat("Row.R8.neighbor(Direction.E) must return Row.R8.",
                   Row.R8.neighbor(Direction.E),
                   is(Row.R8));
    }


}
