/*
 *  RowTest.java
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
     * Tests the {@code shift(int)} method.
     *
     * @see Row#shift(int)
     */
    @Test
    public final void testShift() {
        assertThat("Row.R2.shift(1) must be Row.R3.",
                   Row.R2.shift(1),
                   is(Row.R3));
        assertThat("Row.R2.shift(2) must be Row.R4.",
                   Row.R2.shift(2),
                   is(Row.R4));
        assertThat("Row.R2.shift(6) must be Row.R8.",
                   Row.R2.shift(6),
                   is(Row.R8));
        assertThat("Row.R2.shift(7) must be Row.NULL.",
                   Row.R2.shift(7),
                   is(Row.NULL));
        assertThat("Row.R2.shift(-2) must be Row.NULL.",
                   Row.R2.shift(-2),
                   is(Row.NULL));
        assertThat("Row.R1.shift(-1) must be Row.NULL.",
                   Row.R1.shift(-1),
                   is(Row.NULL));
        assertThat("Row.R1.shift(0) must be Row.R1.",
                   Row.R1.shift(0),
                   is(Row.R1));
        assertThat("Row.R1.shift(+1) must be Row.R2.",
                   Row.R1.shift(+1),
                   is(Row.R2));
        assertThat("Row.R8.shift(-1) must be Row.R7.",
                   Row.R8.shift(-1),
                   is(Row.R7));
        assertThat("Row.R8.shift(0) must be Row.R8.",
                   Row.R8.shift(0),
                   is(Row.R8));
        assertThat("Row.R8.shift(+1) must be Row.NULL.",
                   Row.R8.shift(+1),
                   is(Row.NULL));
    }

}
