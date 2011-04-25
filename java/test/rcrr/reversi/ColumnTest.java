/*
 *  ColumnTest.java
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
     * Tests the {@code getInstance(int)} method when parameter
     * {@code index} is out of bound.
     *
     * @see Column#getInstance(int)
     */
    @Test(expected = IndexOutOfBoundsException.class)
    public final void testGetInstance_boundaryConditions_checkIndexOutOfBounds_index() {
        Column.getInstance(-1);

        /** This statement is never reached, but if it would be, an exception must be risen. */
        Column.getInstance(Column.values().length);
    }

    /**
     * Tests the {@code getInstance(int)} method.
     *
     * @see Column#getInstance(int)
     */
    @Test
    public final void testGetInstance() {
        assertThat("Column.getInstance(0) must return Column.A.",
                   Column.getInstance(0),
                   is(Column.A));
        assertThat("Column.getInstance(2) must return Column.C.",
                   Column.getInstance(2),
                   is(Column.C));
        assertThat("Column.getInstance(7) must return Column.H.",
                   Column.getInstance(7),
                   is(Column.H));
    }

    /**
     * Tests the {@code shift(int)} method.
     *
     * @see Column#shift(int)
     */
    @Test
    public final void testShift() {
        assertThat("Column.A.shift(-1) must return Column.NULL.",
                   Column.A.shift(-1),
                   is(Column.NULL));
        assertThat("Column.A.shift(0) must return Column.A.",
                   Column.A.shift(0),
                   is(Column.A));
        assertThat("Column.A.shift(+1) must return Column.B.",
                   Column.A.shift(+1),
                   is(Column.B));
        assertThat("Column.E.shift(-1) must return Column.D.",
                   Column.E.shift(-1),
                   is(Column.D));
        assertThat("Column.E.shift(0) must return Column.E.",
                   Column.E.shift(0),
                   is(Column.E));
        assertThat("Column.E.shift(+1) must return Column.F.",
                   Column.E.shift(+1),
                   is(Column.F));
        assertThat("Column.H.shift(-1) must return Column.G.",
                   Column.H.shift(-1),
                   is(Column.G));
        assertThat("Column.H.shift(0) must return Column.H.",
                   Column.H.shift(0),
                   is(Column.H));
        assertThat("Column.H.shift(+1) must return Column.NULL.",
                   Column.H.shift(+1),
                   is(Column.NULL));
        assertThat("Column.D.shift(+10) must return Column.NULL.",
                   Column.D.shift(+10),
                   is(Column.NULL));
    }

}
