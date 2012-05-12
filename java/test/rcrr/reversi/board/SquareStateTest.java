/*
 *  SquareStateTest.java
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

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code SquareTest} class.
 */
public class SquareStateTest {

    /** Class constructor. */
    public SquareStateTest() { }

    /**
     * Tests the {@code valueOfSymbol(String)} method.
     *
     * @see SquareState#valueOfSymbol(String)
     */
    @Test
    public final void testValueOfSymbol() {
        assertThat("SquareState.valueOfSymbol(\"@\") is SquareState.BLACK.",
                   SquareState.valueOfSymbol("@"),
                   is(SquareState.BLACK));
        assertThat("SquareState.valueOfSymbol(\"O\") is SquareState.WHITE.",
                   SquareState.valueOfSymbol("O"),
                   is(SquareState.WHITE));
        assertThat("SquareState.valueOfSymbol(\".\") is SquareState.EMPTY.",
                   SquareState.valueOfSymbol("."),
                   is(SquareState.EMPTY));
        assertThat("SquareState.valueOfSymbol(\"?\") is SquareState.OUTER.",
                   SquareState.valueOfSymbol("?"),
                   is(SquareState.OUTER));
        assertThat("SquareState.valueOfSymbol(\"foo\") is SquareState.NULL.",
                   SquareState.valueOfSymbol("foo"),
                   is(SquareState.NULL));
        assertThat("SquareState.valueOfSymbol(null) is SquareState.NULL.",
                   SquareState.valueOfSymbol(null),
                   is(SquareState.NULL));
    }

    /**
     * Tests the {@code symbol()} method.
     *
     * @see SquareState#symbol()
     */
    @Test
    public final void testSymbol() {
        assertThat("SquareState.BLACK.symbol() is @.",
                   SquareState.BLACK.symbol(),
                   is("@"));
        assertThat("SquareState.WHITE.symbol() is O.",
                   SquareState.WHITE.symbol(),
                   is("O"));
        assertThat("SquareState.EMPTY.symbol() is ..",
                   SquareState.EMPTY.symbol(),
                   is("."));
        assertThat("SquareState.OUTER.symbol() is ?.",
                   SquareState.OUTER.symbol(),
                   is("?"));
    }

}
