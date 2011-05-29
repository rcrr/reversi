/*
 *  SearchNodeTest.java
 *
 *  Copyright (c) 2011 Roberto Corradini. All rights reserved.
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
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for the {@code SearchNode} class.
 *
 * @see SearchNode
 */
public class SearchNodeTest {

    /** Class constructor. */
    public SearchNodeTest() { }

    /**
     * Tests the {@code move()} getter method.
     *
     * @see SearchNode#move()
     */
    @Test
    public final void testMove() {
        assertThat("new SearchNode.Builder().withMove(Square.B5).build().move() is Square.B5.",
                   new SearchNode.Builder()
                   .withMove(Square.B5)
                   .build()
                   .move(),
                   is(Square.B5));
    }

    /**
     * Tests the {@code value()} getter method.
     *
     * @see SearchNode#value()
     */
    @Test
    public final void testValue() {
        assertThat("new SearchNode.Builder().withValue(19).build().value() is 19.",
                   new SearchNode.Builder()
                   .withValue(19)
                   .build()
                   .value(),
                   is(19));
    }

    /**
     * Tests the {@code negated()} method.
     *
     * @see SearchNode#negated()
     */
    @Test
    public final void testNegated() {
        assertThat("new SearchNode.Builder().withValue(19).build().negated().value() is -19.",
                   new SearchNode.Builder()
                   .withValue(19)
                   .build()
                   .negated()
                   .value(),
                   is(-19));
    }

    /**
     * Tests the {@code toString()} method.
     *
     * @see SearchNode#toString()
     */
    @Test
    public final void testToString() {
        assertThat("When calling toString() on a search node having"
                   + " a Square.F7 assigned to move"
                   + " and a 37 assigned to value must return"
                   + " [move=F7, value=37].",
                   new SearchNode.Builder()
                   .withMove(Square.F7)
                   .withValue(37)
                   .build()
                   .toString(),
                   is("[move=F7, value=37]"));
    }

    /**
     * Tests if the {@code valueOf} method return an instance of
     * {@code SearchNode} class.
     *
     * @see SearchNode#valueOf(Square, int)
     */
    @Test
    public final void testValueOf() {
        assertThat("SearchNode.valueOf(Square.A1, 7)"
                   + " must return an instance of SearchNode class.",
                   SearchNode.valueOf(Square.A1, 7),
                   instanceOf(SearchNode.class));
    }

}
