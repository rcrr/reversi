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
import static org.junit.Assert.fail;

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
     * Remainds that all tests are missing.
     */
    @Test
    public final void testReminder() {
        fail("Tests for this class are all missing.");
    }

    /**
     * Tests the {@code move()} getter method.
     *
     * @see SearchNode#move()
     */
    @Test
    public final void testMove() {
        assertThat("new SearchNodeBuilder().withMove(Square.B5).build().move() is Square.B5.",
                   new SearchNodeBuilder()
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
        assertThat("new SearchNodeBuilder().withValue(19).build().value() is 19.",
                   new SearchNodeBuilder()
                   .withValue(19)
                   .build()
                   .value(),
                   is(19));
    }

}
