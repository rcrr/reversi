/*
 *  MaximizeDifferenceTest.java
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
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.anyOf;

/**
 * Test Suite for {@code MaximizeDifference} class.
 */
public class MaximizeDifferenceTest {

    /** Class constructor. */
    public MaximizeDifferenceTest() { }

    /**
     * Tests if the {@code MaximizeDifference()} constructor return an instance of
     * {@code MaximizeDifference} class.
     *
     * @see MaximizeDifference#MaximizeDifference()
     */
    @Test
    public final void testMaximizeDifference() {
        assertThat("new MaximizeDifference()"
                   + " must return an instance of MaximizeDifference class.",
                   new MaximizeDifference(),
                   instanceOf(MaximizeDifference.class));
    }

    /**
     * Tests the {@code move(GameSnapshot)} method when parameter {@code gameSnapshot}
     * is {@code null}.
     *
     * @see MaximizeDifference#move(GameSnapshot)
     */
    @Test(expected = NullPointerException.class)
    public final void testMove_boundaryConditions_checkNullParameter() {
        new MaximizeDifference().move(GameSnapshotFixtures.NULL);
    }

    /**
     * Tests the {@code move(GameSnapshot)} method.
     *
     * @see MaximizeDifference#move(GameSnapshot)
     */
    @Test
    @SuppressWarnings("unchecked")
    public final void testMove() {
        assertThat("Given the initial position all the moves are equal.",
                   new MaximizeDifference().move(GameSnapshotFixtures.INITIAL),
                   anyOf(is(Move.valueOf(Square.D3)),
                         is(Move.valueOf(Square.F5)),
                         is(Move.valueOf(Square.E6)),
                         is(Move.valueOf(Square.C4))));

        assertThat("Given the MINIMAX_TEST_CASE_A the expected move is E5.",
                   new MaximizeDifference().move(GameSnapshotFixtures.MINIMAX_TEST_CASE_A),
                   is(Move.valueOf(Square.E5)));

    }

}
