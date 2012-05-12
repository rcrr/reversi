/*
 *  MoveTest.java
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

package rcrr.reversi;

import rcrr.reversi.board.Board;
import rcrr.reversi.board.BoardFixtures;
import rcrr.reversi.board.BoardBuilder;
import rcrr.reversi.board.Player;
import rcrr.reversi.board.Square;
import rcrr.reversi.board.SquareState;

import org.junit.Test;
import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code Move} class.
 */
public class MoveTest {

    /** Class constructor. */
    public MoveTest() { }

    /**
     * Tests that the {@code action()} method returns the action field.
     *
     * @see Move#action()
     */
    @Test
    public final void testAction() {
        assertThat("Move's Action for MOVE_PUT_DISC_A1 is PUT_DISC.",
                   Move.A_REGULAR_INSTANCE.action(),
                   is(Move.Action.PUT_DISC));
    }

    /**
     * Tests that the {@code square()} method returns the square field.
     *
     * @see Move#square()
     */
    @Test
    public final void testSquare() {
        assertThat("Move's square for Move.valueOf(Square.A1) is Square.A1.",
                   Move.valueOf(Square.A1).square(),
                   is(Square.A1));
        assertThat("Move's square for Move.valueOf(Move.Action.PASS) is Square.NULL.",
                   Move.valueOf(Move.Action.PASS).square(),
                   is(Square.NULL));
    }

    /**
     * Tests the {@code valueOf(Square)} factory.
     *
     * @see Move#valueOf(Square)
     */
    @Test
    public final void testValueOf_withSquareParameter() {
        assertThat("Move.valueOf(Square.AN_INSTANCE) is an instance of Move class.",
                   Move.valueOf(Square.AN_INSTANCE), instanceOf(Move.class));
    }

    /**
     * Tests the {@code valueOf(Square)} factory when parameter
     * {@code square} is {@code null}.
     *
     * @see Move#valueOf(Square)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_withSquareParameter_boundaryConditions_checkNullParameter_square() {
        Move.valueOf(Square.NULL);
    }

    /**
     * Tests the {@code valueOf(Action)} factory.
     *
     * @see Move#valueOf(Action)
     */
    @Test
    public final void testValueOf_withActionParameter() {
        assertThat("Move.valueOf(Move.Action.PASS) is an instance of Move class.",
                   Move.valueOf(Move.Action.PASS), instanceOf(Move.class));
    }

    /**
     * Tests the {@code valueOf(Action)} factory when parameter
     * {@code action} is {@code null}.
     *
     * @see Move#valueOf(Action)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_withActionParameter_boundaryConditions_checkNullParameter_action() {
        Move.valueOf(Move.Action.NULL);
    }

    /**
     * Tests the {@code valueOf(Action)} factory when parameter
     * {@code action} is {@code PUT_DISC}.
     *
     * @see Move#valueOf(Action)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testValueOf_withActionParameter_boundaryConditions_checkIllegalArgument_action() {
        Move.valueOf(Move.Action.PUT_DISC);
    }

    /**
     * Tests the {@code valueOf(Action, Square)} factory.
     *
     * @see Move#valueOf(Action, Square)
     */
    @Test
    public final void testValueOf_withActionAndSquareParameters() {
        assertThat("Move.valueOf(Move.Action.PUT_DISC, Square.H8) is an instance of Move class.",
                   Move.valueOf(Move.Action.PUT_DISC, Square.H8), instanceOf(Move.class));
        assertThat("Move.valueOf(Move.Action.PASS, Square.NULL) is an instance of Move class.",
                   Move.valueOf(Move.Action.PASS, Square.NULL), instanceOf(Move.class));
    }

    /**
     * Tests the {@code valueOf(Action, Square)} factory when parameter
     * {@code action} is {@code null}.
     *
     * @see Move#valueOf(Action, Square)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_withActionAndSquareParameters_boundaryConditions_checkNullParameter_action() {
        Move.valueOf(Move.Action.NULL, Square.AN_INSTANCE);
    }

    /**
     * Tests the {@code valueOf(Action, Square)} factory when parameter
     * {@code action} is {@code PUT_DISC} and parameter {@code square} is {@code null}.
     *
     * @see Move#valueOf(Action, Square)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_withActionAndSquareParameters_boundaryConditions_checkNullParameter_square() {
        Move.valueOf(Move.Action.PUT_DISC, Square.NULL);
    }

    /**
     * Tests the {@code valueOf(Action, Square)} factory when parameter
     * {@code action} is not {@code PUT_DISC} and parameter {@code square} is not {@code null}.
     *
     * @see Move#valueOf(Action, Square)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testValueOf_withActionAndSquareParameters_boundaryConditions_checkIllegalArgument() {
        Move.valueOf(Move.Action.AN_INSTANCE, Square.AN_INSTANCE);
    }

}
