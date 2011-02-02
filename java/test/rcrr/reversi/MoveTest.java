/*
 *  MoveTest.java
 *
 *  Copyright (c) 2010 Roberto Corradini. All rights reserved.
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

import org.junit.*;
import static org.junit.Assert.*;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.matchers.JUnitMatchers.*;

public class MoveTest {

    private static final Move.Action AN_ACTION = Move.Action.PASS;
    private static final Move.Action PUT_DISC_ACTION = Move.Action.PUT_DISC;
    private static final Move.Action NULL_ACTION = null;
    private static final Square A_SQUARE = Square.A1;
    private static final Square NULL_SQUARE = null;

    private static final Move MOVE_PUT_DISC_A1 = Move.valueOf(PUT_DISC_ACTION, Square.A1);
    private static final Move MOVE_PASS = Move.valueOf(Move.Action.PASS, null);
    private static final Move MOVE_RESIGN = Move.valueOf(Move.Action.RESIGN, null);

    /**
     * Tests that the action method returns the action field.
     */
    @Test
    public void testAction() {
        assertThat("Move's Action for MOVE_PUT_DISC_A1 is PUT_DISC.",
                   MOVE_PUT_DISC_A1.action(), is(Move.Action.PUT_DISC));
    }

    /**
     * Tests thta the square method returns the square field.
     */
    @Test
    public void testSquare() {
        assertThat("Move's Action for MOVE_PUT_DISC_A1 is PUT_DISC.",
                   MOVE_PUT_DISC_A1.square(), is(Square.A1));
    }

    @Test
    public void testValueOf_withSquareParameter() {
        assertThat("Move.valueOf(Square.A1) is an instance of Move class.",
                   Move.valueOf(Square.A1), instanceOf(Move.class));
    }

    @Test(expected = NullPointerException.class)
    public void testValueOf_boundaryConditions_withSquareParameter() {
        Move.valueOf(NULL_SQUARE);
    }

    @Test
    public void testValueOf_withActionParameter() {
        assertThat("Move.valueOf(Move.Action.PASS) is an instance of Move class.",
                   Move.valueOf(Move.Action.PASS), instanceOf(Move.class));
    }

    @Test(expected = NullPointerException.class)
    public void testValueOf_boundaryConditions_withActionParameter() {
        Move.valueOf(NULL_ACTION);
    }

    @Test
    public void testValueOf_withActionAndSquareParameters() {
        assertThat("Move.valueOf(Move.Action.PUT_DISC, Square.H8) is an instance of Move class.",
                   Move.valueOf(Move.Action.PUT_DISC, Square.H8), instanceOf(Move.class));
    }

    @Test(expected = NullPointerException.class)
    public void testValueOf_boundaryConditions_c1_withActionAndSquareParameters() {
        Move.valueOf(NULL_ACTION, A_SQUARE);
    }

    @Test(expected = NullPointerException.class)
    public void testValueOf_boundaryConditions_c2_withActionAndSquareParameters() {
        Move.valueOf(PUT_DISC_ACTION, NULL_SQUARE);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testValueOf_boundaryConditions_c3_withActionAndSquareParameters() {
        Move.valueOf(AN_ACTION, A_SQUARE);
    }

}
