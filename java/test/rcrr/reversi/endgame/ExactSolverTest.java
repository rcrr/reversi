/*
 *  ExactSolverTest.java
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

package rcrr.reversi.endgame;

import rcrr.reversi.SearchNode;
import rcrr.reversi.GamePosition;
import rcrr.reversi.board.BoardBuilder;
import rcrr.reversi.board.Player;

import rcrr.reversi.board.BitBoard2;

import org.junit.Test;

import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code ExactSolver} class.
 */
public class ExactSolverTest {

    /**
     * FFO position #40, black to move, Turner vs Monnom, Bruxelles 1997.
     * Principal Variation, PV: a2 b1 c1 -- b6 c7 a7 b7 b8 d7 f8 c6 f7 g7
     * Final score is +38
     */
    final static GamePosition FFO_40 = new GamePosition.Builder()
        .withBoard(new BoardBuilder()
                   .withSquaresLiteral(2, 0, 0, 2, 2, 2, 2, 1,
                                       0, 2, 2, 2, 2, 2, 2, 1,
                                       2, 2, 1, 1, 2, 2, 2, 1,
                                       2, 2, 1, 2, 2, 2, 1, 1,
                                       2, 2, 2, 2, 2, 2, 1, 1,
                                       0, 0, 0, 2, 2, 2, 2, 1,
                                       0, 0, 0, 0, 2, 0, 0, 1,
                                       0, 0, 0, 0, 0, 0, 0, 0)
                   .build())
        .withPlayer(Player.BLACK)
        .build();

    /** Class constructor. */
    public ExactSolverTest() { }

    /**
     * Tests the {@code dummy()} method.
     *
     * @see ExactSolver#dummy()
     */
    @Test
    public final void testDummy() {

        System.out.printf("FF0_40:\nBlack to move, Turner vs Monnom, Bruxelles 1997.\n%s\n", FFO_40.board().printBoard());

        assertThat("dummy() is true.",
                   new ExactSolver().dummy(),
                   is(true));
    }

    /**
     * Tests the {@code solve()} method.
     *
     * @see ExactSolver#solve()
     */
    @Test
    public final void testSolve() {

        System.out.printf("FF0_40:\nBlack to move, Turner vs Monnom, Bruxelles 1997.\n%s\n", FFO_40.board().printBoard());

        final SearchNode result = new ExactSolver().solve(FFO_40);

        System.out.printf("%s\n", result);

        System.out.println("BitBoard2.printLog() = " + BitBoard2.printLog());

        assertThat("dummy() is true.",
                   new ExactSolver().dummy(),
                   is(true));
    }

}
