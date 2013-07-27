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
import rcrr.reversi.board.Square;

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

    final static GamePosition FFO_40_A2 = FFO_40.makeMove(Square.A2);
    final static GamePosition FFO_40_A2_B1 = FFO_40_A2.makeMove(Square.B1);
    final static GamePosition FFO_40_A2_B1_C1 = FFO_40_A2_B1.makeMove(Square.C1);
    final static GamePosition FFO_40_A2_B1_C1_PASS = GamePosition.valueOf(FFO_40_A2_B1_C1.board(), FFO_40_A2_B1_C1.player().opponent());
    final static GamePosition FFO_40_A2_B1_C1_PASS_B6 = FFO_40_A2_B1_C1_PASS.makeMove(Square.B6);
    final static GamePosition FFO_40_A2_B1_C1_PASS_B6_C7 = FFO_40_A2_B1_C1_PASS_B6.makeMove(Square.C7);
    final static GamePosition FFO_40_A2_B1_C1_PASS_B6_C7_A7 = FFO_40_A2_B1_C1_PASS_B6_C7.makeMove(Square.A7);
    final static GamePosition FFO_40_A2_B1_C1_PASS_B6_C7_A7_B7 = FFO_40_A2_B1_C1_PASS_B6_C7_A7.makeMove(Square.B7);

    /** Class constructor. */
    public ExactSolverTest() { }

    /**
     * Tests the {@code solve()} method.
     *
     * @see ExactSolver#solve()
     */
    @Test
    public final void testSolveFFO_40() {

        System.out.printf("FF0_40:\nBlack to move, Turner vs Monnom, Bruxelles 1997.\n%s\n", FFO_40.board().printBoard());

        final SearchNode result = new ExactSolver(FFO_40).solve();

        System.out.printf("%s\n", result);

        System.out.println("BitBoard2.printLog() = " + BitBoard2.printLog());

        assertThat("The value is 38.",
                   result.value(),
                   is(38));
    }

    /*
    @Test
    public final void testSolveFFO_40_A2_B1_C1_PASS_B6_C7() {

        System.out.printf("FFO_40_A2_B1_C1_PASS_B6_C7:\nBlack to move, Turner vs Monnom, Bruxelles 1997.\n%s\n", FFO_40_A2_B1_C1_PASS_B6_C7.board().printBoard());

        final SearchNode result = new ExactSolver(FFO_40_A2_B1_C1_PASS_B6_C7).solve();
        System.out.printf("%s\n", result);

        assertThat("The value is 38.",
                   result.value(),
                   is(38));
    }
    */
}
