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
     * FFO position #05, black to move.
     * Move values: G8:+32. G2:+12. B2:-20. G6:-26. G1:-32. G7:-34.
     *
     * Principal Variation, PV: g8 g7 h8 g2 b2 a2 a1 g6 h7 b7 a8 -- h3
     * Final score is +32
     */
    final static GamePosition FFO_05 = new GamePosition.Builder()
        .withBoard(new BoardBuilder()
                   .withSquaresLiteral(0, 2, 2, 2, 2, 2, 0, 0,
                                       0, 0, 2, 1, 1, 2, 0, 1,
                                       1, 1, 2, 1, 2, 1, 1, 0,
                                       1, 1, 2, 1, 2, 1, 1, 2,
                                       1, 1, 2, 2, 1, 2, 2, 2,
                                       1, 1, 1, 1, 2, 2, 0, 2,
                                       1, 0, 1, 2, 2, 2, 0, 0,
                                       0, 1, 1, 1, 1, 1, 0, 0)
                   .build())
        .withPlayer(Player.BLACK)
        .build();

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
     * Tests the {@code solve()} method.
     *
     * @see ExactSolver#solve()
     */
    @Test
    public final void testSolveFFO_05() {

        final SearchNode result = new ExactSolver(FFO_05).solve();

        assertThat("The value is 32.",
                   result.value(),
                   is(32));

        assertThat("The move is G8.",
                   result.move(),
                   is(Square.G8));
    }
}
