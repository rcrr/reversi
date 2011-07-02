/*
 *  AlphaBeta2Analytics.java
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

/**
 * Test Suite for the {@code AlphaBeta2} class.
 *
 * @see AlphaBeta2
 */
public class AlphaBeta2Analytics {

    /** Class constructor. */
    public AlphaBeta2Analytics() { }

    /**
     * Dummy test.
     */
    @Test
    public final void testDummy() {

    GameSnapshot snapshot = new GameSnapshot.Builder()
        .withPosition(new GamePosition.Builder()
                      .withBoard(new Board.Builder()
                                 .withSquaresLiteral(0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 2, 1, 0, 0, 0,
                                                     0, 0, 0, 2, 1, 1, 0, 0,
                                                     0, 0, 0, 2, 1, 2, 0, 0,
                                                     0, 0, 0, 0, 2, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 2, 0, 0)
                                 .build())
                      .withPlayer(Player.BLACK)
                      .build())
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withRegister(MoveRegisterFixtures.EMPTY)
        .build();

    for (int i = 1; i < 11; i++) {
        Strategy strategy = AlphaBeta2.getInstance().searcher(i, new ModifiedWeightedSquares());
        Move move = strategy.move(snapshot);
        System.out.println("move=" + move);
    }

        assertThat("Dummy test always succeds.",
                   true,
                   is(true));
    }

}
