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

import java.util.Map;
import java.util.HashMap;

import org.junit.Test;
import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for the {@code AlphaBeta2} class.
 *
 * @see AlphaBeta2
 */
public class AlphaBeta2Analytics {

    private static final Map<String, Object> NO_STATISTICS = null;

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

    for (int i = 1; i < 5; i++) {
        Strategy strategy = AlphaBeta2.getInstance(AlphaBeta2.Variant.MINIMAX, NO_STATISTICS).searcher(i, new ModifiedWeightedSquares());
        Move move = strategy.move(snapshot);
    }

    assertThat("Dummy test always succeds.",
	       true,
	       is(true));
    }

    private static final EvalFunction EF = new ModifiedWeightedSquares();
    private static final int PLY = 6;
    private static final int INITIAL_RANDOM_MOVES = 10;
    private static final int UPPER_END_BOUND = 54;

    @Test
    public final void testNumberOfBoardEvaluations() {

        final Game game = new Game.Builder()
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK, Actor.valueOf("Random Player A", new RandomStrategy()))
                        .withActor(Player.WHITE, Actor.valueOf("Random Player B", new RandomStrategy()))
                        .build())
            .withPrintStream(new NullPrintStream())
            .withSequence(Game.randomGame(INITIAL_RANDOM_MOVES).sequence())
	    .build();

	while ((64 - game.board().countPieces(SquareState.EMPTY)) < UPPER_END_BOUND) {
	    if (!game.board().hasAnyPlayerAnyLegalMove()) break;
	    for (AlphaBeta2.Variant v : AlphaBeta2.Variant.values()) {
		Map<String, Object> statistics = new HashMap<String, Object>();
		Strategy strategy = AlphaBeta2.getInstance(v, statistics).searcher(PLY, EF);
		Move move = strategy.move(game.lastGameSnapshot());
		System.out.println("statistics=" + statistics);
	    }
	    Move randomMove = game.move();
	    System.out.println("--- --- ---");
	}

        assertThat("The test is used to prepare the analysis table.",
                   true,
                   is(true));
    }

}
