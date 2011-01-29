/*
 *  MinimaxTest.java
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

import org.junit.*;
import static org.junit.Assert.*;

import org.joda.time.Duration;
import org.joda.time.Period;

public class MinimaxTest {

    private DecisionRule minimax = Minimax.getInstance();

    /** Strategy fixtures. */
    private Strategy fixtStrategyA = minimax.searcher(1, new CountDifference());
    private Strategy fixtStrategyB = minimax.searcher(2, new CountDifference());
    private Strategy fixtStrategyC = minimax.searcher(3, new CountDifference());
    private Strategy fixtStrategyD = minimax.searcher(4, new CountDifference());
    private Strategy fixtStrategyE = minimax.searcher(5, new CountDifference());
    private Strategy fixtStrategyF = minimax.searcher(6, new CountDifference());
    private Strategy fixtStrategyG = minimax.searcher(7, new CountDifference());
    private Strategy fixtStrategyH = minimax.searcher(8, new CountDifference());

    /**
     * minimaxSearcher test.
     * The method is tested using the most straightforward evaluation function,
     * based on the count difference method.
     * <p>
     * The test run from depth 1 to 4. Given that the first move is fully symmetric
     * and that when there are legal moves having the same evaluation, the first is
     * selected, the move d3 is always selected.
     */
    @Test
    public void testMinimaxSearcher() {
	assertEquals(Move.valueOf(Square.D3), fixtStrategyA.move(GameSnapshotFixtures.INITIAL));
	assertEquals(Move.valueOf(Square.D3), fixtStrategyB.move(GameSnapshotFixtures.INITIAL));
	assertEquals(Move.valueOf(Square.D3), fixtStrategyC.move(GameSnapshotFixtures.INITIAL));
	assertEquals(Move.valueOf(Square.D3), fixtStrategyD.move(GameSnapshotFixtures.INITIAL));
    }

    /**
     * minimaxSearcher test.
     * <p>
     * The test run from depth 1 to 8. The White has to move.
     * The Black doesn't have any move. The white has four legal moves.
     * The move sequence is not changing the result in any way.
     * If the minimax searches depth enough the moves are all equal, leading
     * to the same forced position. Given that searching one ply returns the
     * move with the higher result, searching eight ply returns the the first
     * legal move.
     */
    @Test
    public void testMinimaxSearcherA() {
	assertEquals(Move.valueOf(Square.E5), fixtStrategyA.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_A));
	assertEquals(Move.valueOf(Square.E5), fixtStrategyB.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_A));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyC.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_A));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyD.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_A));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyE.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_A));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyF.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_A));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyG.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_A));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyH.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_A));

    }

    /**
     * minimaxSearcher test.
     * <p>
     * The test run from depth 1 to 8. The white has to move.
     * The black can now reply.
     * The white has two options: C4 or A3, C4 is more aggressive, but open
     * the game. A3 flips less in the short term, but forces a win in the
     * following two moves. The minimax recognizes it, and searching three
     * ply change the selected move to A3.
     */
    @Test
    public void testMinimaxSearcherB() {
	assertEquals(Move.valueOf(Square.C4), fixtStrategyA.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_B));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyB.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_B));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyC.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_B));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyD.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_B));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyE.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_B));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyF.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_B));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyG.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_B));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyH.move(GameSnapshotFixtures.MINIMAX_TEST_CASE_B));
    }

    /**
     * minimaxSearcher test.
     * <p>
     * The board BoardFixtures.EARLY_GAME_B_9_MOVES is here analyzed.
     * The white has to move, and has two choices: C3 and C6.
     * The two moves lead respectively to two boards: BC3 and BC6.
     * One ply analysis select C3 because the value of the two moves
     * is the same, and then the first is selected.
     * Two ply analysis select again C3, because the value is higher.
     * One ply analysis is checked to the black player for both BC3
     * and BC6 game boards.
     * <p>
     * Searching deeper, the minimax stays on the C3 selection. Those
     * cases have not been verified.
     */
    @Test
    public void testMinimaxSearcherC() {

	/** Manually verified. */
	assertEquals(Move.valueOf(Square.C3), fixtStrategyA.move(GameSnapshotFixtures.EARLY_GAME_B_9_MOVES));
	assertEquals(Move.valueOf(Square.B2), fixtStrategyA.move(GameSnapshotFixtures.EARLY_GAME_BC3_10_MOVES));
	assertEquals(Move.valueOf(Square.H3), fixtStrategyA.move(GameSnapshotFixtures.EARLY_GAME_BC6_10_MOVES));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyB.move(GameSnapshotFixtures.EARLY_GAME_B_9_MOVES));

	/** Not "manually" verified. */
	assertEquals(Move.valueOf(Square.C3), fixtStrategyC.move(GameSnapshotFixtures.EARLY_GAME_B_9_MOVES));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyD.move(GameSnapshotFixtures.EARLY_GAME_B_9_MOVES));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyE.move(GameSnapshotFixtures.EARLY_GAME_B_9_MOVES));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyF.move(GameSnapshotFixtures.EARLY_GAME_B_9_MOVES));
    }

    /** Test the minimax searcher when no legal move is available. */
    @Test
    public void testMinimaxSearcherWhenNoLegalMoveIsAvailable() {	
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyA.move(GameSnapshotFixtures.BLACK_HAS_TO_PASS));
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyB.move(GameSnapshotFixtures.BLACK_HAS_TO_PASS));
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyC.move(GameSnapshotFixtures.BLACK_HAS_TO_PASS));
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyD.move(GameSnapshotFixtures.BLACK_HAS_TO_PASS));
    }

}
