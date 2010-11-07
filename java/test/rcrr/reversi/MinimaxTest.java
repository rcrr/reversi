/*
 *  MinimaxTest.java
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

import org.joda.time.Duration;
import org.joda.time.Period;

public class MinimaxTest {

    /** Game state fixtures. */
    private GameSnapshot fixtGameSnapshotInitial;
    private GameSnapshot fixtGameSnapshotMinimaxA;
    private GameSnapshot fixtGameSnapshotMinimaxB;
    private GameSnapshot fixtGameSnapshotB;
    private GameSnapshot fixtGameSnapshotBC3;
    private GameSnapshot fixtGameSnapshotBC6;
    private GameSnapshot fixtGameSnapshotBlackHasToPass;

    private DecisionRule minimax = Minimax.getInstance();
    private DecisionRule alphabeta = AlphaBeta.getInstance();

    /** Strategy fixtures. */
    private Strategy fixtStrategyA = minimax.searcher(1, new CountDifference());
    private Strategy fixtStrategyB = minimax.searcher(2, new CountDifference());
    private Strategy fixtStrategyC = minimax.searcher(3, new CountDifference());
    private Strategy fixtStrategyD = minimax.searcher(4, new CountDifference());
    private Strategy fixtStrategyE = minimax.searcher(5, new CountDifference());
    private Strategy fixtStrategyF = minimax.searcher(6, new CountDifference());
    private Strategy fixtStrategyG = minimax.searcher(7, new CountDifference());
    private Strategy fixtStrategyH = minimax.searcher(8, new CountDifference());

    /** Strategy fixtures. */
    private Strategy fixtStrategyAab = alphabeta.searcher(1, new CountDifference());
    private Strategy fixtStrategyBab = alphabeta.searcher(2, new CountDifference());
    private Strategy fixtStrategyCab = alphabeta.searcher(3, new CountDifference());
    private Strategy fixtStrategyDab = alphabeta.searcher(4, new CountDifference());
    private Strategy fixtStrategyEab = alphabeta.searcher(5, new CountDifference());
    private Strategy fixtStrategyFab = alphabeta.searcher(6, new CountDifference());
    private Strategy fixtStrategyGab = alphabeta.searcher(7, new CountDifference());
    private Strategy fixtStrategyHab = alphabeta.searcher(8, new CountDifference());
    
    /**
     * Prepares the GameSnapshot fixtures. It depends on the public BoardTest fixtures.
     */
    @Before
    public void setUp() {
	BoardTest bt = new BoardTest();
	bt.setUp();
	fixtGameSnapshotInitial = GameSnapshot.initialGameSnapshot(Period.minutes(1).toStandardDuration());

	fixtGameSnapshotMinimaxA = GameSnapshot.valueOf(GamePosition.valueOf(bt.fixtBoardMinimaxA,
									     Player.WHITE),
							Clock.initialClock(Period.minutes(1).toStandardDuration()));
	fixtGameSnapshotMinimaxB = GameSnapshot.valueOf(GamePosition.valueOf(bt.fixtBoardMinimaxB,
									     Player.WHITE),
							Clock.initialClock(Period.minutes(1).toStandardDuration()));
	fixtGameSnapshotB = GameSnapshot.valueOf(GamePosition.valueOf(bt.fixtBoardB,
								      Player.WHITE),
						 Clock.initialClock(Period.minutes(1).toStandardDuration()));
	fixtGameSnapshotBC3 = GameSnapshot.valueOf(GamePosition.valueOf(bt.fixtBoardBC3,
									Player.BLACK),
						   Clock.initialClock(Period.minutes(1).toStandardDuration()));
	fixtGameSnapshotBC6 = GameSnapshot.valueOf(GamePosition.valueOf(bt.fixtBoardBC6,
									Player.BLACK),
						   Clock.initialClock(Period.minutes(1).toStandardDuration()));
	fixtGameSnapshotBlackHasToPass = GameSnapshot.valueOf(GamePosition.valueOf(bt.fixtBoardBlackHasToPass,
										   Player.BLACK),
							      Clock.initialClock(Period.minutes(1).toStandardDuration()));

    }

    /**
     * Tests the minimaxSearcher ply parameter range.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testMinimaxSearcherPlyRange() {
	Strategy s = minimax.searcher(0, new CountDifference());
    }

    /**
     * Tests the minimaxSearcher ef parameter.
     */
    @Test(expected = NullPointerException.class)
    public void testMinimaxSearcherEfNotNull() {
	Strategy s = minimax.searcher(1, null);
    }

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
	assertEquals(Move.valueOf(Square.D3), fixtStrategyA.move(fixtGameSnapshotInitial));
	assertEquals(Move.valueOf(Square.D3), fixtStrategyB.move(fixtGameSnapshotInitial));
	assertEquals(Move.valueOf(Square.D3), fixtStrategyC.move(fixtGameSnapshotInitial));
	assertEquals(Move.valueOf(Square.D3), fixtStrategyD.move(fixtGameSnapshotInitial));
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
	assertEquals(Move.valueOf(Square.E5), fixtStrategyA.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.E5), fixtStrategyB.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyC.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyD.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyE.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyF.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyG.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyH.move(fixtGameSnapshotMinimaxA));

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
	assertEquals(Move.valueOf(Square.C4), fixtStrategyA.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyB.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyC.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyD.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyE.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyF.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyG.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyH.move(fixtGameSnapshotMinimaxB));
    }

    /**
     * minimaxSearcher test.
     * <p>
     * The board fixtBoardB, prepared in BoardTest is here analyzed.
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
	assertEquals(Move.valueOf(Square.C3), fixtStrategyA.move(fixtGameSnapshotB));
	assertEquals(Move.valueOf(Square.B2), fixtStrategyA.move(fixtGameSnapshotBC3));
	assertEquals(Move.valueOf(Square.H3), fixtStrategyA.move(fixtGameSnapshotBC6));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyB.move(fixtGameSnapshotB));

	/** Not "manually" verified. */
	assertEquals(Move.valueOf(Square.C3), fixtStrategyC.move(fixtGameSnapshotB));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyD.move(fixtGameSnapshotB));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyE.move(fixtGameSnapshotB));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyF.move(fixtGameSnapshotB));
    }

    /**
     * Tests the alphabetaSearcher ply parameter range.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testAlphabetaSearcherPlyRange() {
	Strategy s = alphabeta.searcher(0, new CountDifference());
    }

    /**
     * Tests the alphabetaSearcher ef parameter.
     */
    @Test(expected = NullPointerException.class)
    public void testAlphabetaSearcherEfNotNull() {
	Strategy s = alphabeta.searcher(1, null);
    }

    /** See above. */
    @Test
    public void testAlphabetaSearcher() {
	assertEquals(Move.valueOf(Square.D3), fixtStrategyAab.move(fixtGameSnapshotInitial));
	assertEquals(Move.valueOf(Square.D3), fixtStrategyBab.move(fixtGameSnapshotInitial));
	assertEquals(Move.valueOf(Square.D3), fixtStrategyCab.move(fixtGameSnapshotInitial));
	assertEquals(Move.valueOf(Square.D3), fixtStrategyDab.move(fixtGameSnapshotInitial));
    }

    /** See above. */
    @Test
    public void testAlphabetaSearcherA() {
	assertEquals(Move.valueOf(Square.E5), fixtStrategyAab.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.E5), fixtStrategyBab.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyCab.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyDab.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyEab.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyFab.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyGab.move(fixtGameSnapshotMinimaxA));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyHab.move(fixtGameSnapshotMinimaxA));

    }

    /** See above. */
    @Test
    public void testAlphabetaSearcherB() {
	assertEquals(Move.valueOf(Square.C4), fixtStrategyAab.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.C4), fixtStrategyBab.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyCab.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyDab.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyEab.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyFab.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyGab.move(fixtGameSnapshotMinimaxB));
	assertEquals(Move.valueOf(Square.A3), fixtStrategyHab.move(fixtGameSnapshotMinimaxB));
    }

    /** See above. */
    @Test
    public void testAlphabetaSearcherC() {

	/** Manually verified. */
	assertEquals(Move.valueOf(Square.C3), fixtStrategyAab.move(fixtGameSnapshotB));
	assertEquals(Move.valueOf(Square.B2), fixtStrategyAab.move(fixtGameSnapshotBC3));
	assertEquals(Move.valueOf(Square.H3), fixtStrategyAab.move(fixtGameSnapshotBC6));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyBab.move(fixtGameSnapshotB));

	/** Not "manually" verified. */
	assertEquals(Move.valueOf(Square.C3), fixtStrategyCab.move(fixtGameSnapshotB));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyDab.move(fixtGameSnapshotB));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyEab.move(fixtGameSnapshotB));
	assertEquals(Move.valueOf(Square.C3), fixtStrategyFab.move(fixtGameSnapshotB));
    }

    /** Test the maximizer method. */
    @Test
    public void testMaximizer() {
	
	/** The maximixer method is equivalent to a minimax search one ply deep. */
	assertEquals(Move.valueOf(Square.H3), Minimax.maximizer(new CountDifference()).move(fixtGameSnapshotBC6));
	assertEquals(Move.valueOf(Square.E5), Minimax.maximizer(new CountDifference()).move(fixtGameSnapshotMinimaxA));
    }

    /** Test the maximizer when no legal move is available. */
    @Test
    public void testMaximizerWhenNoLegalMoveIsAvailable() {	
	assertEquals(Move.valueOf(Move.Action.PASS), Minimax.maximizer(new CountDifference()).move(fixtGameSnapshotBlackHasToPass));
    }

    /** Test the minimax searcher when no legal move is available. */
    @Test
    public void testMinimaxSearcherWhenNoLegalMoveIsAvailable() {	
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyA.move(fixtGameSnapshotBlackHasToPass));
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyB.move(fixtGameSnapshotBlackHasToPass));
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyC.move(fixtGameSnapshotBlackHasToPass));
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyD.move(fixtGameSnapshotBlackHasToPass));
    }

    /** Test the alphabeta searcher when no legal move is available. */
    @Test
    public void testAlphabetaSearcherWhenNoLegalMoveIsAvailable() {	
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyAab.move(fixtGameSnapshotBlackHasToPass));
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyBab.move(fixtGameSnapshotBlackHasToPass));
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyCab.move(fixtGameSnapshotBlackHasToPass));
	assertEquals(Move.valueOf(Move.Action.PASS), fixtStrategyDab.move(fixtGameSnapshotBlackHasToPass));
    }

}