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
    private GameState fixtGameStateInitial;
    private GameState fixtGameStateMinimaxA;
    private GameState fixtGameStateMinimaxB;
    private GameState fixtGameStateB;
    private GameState fixtGameStateBC3;
    private GameState fixtGameStateBC6;
    private GameState fixtGameStateBlackHasToPass;

    /** Strategy fixtures. */
    private Strategy fixtStrategyA = Minimax.minimaxSearcher(1, new CountDifference());
    private Strategy fixtStrategyB = Minimax.minimaxSearcher(2, new CountDifference());
    private Strategy fixtStrategyC = Minimax.minimaxSearcher(3, new CountDifference());
    private Strategy fixtStrategyD = Minimax.minimaxSearcher(4, new CountDifference());
    private Strategy fixtStrategyE = Minimax.minimaxSearcher(5, new CountDifference());
    private Strategy fixtStrategyF = Minimax.minimaxSearcher(6, new CountDifference());
    private Strategy fixtStrategyG = Minimax.minimaxSearcher(7, new CountDifference());
    private Strategy fixtStrategyH = Minimax.minimaxSearcher(8, new CountDifference());

    /** Strategy fixtures. */
    private Strategy fixtStrategyAab = Minimax.alphabetaSearcher(1, new CountDifference());
    private Strategy fixtStrategyBab = Minimax.alphabetaSearcher(2, new CountDifference());
    private Strategy fixtStrategyCab = Minimax.alphabetaSearcher(3, new CountDifference());
    private Strategy fixtStrategyDab = Minimax.alphabetaSearcher(4, new CountDifference());
    private Strategy fixtStrategyEab = Minimax.alphabetaSearcher(5, new CountDifference());
    private Strategy fixtStrategyFab = Minimax.alphabetaSearcher(6, new CountDifference());
    private Strategy fixtStrategyGab = Minimax.alphabetaSearcher(7, new CountDifference());
    private Strategy fixtStrategyHab = Minimax.alphabetaSearcher(8, new CountDifference());
    
    /**
     * Prepares the GameState fixtures. It depends on the public BoardTest fixtures.
     */
    @Before
    public void setUp() {
	BoardTest bt = new BoardTest();
	bt.setUp();
	fixtGameStateInitial = GameState.initialGameState(Period.minutes(1).toStandardDuration());
	fixtGameStateMinimaxA = GameState.valueOf(bt.fixtBoardMinimaxA,
						  Player.WHITE,
						  Clock.initialClock(Period.minutes(1).toStandardDuration()));
	fixtGameStateMinimaxB = GameState.valueOf(bt.fixtBoardMinimaxB,
						  Player.WHITE,
						  Clock.initialClock(Period.minutes(1).toStandardDuration()));
	fixtGameStateB = GameState.valueOf(bt.fixtBoardB,
					   Player.WHITE,
					   Clock.initialClock(Period.minutes(1).toStandardDuration()));
	fixtGameStateBC3 = GameState.valueOf(bt.fixtBoardBC3,
					     Player.BLACK,
					     Clock.initialClock(Period.minutes(1).toStandardDuration()));
	fixtGameStateBC6 = GameState.valueOf(bt.fixtBoardBC6,
					     Player.BLACK,
					     Clock.initialClock(Period.minutes(1).toStandardDuration()));
	fixtGameStateBlackHasToPass = GameState.valueOf(bt.fixtBoardBlackHasToPass,
							Player.BLACK,
							Clock.initialClock(Period.minutes(1).toStandardDuration()));

    }

    /**
     * Tests the minimaxSearcher ply parameter range.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testMinimaxSearcherPlyRange() {
	Strategy s = Minimax.minimaxSearcher(0, new CountDifference());
    }

    /**
     * Tests the minimaxSearcher ef parameter.
     */
    @Test(expected = NullPointerException.class)
    public void testMinimaxSearcherEfNotNull() {
	Strategy s = Minimax.minimaxSearcher(1, null);
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
	assertEquals(Square.D3, fixtStrategyA.move(fixtGameStateInitial));
	assertEquals(Square.D3, fixtStrategyB.move(fixtGameStateInitial));
	assertEquals(Square.D3, fixtStrategyC.move(fixtGameStateInitial));
	assertEquals(Square.D3, fixtStrategyD.move(fixtGameStateInitial));
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
	assertEquals(Square.E5, fixtStrategyA.move(fixtGameStateMinimaxA));
	assertEquals(Square.E5, fixtStrategyB.move(fixtGameStateMinimaxA));
	assertEquals(Square.C4, fixtStrategyC.move(fixtGameStateMinimaxA));
	assertEquals(Square.C4, fixtStrategyD.move(fixtGameStateMinimaxA));
	assertEquals(Square.C4, fixtStrategyE.move(fixtGameStateMinimaxA));
	assertEquals(Square.C4, fixtStrategyF.move(fixtGameStateMinimaxA));
	assertEquals(Square.A3, fixtStrategyG.move(fixtGameStateMinimaxA));
	assertEquals(Square.A3, fixtStrategyH.move(fixtGameStateMinimaxA));

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
	assertEquals(Square.C4, fixtStrategyA.move(fixtGameStateMinimaxB));
	assertEquals(Square.C4, fixtStrategyB.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyC.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyD.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyE.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyF.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyG.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyH.move(fixtGameStateMinimaxB));
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
	assertEquals(Square.C3, fixtStrategyA.move(fixtGameStateB));
	assertEquals(Square.B2, fixtStrategyA.move(fixtGameStateBC3));
	assertEquals(Square.H3, fixtStrategyA.move(fixtGameStateBC6));
	assertEquals(Square.C3, fixtStrategyB.move(fixtGameStateB));

	/** Not "manually" verified. */
	assertEquals(Square.C3, fixtStrategyC.move(fixtGameStateB));
	assertEquals(Square.C3, fixtStrategyD.move(fixtGameStateB));
	assertEquals(Square.C3, fixtStrategyE.move(fixtGameStateB));
	assertEquals(Square.C3, fixtStrategyF.move(fixtGameStateB));
    }

    /**
     * Tests the alphabetaSearcher ply parameter range.
     */
    @Test(expected = IllegalArgumentException.class)
    public void testAlphabetaSearcherPlyRange() {
	Strategy s = Minimax.alphabetaSearcher(0, new CountDifference());
    }

    /**
     * Tests the alphabetaSearcher ef parameter.
     */
    @Test(expected = NullPointerException.class)
    public void testAlphabetaSearcherEfNotNull() {
	Strategy s = Minimax.alphabetaSearcher(1, null);
    }

    /** See above. */
    @Test
    public void testAlphabetaSearcher() {
	assertEquals(Square.D3, fixtStrategyAab.move(fixtGameStateInitial));
	assertEquals(Square.D3, fixtStrategyBab.move(fixtGameStateInitial));
	assertEquals(Square.D3, fixtStrategyCab.move(fixtGameStateInitial));
	assertEquals(Square.D3, fixtStrategyDab.move(fixtGameStateInitial));
    }

    /** See above. */
    @Test
    public void testAlphabetaSearcherA() {
	assertEquals(Square.E5, fixtStrategyAab.move(fixtGameStateMinimaxA));
	assertEquals(Square.E5, fixtStrategyBab.move(fixtGameStateMinimaxA));
	assertEquals(Square.C4, fixtStrategyCab.move(fixtGameStateMinimaxA));
	assertEquals(Square.C4, fixtStrategyDab.move(fixtGameStateMinimaxA));
	assertEquals(Square.C4, fixtStrategyEab.move(fixtGameStateMinimaxA));
	assertEquals(Square.C4, fixtStrategyFab.move(fixtGameStateMinimaxA));
	assertEquals(Square.A3, fixtStrategyGab.move(fixtGameStateMinimaxA));
	assertEquals(Square.A3, fixtStrategyHab.move(fixtGameStateMinimaxA));

    }

    /** See above. */
    @Test
    public void testAlphabetaSearcherB() {
	assertEquals(Square.C4, fixtStrategyAab.move(fixtGameStateMinimaxB));
	assertEquals(Square.C4, fixtStrategyBab.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyCab.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyDab.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyEab.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyFab.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyGab.move(fixtGameStateMinimaxB));
	assertEquals(Square.A3, fixtStrategyHab.move(fixtGameStateMinimaxB));
    }

    /** See above. */
    @Test
    public void testAlphabetaSearcherC() {

	/** Manually verified. */
	assertEquals(Square.C3, fixtStrategyAab.move(fixtGameStateB));
	assertEquals(Square.B2, fixtStrategyAab.move(fixtGameStateBC3));
	assertEquals(Square.H3, fixtStrategyAab.move(fixtGameStateBC6));
	assertEquals(Square.C3, fixtStrategyBab.move(fixtGameStateB));

	/** Not "manually" verified. */
	assertEquals(Square.C3, fixtStrategyCab.move(fixtGameStateB));
	assertEquals(Square.C3, fixtStrategyDab.move(fixtGameStateB));
	assertEquals(Square.C3, fixtStrategyEab.move(fixtGameStateB));
	assertEquals(Square.C3, fixtStrategyFab.move(fixtGameStateB));
    }

    /** Test the maximizer method. */
    @Test
    public void testMaximizer() {
	
	/** The maximixer method is equivalent to a minimax search one ply deep. */
	assertEquals(Square.H3, Minimax.maximizer(new CountDifference()).move(fixtGameStateBC6));
	assertEquals(Square.E5, Minimax.maximizer(new CountDifference()).move(fixtGameStateMinimaxA));
    }

    /** Test the maximizer when no legal move is available. */
    @Test
    public void testMaximizerWhenNoLegalMoveIsAvailable() {	
	assertEquals(null, Minimax.maximizer(new CountDifference()).move(fixtGameStateBlackHasToPass));
    }

    /** Test the minimax searcher when no legal move is available. */
    @Test
    public void testMinimaxSearcherWhenNoLegalMoveIsAvailable() {	
	assertEquals(null, fixtStrategyA.move(fixtGameStateBlackHasToPass));
	assertEquals(null, fixtStrategyB.move(fixtGameStateBlackHasToPass));
	assertEquals(null, fixtStrategyC.move(fixtGameStateBlackHasToPass));
	assertEquals(null, fixtStrategyD.move(fixtGameStateBlackHasToPass));
    }

    /** Test the alphabeta searcher when no legal move is available. */
    @Test
    public void testAlphabetaSearcherWhenNoLegalMoveIsAvailable() {	
	assertEquals(null, fixtStrategyAab.move(fixtGameStateBlackHasToPass));
	assertEquals(null, fixtStrategyBab.move(fixtGameStateBlackHasToPass));
	assertEquals(null, fixtStrategyCab.move(fixtGameStateBlackHasToPass));
	assertEquals(null, fixtStrategyDab.move(fixtGameStateBlackHasToPass));
    }

}