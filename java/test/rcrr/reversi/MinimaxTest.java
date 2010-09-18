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

public class MinimaxTest {

    private GameState fixtGameStateInitial;
    private GameState fixtGameStateMinimaxA;
    private GameState fixtGameStateMinimaxB;

    private Strategy fixtStrategyA = Minimax.minimaxSearcher(1, new CountDifference());
    private Strategy fixtStrategyB = Minimax.minimaxSearcher(2, new CountDifference());
    private Strategy fixtStrategyC = Minimax.minimaxSearcher(3, new CountDifference());
    private Strategy fixtStrategyD = Minimax.minimaxSearcher(4, new CountDifference());
    private Strategy fixtStrategyE = Minimax.minimaxSearcher(5, new CountDifference());
    private Strategy fixtStrategyF = Minimax.minimaxSearcher(6, new CountDifference());
    private Strategy fixtStrategyG = Minimax.minimaxSearcher(7, new CountDifference());
    private Strategy fixtStrategyH = Minimax.minimaxSearcher(8, new CountDifference());
    
    @Before
    public void setUp() {
	BoardTest bt = new BoardTest();
	bt.setUp();
	fixtGameStateInitial = GameState.initialGameState(1);
	fixtGameStateMinimaxA = GameState.valueOf(bt.fixtBoardMinimaxA,
						  Player.WHITE,
						  Clock.initialClock(1));
	fixtGameStateMinimaxB = GameState.valueOf(bt.fixtBoardMinimaxB,
						  Player.WHITE,
						  Clock.initialClock(1));
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
     * If the minimax searchs depth enough the moves are all equal, leading
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

}