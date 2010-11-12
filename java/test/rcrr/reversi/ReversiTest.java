/*
 *  ReversiTest.java
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

// To do:
// write all tests

package rcrr.reversi;

import java.util.List;
import java.util.ArrayList;

import org.junit.*;
import static org.junit.Assert.*;

import java.io.PrintStream;

import org.joda.time.Duration;
import org.joda.time.Period;

public class ReversiTest {

    static final Duration STANDARD_GAME_DURATION = Period.minutes(30).toStandardDuration();
    static final PrintStream NO_PRINT = null;
    
    
    @Test
    public void testPAIP_18_4_0() {
	/**
	 * PAIP pp. 614 ends the game [@=53 O=0 (+53)].
	 */
	assertEquals(+53, Reversi.reversi(Minimax.getInstance().searcher(3, new CountDifference()),
					  Minimax.maximizer(new CountDifference()),
					  NO_PRINT,
					  STANDARD_GAME_DURATION));
    }

    @Test
    public void testPAIP_18_6_0() {
	assertEquals(-16, Reversi.reversi(AlphaBeta.getInstance().searcher(4, new CountDifference()),
					  AlphaBeta.getInstance().searcher(4, new WeightedSquares()),
					  NO_PRINT,
					  STANDARD_GAME_DURATION));

	assertEquals(-16, Reversi.reversi(Minimax.getInstance().searcher(4, new CountDifference()),
					  Minimax.getInstance().searcher(4, new WeightedSquares()),
					  NO_PRINT,
					  STANDARD_GAME_DURATION));
    }
    
    @Test
    public void testPAIP_18_6_1() {
	/**
	 * PAIP pp. 620 ends the game [@=24 O=40 (-16)].
	 * The test is consistent with the PAIP game till the last position before the end.
	 * The difference has not fully traced, given the time needed to manually check
	 * a 6 ply search. Anyhow the CL version obtained porting on SBCL the original PAIP
	 * source code play the exact same game, ending with a -30 score.
	 */
	assertEquals(-30, Reversi.reversi(AlphaBeta.getInstance().searcher(6, new CountDifference()),
					AlphaBeta.getInstance().searcher(4, new WeightedSquares()),
					NO_PRINT,
					STANDARD_GAME_DURATION));
    }
    
    @Test
    public void testPAIP_18_6_2() {
	/**
	 * PAIP pp. 621.
	 * The WeightedSquares strategy select F6, while the ModifiedWiightedSquare has to select C1.
	 *
	 * BLACK moves to b1
	 *     a b c d e f g h [@=20 0=1 (19)]
	 *  1  O @ . . . . . . 
	 *  2  . @ . . . @ @ . 
	 *  3  @ @ @ @ @ @ . . 
	 *  4  . @ . @ @ . . . 
	 *  5  @ @ @ @ @ @ . . 
	 *  6  . @ . . . . . . 
	 *  7  . . . . . . . . 
	 *  8  . . . . . . . . [@=29:59, O=29:59]
	 *  Next to play: WHITE, legal moves: [C1, F6]
	 */

	/** Test to be written. */
	assertEquals(0, 0);
    }
    
    @Test
    public void testRandomVsRandom() {
	/**
	 * The test run a series of a thousand games.
	 */
	final Duration TEST_MAX_DURATION = new Duration(1600); // 1.6 seconds
	final long NANOSECONDS_PER_MILLISECOND = 1000000;
	final int THOUSAND = 1000;
	int avarage = 0;
	List<Integer> scores = new ArrayList<Integer>();
	long startTime = System.nanoTime();
	for (int i=0; i<THOUSAND; i++) {
	    int score = Reversi.reversi(new RandomStrategy(),
					new RandomStrategy(),
					NO_PRINT,
					STANDARD_GAME_DURATION);
	    scores.add(score);
	}
	long testTime = System.nanoTime() - startTime;
	Duration d = new Duration(testTime/NANOSECONDS_PER_MILLISECOND);
	assertTrue("Running the test took longer than: " + TEST_MAX_DURATION, d.isShorterThan(TEST_MAX_DURATION));
    }
    
    @Test
    public void testTenGamesRandomVsCountDifference() {
	/**
	 * The test run a series of ten games.
	 */
	final int TEN = 10;
	int avarage = 0;
	List<Integer> scores = new ArrayList<Integer>();
	for (int i=0; i<TEN; i++) {
	    int score = Reversi.reversi(new RandomStrategy(),
					AlphaBeta.getInstance().searcher(2, new WeightedSquares()),
					NO_PRINT,
					STANDARD_GAME_DURATION);
	    assertTrue(score <= +64 && score >= -64);
	    scores.add(score);
	}
    }
    
    @Test
    public void testTenGamesCountDifferenceVsRandom() {
	/**
	 * The test run a series of ten games.
	 */
	final int TEN = 10;
	int avarage = 0;
	List<Integer> scores = new ArrayList<Integer>();
	for (int i=0; i<TEN; i++) {
	    int score = Reversi.reversi(AlphaBeta.getInstance().searcher(2, new WeightedSquares()),
					new RandomStrategy(),
					NO_PRINT,
					STANDARD_GAME_DURATION);
	    assertTrue(score <= +64 && score >= -64);
	    scores.add(score);
	}
    }
    
}