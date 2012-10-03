/*
 *  ReversiRoundRobin2Perf.java
 *
 *  Copyright (c) 2012 Roberto Corradini. All rights reserved.
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
import java.util.Set;
import java.util.HashSet;

import org.junit.Test;

import static org.junit.Assert.assertTrue;

import org.joda.time.Duration;
import org.joda.time.Period;

import rcrr.reversi.board.BitBoard;
import rcrr.reversi.board.BitBoard0;
import rcrr.reversi.board.BitBoard1;
import rcrr.reversi.board.BitBoard2;

/**
 * Test Suite for reversi general performances, using Minimax and a very simple eval function.
 */
public class ReversiRoundRobin2Perf {

    /** The standard game duration is thirty minutes. */
    private static final Duration STANDARD_GAME_DURATION = Period.minutes(30).toStandardDuration();

    /** The search depth. */
    public static final int PLY = 4;

    /** The number of pair games applyed. */
    public static final int N_PAIRS = 50;

    /** Class constructor. */
    public ReversiRoundRobin2Perf() { }

    /**
     * Runs a round robin series with the purpose to profile, sample, or time the execution.
     * <p>
     * The method is run assigning the following values to invocation parameters:
     * <ul>
     *   <li>{@code actors} is a set containing:
     *     <ul>
     *     <li>{@code Actor.valueOf("Alpha-Beta PLY - Count Difference", AlphaBeta.getInstance().searcher(PLY, new CountDifference()))}</li>
     *     </ul>
     *   </li>
     *   <li>{@code nPairs} is equal to {@code N_PAIRS}</li>
     *   <li>{@code gameDuration} is STANDARD_GAME_DURATION (30 minutes)</li>
     * </ul>
     * <p>
     * The method returns a results map from which a formatted output is written to a file.
     *
     * @see Reversi#roundRobin(Set, int, Duration)
     */
    @Test
    public final void testRoundRobin() {

        Set<Actor> actors = new HashSet<Actor>();
        actors.add(Actor.valueOf("Minimax A",
                                 Minimax.getInstance().searcher(PLY,
                                                                new EvalFunction() {
                                                                    public int eval(final GamePosition position) {
                                                                        return position.board().countDifference(position.player());
                                                                    }})));
        actors.add(Actor.valueOf("Minimax B",
                                 Minimax.getInstance().searcher(PLY,
                                                                new EvalFunction() {
                                                                    public int eval(final GamePosition position) {
                                                                        return position.board().countDifference(position.player());
                                                                    }})));

        Map<String, Object> results = Reversi.roundRobin(actors, N_PAIRS, STANDARD_GAME_DURATION);

        /** Output of the test. */
        ReversiTest.write(Reversi.postProcessRoundRobinResults(results),
                          "ReversiRoundRobinPerf",
                          "Method=testRoundRobin");

        System.out.println("BitBoard0.printLog() = " + BitBoard0.printLog());
        System.out.println("BitBoard1.printLog() = " + BitBoard1.printLog());
        System.out.println("BitBoard2.printLog() = " + BitBoard2.printLog());

        assertTrue("The test must run without exceptions.", true);

    }

}
