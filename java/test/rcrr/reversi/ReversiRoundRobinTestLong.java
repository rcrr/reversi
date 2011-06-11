/*
 *  ReversiRoundRobinTestLong.java
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
import java.util.Set;
import java.util.HashSet;

import org.junit.Test;

import static org.junit.Assert.assertTrue;

import org.joda.time.Duration;
import org.joda.time.Period;

/**
 * Test Suite for {@code Reversi} class.
 */
public class ReversiRoundRobinTestLong {

    /** The standard game duration is thirty minutes. */
    private static final Duration STANDARD_GAME_DURATION = Period.minutes(30).toStandardDuration();

    /** Class constructor. */
    public ReversiRoundRobinTestLong() { }

    /**
     * Tests that the round robin series played as described by <i>PAIP 18.8 (pg 629)</i> is reproduced
     * accurately running the {@code Reversi.roundRobin(Set, int, Duration)} method.
     * <p>
     * The method is run assigning the following values to invocation parameters:
     * <ul>
     *   <li>{@code actors} is a set containing:
     *     <ul>
     *     <li>{@code Actor.valueOf("Maximize Count Difference", AbstractDecisionRule.maximizer(new CountDifference()))}</li>
     *     <li>{@code Actor.valueOf("Maximize Mobility", AbstractDecisionRule.maximizer(new Mobility()))}</li>
     *     <li>{@code Actor.valueOf("Maximize Weighted Squares", AbstractDecisionRule.maximizer(new WeightedSquares()))}</li>
     *     <li>{@code Actor.valueOf("Maximize Mod-Weighted Squares", AbstractDecisionRule.maximizer(new ModifiedWeightedSquares()))}</li>
     *     <li>{@code Actor.valueOf("Random Strategy", new RandomStrategy())}</li>
     *     </ul>
     *   </li>
     *   <li>{@code nPairs} is 5</li>
     *   <li>{@code gameDuration} is STANDARD_GAME_DURATION (30 minutes)</li>
     * </ul>
     * <p>
     * The method returns a results map from which a formatted output is written to a file.
     *
     * @see Reversi#roundRobin(Set, int, Duration)
     */
    @Test
    public final void testPAIP_18_8_0() {

        Set<Actor> actors = new HashSet<Actor>();
        actors.add(Actor.valueOf("Maximize Count Difference", AbstractDecisionRule.maximizer(new CountDifference())));
        actors.add(Actor.valueOf("Maximize Mobility", AbstractDecisionRule.maximizer(new Mobility())));
        actors.add(Actor.valueOf("Maximize Weighted Squares", AbstractDecisionRule.maximizer(new WeightedSquares())));
        actors.add(Actor.valueOf("Maximize Mod-Weighted Squares", AbstractDecisionRule.maximizer(new ModifiedWeightedSquares())));
        actors.add(Actor.valueOf("Random Strategy", new RandomStrategy()));

        Map<String, Object> results = Reversi.roundRobin(actors, 5, STANDARD_GAME_DURATION);

        String report = Reversi.postProcessRoundRobinResults(results);
 
        /** Output of the test. */
        ReversiTest.write(report,
                          "ReversiRoundRobinTestLong",
                          "Method=testPAIP_18_8_0");

        assertTrue("The test must run without exceptions.", true);

    }

    /**
     * Tests that the round robin series played as described by <i>PAIP 18.8 (pg 630)</i> is reproduced
     * accurately running the {@code Reversi.roundRobin(Set, int, Duration)} method.
     * <p>
     * The method is run assigning the following values to invocation parameters:
     * <ul>
     *   <li>{@code actors} is a set containing:
     *     <ul>
     *     <li>{@code Actor.valueOf("Alpha-Beta 4 ply Count Difference", AlphaBeta.getInstance().searcher(4, new CountDifference()))}</li>
     *     <li>{@code Actor.valueOf("Alpha-Beta 4 ply Mobility", AlphaBeta.getInstance().searcher(4, new Mobility()))}</li>
     *     <li>{@code Actor.valueOf("Alpha-Beta 4 ply Weighted Squares", AlphaBeta.getInstance().searcher(4, new WeightedSquares()))}</li>
     *     <li>{@code Actor.valueOf("Alpha-Beta 4 ply Mod-Weighted Squares", AlphaBeta.getInstance().searcher(4, new ModifiedWeightedSquares()))}</li>
     *     <li>{@code Actor.valueOf("Random Strategy", new RandomStrategy())}</li>
     *     </ul>
     *   </li>
     *   <li>{@code nPairs} is 5</li>
     *   <li>{@code gameDuration} is STANDARD_GAME_DURATION (30 minutes)</li>
     * </ul>
     * <p>
     * The method returns a results map from which a formatted output is written to a file.
     *
     * @see Reversi#roundRobin(Set, int, Duration)
     */
    @Test
    public final void testPAIP_18_8_1() {

        Set<Actor> actors = new HashSet<Actor>();
        actors.add(Actor.valueOf("Alpha-Beta 4 ply Count Difference", AlphaBeta.getInstance().searcher(4, new CountDifference())));
        actors.add(Actor.valueOf("Alpha-Beta 4 ply Mobility", AlphaBeta.getInstance().searcher(4, new Mobility())));
        actors.add(Actor.valueOf("Alpha-Beta 4 ply Weighted Squares", AlphaBeta.getInstance().searcher(4, new WeightedSquares())));
        actors.add(Actor.valueOf("Alpha-Beta 4 ply Mod-Weighted Squares", AlphaBeta.getInstance().searcher(4, new ModifiedWeightedSquares())));
        actors.add(Actor.valueOf("Random Strategy", new RandomStrategy()));

        Map<String, Object> results = Reversi.roundRobin(actors, 5, STANDARD_GAME_DURATION);

        /** Output of the test. */
        ReversiTest.write(Reversi.postProcessRoundRobinResults(results),
                          "ReversiRoundRobinTestLong",
                          "Method=testPAIP_18_8_1");

        assertTrue("The test must run without exceptions.", true);

    }

}
