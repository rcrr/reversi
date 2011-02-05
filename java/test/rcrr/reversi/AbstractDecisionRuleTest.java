/*
 *  AbstractDecisionRuleTest.java
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
import static org.junit.Assert.assertEquals;

/**
 * The tests provided by this class verify that
 * the {@code maximizer} static method
 * defined by {@code AbstractDecisionRule} works properly.
 * The method is equivalent to a minimax search that is one ply deep.
 */
public class AbstractDecisionRuleTest {

    /** CountDifference is the evaluation function used for testing. */
    private static final EvalFunction COUNT_DIFF = new CountDifference();

    /** Class constructor. */
    public AbstractDecisionRuleTest() { }

    /** Test the maximizer method. */
    @Test
    public final void testMaximizer() {

        /**
         * The maximixer method is equivalent to a minimax search one ply deep.
         */
        assertEquals(Move.valueOf(Square.H3),
                     AbstractDecisionRule.maximizer(COUNT_DIFF)
                     .move(GameSnapshotFixtures.EARLY_GAME_BC6_10_MOVES));

        assertEquals(Move.valueOf(Square.E5),
                     AbstractDecisionRule.maximizer(COUNT_DIFF)
                     .move(GameSnapshotFixtures.MINIMAX_TEST_CASE_A));
    }

    /** Test the maximizer when no legal move is available. */
    @Test
    public final void testMaximizerWhenNoLegalMoveIsAvailable() {
        assertEquals(Move.valueOf(Move.Action.PASS),
                     AbstractDecisionRule.maximizer(COUNT_DIFF)
                     .move(GameSnapshotFixtures.BLACK_HAS_TO_PASS));
    }

}
