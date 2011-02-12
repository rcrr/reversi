/*
 *  AlphaBetaTest.java
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

import java.util.Arrays;
import java.util.Collection;

import org.junit.*;
import static org.junit.Assert.*;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

/**
 * Test Suite for the {@code AlphaBeta} class.
 * <p>
 * The test cases given by the data method are the same used by
 * {@code MinimaxTest}.
 */
@RunWith(Parameterized.class)
public class AlphaBetaTest extends DecisionRuleTestUtils {

    /**
     * The evaluation function used for testing the {@code AlphaBeta}
     * search algorithm is the most simple at our disposal.
     */
    static final EvalFunction COUNT_DIFF = new CountDifference();

    public AlphaBetaTest(GameSnapshot snapshot, Move expectedMove, Integer ply, EvalFunction ef) {
        super(snapshot, expectedMove, ply, ef);
        decisionRule = AlphaBeta.getInstance();
        strategy = decisionRule.searcher(ply, ef);
    }

    @Parameterized.Parameters
    public static Collection data() {
        return Arrays.asList(new Object[][] {

                /** ... */
                { GameSnapshotFixtures.INITIAL, Move.valueOf(Square.D3), 1, COUNT_DIFF },
                { GameSnapshotFixtures.INITIAL, Move.valueOf(Square.D3), 2, COUNT_DIFF },
                { GameSnapshotFixtures.INITIAL, Move.valueOf(Square.D3), 3, COUNT_DIFF },
                { GameSnapshotFixtures.INITIAL, Move.valueOf(Square.D3), 4, COUNT_DIFF },

                /** ... */
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.E5), 1, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.E5), 2, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.C4), 3, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.C4), 4, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.C4), 5, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.C4), 6, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.A3), 7, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.A3), 8, COUNT_DIFF },

                /** ... */
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.C4), 1, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.C4), 2, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 3, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 4, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 5, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 6, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 7, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 8, COUNT_DIFF },

                /** Manually verified. */
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 1, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_BC3_10_MOVES, Move.valueOf(Square.B2), 1, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_BC6_10_MOVES, Move.valueOf(Square.H3), 1, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 2, COUNT_DIFF },

                /** Not "manually" verified. */
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 3, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 4, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 5, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 6, COUNT_DIFF },

                /** Test the alphabeta searcher when no legal move is available. */
                { GameSnapshotFixtures.BLACK_HAS_TO_PASS, Move.valueOf(Move.Action.PASS), 1, COUNT_DIFF },
                { GameSnapshotFixtures.BLACK_HAS_TO_PASS, Move.valueOf(Move.Action.PASS), 2, COUNT_DIFF },
                { GameSnapshotFixtures.BLACK_HAS_TO_PASS, Move.valueOf(Move.Action.PASS), 3, COUNT_DIFF },
                { GameSnapshotFixtures.BLACK_HAS_TO_PASS, Move.valueOf(Move.Action.PASS), 4, COUNT_DIFF },

            });
    }

}
