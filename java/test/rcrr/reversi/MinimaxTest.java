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

import java.util.Arrays;
import java.util.Collection;

import org.junit.*;
import static org.junit.Assert.*;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class MinimaxTest extends DecisionRuleTestUtils {

    static final EvalFunction COUNT_DIFF = new CountDifference();

    public MinimaxTest(GameSnapshot snapshot, Move expectedMove, Integer ply, EvalFunction ef) {
        super(snapshot, expectedMove, ply, ef);
        decisionRule = Minimax.getInstance();
        strategy = decisionRule.searcher(ply, ef);
    }

    @Parameterized.Parameters
    public static Collection data() {
        return Arrays.asList(new Object[][] {

                /**
                 * minimaxSearcher INIT test.
                 * The method is tested using the most straightforward evaluation function,
                 * based on the count difference method.
                 * <p>
                 * The test run from depth 1 to 4. Given that the first move is fully symmetric
                 * and that when there are legal moves having the same evaluation, the first is
                 * selected, the move d3 is always selected.
                 */
                { GameSnapshotFixtures.INITIAL, Move.valueOf(Square.D3), 1, COUNT_DIFF },
                { GameSnapshotFixtures.INITIAL, Move.valueOf(Square.D3), 2, COUNT_DIFF },
                { GameSnapshotFixtures.INITIAL, Move.valueOf(Square.D3), 3, COUNT_DIFF },
                { GameSnapshotFixtures.INITIAL, Move.valueOf(Square.D3), 4, COUNT_DIFF },

                /**
                 * minimaxSearcher A test.
                 * <p>
                 * The test run from depth 1 to 8. The White has to move.
                 * The Black doesn't have any move. The white has four legal moves.
                 * The move sequence is not changing the result in any way.
                 * If the minimax searches depth enough the moves are all equal, leading
                 * to the same forced position. Given that searching one ply returns the
                 * move with the higher result, searching eight ply returns the the first
                 * legal move.
                 */
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.E5), 1, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.E5), 2, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.C4), 3, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.C4), 4, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.C4), 5, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.C4), 6, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.A3), 7, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_A, Move.valueOf(Square.A3), 8, COUNT_DIFF },

                /**
                 * minimaxSearcher B test.
                 * <p>
                 * The test run from depth 1 to 8. The white has to move.
                 * The black can now reply.
                 * The white has two options: C4 or A3, C4 is more aggressive, but open
                 * the game. A3 flips less in the short term, but forces a win in the
                 * following two moves. The minimax recognizes it, and searching three
                 * ply change the selected move to A3.
                 */
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.C4), 1, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.C4), 2, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 3, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 4, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 5, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 6, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 7, COUNT_DIFF },
                { GameSnapshotFixtures.MINIMAX_TEST_CASE_B, Move.valueOf(Square.A3), 8, COUNT_DIFF },

                /**
                 * minimaxSearcher C test.
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
                 * cases have not been verified (test cases 5, 6, 7, 8).
                 */
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 1, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_BC3_10_MOVES, Move.valueOf(Square.B2), 1, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_BC6_10_MOVES, Move.valueOf(Square.H3), 1, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 2, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 3, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 4, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 5, COUNT_DIFF },
                { GameSnapshotFixtures.EARLY_GAME_B_9_MOVES, Move.valueOf(Square.C3), 6, COUNT_DIFF },

                /**
                 * Test the alphabeta searcher when no legal move is available.
                 */
                { GameSnapshotFixtures.BLACK_HAS_TO_PASS, Move.valueOf(Move.Action.PASS), 1, COUNT_DIFF },
                { GameSnapshotFixtures.BLACK_HAS_TO_PASS, Move.valueOf(Move.Action.PASS), 2, COUNT_DIFF },
                { GameSnapshotFixtures.BLACK_HAS_TO_PASS, Move.valueOf(Move.Action.PASS), 3, COUNT_DIFF },
                { GameSnapshotFixtures.BLACK_HAS_TO_PASS, Move.valueOf(Move.Action.PASS), 4, COUNT_DIFF },

            });
    }

}
