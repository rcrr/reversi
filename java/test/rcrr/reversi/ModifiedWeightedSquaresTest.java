/*
 *  ModifiedWeightedSquaresTest.java
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

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

/**
 * Test Suite for the {@code ModifiedWeightedSquares} class.
 *
 * @see ModifiedWeightedSquares
 */
@RunWith(Parameterized.class)
public class ModifiedWeightedSquaresTest extends EvalFunctionTestUtils {

    /** The CASE_ONE_A board. */
    private static final Board CASE_ONE_A = new Board.Builder()
        .withSquaresLiteral(0, 0, 0, 0, 0, 0, 0, 0,
                            1, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0)
        .build();

    /** The CASE_ONE_B board. */
    private static final Board CASE_ONE_B = new Board.Builder()
        .withSquaresLiteral(1, 0, 0, 0, 0, 0, 0, 0,
                            1, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0)
        .build();

    /** The CASE_TWO_A board. */
    private static final Board CASE_TWO_A = new Board.Builder()
        .withSquaresLiteral(0, 0, 0, 0, 0, 0, 0, 0,
                            0, 1, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0)
        .build();

    /** The CASE_TWO_B board. */
    private static final Board CASE_TWO_B = new Board.Builder()
        .withSquaresLiteral(2, 0, 0, 0, 0, 0, 0, 0,
                            0, 1, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0)
        .build();

    /**
     * Class constructor.
     * <p>
     * The evaluation function parameter is set to {@code new ModifiedWeightedSquares()}.
     *
     * @param testFailureMessage the test failure message
     * @param board              the board parameter passed to the evaluation function
     * @param player             the player parameter passed to the evaluation function
     * @param expectedValue      the expected value used by the unit test assertion
     */
    public ModifiedWeightedSquaresTest(final String testFailureMessage,
                                       final Board board,
                                       final Player player,
                                       final Integer expectedValue) {
        super(testFailureMessage,
              GamePosition.valueOf(board, player),
              new ModifiedWeightedSquares(),
              expectedValue);
    }

    /**
     * Returns the data set used by the parameterized test.
     *
     * @return the data set used to run the tests
     */
    @Parameterized.Parameters
    public static Collection data() {
        return Arrays.asList(new Object[][] {

                {"The empty board must return 0.",
                 BoardFixtures.EMPTY, Player.BLACK,
                 0},

                {"The empty board must return 0.",
                 BoardFixtures.EMPTY, Player.WHITE,
                 0},

                {"The initial board must retun 0.",
                 BoardFixtures.INITIAL, Player.BLACK,
                 0},

                {"The initial board must retun 0.",
                 BoardFixtures.INITIAL, Player.WHITE,
                 0},

                {"BoardFixtures.FIRST_MOVE_D3, Player.WHITE must return -9.",
                 BoardFixtures.FIRST_MOVE_D3, Player.WHITE,
                 -9 },

                {"BoardFixtures.FIRST_MOVE_D3, Player.BLACK must return +9.",
                 BoardFixtures.FIRST_MOVE_D3, Player.BLACK,
                 +9},

                {"BoardFixtures.FINAL_B37_W27, Player.BLACK must return +52.",
                 BoardFixtures.FINAL_B37_W27, Player.BLACK,
                 +52},

                {"CASE_ONE_A, Player.BLACK must return -20.",
                 CASE_ONE_A, Player.BLACK,
                 -20},

                {"CASE_ONE_A, Player.WHITE must return +20.",
                 CASE_ONE_A, Player.WHITE,
                 +20},

                {"CASE_ONE_B, Player.BLACK must return -125.",
                 CASE_ONE_B, Player.BLACK,
                 +125},

                {"", CASE_ONE_B, Player.WHITE,
                 -125},

                {"CASE_TWO_A, Player.BLACK must return -40.",
                 CASE_TWO_A, Player.BLACK,
                 -40},

                {"CASE_TWO_A, Player.WHITE must return +40.",
                 CASE_TWO_A, Player.WHITE,
                 +40},

                {"CASE_TWO_B, Player.BLACK must return -115.",
                 CASE_TWO_B, Player.BLACK,
                 -115},

                {"CASE_TWO_B, Player.WHITE must return +115.",
                 CASE_TWO_B, Player.WHITE,
                 +115}

            });
    }

}
