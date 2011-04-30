/*
 *  CountDifferenceTest.java
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
 * Test Suite for the {@code CountDifference} class.
 *
 * @see CountDifference
 */
@RunWith(Parameterized.class)
public class CountDifferenceTest extends EvalFunctionTestUtils {

    /**
     * Class constructor.
     * <p>
     * The evaluation function parameter is set to {@code new CountDifference()}.
     *
     * @param board         the board parameter passed to the evaluation function
     * @param player        the player parameter passed to the evaluation function
     * @param expectedValue is the expected value used by the unit test assertion
     */
    public CountDifferenceTest(final Board board,
                               final Player player,
                               final Integer expectedValue) {
        super(board, player, new CountDifference(), expectedValue);
    }

    /**
     * Returns the data set used by the parameterized test.
     *
     * @return the data set used to run the tests
     */
    @Parameterized.Parameters
    public static Collection data() {
        return Arrays.asList(new Object[][] {

                /** Tests that the empty board returns 0. */
                {BoardFixtures.EMPTY, Player.BLACK, 0},
                {BoardFixtures.EMPTY, Player.WHITE, 0},

                /** Tests that the initial game state returns 0. */
                {BoardFixtures.INITIAL, Player.BLACK, 0},
                {BoardFixtures.INITIAL, Player.WHITE, 0},

                /**
                 * Tests that the game state defined by:
                 * - Board  = FIRST_MOVE_D3 game state
                 * - Player = BLACK
                 * returns a value of +3.
                 */
                {BoardFixtures.FIRST_MOVE_D3, Player.BLACK, +3},

                /**
                 * Tests that the game state defined by:
                 * - Board  = FINAL_B37_W27 game state
                 * - Player = BLACK
                 * returns a value of +10.
                 */
                {BoardFixtures.FINAL_B37_W27, Player.BLACK, +10}

            });
    }

}
