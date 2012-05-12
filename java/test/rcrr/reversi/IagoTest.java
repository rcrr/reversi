/*
 *  IagoTest.java
 *
 *  Copyright (c) 2011, 2012 Roberto Corradini. All rights reserved.
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

import rcrr.reversi.board.Board;
import rcrr.reversi.board.BoardFixtures;
import rcrr.reversi.board.BoardBuilder;
import rcrr.reversi.board.Player;
import rcrr.reversi.board.Square;
import rcrr.reversi.board.SquareState;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

/**
 * Test Suite for the {@code Iago} class.
 *
 * @see Iago
 */
@RunWith(Parameterized.class)
public class IagoTest extends EvalFunctionTestUtils {

    /**
     * Class constructor.
     * <p>
     * The evaluation function parameter is set to {@code new Iago()}.
     *
     * @param testFailureMessage the test failure message
     * @param board              the board parameter passed to the evaluation function
     * @param player             the player parameter passed to the evaluation function
     * @param expectedValue      the expected value used by the unit test assertion
     */
    public IagoTest(final String testFailureMessage,
                    final Board board,
                    final Player player,
                    final Integer expectedValue) {
        super(testFailureMessage,
              GamePosition.valueOf(board, player),
              new Iago(),
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

                /** HOW CAN BE THAT A SYMMETRICAL POSITION HAS AN ADVANTAGE FOR WHITE?!!?? */
                {"This test does't seams correct. It must be investigated.",
                 new BoardBuilder()
                 .withSquaresLiteral(2, 2, 2, 2, 1, 1, 1, 1,
                                     0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0,
                                     0, 0, 0, 0, 0, 0, 0, 0)
                 .build(),
                 Player.WHITE,
                 7325},

            });
    }

}
