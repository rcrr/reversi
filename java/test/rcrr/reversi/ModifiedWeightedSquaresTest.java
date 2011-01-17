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

import org.junit.*;
import static org.junit.Assert.*;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class ModifiedWeightedSquaresTest extends EvalFunctionTestUtils {

    /** The CASE_ONE_A board. */
    private static Board CASE_ONE_A = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        1, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /** The CASE_ONE_B board. */
    private static Board CASE_ONE_B = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(1, 0, 0, 0, 0, 0, 0, 0,
                                        1, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /** The CASE_TWO_A board. */
    private static Board CASE_TWO_A = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 1, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /** The CASE_TWO_B board. */
    private static Board CASE_TWO_B = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(2, 0, 0, 0, 0, 0, 0, 0,
                                        0, 1, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    public ModifiedWeightedSquaresTest(Board board, Player player, Integer expectedValue) {
        super(board, player, expectedValue);
        this.fn = new ModifiedWeightedSquares();
    }

    @Parameterized.Parameters
    public static Collection data() {
        return Arrays.asList(new Object[][] {

                /** Tests that the empty board returns 0. */
                { BoardFixtures.EMPTY, Player.BLACK, 0 },
                { BoardFixtures.EMPTY, Player.WHITE, 0 },

                /** Tests that the initial game state returns 0. */
                { BoardFixtures.INITIAL, Player.BLACK, 0 },
                { BoardFixtures.INITIAL, Player.WHITE, 0 },

                /**
                 * Tests that the game state defined by:
                 * - Board  = FIRST_MOVE_D3 
                 * - Player = WHITE
                 * returns a value of -9.
                 */
                { BoardFixtures.FIRST_MOVE_D3, Player.WHITE, -9 },

                /**
                 * Tests that the game state defined by:
                 * - Board  = FIRST_MOVE_D3 
                 * - Player = BLACK
                 * returns a value of +9.
                 */
                { BoardFixtures.FIRST_MOVE_D3, Player.BLACK, +9 },

                /**
                 * Tests that the game state defined by:
                 * - Board  = FINAL_B37_W27 game state 
                 * - Player = BLACK
                 * returns a value of +52 (50 points more than WeightedSquare.
                 */
                { BoardFixtures.FINAL_B37_W27, Player.BLACK, +52 },

                { CASE_ONE_A, Player.BLACK, -20 },
                { CASE_ONE_A, Player.WHITE, +20 },

                { CASE_ONE_B, Player.BLACK, +125 },
                { CASE_ONE_B, Player.WHITE, -125 },

                { CASE_TWO_A, Player.BLACK, -40 },
                { CASE_TWO_A, Player.WHITE, +40 },

                { CASE_TWO_B, Player.BLACK, -115 },
                { CASE_TWO_B, Player.WHITE, +115 }

            });
    }

}
