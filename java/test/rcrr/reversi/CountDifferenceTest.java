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

import org.junit.*;
import static org.junit.Assert.*;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class CountDifferenceTest extends EvalFunctionTestUtils {

    public CountDifferenceTest(Board board, Player player, Integer expectedValue) {
        super(board, player, expectedValue);
        this.fn = new CountDifference();
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
                 * - Board  = FIRST_MOVE_D3 game state 
                 * - Player = BLACK
                 * returns a value of +3.
                 */
                { BoardFixtures.FIRST_MOVE_D3, Player.BLACK, +3 },

                /**
                 * Tests that the game state defined by:
                 * - Board  = FINAL_B37_W27 game state 
                 * - Player = BLACK
                 * returns a value of +10.
                 */
                { BoardFixtures.FINAL_B37_W27, Player.BLACK, +10 }

            });
    }

}
