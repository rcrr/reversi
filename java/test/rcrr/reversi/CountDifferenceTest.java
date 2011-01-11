/*
 *  CountDifferenceTest.java
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

import org.junit.*;
import static org.junit.Assert.*;

public class CountDifferenceTest {

    private BoardTest boardTest;

    /**
     * Prepares the Board fixtures. It depends on the public BoardTest fixtures.
     */
    @Before
    public void setUp() {
        boardTest = new BoardTest();
        boardTest.setUp();
    }

    @Test
    public void testEval() {

        /** Tests that the empty board returns 0. */
        assertEquals(0, (new CountDifference()).eval(GamePosition.valueOf(BoardFixtures.EMPTY, Player.BLACK)));
        assertEquals(0, (new CountDifference()).eval(GamePosition.valueOf(BoardFixtures.EMPTY, Player.WHITE)));

        /** Tests that the initial game state returns 0. */
        assertEquals(0, (new CountDifference()).eval(GamePosition.valueOf(Board.initialBoard(), Player.BLACK)));
        assertEquals(0, (new CountDifference()).eval(GamePosition.valueOf(Board.initialBoard(), Player.WHITE)));

        /** Tests that the fixtBoardA game state returns -3 for the white. */
        assertEquals(-3, (new CountDifference()).eval(GamePosition.valueOf(boardTest.fixtBoardA, Player.WHITE)));

        /** Tests that the fixtBoardA game state returns +3 for the black. */
        assertEquals(+3, (new CountDifference()).eval(GamePosition.valueOf(boardTest.fixtBoardA, Player.BLACK)));

        /** Tests that the fixtBoardEndGameX game state returns +10 for the black. */
        assertEquals(+10, (new CountDifference()).eval(GamePosition.valueOf(boardTest.fixtBoardEndGameX, Player.BLACK)));

    }
}
