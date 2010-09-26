/*
 *  ModifiedWeightedSquaresTest.java
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

import org.junit.*;
import static org.junit.Assert.*;

public class ModifiedWeightedSquaresTest {

    private BoardTest boardTest;

    private Board fixtBoardMWS1A;
    private Board fixtBoardMWS1B;
    private Board fixtBoardMWS2A;
    private Board fixtBoardMWS2B;
    
    /**
     * Prepares the Board fixtures. It depends on the public BoardTest fixtures.
     */
    @Before
    public void setUp() {
	boardTest = new BoardTest();
	boardTest.setUp();

	fixtBoardMWS1A = 
	    BoardTest.boardFromList(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
						  1, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0));
	
	fixtBoardMWS1B = 
	    BoardTest.boardFromList(Arrays.asList(1, 0, 0, 0, 0, 0, 0, 0,
						  1, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0));

	fixtBoardMWS2A = 
	    BoardTest.boardFromList(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
						  0, 1, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0));
	
	fixtBoardMWS2B = 
	    BoardTest.boardFromList(Arrays.asList(2, 0, 0, 0, 0, 0, 0, 0,
						  0, 1, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0,
						  0, 0, 0, 0, 0, 0, 0, 0));

    }


    @Test
    public void testEvalA() {

	/** The following assertions test the basic cases. */
	assertEquals(-20, (new ModifiedWeightedSquares()).eval(Player.BLACK, fixtBoardMWS1A));
	assertEquals(+20, (new ModifiedWeightedSquares()).eval(Player.WHITE, fixtBoardMWS1A));

	assertEquals(+125, (new ModifiedWeightedSquares()).eval(Player.BLACK, fixtBoardMWS1B));
	assertEquals(-125, (new ModifiedWeightedSquares()).eval(Player.WHITE, fixtBoardMWS1B));

	assertEquals(-40, (new ModifiedWeightedSquares()).eval(Player.BLACK, fixtBoardMWS2A));
	assertEquals(+40, (new ModifiedWeightedSquares()).eval(Player.WHITE, fixtBoardMWS2A));

	assertEquals(-115, (new ModifiedWeightedSquares()).eval(Player.BLACK, fixtBoardMWS2B));
	assertEquals(+115, (new ModifiedWeightedSquares()).eval(Player.WHITE, fixtBoardMWS2B));

    }

    @Test
    public void testEvalB() {

	/** Tests that the empty board returns 0. */
	assertEquals(0, (new ModifiedWeightedSquares()).eval(Player.BLACK, boardTest.fixtBoardEmpty));
	assertEquals(0, (new ModifiedWeightedSquares()).eval(Player.WHITE, boardTest.fixtBoardEmpty));

	/** Tests that the initial game state returns 0. */
	assertEquals(0, (new ModifiedWeightedSquares()).eval(Player.BLACK, Board.initialBoard()));
	assertEquals(0, (new ModifiedWeightedSquares()).eval(Player.WHITE, Board.initialBoard()));

	/** Tests that the fixtBoardA game state returns -9 for the white. */
	assertEquals(-9, (new ModifiedWeightedSquares()).eval(Player.WHITE, boardTest.fixtBoardA));

	/** Tests that the fixtBoardA game state returns +9 for the black. */
	assertEquals(+9, (new ModifiedWeightedSquares()).eval(Player.BLACK, boardTest.fixtBoardA));

	/** Tests that the fixtBoardEndGameX game state returns +52 for the black. */
	assertEquals(+52, (new ModifiedWeightedSquares()).eval(Player.BLACK, boardTest.fixtBoardEndGameX));

    }
}