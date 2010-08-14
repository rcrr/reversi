/*
 *  BoardStateTest.java
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

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

import org.junit.*;
import static org.junit.Assert.*;

public class BoardStateTest {

    private BoardState fixtBoardBlackHasToPass;
    /**
       3 3 3 3 3 3 3 3 3 3
       3 2 1 0 1 0 2 0 0 3
       3 1 1 1 1 1 1 1 2 3
       3 0 1 2 2 1 1 2 2 3
       3 0 1 2 1 2 2 2 2 3
       3 0 1 2 1 2 2 2 2 3
       3 0 1 2 1 1 2 1 2 3
       3 0 1 2 1 1 1 1 0 3
       3 2 2 2 2 2 2 1 2 3
       3 3 3 3 3 3 3 3 3 3
    */

    @Before
    public void setUp() {
	List<Integer> iBoard = Arrays.asList(3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
					     3, 2, 1, 0, 1, 0, 2, 0, 0, 3,
					     3, 1, 1, 1, 1, 1, 1, 1, 2, 3,
					     3, 0, 1, 2, 2, 1, 1, 2, 2, 3,
					     3, 0, 1, 2, 1, 2, 2, 2, 2, 3,
					     3, 0, 1, 2, 1, 2, 2, 2, 2, 3,
					     3, 0, 1, 2, 1, 1, 2, 1, 2, 3,
					     3, 0, 1, 2, 1, 1, 1, 1, 0, 3,
					     3, 2, 2, 2, 2, 2, 2, 1, 2, 3,
					     3, 3, 3, 3, 3, 3, 3, 3, 3, 3);
	List<SquareState> ssBoard = new ArrayList<SquareState>();
	for (Integer i : iBoard) {
	    SquareState ss = null;
	    if (i == 0) ss = SquareState.EMPTY;
	    else if (i == 1) ss = SquareState.BLACK;
	    else if (i == 2) ss = SquareState.WHITE;
	    else if (i == 3) ss = SquareState.OUTER;
	    ssBoard.add(ss);
	}	
	fixtBoardBlackHasToPass = BoardState.valueOf(new MutableBoard(ssBoard));
    }

    @Test
    public void testWouldFlip() {
	assertEquals(Integer.valueOf(73),
		     fixtBoardBlackHasToPass.wouldFlip(78, Player.WHITE, Direction.W));
	assertEquals(null,
		     fixtBoardBlackHasToPass.wouldFlip(78, Player.WHITE, Direction.S));
    }

    @Test
    public void testFindBracketingPiece() {
	assertTrue(true);
    }

    @Test
    public void testCount() {
	assertTrue(true);
    }

    @Test
    public void testCountDifference() {
	assertTrue(true);
    }

    @Test
    public void testPrint() {
	assertTrue(true);
    }

    @Test
    public void testIsValid() {
	assertTrue(true);
    }

    @Test
    public void testIsLegal() {
	assertTrue(true);
    }

    @Test
    public void testNextToPlay() {
	assertTrue(true);
    }

    @Test
    public void testAnyLegalMove() {
	assertTrue(true);
    }

    @Test
    public void testLegalMoves() {
	assertTrue(true);
    }

    @Test
    public void testFinalValue() {
	assertTrue(true);
    }
   
}