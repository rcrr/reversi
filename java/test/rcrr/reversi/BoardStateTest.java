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

    private BoardState fixtBoardInitial;
    private BoardState fixtBoardEmpty;
    private BoardState fixtBoardBlackHasToPass;
    private BoardState fixtBoardEndGameX;
    private BoardState fixtBoardA;
    private BoardState fixtBoardB;
    private BoardState fixtBoardC;

    private static BoardState boardFromList(List<Integer> il) {
	if (il == null) return null;
	if (il.size() != 100) throw new IllegalArgumentException();
	List<SquareState> ssl = new ArrayList<SquareState>();
	for (Integer i : il) {
	    SquareState ss = null;
	    if (i == null || i < 0 || i > 3) throw new IllegalArgumentException();
	    else if (i == 0) ss = SquareState.EMPTY;
	    else if (i == 1) ss = SquareState.BLACK;
	    else if (i == 2) ss = SquareState.WHITE;
	    else if (i == 3) ss = SquareState.OUTER;
	    ssl.add(ss);
	}	
	return BoardState.valueOf(ssl);
    }

    @Before
    public void setUp() {
	fixtBoardInitial = 
	    boardFromList(Arrays.asList(3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 1, 2, 0, 0, 0, 3,
					3, 0, 0, 0, 2, 1, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 3, 3, 3, 3, 3, 3, 3, 3, 3));

	fixtBoardEmpty = 
	    boardFromList(Arrays.asList(3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 3, 3, 3, 3, 3, 3, 3, 3, 3));

	fixtBoardBlackHasToPass = 
	    boardFromList(Arrays.asList(3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
					3, 2, 1, 0, 1, 0, 2, 0, 0, 3,
					3, 1, 1, 1, 1, 1, 1, 1, 2, 3,
					3, 0, 1, 2, 2, 1, 1, 2, 2, 3,
					3, 0, 1, 2, 1, 2, 2, 2, 2, 3,
					3, 0, 1, 2, 1, 2, 2, 2, 2, 3,
					3, 0, 1, 2, 1, 1, 2, 1, 2, 3,
					3, 0, 1, 2, 1, 1, 1, 1, 0, 3,
					3, 2, 2, 2, 2, 2, 2, 1, 2, 3,
					3, 3, 3, 3, 3, 3, 3, 3, 3, 3));

	fixtBoardEndGameX = 
	    boardFromList(Arrays.asList(3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
					3, 2, 2, 2, 2, 2, 1, 1, 1, 3,
					3, 2, 2, 2, 1, 1, 1, 1, 1, 3,
					3, 2, 2, 2, 1, 1, 1, 2, 1, 3,
					3, 2, 2, 1, 2, 1, 1, 2, 1, 3,
					3, 1, 1, 2, 1, 2, 1, 2, 1, 3,
					3, 1, 2, 1, 2, 1, 2, 1, 1, 3,
					3, 1, 1, 1, 1, 1, 1, 2, 1, 3,
					3, 1, 1, 1, 1, 2, 2, 2, 2, 3,
					3, 3, 3, 3, 3, 3, 3, 3, 3, 3));

	fixtBoardA = 
	    boardFromList(Arrays.asList(3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 1, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 1, 1, 0, 0, 0, 3,
					3, 0, 0, 0, 1, 2, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 3, 3, 3, 3, 3, 3, 3, 3, 3));

	fixtBoardB = 
	    boardFromList(Arrays.asList(3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
					3, 0, 0, 0, 1, 1, 1, 0, 0, 3,
					3, 0, 0, 0, 0, 1, 0, 0, 0, 3,
					3, 0, 0, 0, 1, 1, 2, 2, 0, 3,
					3, 0, 0, 0, 1, 1, 0, 0, 0, 3,
					3, 0, 0, 0, 1, 1, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 1, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 3, 3, 3, 3, 3, 3, 3, 3, 3));

	fixtBoardC = 
	    boardFromList(Arrays.asList(3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 2, 1, 1, 1, 0, 0, 2, 0, 3,
					3, 0, 2, 0, 2, 1, 2, 0, 0, 3,
					3, 0, 2, 2, 1, 2, 0, 0, 0, 3,
					3, 0, 0, 1, 1, 0, 2, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 0, 0, 0, 0, 0, 0, 0, 0, 3,
					3, 3, 3, 3, 3, 3, 3, 3, 3, 3));

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
	Integer move = Square.H7.position();

	Direction dir = Direction.W;
	Integer b1 = move + dir.delta();
	Integer b2 = Square.C7.position();
	assertEquals(b2, fixtBoardBlackHasToPass.
		     findBracketingPiece(b1, Player.WHITE, dir));
	
	dir = Direction.NW;
	b1 = move + dir.delta();
	b2 = Square.F5.position();
	assertEquals(b2, fixtBoardBlackHasToPass.
		     findBracketingPiece(b1, Player.WHITE, dir));
	
	dir = Direction.SW;
	b1 = move + dir.delta();
	b2 = null;
	assertEquals(b2, fixtBoardBlackHasToPass.
		     findBracketingPiece(b1, Player.WHITE, dir));

    }

    @Test
    public void testCountPieces() {
	assertEquals(Integer.valueOf(2),
		     BoardState.initialBoard().countPieces(SquareState.BLACK));
    }

    @Test
    public void testCountDifference() {
	assertEquals(Integer.valueOf(0),
		     BoardState.initialBoard().countDifference(Player.BLACK));
	assertEquals(Integer.valueOf(+10),
		     fixtBoardEndGameX.countDifference(Player.BLACK));
	assertEquals(Integer.valueOf(-10),
		     fixtBoardEndGameX.countDifference(Player.WHITE));
	assertEquals(Integer.valueOf(-2),
		     fixtBoardC.countDifference(Player.BLACK));	
    }

    @Test
    public void testIsValid() {
	assertTrue(!BoardState.isValid(0));
	assertTrue(BoardState.isValid(11));
	assertTrue(!BoardState.isValid(19));
	assertTrue(!BoardState.isValid(100));
    }

    @Test
    public void testIsLegal() {
	assertFalse(fixtBoardBlackHasToPass.isLegal(78, Player.BLACK));
	assertTrue(fixtBoardBlackHasToPass.isLegal(78, Player.WHITE));
    }

    @Test
    public void testNextToPlay() {
	assertEquals(Player.BLACK, fixtBoardInitial.nextToPlay(Player.WHITE, null));
	assertEquals(Player.WHITE, fixtBoardInitial.nextToPlay(Player.BLACK, null));
	assertEquals(null, fixtBoardEndGameX.nextToPlay(Player.WHITE, null));
	assertEquals(null, fixtBoardEndGameX.nextToPlay(Player.BLACK, null));
	assertEquals(Player.WHITE, fixtBoardBlackHasToPass.nextToPlay(Player.WHITE, null));
	assertEquals(Player.WHITE, fixtBoardBlackHasToPass.nextToPlay(Player.BLACK, null));
    }

    @Test
    public void testAnyLegalMove() {
	assertTrue(fixtBoardInitial.anyLegalMove(Player.BLACK));
	assertFalse(fixtBoardBlackHasToPass.anyLegalMove(Player.BLACK));
	assertTrue(fixtBoardBlackHasToPass.anyLegalMove(Player.WHITE));
	assertFalse(fixtBoardEndGameX.anyLegalMove(Player.WHITE));
	assertFalse(fixtBoardEndGameX.anyLegalMove(Player.BLACK));
    }

    @Test
    public void testLegalMoves() {
	List<Integer> lm;

	lm = Arrays.asList(35, 46, 53, 64);
	assertEquals(lm, fixtBoardInitial.legalMoves(Player.BLACK));

	lm = Arrays.asList(33, 35, 53);
	assertEquals(lm, fixtBoardA.legalMoves(Player.WHITE));

	lm = Arrays.asList(28, 41, 43, 47, 51, 56, 62, 65, 77);
	assertEquals(lm, fixtBoardC.legalMoves(Player.BLACK));
    }

    @Test
    public void testGet() {
	assertEquals(SquareState.BLACK, fixtBoardC.get(32));
	assertEquals(SquareState.WHITE, fixtBoardC.get(42));
	assertEquals(SquareState.OUTER, fixtBoardC.get(40));
	assertEquals(SquareState.EMPTY, fixtBoardC.get(11));
    }

    @Test
    public void testValueOf() {
	List<SquareState> ssl = new ArrayList<SquareState>();
	for (int i=0; i<100; i++) {
	    ssl.add(fixtBoardC.get(i));
	}
	BoardState boardC = BoardState.valueOf(ssl);
	for (int i=0; i<100; i++) {
	    assertEquals(fixtBoardC.get(i), boardC.get(i));
	}
    }
   
}