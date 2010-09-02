/*
 *  BoardTest.java
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
import java.util.Map;
import java.util.EnumMap;
import java.util.ArrayList;
import java.util.Arrays;

import org.junit.*;
import static org.junit.Assert.*;

public class BoardTest {

    private Board fixtBoardInitial;
    private Board fixtBoardEmpty;
    private Board fixtBoardBlackHasToPass;
    private Board fixtBoardEndGameX;
    private Board fixtBoardA;
    private Board fixtBoardB;
    private Board fixtBoardC;

    private static Board boardFromList(List<Integer> il) {
	if (il == null) return null;
	if (il.size() != Square.values().length) throw new IllegalArgumentException();
	Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(Square.class);
	for (int idx=0; idx<Square.values().length; idx++) {
	    Integer iss = il.get(idx);
	    SquareState ss = null;
	    if (iss == null || iss < 0 || iss > 3) throw new IllegalArgumentException();
	    else if (iss == 0) ss = SquareState.EMPTY;
	    else if (iss == 1) ss = SquareState.BLACK;
	    else if (iss == 2) ss = SquareState.WHITE;
	    else if (iss == 3) ss = SquareState.OUTER;
	    sm.put(Square.getInstance(idx), ss);
	}	
	return Board.valueOf(sm);
    }

    @Before
    public void setUp() {
	fixtBoardInitial = 
	    boardFromList(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 2, 1, 0, 0, 0,
					0, 0, 0, 1, 2, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0));

	fixtBoardEmpty = 
	    boardFromList(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0));

	fixtBoardBlackHasToPass = 
	    boardFromList(Arrays.asList(2, 1, 0, 1, 0, 2, 0, 0,
					1, 1, 1, 1, 1, 1, 1, 2,
					0, 1, 2, 2, 1, 1, 2, 2,
					0, 1, 2, 1, 2, 2, 2, 2,
					0, 1, 2, 1, 2, 2, 2, 2,
					0, 1, 2, 1, 1, 2, 1, 2,
					0, 1, 2, 1, 1, 1, 1, 0,
					2, 2, 2, 2, 2, 2, 1, 2));

	fixtBoardEndGameX = 
	    boardFromList(Arrays.asList(2, 2, 2, 2, 2, 1, 1, 1,
					2, 2, 2, 1, 1, 1, 1, 1,
					2, 2, 2, 1, 1, 1, 2, 1,
					2, 2, 1, 2, 1, 1, 2, 1,
					1, 1, 2, 1, 2, 1, 2, 1,
					1, 2, 1, 2, 1, 2, 1, 1,
					1, 1, 1, 1, 1, 1, 2, 1,
					1, 1, 1, 1, 2, 2, 2, 2));

	fixtBoardA = 
	    boardFromList(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 1, 0, 0, 0, 0,
					0, 0, 0, 1, 1, 0, 0, 0,
					0, 0, 0, 1, 2, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0));

	fixtBoardB = 
	    boardFromList(Arrays.asList(0, 0, 0, 1, 1, 1, 0, 0,
					0, 0, 0, 0, 1, 0, 0, 0,
					0, 0, 0, 1, 1, 2, 2, 0,
					0, 0, 0, 1, 1, 0, 0, 0,
					0, 0, 0, 1, 1, 0, 0, 0,
					0, 0, 0, 0, 1, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0));

	fixtBoardC = 
	    boardFromList(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					2, 1, 1, 1, 0, 0, 2, 0,
					0, 2, 0, 2, 1, 2, 0, 0,
					0, 2, 2, 1, 2, 0, 0, 0,
					0, 0, 1, 1, 0, 2, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0));

    }

    /** 
     * findBracketingPiece is a "private" method in Board class.
     * It is used by only one "client":
     * - wouldFlip
     */
    @Test
    public void testFindBracketingPiece() {
	Square move = Square.H7;
	Direction dir;
	Square b1;
	Square b2;

	dir = Direction.W;
	b1 = move.neighbors().get(dir);
	b2 = Square.C7;
	assertEquals(b2, fixtBoardBlackHasToPass.
		     findBracketingPiece(b1, Player.WHITE, dir));

	dir = Direction.NW;
	b1 = move.neighbors().get(dir);
	b2 = Square.F5;
	assertEquals(b2, fixtBoardBlackHasToPass.
		     findBracketingPiece(b1, Player.WHITE, dir));
	
	dir = Direction.SW;
	b1 = move.neighbors().get(dir);
	b2 = null;
	assertEquals(b2, fixtBoardBlackHasToPass.
		     findBracketingPiece(b1, Player.WHITE, dir));

    }

    /** 
     * wouldFlip is a "private" method in Board class.
     * It is used by two "clients":
     * - makeMove
     * - isLegal
     */
    @Test
    public void testWouldFlip() {
	assertEquals(Square.C7,
		     fixtBoardBlackHasToPass.wouldFlip(Square.H7, Player.WHITE, Direction.W));
	assertEquals(null,
		     fixtBoardBlackHasToPass.wouldFlip(Square.H7, Player.WHITE, Direction.S));
    }

    /**
     *
     */
    @Test
    public void testIsLegal() {

	boolean thrown;

	thrown = false;
	try {
	    fixtBoardInitial.isLegal(null, Player.BLACK);
	} catch (NullPointerException npe) {
	    thrown = true;
	}
	assertTrue(thrown);

	thrown = false;
	try {
	    fixtBoardInitial.isLegal(Square.D3, null);
	} catch (NullPointerException npe) {
	    thrown = true;
	}
	assertTrue(thrown);

	assertTrue(fixtBoardInitial.isLegal(Square.D3, Player.BLACK));
	assertFalse(fixtBoardInitial.isLegal(Square.E3, Player.BLACK));

	assertFalse(fixtBoardBlackHasToPass.isLegal(Square.H7, Player.BLACK));
	assertTrue(fixtBoardBlackHasToPass.isLegal(Square.H7, Player.WHITE));
    }

    /**
     *
     */
    @Test
    public void testMakeMove() {
	Board b = fixtBoardInitial.makeMove(Square.D3, Player.BLACK);
	for (Square sq : Square.values()) {
	    assertEquals(fixtBoardA.get(sq), b.get(sq));
	}
    }

    @Test
    public void testCountPieces() {
	assertEquals(2, Board.initialBoard().countPieces(SquareState.BLACK));
    }

    @Test
    public void testCountDifference() {
	assertEquals(0, Board.initialBoard().countDifference(Player.BLACK));
	assertEquals(+10, fixtBoardEndGameX.countDifference(Player.BLACK));
	assertEquals(-10, fixtBoardEndGameX.countDifference(Player.WHITE));
	assertEquals(-2, fixtBoardC.countDifference(Player.BLACK));	
    }

    @Test
    public void testNextToPlay() {
	assertEquals(Player.BLACK, fixtBoardInitial.nextToPlay(Player.WHITE));
	assertEquals(Player.WHITE, fixtBoardInitial.nextToPlay(Player.BLACK));
	assertEquals(null, fixtBoardEndGameX.nextToPlay(Player.WHITE));
	assertEquals(null, fixtBoardEndGameX.nextToPlay(Player.BLACK));
	assertEquals(Player.WHITE, fixtBoardBlackHasToPass.nextToPlay(Player.WHITE));
	assertEquals(Player.WHITE, fixtBoardBlackHasToPass.nextToPlay(Player.BLACK));
    }

    @Test
    public void testHasAnyLegalMove() {
	assertTrue(fixtBoardInitial.hasAnyLegalMove(Player.BLACK));
	assertFalse(fixtBoardBlackHasToPass.hasAnyLegalMove(Player.BLACK));
	assertTrue(fixtBoardBlackHasToPass.hasAnyLegalMove(Player.WHITE));
	assertFalse(fixtBoardEndGameX.hasAnyLegalMove(Player.WHITE));
	assertFalse(fixtBoardEndGameX.hasAnyLegalMove(Player.BLACK));
    }

    @Test
    public void testLegalMoves() {
	List<Square> lm;

	lm = Arrays.asList(Square.D3, Square.C4, Square.F5, Square.E6);
	assertEquals(lm, fixtBoardInitial.legalMoves(Player.BLACK));

	lm = Arrays.asList(Square.C3, Square.E3, Square.C5);
	assertEquals(lm, fixtBoardA.legalMoves(Player.WHITE));

	lm = Arrays.asList(Square.H2, Square.A4, Square.C4, 
			   Square.G4, Square.A5, Square.F5, 
			   Square.B6, Square.E6, Square.G7);
	assertEquals(lm, fixtBoardC.legalMoves(Player.BLACK));
    }

    @Test
    public void testGet() {
	assertEquals(SquareState.BLACK, fixtBoardC.get(Square.B3));
	assertEquals(SquareState.WHITE, fixtBoardC.get(Square.B4));
	assertEquals(SquareState.OUTER, fixtBoardC.get(null));
	assertEquals(SquareState.EMPTY, fixtBoardC.get(Square.A1));
    }

    @Test
    public void testValueOf() {
	Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(Square.class);
	for (Square sq : Square.values()) {
	    sm.put(sq, fixtBoardC.get(sq));
	}
	Board boardC = Board.valueOf(sm);
	for (Square sq : Square.values()) {
	    assertEquals(fixtBoardC.get(sq), boardC.get(sq));
	}
    }

}