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
import java.util.HashMap;
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
    public Board fixtBoardB;
    private Board fixtBoardC;
    private Board fixtBoardEqlA;
    private Board fixtBoardEqlB;
    public Board fixtBoardMinimaxA;
    public Board fixtBoardMinimaxB;

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

	List<Integer> listFixtBoardEql = 
	    Arrays.asList(2, 1, 0, 1, 0, 2, 0, 0,
			  1, 1, 1, 1, 1, 1, 1, 2,
			  0, 1, 2, 2, 1, 1, 2, 2,
			  0, 1, 2, 1, 2, 2, 2, 2,
			  0, 1, 2, 1, 2, 2, 2, 2,
			  0, 1, 2, 1, 1, 2, 1, 2,
			  0, 1, 2, 1, 1, 1, 1, 0,
			  2, 2, 2, 2, 2, 2, 1, 2);
	fixtBoardEqlA = boardFromList(listFixtBoardEql);
	fixtBoardEqlB = boardFromList(listFixtBoardEql);

	fixtBoardMinimaxA = 
	    boardFromList(Arrays.asList(2, 0, 2, 0, 2, 0, 2, 0,
					1, 0, 1, 0, 1, 0, 1, 0,
					0, 0, 1, 0, 1, 0, 1, 0,
					0, 0, 0, 0, 1, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0));

	fixtBoardMinimaxB = 
	    boardFromList(Arrays.asList(2, 1, 2, 0, 0, 0, 0, 0,
					1, 0, 1, 0, 0, 0, 0, 0,
					0, 0, 1, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0,
					0, 0, 0, 0, 0, 0, 0, 0));
    }

    @Test
    public void testEquals() {
	assertFalse(fixtBoardInitial.equals(null));
	assertFalse(fixtBoardInitial.equals(new Object()));
	assertFalse(fixtBoardInitial.equals(fixtBoardA));

	assertTrue(fixtBoardInitial.equals(fixtBoardInitial));
	assertTrue(fixtBoardInitial.equals(Board.initialBoard()));
	assertTrue(Board.initialBoard().equals(fixtBoardInitial));

	assertTrue(fixtBoardEqlA.equals(fixtBoardEqlA));

	assertTrue(fixtBoardEqlA.equals(fixtBoardEqlB));
	assertTrue(fixtBoardEqlB.equals(fixtBoardEqlA));
    }

    @Test
    public void testHashCode() {
	assertEquals(fixtBoardInitial.hashCode(), fixtBoardInitial.hashCode());
	assertEquals(fixtBoardEmpty.hashCode(), fixtBoardEmpty.hashCode());
	assertEquals(fixtBoardBlackHasToPass.hashCode(), fixtBoardBlackHasToPass.hashCode());
	assertEquals(fixtBoardEndGameX.hashCode(), fixtBoardEndGameX.hashCode());
	assertEquals(fixtBoardA.hashCode(), fixtBoardA.hashCode());
	assertEquals(fixtBoardB.hashCode(), fixtBoardB.hashCode());
	assertEquals(fixtBoardC.hashCode(), fixtBoardC.hashCode());
	assertEquals(fixtBoardEqlA.hashCode(), fixtBoardEqlA.hashCode());
	assertEquals(fixtBoardEqlB.hashCode(), fixtBoardEqlB.hashCode());

	assertEquals(fixtBoardEqlA.hashCode(), fixtBoardEqlB.hashCode());
	assertEquals(fixtBoardEqlB.hashCode(), fixtBoardEqlA.hashCode());
    }

    /** 
     * findBracketingPiece is a "private" method in Board class.
     * It is used by only one "client":
     * - wouldFlip
     */
    @Test
    public void testFindBracketingPiece() {
	{
	    Square move = Square.H7;
	    Direction dir = Direction.W;
	    Square b1 = move.neighbors().get(dir);
	    Square b2 = Square.C7;
	    assertEquals(b2, fixtBoardBlackHasToPass.
			 findBracketingPiece(b1, Player.WHITE, dir));
	}

	{
	    Square move = Square.H7;
	    Direction dir = Direction.NW;
	    Square b1 = move.neighbors().get(dir);
	    Square b2 = Square.F5;
	    assertEquals(b2, fixtBoardBlackHasToPass.
			 findBracketingPiece(b1, Player.WHITE, dir));
	}

	{	
	    Square move = Square.H7;
	    Direction dir = Direction.SW;
	    Square b1 = move.neighbors().get(dir);
	    Square b2 = null;
	    assertEquals(b2, fixtBoardBlackHasToPass.
			 findBracketingPiece(b1, Player.WHITE, dir));
	}
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

	try {
	    fixtBoardInitial.isLegal(null, Player.BLACK);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	try {
	    fixtBoardInitial.isLegal(Square.D3, null);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	assertFalse(fixtBoardInitial.isLegal(Square.D4, Player.BLACK));
	assertFalse(fixtBoardInitial.isLegal(Square.D4, Player.WHITE));

	assertFalse(fixtBoardInitial.isLegal(Square.A1, Player.BLACK));
	assertFalse(fixtBoardInitial.isLegal(Square.A1, Player.WHITE));

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
	try {
	    fixtBoardInitial.makeMove(null, Player.BLACK);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	try {
	    fixtBoardInitial.makeMove(Square.D3, null);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	try {
	    fixtBoardInitial.makeMove(Square.A1, Player.BLACK);
	    fail("An exception must be risen.");
	} catch (IllegalArgumentException iae) {
	    assertTrue(true);
	}

	Board b = fixtBoardInitial.makeMove(Square.D3, Player.BLACK);
	for (Square sq : Square.values()) {
	    assertEquals(fixtBoardA.get(sq), b.get(sq));
	}
    }

    @Test
    public void testCountPieces() {
	try {
	    fixtBoardInitial.countPieces(null);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	assertEquals(2, Board.initialBoard().countPieces(SquareState.BLACK));
	assertEquals(2, Board.initialBoard().countPieces(SquareState.WHITE));
	assertEquals(60, Board.initialBoard().countPieces(SquareState.EMPTY));
	assertEquals(0, Board.initialBoard().countPieces(SquareState.OUTER));
    }

    @Test
    public void testCountDifference() {
	try {
	    fixtBoardInitial.countDifference(null);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	assertEquals(0, Board.initialBoard().countDifference(Player.BLACK));
	assertEquals(+10, fixtBoardEndGameX.countDifference(Player.BLACK));
	assertEquals(-10, fixtBoardEndGameX.countDifference(Player.WHITE));
	assertEquals(-2, fixtBoardC.countDifference(Player.BLACK));	
    }

    @Test
    public void testNextToPlay() {
	try {
	    fixtBoardInitial.nextToPlay(null);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

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
    public void testHasAnyPlayerAnyLegalMove() {
	assertFalse(fixtBoardEmpty.hasAnyPlayerAnyLegalMove());
	assertTrue(fixtBoardInitial.hasAnyPlayerAnyLegalMove());
	assertTrue(fixtBoardBlackHasToPass.hasAnyPlayerAnyLegalMove());
	assertFalse(fixtBoardEndGameX.hasAnyPlayerAnyLegalMove());
    }

    @Test
    public void testLegalMoves() {
	try {
	    fixtBoardInitial.legalMoves(null);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	{
	    List<Square> lm = Arrays.asList(Square.D3, Square.C4, Square.F5, Square.E6);
	    assertEquals(lm, fixtBoardInitial.legalMoves(Player.BLACK));
	}

	{
	    List<Square> lm = Arrays.asList(Square.C3, Square.E3, Square.C5);
	    assertEquals(lm, fixtBoardA.legalMoves(Player.WHITE));
	}

	{
	    List<Square> lm = Arrays.asList(Square.H2, Square.A4, Square.C4, 
					    Square.G4, Square.A5, Square.F5, 
					    Square.B6, Square.E6, Square.G7);
	    assertEquals(lm, fixtBoardC.legalMoves(Player.BLACK));
	}

	{
	    List<Square> lm = new ArrayList<Square>();
	    assertEquals(lm, fixtBoardBlackHasToPass.legalMoves(Player.BLACK));
	}

	{
	    List<Square> lm = Arrays.asList(Square.A3, Square.C4, Square.G4, Square.E5);
	    assertEquals(lm, fixtBoardMinimaxA.legalMoves(Player.WHITE));
	}
    }

    @Test
    public void testGet() {
	assertEquals(SquareState.BLACK, fixtBoardC.get(Square.B3));
	assertEquals(SquareState.WHITE, fixtBoardC.get(Square.B4));
	assertEquals(SquareState.OUTER, fixtBoardC.get(null));
	assertEquals(SquareState.EMPTY, fixtBoardC.get(Square.A1));
    }

    @Test
    public void testPrintBoard() {
	StringBuilder initialBoard = new StringBuilder();
	initialBoard.append("    a b c d e f g h \n");
	initialBoard.append(" 1  . . . . . . . . \n");
	initialBoard.append(" 2  . . . . . . . . \n");
	initialBoard.append(" 3  . . . . . . . . \n");
	initialBoard.append(" 4  . . . O @ . . . \n");
	initialBoard.append(" 5  . . . @ O . . . \n");
	initialBoard.append(" 6  . . . . . . . . \n");
	initialBoard.append(" 7  . . . . . . . . \n");
	initialBoard.append(" 8  . . . . . . . . \n");
	assertEquals(initialBoard.toString(), fixtBoardInitial.printBoard());
    }

    @Test
    public void testPrintCount() {
	assertEquals("[@=2 0=2 (0)]", fixtBoardInitial.printCount());
	assertEquals("[@=26 0=28 (-2)]", fixtBoardBlackHasToPass.printCount());
    }

    @Test
    public void testPrintBoardWithCount() {
	StringBuilder initialBoard = new StringBuilder();
	initialBoard.append("    a b c d e f g h [@=2 0=2 (0)]\n");
	initialBoard.append(" 1  . . . . . . . . \n");
	initialBoard.append(" 2  . . . . . . . . \n");
	initialBoard.append(" 3  . . . . . . . . \n");
	initialBoard.append(" 4  . . . O @ . . . \n");
	initialBoard.append(" 5  . . . @ O . . . \n");
	initialBoard.append(" 6  . . . . . . . . \n");
	initialBoard.append(" 7  . . . . . . . . \n");
	initialBoard.append(" 8  . . . . . . . . \n");
	assertEquals(initialBoard.toString(), fixtBoardInitial.printBoardWithCount());
    }

    @Test
    public void testValueOf() {
	/**
	 * Tests if the valueOf method throws a NullPointerException when
	 * the passed map is null.
	 */
	try {
	    Board.valueOf(null);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	/**
	 * Tests if the valueOf method throws an IllegalArgumentException when
	 * the passed map has one or more missing keys.
	 */
	Map<Square, SquareState> notCompleteSquareMap = new EnumMap<Square, SquareState>(Square.class);
	notCompleteSquareMap.put(Square.A1, SquareState.EMPTY);
	try {
	    Board.valueOf(notCompleteSquareMap);
	    fail("An exception must be risen.");
	} catch (IllegalArgumentException iae) {
	    assertTrue(true);
	}
 
	/**
	 * Tests if the valueOf method throws a NullPointerException when
	 * the passed map has a null key.
	 */
	Map<Square, SquareState> corruptedSquareHashMap = new HashMap<Square, SquareState>();
	for (Square sq : Square.values()) {
	    corruptedSquareHashMap.put(sq, SquareState.EMPTY);
	}
	corruptedSquareHashMap.remove(Square.H8);
	corruptedSquareHashMap.put(null, SquareState.EMPTY);
	try {
	    Board corruptedBoard = Board.valueOf(corruptedSquareHashMap);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}
 
	/**
	 * Tests if the valueOf method returns the supposed Board. It is the
	 * standard usage under expected behavior.
	 */
	Map<Square, SquareState> squareMap = new EnumMap<Square, SquareState>(Square.class);
	for (Square sq : Square.values()) {
	    squareMap.put(sq, fixtBoardC.get(sq));
	}
	Board boardC0 = Board.valueOf(squareMap);
	for (Square sq : Square.values()) {
	    assertEquals(fixtBoardC.get(sq), boardC0.get(sq));
	}

 	/**
	 * Tests if the valueOf method returns the supposed Board. It is the
	 * standard usage under expected behavior.
	 * In this test the passed map is an HashMap instead of the "standard" EnumMap.
	 */
	Map<Square, SquareState> squareHashMap = new HashMap<Square, SquareState>();
	for (Square sq : Square.values()) {
	    squareHashMap.put(sq, fixtBoardC.get(sq));
	}
	Board boardC1 = Board.valueOf(squareHashMap);
	for (Square sq : Square.values()) {
	    assertEquals(fixtBoardC.get(sq), boardC1.get(sq));
	}
	
    }

}