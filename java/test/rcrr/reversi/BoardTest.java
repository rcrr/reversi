/*
 *  BoardTest.java
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

import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.EnumMap;
import java.util.ArrayList;
import java.util.Arrays;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for {@code Board} class.
 * <p>
 * Several thing to do:
 * <ul>
 *   <li>Comment assert statement.</li>
 *   <li>Rewrite Exception checking.</li>
 *   <li>Split tests into more granular methods.</li>
 *   <li>Complete javadocs and style.</li>
 * </ul>
 */
public class BoardTest {

    @Test
    public final void testReviewReminder() {
        fail("The Test Suite must be reviewed!");
    }

    /**
     * Tests the {@code countDifference(Player)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see Board#countDifference(Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testCountDifference_boundaryConditions_checkNullParameter_player() {
        new BoardBuilder().build()
            .countDifference(Player.NULL);
    }

    /**
     * Tests the {@code countDifference(Player)} method.
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.countDifference(Player.BLACK)} must return a count equal to 0.</li>
     *   <li>{@code BoardFixtures.FINAL_B37_W27.countDifference(Player.BLACK)} must return a count equal to +10.</li>
     *   <li>{@code BoardFixtures.FINAL_B37_W27.countDifference(Player.WHITE)} must return a count equal to -10.</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_C_12_MOVES.countDifference(Player.BLACK)} must return a count equal to -2.</li>
     * </ul>
     *
     * @see Board#countDifference(Player)
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#FINAL_B37_W27
     * @see BoardFixtures#EARLY_GAME_C_12_MOVES
     */
    @Test
    public void testCountDifference() {
        assertThat("BoardFixtures.INITIAL.countDifference(Player.BLACK)"
                   + " must return a count equal to 0.",
                   BoardFixtures.INITIAL.countDifference(Player.BLACK),
                   is(0));
        assertThat("BoardFixtures.INITIAL.countDifference(Player.BLACK)"
                   + " must return a count equal to +10.",
                   BoardFixtures.FINAL_B37_W27.countDifference(Player.BLACK),
                   is(+10));
        assertThat("BoardFixtures.INITIAL.countDifference(Player.WHITE)"
                   + " must return a count equal to -10.",
                   BoardFixtures.FINAL_B37_W27.countDifference(Player.WHITE),
                   is(-10));
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.countDifference(Player.BLACK)"
                   + " must return a count equal to -2.",
                   BoardFixtures.EARLY_GAME_C_12_MOVES.countDifference(Player.BLACK),
                   is(-2));
    }

    @Test
    public void testCountPieces() {
        try {
            BoardFixtures.INITIAL.countPieces(null);
            fail("A NullPointerException must be risen when the parameter player is null.");
        } catch (NullPointerException npe) {
            assertTrue(true);
        }

        assertThat("Black player has two discs in the initial board configuration.",
                   BoardFixtures.INITIAL.countPieces(SquareState.BLACK), is(2));
        assertThat("White player has two discs in the initial board configuration.",
                   BoardFixtures.INITIAL.countPieces(SquareState.WHITE), is(2));
        assertThat("There are sixty empty squares in the initial board configuration.",
                   BoardFixtures.INITIAL.countPieces(SquareState.EMPTY), is(60));
        assertThat("There are no outer squares in any board configuration.",
                   BoardFixtures.INITIAL.countPieces(SquareState.OUTER), is(0));
    }

    @Test
    public void testEmptyBoard() {
        assertEquals(BoardFixtures.EMPTY, Board.emptyBoard());
        Board empty = Board.emptyBoard();
        for (Square square : Square.values()) {
            assertEquals(SquareState.EMPTY, empty.get(square));
        }
    }

    @Test
    public void testEquals() {
        assertFalse(BoardFixtures.INITIAL.equals(null));
        assertFalse(BoardFixtures.INITIAL.equals(new Object()));
        assertFalse(BoardFixtures.INITIAL.equals(BoardFixtures.FIRST_MOVE_D3));

        assertTrue(BoardFixtures.INITIAL.equals(BoardFixtures.INITIAL));
        assertTrue(BoardFixtures.INITIAL.equals(Board.initialBoard()));
        assertTrue(Board.initialBoard().equals(BoardFixtures.INITIAL));

        assertTrue(BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_A));
        assertTrue(BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_B));

        assertTrue(BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_B));
        assertTrue(BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_A));
    }

    @Test
    public void testGet() {
        assertEquals(SquareState.BLACK, BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.B3));
        assertEquals(SquareState.WHITE, BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.B4));
        assertEquals(SquareState.OUTER, BoardFixtures.EARLY_GAME_C_12_MOVES.get(null));
        assertEquals(SquareState.EMPTY, BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.A1));
    }

    @Test
    public void testHasAnyLegalMove() {
        assertTrue(BoardFixtures.INITIAL.hasAnyLegalMove(Player.BLACK));
        assertFalse(BoardFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(Player.BLACK));
        assertTrue(BoardFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(Player.WHITE));
        assertFalse(BoardFixtures.FINAL_B37_W27.hasAnyLegalMove(Player.WHITE));
        assertFalse(BoardFixtures.FINAL_B37_W27.hasAnyLegalMove(Player.BLACK));
    }

    @Test
    public void testHasAnyPlayerAnyLegalMove() {
        assertFalse(BoardFixtures.EMPTY.hasAnyPlayerAnyLegalMove());
        assertTrue(BoardFixtures.INITIAL.hasAnyPlayerAnyLegalMove());
        assertTrue(BoardFixtures.BLACK_HAS_TO_PASS.hasAnyPlayerAnyLegalMove());
        assertFalse(BoardFixtures.FINAL_B37_W27.hasAnyPlayerAnyLegalMove());
    }

    @Test
    public void testHashCode() {
        assertEquals(BoardFixtures.INITIAL.hashCode(), BoardFixtures.INITIAL.hashCode());
        assertEquals(BoardFixtures.EMPTY.hashCode(), BoardFixtures.EMPTY.hashCode());
        assertEquals(BoardFixtures.BLACK_HAS_TO_PASS.hashCode(), BoardFixtures.BLACK_HAS_TO_PASS.hashCode());
        assertEquals(BoardFixtures.FINAL_B37_W27.hashCode(), BoardFixtures.FINAL_B37_W27.hashCode());
        assertEquals(BoardFixtures.FIRST_MOVE_D3.hashCode(), BoardFixtures.FIRST_MOVE_D3.hashCode());
        assertEquals(BoardFixtures.EARLY_GAME_B_9_MOVES.hashCode(), BoardFixtures.EARLY_GAME_B_9_MOVES.hashCode());
        assertEquals(BoardFixtures.EARLY_GAME_C_12_MOVES.hashCode(), BoardFixtures.EARLY_GAME_C_12_MOVES.hashCode());
        assertEquals(BoardFixtures.EQL_TEST_A.hashCode(), BoardFixtures.EQL_TEST_A.hashCode());
        assertEquals(BoardFixtures.EQL_TEST_B.hashCode(), BoardFixtures.EQL_TEST_B.hashCode());

        assertEquals(BoardFixtures.EQL_TEST_A.hashCode(), BoardFixtures.EQL_TEST_B.hashCode());
        assertEquals(BoardFixtures.EQL_TEST_B.hashCode(), BoardFixtures.EQL_TEST_A.hashCode());
    }

    @Test
    public void testInitialBoard() {
        assertEquals(BoardFixtures.INITIAL, Board.initialBoard());
        Board initial = Board.initialBoard();
        for (Square square : Square.values()) {
            SquareState actual = initial.get(square);
            SquareState expected;
            switch (square) {
            case D4: expected = SquareState.WHITE; break;
            case E4: expected = SquareState.BLACK; break;
            case D5: expected = SquareState.BLACK; break;
            case E5: expected = SquareState.WHITE; break;
            default: expected = SquareState.EMPTY; break;
            }
            assertEquals(expected, actual);
        }
    }

    @Test
    public void testIsLegal() {

        try {
            BoardFixtures.INITIAL.isLegal(null, Player.BLACK);
            fail("An exception must be risen.");
        } catch (NullPointerException npe) {
            assertTrue(true);
        }

        try {
            BoardFixtures.INITIAL.isLegal(Square.D3, null);
            fail("An exception must be risen.");
        } catch (NullPointerException npe) {
            assertTrue(true);
        }

        /** D4 is already occupied by a disk in the initial board. */
        assertFalse(BoardFixtures.INITIAL.isLegal(Square.D4, Player.BLACK));
        assertFalse(BoardFixtures.INITIAL.isLegal(Square.D4, Player.WHITE));

        /** Corner A1 is not a legal move given the initial board. */
        assertFalse(BoardFixtures.INITIAL.isLegal(Square.A1, Player.BLACK));
        assertFalse(BoardFixtures.INITIAL.isLegal(Square.A1, Player.WHITE));

        /** D3 is one of the four legal moves that the black player has in the opening. */
        assertTrue(BoardFixtures.INITIAL.isLegal(Square.D3, Player.BLACK));

        /** E3 is not among the legal opening moves. */
        assertFalse(BoardFixtures.INITIAL.isLegal(Square.E3, Player.BLACK));

        assertFalse(BoardFixtures.BLACK_HAS_TO_PASS.isLegal(Square.H7, Player.BLACK));
        assertTrue(BoardFixtures.BLACK_HAS_TO_PASS.isLegal(Square.H7, Player.WHITE));

        assertTrue(BoardFixtures.EARLY_GAME_B_9_MOVES.isLegal(Square.C3, Player.WHITE));
        assertTrue(BoardFixtures.EARLY_GAME_B_9_MOVES.isLegal(Square.C6, Player.WHITE));

    }

    @Test
    public void testLegalMoves() {
        try {
            BoardFixtures.INITIAL.legalMoves(null);
            fail("An exception must be risen.");
        } catch (NullPointerException npe) {
            assertTrue(true);
        }

        {
            List<Square> lm = Arrays.asList(Square.D3, Square.C4, Square.F5, Square.E6);
            assertEquals(lm, BoardFixtures.INITIAL.legalMoves(Player.BLACK));
        }

        {
            List<Square> lm = Arrays.asList(Square.C3, Square.E3, Square.C5);
            assertEquals(lm, BoardFixtures.FIRST_MOVE_D3.legalMoves(Player.WHITE));
        }

        {
            List<Square> lm = Arrays.asList(Square.H2, Square.A4, Square.C4, 
                                            Square.G4, Square.A5, Square.F5, 
                                            Square.B6, Square.E6, Square.G7);
            assertEquals(lm, BoardFixtures.EARLY_GAME_C_12_MOVES.legalMoves(Player.BLACK));
        }

        {
            List<Square> lm = new ArrayList<Square>();
            assertEquals(lm, BoardFixtures.BLACK_HAS_TO_PASS.legalMoves(Player.BLACK));
        }

        {
            List<Square> lm = Arrays.asList(Square.A3, Square.C4, Square.G4, Square.E5);
            assertEquals(lm, BoardFixtures.MINIMAX_TEST_CASE_A.legalMoves(Player.WHITE));
        }

        {
            List<Square> lm = Arrays.asList(Square.C3, Square.C6);
            assertEquals(lm, BoardFixtures.EARLY_GAME_B_9_MOVES.legalMoves(Player.WHITE));
        }

        {
            List<Square> lm = Arrays.asList(Square.B2, Square.C2, Square.D2,
                                            Square.F2, Square.G2, Square.C4,
                                            Square.G4);
            assertEquals(lm, BoardFixtures.EARLY_GAME_BC3_10_MOVES.legalMoves(Player.BLACK));
        }

        {
            List<Square> lm = Arrays.asList(Square.H3, Square.C4, Square.F4,
                                            Square.G4, Square.C5, Square.F5,
                                            Square.D6);
            assertEquals(lm, BoardFixtures.EARLY_GAME_BC6_10_MOVES.legalMoves(Player.BLACK));
        }

    }

    /**
     * Tests the mechanics of the makeMove() method.
     */
    @Test
    public void testMakeMove() {

        /** Tests that a null player cannot be passed to makeMove. */
        try {
            BoardFixtures.INITIAL.makeMove(Square.D3, null);
            fail("An exception must be risen.");
        } catch (NullPointerException npe) {
            assertTrue(true);
        }

        /** Tests that a null move cannot be passed to makeMove when a legal one is there. */
        try {
            BoardFixtures.INITIAL.makeMove(null, Player.BLACK);
            fail("An exception must be risen.");
        } catch (NullPointerException npe) {
            assertTrue(true);
        }
        
        /** 
         * Tests that a null move can be a valid parameter when no legal moves are available to the player.
         * Tests also that the return value is the board itself.
         */
        assertEquals(BoardFixtures.BLACK_HAS_TO_PASS, BoardFixtures.BLACK_HAS_TO_PASS.makeMove(null, Player.BLACK));

        /** Tests that an illegal move cannot be passed to makeMove. */
        try {
            BoardFixtures.INITIAL.makeMove(Square.A1, Player.BLACK);
            fail("An exception must be risen.");
        } catch (IllegalArgumentException iae) {
            assertTrue(true);
        }

        /** Move D3 by black sent to the initial board returns the BoardFixtures.FIRST_MOVE_D3. */
        assertTrue(BoardFixtures.FIRST_MOVE_D3.equals(BoardFixtures.INITIAL.makeMove(Square.D3, Player.BLACK)));
        /** Move C3 by white sent to the BoardFixtures.EARLY_GAME_B_9_MOVES board returns the BoardFixtures.EARLY_GAME_BC3_10_MOVES. */
        assertTrue(BoardFixtures.EARLY_GAME_BC3_10_MOVES.equals(BoardFixtures.EARLY_GAME_B_9_MOVES.makeMove(Square.C3, Player.WHITE)));
        /** Move C6 by white sent to the BoardFixtures.EARLY_GAME_B_9_MOVES board returns the BoardFixtures.EARLY_GAME_BC6_10_MOVES. */
        assertTrue(BoardFixtures.EARLY_GAME_BC6_10_MOVES.equals(BoardFixtures.EARLY_GAME_B_9_MOVES.makeMove(Square.C6, Player.WHITE)));

        /** A few basic cases, designed to test the function as much as possible. */
        assertTrue(BoardFixtures.MAKE_MOVE_TEST_CASE_A_AFTER.equals(BoardFixtures.MAKE_MOVE_TEST_CASE_A_BEFORE.makeMove(Square.D4, Player.WHITE)));
        assertTrue(BoardFixtures.MAKE_MOVE_TEST_CASE_B_AFTER.equals(BoardFixtures.MAKE_MOVE_TEST_CASE_B_BEFORE.makeMove(Square.D4, Player.WHITE)));
        assertTrue(BoardFixtures.MAKE_MOVE_TEST_CASE_C_AFTER.equals(BoardFixtures.MAKE_MOVE_TEST_CASE_C_BEFORE.makeMove(Square.D4, Player.WHITE)));
        assertTrue(BoardFixtures.MAKE_MOVE_TEST_CASE_D_AFTER.equals(BoardFixtures.MAKE_MOVE_TEST_CASE_D_BEFORE.makeMove(Square.B4, Player.WHITE)));

    }

    @Test
    public void testNextToPlay() {
        try {
            BoardFixtures.INITIAL.nextToPlay(null);
            fail("An exception must be risen.");
        } catch (NullPointerException npe) {
            assertTrue(true);
        }

        assertEquals(Player.BLACK, BoardFixtures.INITIAL.nextToPlay(Player.WHITE));
        assertEquals(Player.WHITE, BoardFixtures.INITIAL.nextToPlay(Player.BLACK));
        assertEquals(null, BoardFixtures.FINAL_B37_W27.nextToPlay(Player.WHITE));
        assertEquals(null, BoardFixtures.FINAL_B37_W27.nextToPlay(Player.BLACK));
        assertEquals(Player.WHITE, BoardFixtures.BLACK_HAS_TO_PASS.nextToPlay(Player.WHITE));
        assertEquals(Player.WHITE, BoardFixtures.BLACK_HAS_TO_PASS.nextToPlay(Player.BLACK));
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
        assertEquals(initialBoard.toString(), BoardFixtures.INITIAL.printBoard());
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
        assertEquals(initialBoard.toString(), BoardFixtures.INITIAL.printBoardWithCount());
    }

    @Test
    public void testPrintCount() {
        assertEquals("[@=2 0=2 (0)]", BoardFixtures.INITIAL.printCount());
        assertEquals("[@=26 0=28 (-2)]", BoardFixtures.BLACK_HAS_TO_PASS.printCount());
    }

    /**
     * Test the {@code valueOf(Map<Square, SquareState)} factory.
     * <p>
     * The factory receives the squareMap parameter, and any further change to it
     * must not be reflected to the returned board instance.
     *
     * @see Board#valueOf(Map)
     */
    @Test
    public final void testValueOf_squareMapMustBeUnchangeable() {

        final Map<Square, SquareState> changeable = new EnumMap<Square, SquareState>(Square.class);
        for (Square sq : Square.values()) {
            changeable.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        final Board instance = Board.valueOf(changeable);
        changeable.put(Square.A1, SquareState.BLACK);

        assertThat("The board instance must be not affected by a"
                   + " change in the squareMap parameter.",
                   instance.get(Square.A1), is(SquareState.EMPTY));
    }

    /**
     * Test the {@code valueOf(Map<Square, SquareState)} factory.
     * <p>
     * The factory receives the squareMap parameter, it cannot contains null values.
     *
     * @see Board#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_squareMapMustNotContainNullValues() {
        final Map<Square, SquareState> corruptedSquares = new HashMap<Square, SquareState>();
        for (Square sq : Square.values()) {
            corruptedSquares.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        corruptedSquares.put(Square.B3, SquareState.NULL);
        Board.valueOf(corruptedSquares);
    }

    /**
     * Test the {@code valueOf(Map<Square, SquareState)} factory.
     * <p>
     * The factory receives the squareMap parameter, it cannot contains null keys.
     *
     * @see Board#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_squareMapMustNotContainNullKeys() {
        final Map<Square, SquareState> corruptedSquares = new HashMap<Square, SquareState>();
        for (Square sq : Square.values()) {
            corruptedSquares.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        corruptedSquares.remove(Square.H8);
        corruptedSquares.put(Square.NULL, SquareState.EMPTY);
        Board.valueOf(corruptedSquares);
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
        {
            Map<Square, SquareState> notCompleteSquareMap = new EnumMap<Square, SquareState>(Square.class);
            notCompleteSquareMap.put(Square.A1, SquareState.EMPTY);
            try {
                Board.valueOf(notCompleteSquareMap);
                fail("An exception must be risen.");
            } catch (IllegalArgumentException iae) {
                assertTrue(true);
            }
        }
 
        /**
         * Tests if the valueOf method throws a NullPointerException when
         * the passed map has a null key.
         */
        {
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
        }
 
        /**
         * Tests if the valueOf method returns the supposed Board. It is the
         * standard usage under expected behavior.
         */
        {
            Map<Square, SquareState> squareMap = new EnumMap<Square, SquareState>(Square.class);
            for (Square sq : Square.values()) {
                squareMap.put(sq, BoardFixtures.EARLY_GAME_C_12_MOVES.get(sq));
            }
            Board boardC0 = Board.valueOf(squareMap);
            for (Square sq : Square.values()) {
                assertEquals(BoardFixtures.EARLY_GAME_C_12_MOVES.get(sq), boardC0.get(sq));
            }
        }

        /**
         * Tests if the valueOf method returns the supposed Board. It is the
         * standard usage under expected behavior.
         * In this test the passed map is an HashMap instead of the "standard" EnumMap.
         */
        {
            Map<Square, SquareState> squareHashMap = new HashMap<Square, SquareState>();
            for (Square sq : Square.values()) {
                squareHashMap.put(sq, BoardFixtures.EARLY_GAME_C_12_MOVES.get(sq));
            }
            Board boardC1 = Board.valueOf(squareHashMap);
            for (Square sq : Square.values()) {
                assertEquals(BoardFixtures.EARLY_GAME_C_12_MOVES.get(sq), boardC1.get(sq));
            }
        }
        
    }

    /** 
     * findBracketingPiece is a "private" method in Board class.
     * It is used by only one "client":
     * - the wouldFlip method
     */
    @Test
    public void testFindBracketingPiece() {
        {
            Square move = Square.H7;
            Direction dir = Direction.W;
            Square b1 = move.neighbors().get(dir);
            Square b2 = Square.C7;
            assertEquals(b2, BoardFixtures.BLACK_HAS_TO_PASS.
                         findBracketingPiece(b1, Player.WHITE, dir));
        }

        {
            Square move = Square.H7;
            Direction dir = Direction.NW;
            Square b1 = move.neighbors().get(dir);
            Square b2 = Square.F5;
            assertEquals(b2, BoardFixtures.BLACK_HAS_TO_PASS.
                         findBracketingPiece(b1, Player.WHITE, dir));
        }

        {       
            Square move = Square.H7;
            Direction dir = Direction.SW;
            Square b1 = move.neighbors().get(dir);
            Square b2 = null;
            assertEquals(b2, BoardFixtures.BLACK_HAS_TO_PASS.
                         findBracketingPiece(b1, Player.WHITE, dir));
        }
    }

    /** 
     * wouldFlip is a "private" method in Board class.
     * It is used by only two "clients":
     * - the makeMove method
     * - the isLegal methid
     * both defined in the class itself.
     */
    @Test
    public void testWouldFlip() {
        assertEquals(Square.C7,
                     BoardFixtures.BLACK_HAS_TO_PASS.wouldFlip(Square.H7, Player.WHITE, Direction.W));
        assertEquals(null,
                     BoardFixtures.BLACK_HAS_TO_PASS.wouldFlip(Square.H7, Player.WHITE, Direction.S));
    }

}
