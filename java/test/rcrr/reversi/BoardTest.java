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

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
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

    /** Class constructor. */
    public BoardTest() { }

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
    public final void testCountDifference() {
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

    /**
     * Tests the {@code countPieces(SquareState)} method when parameter
     * {@code color} is {@code null}.
     *
     * @see Board#countPieces(SquareState)
     */
    @Test(expected = NullPointerException.class)
    public final void testCountPieces_boundaryConditions_checkNullParameter_color() {
        new BoardBuilder().build()
            .countPieces(SquareState.NULL);
    }

    /**
     * Tests the {@code countPieces(SquareState)} method.
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.countPieces(SquareState.BLACK)} must return a count equal to 2.</li>
     *   <li>{@code BoardFixtures.INITIAL.countPieces(SquareState.WHITE)} must return a count equal to 2.</li>
     *   <li>{@code BoardFixtures.INITIAL.countPieces(SquareState.EMPTY)} must return a count equal to 60.</li>
     *   <li>{@code BoardFixtures.INITIAL.countPieces(SquareState.OUTER)} must return a count equal to 0.</li>
     * </ul>
     *
     * @see Board#countPieces(SquareState)
     * @see BoardFixtures#INITIAL
     */
    @Test
    public final void testCountPieces() {
        assertThat("Black player has two discs in the initial board configuration.",
                   BoardFixtures.INITIAL.countPieces(SquareState.BLACK),
                   is(2));
        assertThat("White player has two discs in the initial board configuration.",
                   BoardFixtures.INITIAL.countPieces(SquareState.WHITE),
                   is(2));
        assertThat("There are sixty empty squares in the initial board configuration.",
                   BoardFixtures.INITIAL.countPieces(SquareState.EMPTY),
                   is(60));
        assertThat("There are no outer squares in any board configuration.",
                   BoardFixtures.INITIAL.countPieces(SquareState.OUTER),
                   is(0));
    }

    /**
     * Tests the {@code emptyBoard()} factory.
     * <p>
     * The test runs two types of assertion.
     * The first relates to the {@code equals(Object)} method and checks that
     * the returned board is equal to {@code BoardFixtures.EMPTY}.
     * The second relates on the {@code get(Square)} method and checks that all
     * the squares of the returned board have a {@code SquereState.EMPTY} value.
     *
     * @see Board#emptyBoard()
     * @see BoardFixtures#EMPTY
     * @see Board#equals(Object)
     * @see Board#get(Square)
     */
    @Test
    public final void testEmptyBoard() {
        assertThat("Board.emptyBoard() must return a board being equal to BoardFixtures.EMPTY.",
                   Board.emptyBoard(),
                   is(BoardFixtures.EMPTY));

        Board empty = Board.emptyBoard();
        for (Square square : Square.values()) {
            assertThat("Each square state returned by the get method iterating on"
                       + " the squares of a board returned by Board.emptyBoard()"
                       + " must return a SquareState.EMPTY value.",
                       empty.get(square),
                       is(SquareState.EMPTY));
        }
    }

    /**
     * Tests the {@code equals(Object)} method when the objects are different.
     * <p>
     * The test runs three assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL is not BoardFixtures.NULL}</li>
     *   <li>{@code BoardFixtures.INITIAL is not new Object()}</li>
     *   <li>{@code BoardFixtures.INITIAL is not BoardFixtures.FIRST_MOVE_D3}</li>
     * </ul>
     *
     * @see Board#equals(Object)
     */
    @Test
    public final void testEquals_whenAreDifferent() {
        assertThat("BoardFixtures.INITIAL must not be equal to BoardFixtures.NULL.",
                   BoardFixtures.INITIAL,
                   is(not(BoardFixtures.NULL)));
        assertThat("BoardFixtures.INITIAL must not be equal to a new Object().",
                   BoardFixtures.INITIAL,
                   is(not(new Object())));
        assertThat("BoardFixtures.INITIAL must not be equal to BoardFixtures.FIRST_MOVE_D3.",
                   BoardFixtures.INITIAL,
                   is(not(BoardFixtures.FIRST_MOVE_D3)));
    }


    /**
     * Tests the {@code equals(Object)} method when the two objects are the same.
     * <p>
     * The test runs three assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL is BoardFixtures.INITIAL}</li>
     *   <li>{@code BoardFixtures.EQL_TEST_A is EQL_TEST_A}</li>
     *   <li>{@code BoardFixtures.EQL_TEST_B is EQL_TEST_B}</li>
     * </ul>
     *
     * @see Board#equals(Object)
     */
    @Test
    public final void testEquals_whenAreTheSameObject() {
        assertThat("BoardFixtures.INITIAL must be equal to BoardFixtures.INITIAL.",
                   BoardFixtures.INITIAL,
                   is(BoardFixtures.INITIAL));
        assertThat("BoardFixtures.EQL_TEST_A must be equal to BoardFixtures.EQL_TEST_A.",
                   BoardFixtures.EQL_TEST_A,
                   is(BoardFixtures.EQL_TEST_A));
        assertThat("BoardFixtures.EQL_TEST_B must be equal to BoardFixtures.EQL_TEST_B.",
                   BoardFixtures.EQL_TEST_B,
                   is(BoardFixtures.EQL_TEST_B));
    }

    /**
     * Tests the {@code equals(Object)} method.
     * <p>
     * The test runs six assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL is Board.initialBoard()}</li>
     *   <li>{@code Board.initialBoard() is BoardFixtures.INITIAL}</li>
     *   <li>{@code BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_A)} is true.</li>
     *   <li>{@code BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_B)} is true.</li>
     *   <li>{@code BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_B)} is true.</li>
     *   <li>{@code BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_A)} is true.</li>
     * </ul>
     *
     * @see Board#equals(Object)
     * @see BoardFixtures#EQL_TEST_A
     * @see BoardFixtures#EQL_TEST_B
     */
    @Test
    public final void testEquals_whenAreNotTheSameObject_butAreEqual() {
        assertThat("BoardFixtures.INITIAL must be equal to Board.initialBoard().",
                   BoardFixtures.INITIAL,
                   is(Board.initialBoard()));
        assertThat("Board.initialBoard() must be equal to BoardFixtures.INITIAL.",
                   Board.initialBoard(),
                   is(BoardFixtures.INITIAL));
        assertTrue("BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_A)"
                   + " must return true.",
                   BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_A));
        assertTrue("BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_B)"
                   + " must return true.",
                   BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_B));
        assertTrue("BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_B)"
                   + " must return true.",
                   BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_B));
        assertTrue("BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_A)"
                   + " must return true.",
                   BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_A));
    }

    /**
     * Tests the {@code get(Square)} method.
     * <p>
     * The test runs four assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.B3)} is {@code SquareState.BLACK}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.B4)} is {@code SquareState.WHITE}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.NULL)} is {@code SquareState.OUTER}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.A1)} is {@code SquareState.EMPTY}</li>
     * </ul>
     *
     * @see Board#get(Square)
     * @see BoardFixtures#EARLY_GAME_C_12_MOVES
     */
    @Test
    public final void testGet() {
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.B3)"
                   + " must be SquareState.BLACK.",
                   BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.B3),
                   is(SquareState.BLACK));
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.B4)"
                   + " must be SquareState.WHITE.",
                   BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.B4),
                   is(SquareState.WHITE));
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.NULL)"
                   + " must be SquareState.OUTER.",
                   BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.NULL),
                   is(SquareState.OUTER));
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.A1)"
                   + " must be SquareState.EMPTY.",
                   BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.A1),
                   is(SquareState.EMPTY));
    }

    /**
     * Tests the {@code hasAnyLegalMove(Player)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see Board#hasAnyLegalMove(Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testHasAnyLegalMove_boundaryConditions_checkNullParameter_player() {
        new BoardBuilder().build()
            .hasAnyLegalMove(Player.NULL);
    }

    /**
     * Tests the {@code hasAnyLegalMove(Player)} method.
     * <p>
     * The test runs five assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.hasAnyLegalMove(Player.BLACK)} is {@code true}</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(Player.BLACK)} is {@code false}</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(Player.WHITE)} is {@code true}</li>
     *   <li>{@code BoardFixtures.FINAL_B37_W27.hasAnyLegalMove(Player.WHITE)} is {@code false}</li>
     *   <li>{@code BoardFixtures.FINAL_B37_W27.hasAnyLegalMove(Player.BLACK)} is {@code false}</li>
     * </ul>
     *
     * @see Board#hasAnyLegalMove(Player)
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     * @see BoardFixtures#FINAL_B37_W27
     */
    @Test
    public final void testHasAnyLegalMove() {
        assertThat("BoardFixtures.INITIAL.hasAnyLegalMove(Player.BLACK)"
                   + " must be true.",
                   BoardFixtures.INITIAL.hasAnyLegalMove(Player.BLACK),
                   is(true));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(Player.BLACK)"
                    + " must be false.",
                   BoardFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(Player.BLACK),
                    is(false));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(Player.WHITE)"
                   + " must be true",
                   BoardFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(Player.WHITE),
                   is(true));
        assertThat("BoardFixtures.FINAL_B37_W27.hasAnyLegalMove(Player.WHITE)"
                    + " must be false.",
                   BoardFixtures.FINAL_B37_W27.hasAnyLegalMove(Player.WHITE),
                   is(false));
        assertThat("BoardFixtures.FINAL_B37_W27.hasAnyLegalMove(Player.BLACK)"
                    + " must be false.",
                   BoardFixtures.FINAL_B37_W27.hasAnyLegalMove(Player.BLACK),
                   is(false));
    }

    /**
     * Tests the {@code hasAnyPlayerAnyLegalMove()} method.
     * <p>
     * The test runs four assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.EMPTY.hasAnyPlayerAnyLegalMove()} is {@code false}</li>
     *   <li>{@code BoardFixtures.INITIAL.hasAnyPlayerAnyLegalMove()} is {@code true}</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.hasAnyPlayerAnyLegalMove()} is {@code true}</li>
     *   <li>{@code BoardFixtures.FINAL_B37_W27.hasAnyPlayerAnyLegalMove()} is {@code false}</li>
     * </ul>
     *
     * @see Board#hasAnyPlayerAnyLegalMove()
     * @see BoardFixtures#EMPTY
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     * @see BoardFixtures#FINAL_B37_W27
     */
    @Test
    public final void testHasAnyPlayerAnyLegalMove() {
        assertThat("BoardFixtures.EMPTY.hasAnyPlayerAnyLegalMove()"
                   + " must be false.",
                   BoardFixtures.EMPTY.hasAnyPlayerAnyLegalMove(),
                   is(false));
        assertThat("BoardFixtures.INITIAL.hasAnyPlayerAnyLegalMove()"
                   + " must be true.",
                   BoardFixtures.INITIAL.hasAnyPlayerAnyLegalMove(),
                   is(true));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.hasAnyPlayerAnyLegalMove()"
                   + " must be true.",
                   BoardFixtures.BLACK_HAS_TO_PASS.hasAnyPlayerAnyLegalMove(),
                   is(true));
        assertThat("BoardFixtures.FINAL_B37_W27.hasAnyPlayerAnyLegalMove()"
                    + " must be false.",
                    BoardFixtures.FINAL_B37_W27.hasAnyPlayerAnyLegalMove(),
                    is(false));
    }

    /**
     * Tests the {@code hashCode()} method.
     * <p>
     * The test runs nine assertions on different boards. The test verify that calling twice the method on
     * a given board the returned value is the same.
     *
     * @see Board#hashCode()
     */
    @Test
    public final void testHashCode_isConsistentWhenCalledMoreThanOnce() {
        utilHashCode(BoardFixtures.INITIAL, "BoardFixtures.INITIAL");
        utilHashCode(BoardFixtures.EMPTY, "BoardFixtures.EMPTY");
        utilHashCode(BoardFixtures.BLACK_HAS_TO_PASS, "BoardFixtures.BLACK_HAS_TO_PASS");
        utilHashCode(BoardFixtures.FINAL_B37_W27, "BoardFixtures.FINAL_B37_W27");
        utilHashCode(BoardFixtures.FIRST_MOVE_D3, "BoardFixtures.FIRST_MOVE_D3");
        utilHashCode(BoardFixtures.EARLY_GAME_B_9_MOVES, "BoardFixtures.EARLY_GAME_B_9_MOVES");
        utilHashCode(BoardFixtures.EARLY_GAME_C_12_MOVES, "BoardFixtures.EARLY_GAME_C_12_MOVES");
        utilHashCode(BoardFixtures.EQL_TEST_A, "BoardFixtures.EQL_TEST_A");
        utilHashCode(BoardFixtures.EQL_TEST_B, "BoardFixtures.EQL_TEST_B");
    }

    /**
     * Util method used by testHashCode_isConsistentWhenCalledMoreThanOnce.
     *
     * @param board  the board
     * @param symbol the board literal
     */
    private void utilHashCode(final Board board, final String symbol) {
        assertEquals(symbol + " must have a consistent hash code.", board.hashCode(), board.hashCode());
    }

    /**
     * Tests the {@code hashCode()} method.
     * <p>
     * The test runs two assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.EQL_TEST_A.hashCode()} is equal to {@code BoardFixtures.EQL_TEST_B.hashCode()}</li>
     *   <li>{@code BoardFixtures.EQL_TEST_B.hashCode()} is equal to {@code BoardFixtures.EQL_TEST_A.hashCode()}</li>
     * </ul>
     *
     * @see Board#hashCode()
     * @see BoardFixtures#EQL_TEST_A
     * @see BoardFixtures#EQL_TEST_B
     */
    @Test
    public final void testHashCode_isConsistentWhenCalledOnEqualObjects() {
        assertEquals("BoardFixtures.EQL_TEST_A and BoardFixtures.EQL_TEST_B must have the same hash.",
                     BoardFixtures.EQL_TEST_A.hashCode(), BoardFixtures.EQL_TEST_B.hashCode());
        assertEquals("BoardFixtures.EQL_TEST_B and BoardFixtures.EQL_TEST_A must have the same hash.",
                     BoardFixtures.EQL_TEST_B.hashCode(), BoardFixtures.EQL_TEST_A.hashCode());
    }

    /**
     * Tests the {@code initialBoard()} method.
     * <p>
     * The test runs two kind of assertions:
     * <p>
     * <ul>
     *   <li>{@code Board.initialBoard()} is equal to {@code BoardFixtures.INITIAL}</li>
     *   <li>For each board square the state is checked against the expected one</li>
     * </ul>
     *
     * @see Board#initialBoard()
     * @see BoardFixtures#INITIAL
     */
    @Test
    public final void testInitialBoard() {
        assertEquals("Board.initialBoard() must be equal to BoardFixtures.INITIAL.",
                     BoardFixtures.INITIAL, Board.initialBoard());
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
            assertEquals("Square:" + actual + " must be equal to " + expected + ".",
                         expected, actual);
        }
    }

    /**
     * Tests the {@code isLegal(Square, Player)} method when parameter
     * {@code move} is {@code null}.
     *
     * @see Board#isLegal(Square, Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testIsLegal_boundaryConditions_checkNullParameter_move() {
        new BoardBuilder().build()
            .isLegal(Square.NULL, Player.AN_INSTANCE);
    }

    /**
     * Tests the {@code isLegal(Square, Player)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see Board#isLegal(Square, Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testIsLegal_boundaryConditions_checkNullParameter_player() {
        new BoardBuilder().build()
            .isLegal(Square.AN_INSTANCE, Player.NULL);
    }

    /**
     * Tests the {@code isLegal(Square, Player)} method when the target square
     * for the move is already occupied.
     * <p>
     * The test runs two assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.isLegal(Square.D4, Player.BLACK)} is false.</li>
     *   <li>{@code BoardFixtures.INITIAL.isLegal(Square.D4, Player.WHITE)} is false.</li>
     * </ul>
     *
     * @see Board#isLegal(Square, Player)
     * @see BoardFixtures#INITIAL
     */
    @Test
    public final void testIsLegal_whenAlreadyOccupied() {
        assertThat("BoardFixtures.INITIAL.isLegal(Square.D4, Player.BLACK)"
                   + " must be false.",
                   BoardFixtures.INITIAL.isLegal(Square.D4, Player.BLACK),
                   is(false));
        assertThat("BoardFixtures.INITIAL.isLegal(Square.D4, Player.WHITE)"
                   + " must be false.",
                   BoardFixtures.INITIAL.isLegal(Square.D4, Player.WHITE),
                   is(false));
    }

    /**
     * Tests the {@code isLegal(Square, Player)} method when no disc would be flipped.
     * <p>
     * The test runs four assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.isLegal(Square.A1, Player.BLACK)} is false.</li>
     *   <li>{@code BoardFixtures.INITIAL.isLegal(Square.A1, Player.WHITE)} is false.</li>
     *   <li>{@code BoardFixtures.INITIAL.isLegal(Square.E3, Player.BLACK)} is false.</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.isLegal(Square.H7, Player.BLACK)} is false.</li>
     * </ul>
     *
     * @see Board#isLegal(Square, Player)
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     */
    @Test
    public final void testIsLegal_whenNoDiscWouldBeFlipped() {
        assertThat("BoardFixtures.INITIAL.isLegal(Square.A1, Player.BLACK)"
                   + " must return false.",
                   BoardFixtures.INITIAL.isLegal(Square.A1, Player.BLACK),
                    is(false));
        assertThat("BoardFixtures.INITIAL.isLegal(Square.A1, Player.WHITE)"
                   + " must return false.",
                   BoardFixtures.INITIAL.isLegal(Square.A1, Player.WHITE),
                    is(false));
        assertThat("BoardFixtures.INITIAL.isLegal(Square.E3, Player.BLACK)"
                    + " must be false.",
                   BoardFixtures.INITIAL.isLegal(Square.E3, Player.BLACK),
                   is(false));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.isLegal(Square.H7, Player.BLACK)"
                   + " must be false.",
                   BoardFixtures.BLACK_HAS_TO_PASS.isLegal(Square.H7, Player.BLACK),
                   is(false));
    }

    /**
     * Tests the {@code isLegal(Square, Player)} method when the move is legal.
     * <p>
     * The test runs four assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.isLegal(Square.D3, Player.BLACK)} is true.</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.isLegal(Square.H7, Player.WHITE)} is true.</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_B_9_MOVES.isLegal(Square.C3, Player.WHITE)} is true.</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_B_9_MOVES.isLegal(Square.C6, Player.WHITE)} is true.</li>
     * </ul>
     *
     * @see Board#isLegal(Square, Player)
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     * @see BoardFixtures#EARLY_GAME_B_9_MOVES
     */
    @Test
    public final void testIsLegal_whenItIs() {
        assertThat("BoardFixtures.INITIAL.isLegal(Square.D3, Player.BLACK)"
                   + " must be true.",
                   BoardFixtures.INITIAL.isLegal(Square.D3, Player.BLACK),
                   is(true));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.isLegal(Square.H7, Player.WHITE)"
                   + " must be true.",
                   BoardFixtures.BLACK_HAS_TO_PASS.isLegal(Square.H7, Player.WHITE),
                   is(true));
        assertThat("BoardFixtures.EARLY_GAME_B_9_MOVES.isLegal(Square.C3, Player.WHITE)"
                   + " must be true.",
                   BoardFixtures.EARLY_GAME_B_9_MOVES.isLegal(Square.C3, Player.WHITE),
                   is(true));
        assertThat("BoardFixtures.EARLY_GAME_B_9_MOVES.isLegal(Square.C6, Player.WHITE)"
                   + " must be true.",
                   BoardFixtures.EARLY_GAME_B_9_MOVES.isLegal(Square.C6, Player.WHITE),
                   is(true));

    }

    /**
     * Tests the {@code legalMoves(Player)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see Board#legalMoves(Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testLegalMoves_boundaryConditions_checkNullParameter_player() {
        new BoardBuilder().build()
            .legalMoves(Player.NULL);
    }

    /**
     * Tests the {@code legalMoves(Player)} method.
     * <p>
     * The test runs the following assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.legalMoves(Player.BLACK)} must return {@code [D3 C4 F5 E6]}</li>
     *   <li>{@code BoardFixtures.FIRST_MOVE_D3.legalMoves(Player.WHITE)} must return {@code [C3 E3 C5]}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_C_12_MOVES.legalMoves(Player.BLACK)} must return {@code [H2 A4 C4 G4 A5 F5 B6 E6 G7]}</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.legalMoves(Player.BLACK)} must return {@code []}</li>
     *   <li>{@code BoardFixtures.MINIMAX_TEST_CASE_A.legalMoves(Player.WHITE)} must return {@code [A3 C4 G4 E5]}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_B_9_MOVES.legalMoves(Player.WHITE)} must return {@code [C3 C6]}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_BC3_10_MOVES.legalMoves(Player.BLACK)} must return {@code [B2 C2 D2 F2 G2 C4 G4]}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_BC6_10_MOVES.legalMoves(Player.BLACK)} must return {@code [H3 C4 F4 G4 C5 F5 D6]}</li>
     * </ul>
     *
     * @see Board#legalMoves(Player)
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#FIRST_MOVE_D3
     * @see BoardFixtures#EARLY_GAME_C_12_MOVES
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     * @see BoardFixtures#MINIMAX_TEST_CASE_A
     * @see BoardFixtures#EARLY_GAME_B_9_MOVES
     * @see BoardFixtures#EARLY_GAME_BC3_10_MOVES
     * @see BoardFixtures#EARLY_GAME_BC6_10_MOVES
     */
    @Test
    public final void testLegalMoves() {
        assertThat("BoardFixtures.INITIAL.legalMoves(Player.BLACK)"
                   + " must return [D3 C4 F5 E6]",
                   BoardFixtures.INITIAL.legalMoves(Player.BLACK),
                   is(Arrays.asList(Square.D3, Square.C4, Square.F5, Square.E6)));
        assertThat("BoardFixtures.FIRST_MOVE_D3.legalMoves(Player.WHITE)"
                   + " must return [C3 E3 C5]",
                   BoardFixtures.FIRST_MOVE_D3.legalMoves(Player.WHITE),
                   is(Arrays.asList(Square.C3, Square.E3, Square.C5)));
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.legalMoves(Player.BLACK)"
                   + " must return [H2 A4 C4 G4 A5 F5 B6 E6 G7]",
                   BoardFixtures.EARLY_GAME_C_12_MOVES.legalMoves(Player.BLACK),
                   is(Arrays.asList(Square.H2, Square.A4, Square.C4, 
                                    Square.G4, Square.A5, Square.F5, 
                                    Square.B6, Square.E6, Square.G7)));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.legalMoves(Player.BLACK)"
                   + " must return []",
                   BoardFixtures.BLACK_HAS_TO_PASS.legalMoves(Player.BLACK)
                   .isEmpty(),
                   is(true));
        assertThat("BoardFixtures.MINIMAX_TEST_CASE_A.legalMoves(Player.WHITE)"
                   + " must return [A3 C4 G4 E5]",
                   BoardFixtures.MINIMAX_TEST_CASE_A.legalMoves(Player.WHITE),
                   is(Arrays.asList(Square.A3, Square.C4, Square.G4, Square.E5)));
        assertThat("BoardFixtures.EARLY_GAME_B_9_MOVES.legalMoves(Player.WHITE)"
                   + " must return [C3 C6]",
                   BoardFixtures.EARLY_GAME_B_9_MOVES.legalMoves(Player.WHITE),
                   is(Arrays.asList(Square.C3, Square.C6)));
        assertThat("BoardFixtures.EARLY_GAME_BC3_10_MOVES.legalMoves(Player.BLACK)"
                   + " must return [B2 C2 D2 F2 G2 C4 G4]",
                   BoardFixtures.EARLY_GAME_BC3_10_MOVES.legalMoves(Player.BLACK),
                   is(Arrays.asList(Square.B2, Square.C2, Square.D2,
                                    Square.F2, Square.G2, Square.C4,
                                    Square.G4)));
        assertThat("BoardFixtures.EARLY_GAME_BC6_10_MOVES.legalMoves(Player.BLACK)"
                   + " must return [H3 C4 F4 G4 C5 F5 D6]",
                   BoardFixtures.EARLY_GAME_BC6_10_MOVES.legalMoves(Player.BLACK),
                   is(Arrays.asList(Square.H3, Square.C4, Square.F4,
                                            Square.G4, Square.C5, Square.F5,
                                            Square.D6)));
    }

    /**
     * Tests the mechanics of the makeMove() method.
     */
    @Test
    public final void testMakeMove() {

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
    public final void testNextToPlay() {
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
    public final void testPrintBoard() {
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
    public final void testPrintBoardWithCount() {
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
    public final void testPrintCount() {
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
    public final void testValueOf() {

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
     * Tests the {@code findBracketingPiece(Square, Player, Direction)} private method.
     * <p>
     * {@code wouldFlip(Square, Player, Direction)} is a "private" method in Board class.
     * The {@code wouldFlip(Square, Player, Direction)} method is called by only one "client method":
     * <ul>
     *   <li>{@code wouldFlip(Square, Player, Direction)}</li>
     * </ul>
     * <p>
     * that is defined in the board class itself.
     * The test applies reflection to access the private method.
     * <p>
     * The test run the following checks:
     * <ul>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.findBracketingPiece(Square.H7, Player.WHITE, Direction.W)} must return {@code Square.C7}.</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.findBracketingPiece(Square.H7, Player.WHITE, Direction.NW)} must return {@code Square.F5}.</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.findBracketingPiece(Square.H7, Player.WHITE, Direction.SW)} must return {@code Square.NULL}.</li>
     * </ul>
     *
     * @throws NoSuchMethodException     if findBracketingPiece method is not found
     * @throws IllegalAccessException    if findBracketingPiece method invocation done by reflection rises a security exception
     * @throws InvocationTargetException if findBracketingPiece method invocation rises execution exceptions
     */
    @Test
    public final void testFindBracketingPiece()
        throws NoSuchMethodException,
               IllegalAccessException,
               InvocationTargetException {

        assertThat("Given that"
                   + " board is BoardFixtures.BLACK_HAS_TO_PASS,"
                   + " player is Player.WHITE,"
                   + " move is Square.H7,"
                   + " direction is Direction.W"
                   + " findBracketingPiece must return Square.C7",
                   utilFindBracketingPiece(BoardFixtures.BLACK_HAS_TO_PASS,
                                           Player.WHITE,
                                           Square.H7,
                                           Direction.W),
                   is(Square.C7));

        assertThat("Given that"
                   + " board is BoardFixtures.BLACK_HAS_TO_PASS,"
                   + " player is Player.WHITE,"
                   + " move is Square.H7,"
                   + " direction is Direction.NW"
                   + " findBracketingPiece must return Square.F5",
                   utilFindBracketingPiece(BoardFixtures.BLACK_HAS_TO_PASS,
                                           Player.WHITE,
                                           Square.H7,
                                           Direction.NW),
                   is(Square.F5));

        assertThat("Given that"
                   + " board is BoardFixtures.BLACK_HAS_TO_PASS,"
                   + " player is Player.WHITE,"
                   + " move is Square.H7,"
                   + " direction is Direction.SW"
                   + " findBracketingPiece must return Square.NULL",
                   utilFindBracketingPiece(BoardFixtures.BLACK_HAS_TO_PASS,
                                           Player.WHITE,
                                           Square.H7,
                                           Direction.SW),
                   is(Square.NULL));
    }

    /**
     * Util method used by testFindBracketingPiece.
     *
     * @param board  the board
     * @param player the player that has to move
     * @param move   the player's move
     * @param dir    the board direction
     * @return       the bracketing square
     *
     * @throws NoSuchMethodException     if findBracketingPiece method is not found
     * @throws IllegalAccessException    if findBracketingPiece method invocation done by reflection rises a security exception
     * @throws InvocationTargetException if findBracketingPiece method invocation rises execution exceptions
     */
    private Square utilFindBracketingPiece(final Board board, final Player player, final Square move, final Direction dir)
        throws NoSuchMethodException,
               IllegalAccessException,
               InvocationTargetException {

        Method method = Board.class.getDeclaredMethod("findBracketingPiece", Square.class, Player.class, Direction.class);
        method.setAccessible(true);

        Square firstStepInTheGivenDirection = move.neighbors().get(dir);
        Square bracketing = (Square) method.invoke(board, firstStepInTheGivenDirection, player, dir);

        return bracketing;
    }

    /**
     * Tests the {@code wouldFlip(Square, Player, Direction)} private method.
     * <p>
     * {@code wouldFlip(Square, Player, Direction)} is a "private" method in Board class.
     * The {@code wouldFlip(Square, Player, Direction)} method is called by only two "client methods":
     * <ul>
     *   <li>{@code makeMove(Square, Player)}</li>
     *   <li>{@code isLegal(Square, Player)}</li>
     * </ul>
     * <p>
     * that are both defined in the board class itself.
     * The test applies reflection to access the private method.
     * <p>
     * The test run the following checks:
     * <ul>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.wouldFlip(Square.H7, Player.WHITE, Direction.W)} must return {@code Square.C7}.</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.wouldFlip(Square.H7, Player.WHITE, Direction.S)} must return {@code Square.NULL}.</li>
     * </ul>
     *
     * @throws NoSuchMethodException     if wouldFlip method is not found
     * @throws IllegalAccessException    if wouldFlip method invocation done by reflection rises a security exception
     * @throws InvocationTargetException if wouldFlip method invocation rises execution exceptions
     *
     * @see Board#makeMove(Square, Player)
     * @see Board#isLegal(Square, Player)
     */
    @Test
    public final void testWouldFlip()
        throws NoSuchMethodException,
               IllegalAccessException,
               InvocationTargetException {

        Method method = Board.class.getDeclaredMethod("wouldFlip", Square.class, Player.class, Direction.class);
        method.setAccessible(true);

        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.wouldFlip(Square.H7, Player.WHITE, Direction.W)"
                   + " must return Square.C7.",
                   (Square) method.invoke(BoardFixtures.BLACK_HAS_TO_PASS, Square.H7, Player.WHITE, Direction.W),
                   is(Square.C7));

        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.wouldFlip(Square.H7, Player.WHITE, Direction.S)"
                   + " must return Square.NULL.",
                   (Square) method.invoke(BoardFixtures.BLACK_HAS_TO_PASS, Square.H7, Player.WHITE, Direction.S),
                   is(Square.NULL));
    }

}
