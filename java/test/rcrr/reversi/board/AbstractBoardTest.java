/*
 *  AbstractBoardTest.java
 *
 *  Copyright (c) 2012 Roberto Corradini. All rights reserved.
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

package rcrr.reversi.board;

import java.util.Map;
import java.util.HashMap;
import java.util.EnumMap;
import java.util.Arrays;

import java.io.IOException;
import java.io.ByteArrayOutputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.ByteArrayInputStream;
import java.io.ObjectInput;
import java.io.ObjectInputStream;

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for any {@code Board} class implementation that derives from the {@code AbstractBoard} abstract class.
 */
public class AbstractBoardTest {

    /**
     * Returns a string object reporting the content of the {@code bytes} parameter.
     *
     * @param bytes a byte array to transform into a printable string
     * @return      a string describing the {@code bytes} parameter
     */
    private static final String byteArrayToString(final byte[] bytes) {
        final StringBuffer sb = new StringBuffer();
        int index = 0;
        for (byte b : bytes) {
            sb.append(String.format("[%3d]=%02X\n", index, b));
            index++;
        }
        return sb.toString();
    }

    /**
     * Returns a byte array obtained serializing the {@code board} parameter.
     *
     * @param board the board to transform
     * @return      the byte array holding the serialized board
     */
    private static final byte[] serializeToByteArray(final Board board) {
        byte[] results = null;
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            ObjectOutput out = new ObjectOutputStream(bos);   
            out.writeObject(board);
            results = bos.toByteArray();
        } catch (IOException ioe) {
            throw new RuntimeException(ioe);
        }
        return results;
    }

    /**
     * Returns a new board obtained deserializing the {@code bytes} parameter.
     *
     * @param bytes the byte array holding the serialized board
     * @return      the new board
     */
    private static final Board deserializeFromByteArray(final byte[] bytes) {
        Board result = null;
        try {
            ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
            ObjectInput in = new ObjectInputStream(bis);
            result = (Board) in.readObject();
        } catch (IOException ioe) {
            throw new RuntimeException(ioe);
        } catch (ClassNotFoundException cnfe) {
            throw new RuntimeException(cnfe);
        }
        return result;
    }

    /**
     * Returns a new board being a copy of the {@code board} parameter.
     * The method apply a serialization and a deserialization of the passed
     * board.
     *
     * @param board the board to be copied
     * @return      the new copied board
     */
    private static final Board serializationRoundTrip(final Board board) {
        return deserializeFromByteArray(serializeToByteArray(board));
    }

    /** Class constructor. */
    public AbstractBoardTest() { }

    /**
     * Tests the serialization process.
     *
     * @see AbstractBoard
     * @see BoardFixtures#AN_INSTANCE
     */
    @Test
    public final void testSerialization() {

        assertThat("serializationRoundTrip(BoardFixtures.AN_INSTANCE) is"
                   + " equal to BoardFixtures.AN_INSTANCE.",
                   serializationRoundTrip(new BoardBuilder(BoardFixtures.AN_INSTANCE).build()),
                   is(new BoardBuilder(BoardFixtures.AN_INSTANCE).build()));

    }

    /**
     * Tests the {@code countDifference(Player)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see AbstractBoard#countDifference(Player)
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
     *   <li>{@code BoardFixtures.EARLY_GAME_C_12_MOVES.countDifference(Player.BLACK)}
     *       must return a count equal to -2.</li>
     * </ul>
     *
     * @see AbstractBoard#countDifference(Player)
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#FINAL_B37_W27
     * @see BoardFixtures#EARLY_GAME_C_12_MOVES
     */
    @Test
    public final void testCountDifference() {
        assertThat("BoardFixtures.INITIAL.countDifference(Player.BLACK)"
                   + " must return a count equal to 0.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().countDifference(Player.BLACK),
                   is(0));
        assertThat("BoardFixtures.INITIAL.countDifference(Player.BLACK)"
                   + " must return a count equal to +10.",
                   new BoardBuilder(BoardFixtures.FINAL_B37_W27).build().countDifference(Player.BLACK),
                   is(+10));
        assertThat("BoardFixtures.INITIAL.countDifference(Player.WHITE)"
                   + " must return a count equal to -10.",
                   new BoardBuilder(BoardFixtures.FINAL_B37_W27).build().countDifference(Player.WHITE),
                   is(-10));
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.countDifference(Player.BLACK)"
                   + " must return a count equal to -2.",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_C_12_MOVES).build().countDifference(Player.BLACK),
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
     * @see AbstractBoard#countPieces(SquareState)
     * @see BoardFixtures#INITIAL
     */
    @Test
    public final void testCountPieces() {
        assertThat("Black player has two discs in the initial board configuration.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().countPieces(SquareState.BLACK),
                   is(2));
        assertThat("White player has two discs in the initial board configuration.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().countPieces(SquareState.WHITE),
                   is(2));
        assertThat("There are sixty empty squares in the initial board configuration.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().countPieces(SquareState.EMPTY),
                   is(60));
        assertThat("There are no outer squares in any board configuration.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().countPieces(SquareState.OUTER),
                   is(0));
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
     * @see AbstractBoard#equals(Object)
     */
    @Test
    public final void testEquals_whenAreDifferent() {
        assertThat("BoardFixtures.INITIAL must not be equal to BoardFixtures.NULL.",
                   new BoardBuilder(BoardFixtures.INITIAL).build(),
                   is(not(BoardFixtures.NULL)));
        assertThat("BoardFixtures.INITIAL must not be equal to a new Object().",
                   new BoardBuilder(BoardFixtures.INITIAL).build(),
                   is(not(new Object())));
        assertThat("BoardFixtures.INITIAL must not be equal to BoardFixtures.FIRST_MOVE_D3.",
                   new BoardBuilder(BoardFixtures.INITIAL).build(),
                   is(not(new BoardBuilder(BoardFixtures.FIRST_MOVE_D3).build())));
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
     * @see AbstractBoard#equals(Object)
     */
    @Test
    public final void testEquals_whenAreTheSameObject() {
        assertThat("BoardFixtures.INITIAL must be equal to BoardFixtures.INITIAL.",
                   new BoardBuilder(BoardFixtures.INITIAL).build(),
                   is(new BoardBuilder(BoardFixtures.INITIAL).build()));
        assertThat("BoardFixtures.EQL_TEST_A must be equal to BoardFixtures.EQL_TEST_A.",
                   new BoardBuilder(BoardFixtures.EQL_TEST_A).build(),
                   is(new BoardBuilder(BoardFixtures.EQL_TEST_A).build()));
        assertThat("BoardFixtures.EQL_TEST_B must be equal to BoardFixtures.EQL_TEST_B.",
                   new BoardBuilder(BoardFixtures.EQL_TEST_B).build(),
                   is(new BoardBuilder(BoardFixtures.EQL_TEST_B).build()));
    }

    /**
     * Tests the {@code equals(Object)} method.
     * <p>
     * The test runs six assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL is BoardFactoryHolder.getInstance().boardFactory().initialBoard()}</li>
     *   <li>{@code BoardFactoryHolder.getInstance().boardFactory().initialBoard() is BoardFixtures.INITIAL}</li>
     *   <li>{@code BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_A)} is true.</li>
     *   <li>{@code BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_B)} is true.</li>
     *   <li>{@code BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_B)} is true.</li>
     *   <li>{@code BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_A)} is true.</li>
     * </ul>
     *
     * @see AbstractBoard#equals(Object)
     * @see BoardFixtures#EQL_TEST_A
     * @see BoardFixtures#EQL_TEST_B
     */
    @Test
    public final void testEquals_whenAreNotTheSameObject_andAreEqual() {

        assertThat("BoardFixtures.INITIAL must be equal to BoardFactoryHolder.getInstance().boardFactory().initialBoard().",
                   new BoardBuilder(BoardFixtures.INITIAL).build(),
                   is(BoardFactoryHolder.getInstance().boardFactory().initialBoard()));
        assertThat("BoardFactoryHolder.getInstance().boardFactory().initialBoard() must be equal to BoardFixtures.INITIAL.",
                   BoardFactoryHolder.getInstance().boardFactory().initialBoard(),
                   is(new BoardBuilder(BoardFixtures.INITIAL).build()));

        /** Checks that the two object are really not the same. */
        assertFalse("BoardFixtures.EQL_TEST_A and BoardFixtures.EQL_TEST_B"
                    + " must be two different object, otherwise the test is fouled.",
                    new BoardBuilder(BoardFixtures.EQL_TEST_A).build() == new BoardBuilder(BoardFixtures.EQL_TEST_B).build());

        assertTrue("BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_A)"
                   + " must return true.",
                   new BoardBuilder(BoardFixtures.EQL_TEST_A).build().equals(BoardFixtures.EQL_TEST_A));
        assertTrue("BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_B)"
                   + " must return true.",
                   new BoardBuilder(BoardFixtures.EQL_TEST_B).build().equals(BoardFixtures.EQL_TEST_B));
        assertTrue("BoardFixtures.EQL_TEST_A.equals(BoardFixtures.EQL_TEST_B)"
                   + " must return true.",
                   new BoardBuilder(BoardFixtures.EQL_TEST_A).build().equals(BoardFixtures.EQL_TEST_B));
        assertTrue("BoardFixtures.EQL_TEST_B.equals(BoardFixtures.EQL_TEST_A)"
                   + " must return true.",
                   new BoardBuilder(BoardFixtures.EQL_TEST_B).build().equals(BoardFixtures.EQL_TEST_A));
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
     * @see AbstractBoard#get(Square)
     * @see BoardFixtures#EARLY_GAME_C_12_MOVES
     */
    @Test
    public final void testGet() {
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.B3)"
                   + " must be SquareState.BLACK.",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_C_12_MOVES).build().get(Square.B3),
                   is(SquareState.BLACK));
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.B4)"
                   + " must be SquareState.WHITE.",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_C_12_MOVES).build().get(Square.B4),
                   is(SquareState.WHITE));
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.NULL)"
                   + " must be SquareState.OUTER.",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_C_12_MOVES).build().get(Square.NULL),
                   is(SquareState.OUTER));
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.get(Square.A1)"
                   + " must be SquareState.EMPTY.",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_C_12_MOVES).build().get(Square.A1),
                   is(SquareState.EMPTY));
    }

    /**
     * Tests the {@code hasAnyLegalMove(Player)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see AbstractBoard#hasAnyLegalMove(Player)
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
     * @see AbstractBoard#hasAnyLegalMove(Player)
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     * @see BoardFixtures#FINAL_B37_W27
     */
    @Test
    public final void testHasAnyLegalMove() {
        assertThat("BoardFixtures.INITIAL.hasAnyLegalMove(Player.BLACK)"
                   + " must be true.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().hasAnyLegalMove(Player.BLACK),
                   is(true));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(Player.BLACK)"
                    + " must be false.",
                   new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build().hasAnyLegalMove(Player.BLACK),
                    is(false));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(Player.WHITE)"
                   + " must be true",
                   new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build().hasAnyLegalMove(Player.WHITE),
                   is(true));
        assertThat("BoardFixtures.FINAL_B37_W27.hasAnyLegalMove(Player.WHITE)"
                    + " must be false.",
                   new BoardBuilder(BoardFixtures.FINAL_B37_W27).build().hasAnyLegalMove(Player.WHITE),
                   is(false));
        assertThat("BoardFixtures.FINAL_B37_W27.hasAnyLegalMove(Player.BLACK)"
                    + " must be false.",
                   new BoardBuilder(BoardFixtures.FINAL_B37_W27).build().hasAnyLegalMove(Player.BLACK),
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
     * @see AbstractBoard#hasAnyPlayerAnyLegalMove()
     * @see BoardFixtures#EMPTY
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     * @see BoardFixtures#FINAL_B37_W27
     */
    @Test
    public final void testHasAnyPlayerAnyLegalMove() {
        assertThat("BoardFixtures.EMPTY.hasAnyPlayerAnyLegalMove()"
                   + " must be false.",
                   new BoardBuilder(BoardFixtures.EMPTY).build().hasAnyPlayerAnyLegalMove(),
                   is(false));
        assertThat("BoardFixtures.INITIAL.hasAnyPlayerAnyLegalMove()"
                   + " must be true.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().hasAnyPlayerAnyLegalMove(),
                   is(true));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.hasAnyPlayerAnyLegalMove()"
                   + " must be true.",
                   new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build().hasAnyPlayerAnyLegalMove(),
                   is(true));
        assertThat("BoardFixtures.FINAL_B37_W27.hasAnyPlayerAnyLegalMove()"
                    + " must be false.",
                   new BoardBuilder(BoardFixtures.FINAL_B37_W27).build().hasAnyPlayerAnyLegalMove(),
                    is(false));
    }

    /**
     * Tests the {@code hashCode()} method.
     * <p>
     * The test runs nine assertions on different boards. The test verify that calling twice the method on
     * a given board the returned value is the same.
     *
     * @see AbstractBoard#hashCode()
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
     * Tests the {@code hashCode()} method.
     * <p>
     * The test runs two assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFixtures.EQL_TEST_A.hashCode()} is equal to {@code BoardFixtures.EQL_TEST_B.hashCode()}</li>
     *   <li>{@code BoardFixtures.EQL_TEST_B.hashCode()} is equal to {@code BoardFixtures.EQL_TEST_A.hashCode()}</li>
     * </ul>
     *
     * @see AbstractBoard#hashCode()
     * @see BoardFixtures#EQL_TEST_A
     * @see BoardFixtures#EQL_TEST_B
     */
    @Test
    public final void testHashCode_isConsistentWhenCalledOnEqualObjects() {
        assertEquals("BoardFixtures.EQL_TEST_A and BoardFixtures.EQL_TEST_B must have the same hash.",
                     new BoardBuilder(BoardFixtures.EQL_TEST_A).build().hashCode(),
                     new BoardBuilder(BoardFixtures.EQL_TEST_B).build().hashCode());
    }

    /**
     * Tests the {@code isLegal(Square, Player)} method when parameter
     * {@code move} is {@code null}.
     *
     * @see AbstractBoard#isLegal(Square, Player)
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
     * @see AbstractBoard#isLegal(Square, Player)
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
     * @see AbstractBoard#isLegal(Square, Player)
     * @see BoardFixtures#INITIAL
     */
    @Test
    public final void testIsLegal_whenAlreadyOccupied() {
        assertThat("BoardFixtures.INITIAL.isLegal(Square.D4, Player.BLACK)"
                   + " must be false.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().isLegal(Square.D4, Player.BLACK),
                   is(false));
        assertThat("BoardFixtures.INITIAL.isLegal(Square.D4, Player.WHITE)"
                   + " must be false.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().isLegal(Square.D4, Player.WHITE),
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
     * @see AbstractBoard#isLegal(Square, Player)
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     */
    @Test
    public final void testIsLegal_whenNoDiscWouldBeFlipped() {
        assertThat("BoardFixtures.INITIAL.isLegal(Square.A1, Player.BLACK)"
                   + " must return false.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().isLegal(Square.A1, Player.BLACK),
                    is(false));
        assertThat("BoardFixtures.INITIAL.isLegal(Square.A1, Player.WHITE)"
                   + " must return false.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().isLegal(Square.A1, Player.WHITE),
                    is(false));
        assertThat("BoardFixtures.INITIAL.isLegal(Square.E3, Player.BLACK)"
                    + " must be false.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().isLegal(Square.E3, Player.BLACK),
                   is(false));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.isLegal(Square.H7, Player.BLACK)"
                   + " must be false.",
                   new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build().isLegal(Square.H7, Player.BLACK),
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
     * @see AbstractBoard#isLegal(Square, Player)
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     * @see BoardFixtures#EARLY_GAME_B_9_MOVES
     */
    @Test
    public final void testIsLegal_whenItIs() {
        assertThat("BoardFixtures.INITIAL.isLegal(Square.D3, Player.BLACK)"
                   + " must be true.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().isLegal(Square.D3, Player.BLACK),
                   is(true));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.isLegal(Square.H7, Player.WHITE)"
                   + " must be true.",
                   new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build().isLegal(Square.H7, Player.WHITE),
                   is(true));
        assertThat("BoardFixtures.EARLY_GAME_B_9_MOVES.isLegal(Square.C3, Player.WHITE)"
                   + " must be true.",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_B_9_MOVES).build().isLegal(Square.C3, Player.WHITE),
                   is(true));
        assertThat("BoardFixtures.EARLY_GAME_B_9_MOVES.isLegal(Square.C6, Player.WHITE)"
                   + " must be true.",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_B_9_MOVES).build().isLegal(Square.C6, Player.WHITE),
                   is(true));

    }

    /**
     * Tests the {@code legalMoves(Player)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see AbstractBoard#legalMoves(Player)
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
     *   <li>{@code BoardFixtures.EARLY_GAME_C_12_MOVES.legalMoves(Player.BLACK)}
     *       must return {@code [H2 A4 C4 G4 A5 F5 B6 E6 G7]}</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.legalMoves(Player.BLACK)} must return {@code []}</li>
     *   <li>{@code BoardFixtures.MINIMAX_TEST_CASE_A.legalMoves(Player.WHITE)} must return {@code [A3 C4 G4 E5]}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_B_9_MOVES.legalMoves(Player.WHITE)} must return {@code [C3 C6]}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_BC3_10_MOVES.legalMoves(Player.BLACK)}
     *       must return {@code [B2 C2 D2 F2 G2 C4 G4]}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_BC6_10_MOVES.legalMoves(Player.BLACK)}
     *       must return {@code [H3 C4 F4 G4 C5 F5 D6]}</li>
     * </ul>
     *
     * @see AbstractBoard#legalMoves(Player)
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
                   new BoardBuilder(BoardFixtures.INITIAL).build().legalMoves(Player.BLACK),
                   is(Arrays.asList(Square.D3, Square.C4, Square.F5, Square.E6)));
        assertThat("BoardFixtures.FIRST_MOVE_D3.legalMoves(Player.WHITE)"
                   + " must return [C3 E3 C5]",
                   new BoardBuilder(BoardFixtures.FIRST_MOVE_D3).build().legalMoves(Player.WHITE),
                   is(Arrays.asList(Square.C3, Square.E3, Square.C5)));
        assertThat("BoardFixtures.EARLY_GAME_C_12_MOVES.legalMoves(Player.BLACK)"
                   + " must return [H2 A4 C4 G4 A5 F5 B6 E6 G7]",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_C_12_MOVES).build().legalMoves(Player.BLACK),
                   is(Arrays.asList(Square.H2, Square.A4, Square.C4,
                                    Square.G4, Square.A5, Square.F5,
                                    Square.B6, Square.E6, Square.G7)));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.legalMoves(Player.BLACK)"
                   + " must return []",
                   new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build().legalMoves(Player.BLACK)
                   .isEmpty(),
                   is(true));
        assertThat("BoardFixtures.MINIMAX_TEST_CASE_A.legalMoves(Player.WHITE)"
                   + " must return [A3 C4 G4 E5]",
                   new BoardBuilder(BoardFixtures.MINIMAX_TEST_CASE_A).build().legalMoves(Player.WHITE),
                   is(Arrays.asList(Square.A3, Square.C4, Square.G4, Square.E5)));
        assertThat("BoardFixtures.EARLY_GAME_B_9_MOVES.legalMoves(Player.WHITE)"
                   + " must return [C3 C6]",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_B_9_MOVES).build().legalMoves(Player.WHITE),
                   is(Arrays.asList(Square.C3, Square.C6)));
        assertThat("BoardFixtures.EARLY_GAME_BC3_10_MOVES.legalMoves(Player.BLACK)"
                   + " must return [B2 C2 D2 F2 G2 C4 G4]",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_BC3_10_MOVES).build().legalMoves(Player.BLACK),
                   is(Arrays.asList(Square.B2, Square.C2, Square.D2,
                                    Square.F2, Square.G2, Square.C4,
                                    Square.G4)));
        assertThat("BoardFixtures.EARLY_GAME_BC6_10_MOVES.legalMoves(Player.BLACK)"
                   + " must return [H3 C4 F4 G4 C5 F5 D6]",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_BC6_10_MOVES).build().legalMoves(Player.BLACK),
                   is(Arrays.asList(Square.H3, Square.C4, Square.F4,
                                    Square.G4, Square.C5, Square.F5,
                                    Square.D6)));
    }

    /**
     * Tests the {@code legalMoves(Player)} method.
     * <p>
     * The test runs the following assertions:
     */
    @Test
    public final void testIsLegal_moreCases() {

        final Board tc0 = new BoardBuilder()
        .withSquaresLiteral(2, 0, 1, 2, 1, 0, 0, 1,
                            0, 2, 0, 0, 0, 0, 1, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0)
        .build();

        assertThat("Board tc0, move G1, is NOT legal.",
                   tc0.isLegal(Square.G1, Player.BLACK),
                   is(false));
    }

    /**
     * Tests the {@code makeMove(Square, Player)} method when parameter
     * {@code move} is {@code null} and the player has legal moves.
     * <p>
     * The {@code BoardFixtures.INITIAL} board is used by this test given
     * that both players have legal moves in such a case.
     *
     * @see AbstractBoard#makeMove(Square, Player)
     * @see BoardFixtures#INITIAL
     */
    @Test(expected = NullPointerException.class)
    public final void testMakeMove_boundaryConditions_checkNullParameter_move() {
        new BoardBuilder(BoardFixtures.INITIAL).build().makeMove(Square.NULL, Player.AN_INSTANCE);
    }

    /**
     * Tests the {@code makeMove(Square, Player)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see AbstractBoard#makeMove(Square, Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testMakeMove_boundaryConditions_checkNullParameter_player() {
        new BoardBuilder().build()
            .makeMove(Square.AN_INSTANCE, Player.NULL);
    }

    /**
     * Tests the {@code makeMove(Square, Player)} method when parameter
     * {@code move} is not legal.
     * <p>
     * The test run the following code:
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.makeMove(Square.A1, Player.BLACK)}</li>
     * </ul>
     * expecting that an {@code IllegalArgumentException} is rised.
     *
     * @see AbstractBoard#makeMove(Square, Player)
     * @see BoardFixtures#INITIAL
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testMakeMove_boundaryConditions_checkIllegalMove() {
        new BoardBuilder(BoardFixtures.INITIAL).build().makeMove(Square.A1, Player.BLACK);
    }

    /**
     * Tests the {@code makeMove(Square, Player)} factory method when the player
     * does not have a legal move.
     * <p>
     * Tests that the following case does rise an exception:
     * <ul>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.makeMove(Square.NULL, Player.BLACK)}</li>
     * </ul>
     * <p>
     *
     * @see AbstractBoard#makeMove(Square, Player)
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     */
    @Test(expected = NullPointerException.class)
    public final void testMakeMove_whenNoLegalMoveIsAvailableToThePlayer() {
        new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build().makeMove(Square.NULL, Player.BLACK);
    }

    /**
     * Tests the mechanics of the {@code makeMove(Square, Player)} factory method.
     * <p>
     * The test run the following assertions:
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.makeMove(Square.D3, Player.BLACK)}
     *       is {@code BoardFixtures.FIRST_MOVE_D3}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_B_9_MOVES.makeMove(Square.C3, Player.WHITE)}
     *       is {@code BoardFixtures.EARLY_GAME_BC3_10_MOVES}</li>
     *   <li>{@code BoardFixtures.EARLY_GAME_B_9_MOVES.makeMove(Square.C6, Player.WHITE)}
     *       is {@code BoardFixtures.EARLY_GAME_BC6_10_MOVES}</li>
     *   <li>{@code BoardFixtures.MAKE_MOVE_TEST_CASE_A_BEFORE.makeMove(Square.D4, Player.WHITE)}
     *       is {@code BoardFixtures.MAKE_MOVE_TEST_CASE_A_AFTER}</li>
     *   <li>{@code BoardFixtures.MAKE_MOVE_TEST_CASE_B_BEFORE.makeMove(Square.D4, Player.WHITE)}
     *       is {@code BoardFixtures.MAKE_MOVE_TEST_CASE_B_AFTER}</li>
     *   <li>{@code BoardFixtures.MAKE_MOVE_TEST_CASE_C_BEFORE.makeMove(Square.D4, Player.WHITE)}
     *       is {@code BoardFixtures.MAKE_MOVE_TEST_CASE_C_AFTER}</li>
     *   <li>{@code BoardFixtures.MAKE_MOVE_TEST_CASE_D_BEFORE.makeMove(Square.B4, Player.WHITE)}
     *       is {@code BoardFixtures.MAKE_MOVE_TEST_CASE_D_AFTER}</li>
     * </ul>
     *
     * @see AbstractBoard#makeMove(Square, Player)
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#FIRST_MOVE_D3
     * @see BoardFixtures#EARLY_GAME_B_9_MOVES
     * @see BoardFixtures#EARLY_GAME_BC3_10_MOVES
     * @see BoardFixtures#EARLY_GAME_BC6_10_MOVES
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_A_BEFORE
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_A_AFTER
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_B_BEFORE
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_B_AFTER
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_C_BEFORE
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_C_AFTER
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_D_BEFORE
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_D_AFTER
     */
    @Test
    public final void testMakeMove() {

        /**
         * Move D3 by black sent to the initial board returns the BoardFixtures.FIRST_MOVE_D3.
         */
        assertThat("BoardFixtures.INITIAL.makeMove(Square.D3, Player.BLACK)"
                   + "must return BoardFixtures.FIRST_MOVE_D3.",
                   new BoardBuilder(BoardFixtures.INITIAL).build().makeMove(Square.D3, Player.BLACK),
                   is(BoardFixtures.FIRST_MOVE_D3));

        /**
         * Move C3 by white sent to the BoardFixtures.EARLY_GAME_B_9_MOVES board returns
         * the BoardFixtures.EARLY_GAME_BC3_10_MOVES.
         */
        assertThat("BoardFixtures.EARLY_GAME_B_9_MOVES.makeMove(Square.C3, Player.WHITE)"
                   + " must return BoardFixtures.EARLY_GAME_BC3_10_MOVES.",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_B_9_MOVES).build().makeMove(Square.C3, Player.WHITE),
                   is(BoardFixtures.EARLY_GAME_BC3_10_MOVES));

        /**
         * Move C6 by white sent to the BoardFixtures.EARLY_GAME_B_9_MOVES board returns
         * the BoardFixtures.EARLY_GAME_BC6_10_MOVES.
         */
        assertThat("BoardFixtures.EARLY_GAME_B_9_MOVES.makeMove(Square.C6, Player.WHITE)"
                   + " must return BoardFixtures.EARLY_GAME_BC6_10_MOVES.",
                   new BoardBuilder(BoardFixtures.EARLY_GAME_B_9_MOVES).build().makeMove(Square.C6, Player.WHITE),
                   is(BoardFixtures.EARLY_GAME_BC6_10_MOVES));

        assertThat("BoardFixtures.MAKE_MOVE_TEST_CASE_A_BEFORE.makeMove(Square.D4, Player.WHITE)"
                   + " must return BoardFixtures.MAKE_MOVE_TEST_CASE_A_AFTER.",
                   new BoardBuilder(BoardFixtures.MAKE_MOVE_TEST_CASE_A_BEFORE).build().makeMove(Square.D4, Player.WHITE),
                   is(BoardFixtures.MAKE_MOVE_TEST_CASE_A_AFTER));

        assertThat("BoardFixtures.MAKE_MOVE_TEST_CASE_B_BEFORE.makeMove(Square.D4, Player.WHITE)"
                   + " must return BoardFixtures.MAKE_MOVE_TEST_CASE_B_AFTER.",
                   new BoardBuilder(BoardFixtures.MAKE_MOVE_TEST_CASE_B_BEFORE).build().makeMove(Square.D4, Player.WHITE),
                   is(BoardFixtures.MAKE_MOVE_TEST_CASE_B_AFTER));

        assertThat("BoardFixtures.MAKE_MOVE_TEST_CASE_C_BEFORE.makeMove(Square.D4, Player.WHITE)"
                   + " must return BoardFixtures.MAKE_MOVE_TEST_CASE_C_AFTER.",
                   new BoardBuilder(BoardFixtures.MAKE_MOVE_TEST_CASE_C_BEFORE).build().makeMove(Square.D4, Player.WHITE),
                   is(BoardFixtures.MAKE_MOVE_TEST_CASE_C_AFTER));

        assertThat("BoardFixtures.MAKE_MOVE_TEST_CASE_D_BEFORE.makeMove(Square.B4, Player.WHITE)"
                   + " must return BoardFixtures.MAKE_MOVE_TEST_CASE_D_AFTER.",
                   new BoardBuilder(BoardFixtures.MAKE_MOVE_TEST_CASE_D_BEFORE).build().makeMove(Square.B4, Player.WHITE),
                   is(BoardFixtures.MAKE_MOVE_TEST_CASE_D_AFTER));

    }

    /**
     * Tests the {@code printBoard()} method.
     * <p>
     * The test run the following assertion:
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.printBoard()} is equal to
     *       the output of {@code printBuffer.toString()}</li>
     * </ul>
     * <p>
     * where {@code printBuffer} is a properly prepared {@code StringBuilder}.
     *
     * @see AbstractBoard#printBoard()
     * @see BoardFixtures#INITIAL
     */
    @Test
    public final void testPrintBoard() {
        StringBuilder printBuffer = new StringBuilder();
        printBuffer.append("    a b c d e f g h \n");
        printBuffer.append(" 1  . . . . . . . . \n");
        printBuffer.append(" 2  . . . . . . . . \n");
        printBuffer.append(" 3  . . . . . . . . \n");
        printBuffer.append(" 4  . . . O @ . . . \n");
        printBuffer.append(" 5  . . . @ O . . . \n");
        printBuffer.append(" 6  . . . . . . . . \n");
        printBuffer.append(" 7  . . . . . . . . \n");
        printBuffer.append(" 8  . . . . . . . . \n");
        assertThat("BoardFixtures.INITIAL.printBoard()"
                   + " must be equal to the output of printBuffer.toString()",
                   new BoardBuilder(BoardFixtures.INITIAL).build().printBoard(),
                   is(printBuffer.toString()));
    }

    /**
     * Tests the {@code printBoardWithCount()} method.
     * <p>
     * The test run the following assertion:
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.printBoardWithCount()} is equal to
     *       the output of {@code printBuffer.toString()}</li>
     * </ul>
     * <p>
     * where {@code printBuffer} is a properly prepared {@code StringBuilder}.
     *
     * @see AbstractBoard#printBoardWithCount()
     * @see BoardFixtures#INITIAL
     */
    @Test
    public final void testPrintBoardWithCount() {
        StringBuilder printBuffer = new StringBuilder();
        printBuffer.append("    a b c d e f g h [@=2 0=2 (0)]\n");
        printBuffer.append(" 1  . . . . . . . . \n");
        printBuffer.append(" 2  . . . . . . . . \n");
        printBuffer.append(" 3  . . . . . . . . \n");
        printBuffer.append(" 4  . . . O @ . . . \n");
        printBuffer.append(" 5  . . . @ O . . . \n");
        printBuffer.append(" 6  . . . . . . . . \n");
        printBuffer.append(" 7  . . . . . . . . \n");
        printBuffer.append(" 8  . . . . . . . . \n");
        assertThat("BoardFixtures.INITIAL.printBoardWithCount()"
                   + " must be equal to the output of printBuffer.toString()",
                   new BoardBuilder(BoardFixtures.INITIAL).build().printBoardWithCount(),
                   is(printBuffer.toString()));
    }

    /**
     * Tests the {@code printCount()} method.
     * <p>
     * The test run the following assertions:
     * <ul>
     *   <li>{@code BoardFixtures.INITIAL.printCount() is [@=2 0=2 (0)]}</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.printCount() is [@=26 0=28 (-2)]}</li>
     * </ul>
     *
     * @see AbstractBoard#printCount()
     * @see BoardFixtures#INITIAL
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     */
    @Test
    public final void testPrintCount() {
        assertThat("BoardFixtures.INITIAL.printCount()"
                   + " must return [@=2 0=2 (0)].",
                   new BoardBuilder(BoardFixtures.INITIAL).build().printCount(),
                   is("[@=2 0=2 (0)]"));
        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.printCount()"
                   + " must return [@=26 0=28 (-2)].",
                   new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build().printCount(),
                   is("[@=26 0=28 (-2)]"));
    }

    /**
     * Util method used by testHashCode_isConsistentWhenCalledMoreThanOnce.
     *
     * @param board  the board
     * @param symbol the board literal
     */
    private void utilHashCode(final Board board, final String symbol) {
        final Board renewed = new BoardBuilder(board).build();
        assertEquals(symbol + " must have a consistent hash code.", renewed.hashCode(), renewed.hashCode());
    }

}
