/*
 *  EnumMapBoardTest.java
 *
 *  Copyright (c) 2010, 2011, 2012 Roberto Corradini. All rights reserved.
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

import org.junit.After;
import org.junit.Before;
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
 * Test Suite for {@code Board} class.
 */
public class EnumMapBoardTest extends AbstractBoardTest {

    /** The applicationWideBoardFactory field. */
    private BoardFactory applicationWideBoardFactory = null;

    /** Class constructor. */
    public EnumMapBoardTest() { }

    @Before
    public void setBoardFactory() {
        this.applicationWideBoardFactory = BoardFactoryHolder.getInstance().boardFactory();
        BoardFactoryHolder.getInstance().setBoardFactory(new EnumMapBoardFactory());
    }

    @After
    public void unsetBoardFactory() {
        BoardFactoryHolder.getInstance().setBoardFactory(this.applicationWideBoardFactory);
        this.applicationWideBoardFactory = null;
    }

    /**
     * Test the {@code valueOf(Map<Square, SquareState)} factory.
     * <p>
     * The factory receives the squares parameter, and any further change to it
     * must not be reflected to the returned board instance.
     *
     * @see EnumMapBoard#valueOf(Map)
     */
    @Test
    public final void testValueOf_squaresMustBeUnchangeable() {

        final Map<Square, SquareState> changeable = new EnumMap<Square, SquareState>(Square.class);
        for (Square sq : Square.values()) {
            changeable.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        final Board instance = EnumMapBoard.valueOf(changeable);
        changeable.put(Square.A1, SquareState.BLACK);

        assertThat("The board instance must be not affected by a"
                   + " change in the squares parameter.",
                   instance.get(Square.A1), is(SquareState.EMPTY));
    }

    /**
     * Test the {@code valueOf(Map<Square, SquareState)} factory.
     * <p>
     * The factory receives the squares parameter, it cannot contains null values.
     *
     * @see EnumMapBoard#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_squaresMustNotContainNullValues() {
        final Map<Square, SquareState> corruptedSquares = new HashMap<Square, SquareState>();
        for (Square sq : Square.values()) {
            corruptedSquares.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        corruptedSquares.put(Square.B3, SquareState.NULL);
        EnumMapBoard.valueOf(corruptedSquares);
    }

    /**
     * Test the {@code valueOf(Map<Square, SquareState>)} factory.
     * <p>
     * The factory receives the squares parameter, it cannot contains null keys.
     *
     * @see EnumMapBoard#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_squaresMustNotContainNullKeys() {
        final Map<Square, SquareState> corruptedSquares = new HashMap<Square, SquareState>();
        for (Square sq : Square.values()) {
            corruptedSquares.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        corruptedSquares.remove(Square.H8);
        corruptedSquares.put(Square.NULL, SquareState.EMPTY);
        EnumMapBoard.valueOf(corruptedSquares);
    }

    /**
     * Tests the {@code valueOf(Map<Square, SquareState>)} factory when parameter
     * {@code squares} is {@code null}.
     *
     * @see EnumMapBoard#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_checkNullParameter_squares() {
        Map<Square, SquareState> nullSquares = null;
        EnumMapBoard.valueOf(nullSquares);
    }

    /**
     * Tests the {@code valueOf(Map<Square, SquareState>)} factory when parameter
     * {@code squares} is missing one or more key.
     *
     * @see EnumMapBoard#valueOf(Map)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testValueOf_boundaryConditions_checkMissingKey_squares() {
        Map<Square, SquareState> incompleteSquares = new EnumMap<Square, SquareState>(Square.class);
        incompleteSquares.put(Square.A1, SquareState.EMPTY);
        EnumMapBoard.valueOf(incompleteSquares);
    }

    /**
     * Tests the {@code valueOf(Map<Square, SquareState>)} factory.
     * <p>
     * After preparing the {@code Map<Square, SquareState> squares} parameter by taking the square values
     * from {@code BoardFixtures.EARLY_GAME_C_12_MOVES}, the test run the following assertions:
     * <ul>
     *   <li>{@code EnumMapBoard.valueOf(squares)} is a member of the {@code EnumMapBoard} class</li>
     *   <li>{@code EnumMapBoard.valueOf(squares)} is equal to {@code BoardFixtures.EARLY_GAME_C_12_MOVES}</li>
     * </ul>
     *
     * @see EnumMapBoard#valueOf(Map)
     * @see BoardFixtures#EARLY_GAME_C_12_MOVES
     */
    @Test
    public final void testValueOf() {
        Map<Square, SquareState> squares = new HashMap<Square, SquareState>();
        for (Square sq : Square.values()) {
            squares.put(sq, BoardFixtures.EARLY_GAME_C_12_MOVES.get(sq));
        }

        assertThat("After preparing the Map<Square, SquareState> squares parameter by"
                   + " taking the square values from BoardFixtures.EARLY_GAME_C_12_MOVES,"
                   + " EnumMapBoard.valueOf(squares)"
                   + " must return an instance of the EnumMapBoard class.",
                   EnumMapBoard.valueOf(squares),
                   instanceOf(EnumMapBoard.class));

        assertThat("After preparing the Map<Square, SquareState> squares parameter by"
                   + " taking the square values from BoardFixtures.EARLY_GAME_C_12_MOVES,"
                   + " EnumMapBoard.valueOf(squares)"
                   + " must be equal to BoardFixtures.EARLY_GAME_C_12_MOVES.",
                   EnumMapBoard.valueOf(squares),
                   is(BoardFixtures.EARLY_GAME_C_12_MOVES));
    }

    /**
     * Tests the {@code findBracketingPiece(Square, Player, Direction)} private method.
     * <p>
     * {@code wouldFlip(Square, Player, Direction)} is a "private" method in EnumMapBoard class.
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
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.findBracketingPiece(Square.H7, Player.WHITE, Direction.W)}
     *       must return {@code Square.C7}.</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.findBracketingPiece(Square.H7, Player.WHITE, Direction.NW)}
     *       must return {@code Square.F5}.</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.findBracketingPiece(Square.H7, Player.WHITE, Direction.SW)}
     *       must return {@code Square.NULL}.</li>
     * </ul>
     *
     * @throws NoSuchMethodException     if findBracketingPiece method is not found
     * @throws IllegalAccessException    if findBracketingPiece method invocation
     *                                   done by reflection rises a security exception
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
     * @throws IllegalAccessException    if findBracketingPiece method invocation done by reflection
     *                                   rises a security exception
     * @throws InvocationTargetException if findBracketingPiece method invocation rises execution exceptions
     */
    private Square utilFindBracketingPiece(final Board board,
                                           final Player player,
                                           final Square move,
                                           final Direction dir)
        throws NoSuchMethodException,
               IllegalAccessException,
               InvocationTargetException {

        final Method method = EnumMapBoard.class.getDeclaredMethod("findBracketingPiece",
                                                                   Square.class,
                                                                   Player.class,
                                                                   Direction.class);
        method.setAccessible(true);

        final Square firstStepInTheGivenDirection = move.neighbors().get(dir);
        final Square bracketing = (Square) method.invoke(new BoardBuilder(board).build(),
                                                         firstStepInTheGivenDirection, player, dir);

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
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.wouldFlip(Square.H7, Player.WHITE, Direction.W)}
     *       must return {@code Square.C7}.</li>
     *   <li>{@code BoardFixtures.BLACK_HAS_TO_PASS.wouldFlip(Square.H7, Player.WHITE, Direction.S)}
     *       must return {@code Square.NULL}.</li>
     * </ul>
     *
     * @throws NoSuchMethodException     if wouldFlip method is not found
     * @throws IllegalAccessException    if wouldFlip method invocation done by reflection rises a security exception
     * @throws InvocationTargetException if wouldFlip method invocation rises execution exceptions
     *
     * @see EnumMapBoard#makeMove(Square, Player)
     * @see EnumMapBoard#isLegal(Square, Player)
     */
    @Test
    public final void testWouldFlip()
        throws NoSuchMethodException,
               IllegalAccessException,
               InvocationTargetException {

        final Method method = EnumMapBoard.class.getDeclaredMethod("wouldFlip",
                                                                   Square.class,
                                                                   Player.class,
                                                                   Direction.class);
        method.setAccessible(true);

        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.wouldFlip(Square.H7, Player.WHITE, Direction.W)"
                   + " must return Square.C7.",
                   (Square) method.invoke(new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build(),
                                          Square.H7,
                                          Player.WHITE,
                                          Direction.W),
                   is(Square.C7));

        assertThat("BoardFixtures.BLACK_HAS_TO_PASS.wouldFlip(Square.H7, Player.WHITE, Direction.S)"
                   + " must return Square.NULL.",
                   (Square) method.invoke(new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build(),
                                          Square.H7,
                                          Player.WHITE,
                                          Direction.S),
                   is(Square.NULL));
    }

}
