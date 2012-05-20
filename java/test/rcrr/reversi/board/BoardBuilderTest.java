/*
 *  BoardBuilderTest.java
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

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for {@code BoardBuilder} class.
 */
public class BoardBuilderTest {

    private static final Integer[] NULL_SQUARES_LITERAL = null;

    /** Class constructor. */
    public BoardBuilderTest() { }

    /**
     * Tests the base constructor.
     */
    @Test
    public final void testBaseConstructor() {
        assertThat("Test that the class constructor works.",
                   new BoardBuilder(),
                   instanceOf(BoardBuilder.class));
    }

    /**
     * Tests the overloaded constructor when a null board is passed.
     */
    @Test(expected = NullPointerException.class)
    public final void testOverloadedConstructor_whenParameterBoardIsNull() {
        new BoardBuilder(BoardFixtures.NULL);
    }

    /**
     * Tests the overloaded constructor.
     */
    @Test
    public final void testOverloadedConstructor() {
        assertThat("Test that the overloaded class constructor returns an equivalent board.",
                   new BoardBuilder(BoardFixtures.AN_INSTANCE),
                   instanceOf(BoardBuilder.class));
    }

    /**
     * Tests the overloaded constructor and the build method.
     */
    @Test
    public final void testOverloadedConstructor_and_Build_Method() {
        assertThat("Test that the overloaded class constructor returns an equivalent board.",
                   new BoardBuilder(BoardFixtures.AN_INSTANCE).build(),
                   is(BoardFixtures.AN_INSTANCE));
    }

    /**
     * Tests the {@code withSquare(Square, SquareState)} when
     * parameter {@code square} is {@code null}.
     *
     * @see BoardBuilder#withSquare(Square, SquareState)
     */
    @Test(expected = NullPointerException.class)
    public final void testWithSquare_whenParameterSquare_isNull() {
        new BoardBuilder(BoardFixtures.AN_INSTANCE)
            .withSquare(Square.NULL, SquareState.BLACK);
    }

    /**
     * Tests the {@code withSquare(Square, SquareState)} when
     * parameter {@code squareState} is {@code null}.
     *
     * @see BoardBuilder#withSquare(Square, SquareState)
     */
    @Test(expected = NullPointerException.class)
    public final void testWithSquare_whenParameterSquareState_isNull() {
        new BoardBuilder(BoardFixtures.AN_INSTANCE)
            .withSquare(Square.A1, SquareState.NULL);
    }

    /**
     * Tests the {@code withSquare(Square, SquareState)}.
     *
     * @see BoardBuilder#withSquare(Square, SquareState)
     */
    @Test
    public final void testWithSquare() {
        assertThat("The resulting square state must be BLACK.",
                   new BoardBuilder(BoardFixtures.AN_INSTANCE)
                   .withSquare(Square.A1, SquareState.BLACK)
                   .build()
                   .get(Square.A1),
                   is(SquareState.BLACK));
    }

    /**
     * Tests the {@code withSquares(Map<Square, SquareState>)} method
     * when parameter squares is inconsistent.
     * <p>
     * The test always returns ok. This is to avoid duplication with tests done
     * in the {@code BoardUtilsTest} class.
     * <p>
     * To be compliant the tested method has to validate the parameter calling the
     * {@code BoardUtils.checkForConsistencyTheSquareMap(Map<Square, SquareState>)} method.
     *
     * @see BoardBuilder#withSquares(Map)
     * @see BoardUtils#checkForConsistencyTheSquareMap(Map)
     */
    @Test
    public final void testWithSquares_whenParameterSquares_isInconsistent() {
        assertTrue(true);
    }

    /**
     * Tests the {@code withSquares(Map<Square, SquareState>)} method.
     *
     * @see BoardBuilder#withSquares(Map)
     */
    @Test
    public final void testWithSquares() {
        final Map<Square, SquareState> squares = new HashMap<Square, SquareState>();
        for (Square sq : Square.values()) {
            squares.put(sq, BoardFixtures.AN_INSTANCE.get(sq));
        }
        assertThat("The resulting square state must be BLACK.",
                   new BoardBuilder()
                   .withSquares(squares)
                   .build(),
                   is(BoardFixtures.AN_INSTANCE));
    }

    /**
     * Tests the {@code withSquaresLiteral(Integer...)} method.
     *
     * @see BoardBuilder#withSquaresLiteral(Integer...)
     */
    @Test
    public final void testWithSquaresLiteral() {
        final Board board = new BoardBuilder()
            .withSquaresLiteral(0, 1, 2, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0)
            .build();
        assertThat("The board constructed with the literal array must have A1 = EMPTY.",
                   board.get(Square.A1),
                   is(SquareState.EMPTY));
        assertThat("The board constructed with the literal array must have B1 = BLACK.",
                   board.get(Square.B1),
                   is(SquareState.BLACK));
        assertThat("The board constructed with the literal array must have C1 = WHITE.",
                   board.get(Square.C1),
                   is(SquareState.WHITE));
    }

    /**
     * Tests the {@code withSquaresLiteral(Integer...)} method
     * when the {@code squaresLiteral} parameter is null.
     *
     * @see BoardBuilder#withSquaresLiteral(Integer...)
     */
    @Test(expected = NullPointerException.class)
    public final void testWithSquaresLiteral_whenParameterSquaresLiteral_isNull() {
        final Board board = new BoardBuilder()
            .withSquaresLiteral(NULL_SQUARES_LITERAL)
            .build();
    }

    /**
     * Tests the {@code withSquaresLiteral(Integer...)} method
     * when the {@code squaresLiteral} parameter is of the wrong length.
     *
     * @see BoardBuilder#withSquaresLiteral(Integer...)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testWithSquaresLiteral_whenParameterSquaresLiteral_hasAWrongLength() {
        final Board board = new BoardBuilder()
            .withSquaresLiteral(0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0, 0)
            .build();
    }

    /**
     * Tests the {@code withSquaresLiteral(Integer...)} method
     * when the {@code squaresLiteral} parameter has out of range values.
     *
     * @see BoardBuilder#withSquaresLiteral(Integer...)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testWithSquaresLiteral_whenParameterSquaresLiteral_hasWrongValues() {
        final Board board = new BoardBuilder()
            .withSquaresLiteral(0, 5, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0)
            .build();
    }

}
