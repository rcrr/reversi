/*
 *  IndexedBoardTest.java
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

import java.math.BigInteger;

import java.util.Set;
import java.util.TreeSet;
import java.util.Map;
import java.util.HashMap;
import java.util.EnumMap;
import java.util.List;
import java.util.ArrayList;
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
 * Test Suite for {@code IndexedBoard} class.
 */
public class IndexedBoardTest extends AbstractBoardTest {

    /** The applicationWideBoardFactory field. */
    private BoardFactory applicationWideBoardFactory = null;

    /** Class constructor. */
    public IndexedBoardTest() { }

    @Before
    public void setBoardFactory() {
        this.applicationWideBoardFactory = BoardFactoryHolder.getInstance().boardFactory();
        BoardFactoryHolder.getInstance().setBoardFactory(new IndexedBoardFactory());
    }

    @After
    public void unsetBoardFactory() {
        BoardFactoryHolder.getInstance().setBoardFactory(this.applicationWideBoardFactory);
        this.applicationWideBoardFactory = null;
    }

    @Test
    public void testComputeIndexes_blackHasToPass() {

        final IndexedBoard blackHasToPass = (IndexedBoard) new BoardBuilder(BoardFixtures.BLACK_HAS_TO_PASS).build();

        final int[] indexes = blackHasToPass.indexes();

        final int[] expectedIndexes = {518,  //  0, R1
                                       5467, //  1, R2
                                       6231, //  2, R3
                                       6528, //  3, R4 
                                       6528, //  4, R5
                                       5718, //  5, R6
                                       1101, //  6, R7
                                       5831, //  7, R8
                                       4379, //  8, A
                                       5467, //  9, B
                                       6555, // 10, C
                                       5476, // 11, D
                                       5574, // 12, E
                                       5819, // 13, F
                                       3396, // 14, G
                                       5100, // 15, H
                                       21,   // 16, A6_C8
                                       75,   // 17, A5_D8
                                       210,  // 18, A4_E8
                                       615,  // 19, A3_F8
                                       1102, // 20, A2_G8
                                       5801, // 21, A1_H8
                                       481,  // 22, B1_H7
                                       714,  // 23, C1_H6
                                       229,  // 24, D1_H5
                                       75,   // 25, E1_H4
                                       23,   // 26, F1_H3
                                       3,    // 27, C1_A3
                                       13,   // 28, D1_A4
                                       48,   // 29, E1_A5
                                       158,  // 30, F1_A6
                                       444,  // 31, G1_A7
                                       5736, // 32, H1_A8
                                       2105, // 33, H2_B8
                                       620,  // 34, H3_C8
                                       215,  // 35, H4_D8
                                       68,   // 36, H5_E8
                                       23    // 37, H6_F8
        };

        for (int i = 0; i < indexes.length; i++) {
            assertThat("Computed index (" + i + ", " + Line.values()[i] + ") must be equal to expected.",
                       expectedIndexes[i],
                       is(indexes[i]));
        }

    }


    @Test
    public void testComputeIndexes_initial() {

        final IndexedBoard initial = (IndexedBoard) BoardFactoryHolder.getInstance().boardFactory().initialBoard();

        final int[] indexes = initial.indexes();

        final int[] expectedIndexes = {0,   //  0, R1
                                       0,   //  1, R2
                                       0,   //  2, R3
                                       135, //  3, R4 
                                       189, //  4, R5
                                       0,   //  5, R6
                                       0,   //  6, R7
                                       0,   //  7, R8
                                       0,   //  8, A
                                       0,   //  9, B
                                       0,   // 10, C
                                       135, // 11, D
                                       189, // 12, E
                                       0,   // 13, F
                                       0,   // 14, G
                                       0,   // 15, H
                                       0,   // 16, A6_C8
                                       0,   // 17, A5_D8
                                       0,   // 18, A4_E8
                                       0,   // 19, A3_F8
                                       27,  // 20, A2_G8
                                       216, // 21, A1_H8
                                       27,  // 22, B1_H7
                                       0,   // 23, C1_H6
                                       0,   // 24, D1_H5
                                       0,   // 25, E1_H4
                                       0,   // 26, F1_H3
                                       0,   // 27, C1_A3
                                       0,   // 28, D1_A4
                                       0,   // 29, E1_A5
                                       0,   // 30, F1_A6
                                       54,  // 31, G1_A7
                                       108, // 32, H1_A8
                                       54,  // 33, H2_B8
                                       0,   // 34, H3_C8
                                       0,   // 35, H4_D8
                                       0,   // 36, H5_E8
                                       0    // 37, H6_F8
        };

        for (int i = 0; i < indexes.length; i++) {
            assertThat("Computed index (" + i + ", " + Line.values()[i] + ") must be equal to expected.",
                       expectedIndexes[i],
                       is(indexes[i]));
        }

    }

    /**
     * Test the {@code valueOf(Map<Square, SquareState)} factory.
     * <p>
     * The factory receives the squares parameter, and any further change to it
     * must not be reflected to the returned board instance.
     *
     * @see IndexedBoard#valueOf(Map)
     */
    @Test
    public final void testValueOf_squaresMustBeUnchangeable() {

        final Map<Square, SquareState> changeable = new EnumMap<Square, SquareState>(Square.class);
        for (Square sq : Square.values()) {
            changeable.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        final Board instance = IndexedBoard.valueOf(changeable);
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
     * @see IndexedBoard#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_squaresMustNotContainNullValues() {
        final Map<Square, SquareState> corruptedSquares = new HashMap<Square, SquareState>();
        for (Square sq : Square.values()) {
            corruptedSquares.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        corruptedSquares.put(Square.B3, SquareState.NULL);
        IndexedBoard.valueOf(corruptedSquares);
    }

    /**
     * Test the {@code valueOf(Map<Square, SquareState>)} factory.
     * <p>
     * The factory receives the squares parameter, it cannot contains null keys.
     *
     * @see IndexedBoard#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_squaresMustNotContainNullKeys() {
        final Map<Square, SquareState> corruptedSquares = new HashMap<Square, SquareState>();
        for (Square sq : Square.values()) {
            corruptedSquares.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        corruptedSquares.remove(Square.H8);
        corruptedSquares.put(Square.NULL, SquareState.EMPTY);
        IndexedBoard.valueOf(corruptedSquares);
    }

    /**
     * Tests the {@code valueOf(Map<Square, SquareState>)} factory when parameter
     * {@code squares} is {@code null}.
     *
     * @see IndexedBoard#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_checkNullParameter_squares() {
        Map<Square, SquareState> nullSquares = null;
        IndexedBoard.valueOf(nullSquares);
    }

    /**
     * Tests the {@code valueOf(Map<Square, SquareState>)} factory when parameter
     * {@code squares} is missing one or more key.
     *
     * @see IndexedBoard#valueOf(Map)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testValueOf_boundaryConditions_checkMissingKey_squares() {
        Map<Square, SquareState> incompleteSquares = new EnumMap<Square, SquareState>(Square.class);
        incompleteSquares.put(Square.A1, SquareState.EMPTY);
        IndexedBoard.valueOf(incompleteSquares);
    }

    /**
     * Tests the {@code valueOf(Map<Square, SquareState>)} factory.
     * <p>
     * After preparing the {@code Map<Square, SquareState> squares} parameter by taking the square values
     * from {@code BoardFixtures.EARLY_GAME_C_12_MOVES}, the test run the following assertions:
     * <ul>
     *   <li>{@code IndexedBoard.valueOf(squares)} is a member of the {@code IndexedBoard} class</li>
     *   <li>{@code IndexedBoard.valueOf(squares)} is equal to {@code BoardFixtures.EARLY_GAME_C_12_MOVES}</li>
     * </ul>
     *
     * @see IndexedBoard#valueOf(Map)
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
                   + " IndexedBoard.valueOf(squares)"
                   + " must return an instance of the IndexedBoard class.",
                   IndexedBoard.valueOf(squares),
                   instanceOf(IndexedBoard.class));

        assertThat("After preparing the Map<Square, SquareState> squares parameter by"
                   + " taking the square values from BoardFixtures.EARLY_GAME_C_12_MOVES,"
                   + " IndexedBoard.valueOf(squares)"
                   + " must be equal to BoardFixtures.EARLY_GAME_C_12_MOVES.",
                   IndexedBoard.valueOf(squares),
                   is(BoardFixtures.EARLY_GAME_C_12_MOVES));
    }

}
