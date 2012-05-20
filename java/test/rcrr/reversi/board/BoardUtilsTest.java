/*
 *  BoardUtilsTest.java
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

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code BoardUtils} class.
 */
public class BoardUtilsTest {

    /** Class constructor. */
    public BoardUtilsTest() { }

    /**
     * Test the {@code checkForConsistencyTheSquareMap(Map<Square, SquareState)} method.
     * <p>
     * The method receives the {@code squares} parameter, it cannot contains null values.
     *
     * @see BoardUtils#checkForConsistencyTheSquareMap(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testCheckForConsistencyTheSquareMap_squaresMustNotContainNullValues() {
        final Map<Square, SquareState> corruptedSquares = new HashMap<Square, SquareState>();
        for (Square sq : Square.values()) {
            corruptedSquares.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        corruptedSquares.put(Square.B3, SquareState.NULL);
        BoardUtils.checkForConsistencyTheSquareMap(corruptedSquares);
    }


    /**
     * Test the {@code checkForConsistencyTheSquareMap(Map<Square, SquareState)} method.
     * <p>
     * The method receives the {@code squares} parameter, it cannot contains null keys.
     *
     * @see BoardUtils#checkForConsistencyTheSquareMap(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_squaresMustNotContainNullKeys() {
        final Map<Square, SquareState> corruptedSquares = new HashMap<Square, SquareState>();
        for (Square sq : Square.values()) {
            corruptedSquares.put(sq, BoardFixtures.INITIAL.get(sq));
        }
        corruptedSquares.remove(Square.H8);
        corruptedSquares.put(Square.NULL, SquareState.EMPTY);
        BoardUtils.checkForConsistencyTheSquareMap(corruptedSquares);
    }

    /**
     * Tests the {@code checkForConsistencyTheSquareMap(Map<Square, SquareState)} method when parameter
     * {@code squares} is {@code null}.
     *
     * @see BoardUtils#checkForConsistencyTheSquareMap(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_checkNullParameter_squares() {
        Map<Square, SquareState> nullSquares = null;
        BoardUtils.checkForConsistencyTheSquareMap(nullSquares);
    }

    /**
     * Tests the {@code checkForConsistencyTheSquareMap(Map<Square, SquareState)} method when parameter
     * {@code squares} is missing one or more key.
     *
     * @see BoardUtils#checkForConsistencyTheSquareMap(Map)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testValueOf_boundaryConditions_checkMissingKey_squares() {
        Map<Square, SquareState> incompleteSquares = new EnumMap<Square, SquareState>(Square.class);
        incompleteSquares.put(Square.A1, SquareState.EMPTY);
        BoardUtils.checkForConsistencyTheSquareMap(incompleteSquares);
    }

}
