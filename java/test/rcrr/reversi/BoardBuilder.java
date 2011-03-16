/*
 *  BoardBuilder.java
 *
 *  Copyright (c) 2011 Roberto Corradini. All rights reserved.
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

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.EnumMap;

/**
 * A board builder is a facility to generate board instances for testing.
 * <p>
 * {@code BoardBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class BoardBuilder {

    /** Empty square is represented by the 0 integer literal value. */
    private static final int EMPTY = 0;

    /** Black square is represented by the 1 integer literal value. */
    private static final int BLACK = 1;

    /** White square is represented by the 2 integer literal value. */
    private static final int WHITE = 2;

    /** Outer square is represented by the 3 integer literal value. */
    private static final int OUTER = 3;

    /** The squares field. */
    private Map<Square, SquareState> squares;

    /**
     * The class constructor.
     */
    public BoardBuilder() {
        this.squares = Board.emptyBoardSquares();
    }

    /**
     * Returns a new instance of a board object.
     *
     * @return the board instance as prepared by the current board's builder
     */
    public synchronized Board build() {
        return Board.valueOf(squares);
    }

    /**
     * The method returns the square's state to the given board's square.
     *
     * @param square the board's square
     * @return       the selected square's state
     */
    private synchronized SquareState get(final Square square) {
        return this.squares.get(square);
    }

    /**
     * The method assigns a square state to the given board's square.
     *
     * @param square      the board's square
     * @param squareState the square's state value
     */
    private synchronized void put(final Square square, final SquareState squareState) {
        this.squares.put(square, squareState);
    }

    /**
     * The setter method for the squares field.
     *
     * @param squares the update for the square field
     */
    private synchronized void setSquares(final Map<Square, SquareState> squares) {
        this.squares = squares;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code squares} field
     * as described by the {@code boardLiteral} parameter.
     * <p>
     * The board litearal is a list of integers where the assignements are as follow:
     * <ul>
     *  <li>0 --> EMPTY</li>
     *  <li>1 --> BLACK</li>
     *  <li>2 --> WHITE</li>
     *  <li>3 --> OUTER</li>
     * </ul>
     * Values less than zero or greater than three are not allowed.
     * <p>
     * The {@code boardLiteral} parameter cannot be null.
     *
     * @param boardLiteral a literal representation of the board
     *                     described by a list of integer values
     * @return             the {@code this} reference
     */
    public BoardBuilder withBoardLiteral(final List<Integer> boardLiteral) {
        if (boardLiteral == null) {
            throw new NullPointerException("Parameter boardLiteral cannot be null.");
        }
        if (boardLiteral.size() != Square.values().length) {
            throw new IllegalArgumentException("Parameter boardLiteral has the wrong length.");
        }
        final Map<Square, SquareState> transientSquares = new EnumMap<Square, SquareState>(Square.class);
        for (int index = 0; index < Square.values().length; index++) {
            final Integer integerSquareState = boardLiteral.get(index);
            SquareState squareState = null;
            if (integerSquareState == null || integerSquareState < EMPTY || integerSquareState > OUTER) {
                throw new IllegalArgumentException("Parameter boardLiteral contains a wrong value.");
            } else if (integerSquareState == EMPTY) {
                squareState = SquareState.EMPTY;
            } else if (integerSquareState == BLACK) {
                squareState = SquareState.BLACK;
            } else if (integerSquareState == WHITE) {
                squareState = SquareState.WHITE;
            } else if (integerSquareState == OUTER) {
                squareState = SquareState.OUTER;
            }
            transientSquares.put(Square.getInstance(index), squareState);
        }
        return withSquares(transientSquares);
    }

    /**
     * Returns the {@code this} reference after setting the new {@code squares} field
     * as described by the {@code squaresLiteral} parameter.
     * <p>
     * The squares litearal is a variable length argument used to build
     * the {@code <Integer>List boardLiteral} argument that is then passed to the
     * {@code withBoardLiteral(<Integer>List)} method.
     * <p>
     * The {@code squaresLiteral} parameter cannot be null.
     *
     * @param squaresLiteral a literal representation of the board
     *                       described by a variable length integer values
     * @return               the {@code this} reference
     *
     * @see #withBoardLiteral(List)
     */
    public BoardBuilder withSquaresLiteral(final Integer... squaresLiteral) {
        if (squaresLiteral == null) {
            throw new NullPointerException("Parameter squaresLiteral cannot be null.");
        }
        return (withBoardLiteral(Arrays.asList(squaresLiteral)));
    }

    /**
     * Returns the {@code this} reference after setting the new {@code squareState}
     * value to the {@code square} entry.
     *
     * @param square      the board's square
     * @param squareState the board's square assigned state
     * @return            the {@code this} reference
     */
    public BoardBuilder withSquare(final Square square, final SquareState squareState) {
        put(square, squareState);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code squares}
     * field value.
     *
     * @param squares the board squares' map
     * @return        the {@code this} reference
     */
    public BoardBuilder withSquares(final Map<Square, SquareState> squares) {
        setSquares(squares);
        return this;
    }
}
