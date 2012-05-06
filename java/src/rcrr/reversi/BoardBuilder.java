/*
 *  BoardBuilder.java
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

package rcrr.reversi;

import java.util.Map;
import java.util.EnumMap;

/**
 * An instance of this class encapsulates the information needed to instantiate
 * and initialize a board object. That process is triggered when the {@code build()}
 * method is called.
 * <p>
 * The builder has one property, the {@code squares} field. It is initialized as follow:
 * <ul>
 *   <li>{@code squares = BoardUtils.emptyBoardSquares()}</li>
 * </ul>
 * <p>
 * The {@code Builder} class is mutable, and it is thread-safe.
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
     * Construct a new builder.
     */
    public BoardBuilder() {
        this.squares = BoardUtils.emptyBoardSquares();
    }

    /**
     * Construct a new builder using as starting values the state extracted
     * from the {@code board} parameter.
     *
     * @param board the default state
     */
    public BoardBuilder(final Board board) {
        final EnumMap<Square, SquareState> squares = new EnumMap<Square, SquareState>(Square.class);
        for (final Square sq : Square.values()) {
            squares.put(sq,board.get(sq));
        }
        this.squares = squares;
    }

    /**
     * Returns a new instance of a board object.
     *
     * @return the board instance as prepared by the current board's builder
     */
    public synchronized Board build() {
        //return EnumMapBoard.valueOf(squares);
        return BoardFactoryHolder.getInstance().boardFactory().valueOf(squares);
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
     * as described by the {@code squaresLiteral} parameter.
     * <p>
     * The squares litearal is a variable length argument composed by integers
     * where the assignements are as follow:
     * <ul>
     *  <li>0 --> EMPTY</li>
     *  <li>1 --> BLACK</li>
     *  <li>2 --> WHITE</li>
     *  <li>3 --> OUTER</li>
     * </ul>
     * Values less than zero or greater than three are not allowed.
     * <p>
     * The {@code squaresLiteral} parameter cannot be null.
     * The {@code squaresLiteral} lenght must be equal to the number of
     * squares that compose a board.
     *
     * @param squaresLiteral a literal representation of the board
     *                       described by a variable length integer values
     * @return               the {@code this} reference
     */
    public BoardBuilder withSquaresLiteral(final Integer... squaresLiteral) {
        if (squaresLiteral == null) {
            throw new NullPointerException("Parameter squaresLiteral cannot be null.");
        }
        if (squaresLiteral.length != Square.values().length) {
            throw new IllegalArgumentException("Parameter squaresLiteral has the wrong length.");
        }
        final Map<Square, SquareState> transientSquares = new EnumMap<Square, SquareState>(Square.class);
        for (int index = 0; index < Square.values().length; index++) {
            final Integer integerSquareState = squaresLiteral[index];
            SquareState squareState = null;
            if (integerSquareState == null || integerSquareState < EMPTY || integerSquareState > OUTER) {
                throw new IllegalArgumentException("Parameter squaresLiteral contains a wrong value.");
            } else if (integerSquareState == EMPTY) {
                squareState = SquareState.EMPTY;
            } else if (integerSquareState == BLACK) {
                squareState = SquareState.BLACK;
            } else if (integerSquareState == WHITE) {
                squareState = SquareState.WHITE;
            } else if (integerSquareState == OUTER) {
                squareState = SquareState.OUTER;
            }
            transientSquares.put(Square.values()[index], squareState);
        }
        return withSquares(transientSquares);

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
