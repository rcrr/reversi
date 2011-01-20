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
 * Documents thread-safaty ...
 */
public final class BoardBuilder {

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
    public Board build() {
        return Board.valueOf(squares);
    }

    /**
     * The method returns the square's state to the given board's square.
     *
     * @param square the board's square
     * @return       the selected square's state
     */
    private SquareState get(final Square square) {
        return this.squares.get(square);
    }

    /**
     * The method assigns a square state to the given board's square.
     *
     * @param square      the board's square
     * @param squareState the square's state value
     */
    private void put(final Square square, final SquareState squareState) {
        this.squares.put(square, squareState);
    }

    /**
     * The setter method for the squares field.
     *
     * @param squares the update for the square field
     */
    private void setSquares(final Map<Square, SquareState> squares) {
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
    public BoardBuilder withBoardLiteral(List<Integer> boardLiteral) {
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
            if (integerSquareState == null || integerSquareState < 0 || integerSquareState > 3) {
                throw new IllegalArgumentException("Parameter boardLiteral contains a wrong value.");
            } else if (integerSquareState == 0) {
                squareState = SquareState.EMPTY;
            } else if (integerSquareState == 1) {
                squareState = SquareState.BLACK;
            } else if (integerSquareState == 2) {
                squareState = SquareState.WHITE;
            } else if (integerSquareState == 3) {
                squareState = SquareState.OUTER;
            }
            transientSquares.put(Square.getInstance(index), squareState);
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
