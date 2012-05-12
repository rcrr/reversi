/*
 *  SearchNode.java
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

import rcrr.reversi.board.Board;
import rcrr.reversi.board.Player;
import rcrr.reversi.board.Square;

/**
 * An object representing a node part of the search tree.
 * <p>
 * {@code SearchNode} is immutable.
 */
public final class SearchNode {

    /**
     * An instance of this class encapsulates the information needed to instantiate
     * and initialize a search node object. That process is triggered when the {@code build()}
     * method is called.
     * <p>
     * The builder properties and the respectives initializations are:
     * <ul>
     *   <li>{@code move = Square.A1;}</li>
     *   <li>{@code value = 0;}</li>
     * </ul>
     * <p>
     * The {@code Builder} class is mutable, and it is thread-safe.
     * The object status is guarded by a lock on {@code this}.
     */
    public static final class Builder {

        /** The move field. */
        private Square move;

        /** The value field. */
        private int value;

        /**
         * Construct a new builder.
         */
        public Builder() {
            this.move = Square.A1;
            this.value = 0;
        }

        /**
         * Returns a new instance of a search node object.
         *
         * @return the search node instance as prepared by the current search node's builder
         */
        public synchronized SearchNode build() {
            return SearchNode.valueOf(move, value);
        }

        /**
         * The method returns the move field.
         *
         * @return the move field
         */
        public synchronized Square getMove() {
            return this.move;
        }

        /**
         * The method returns the value field.
         *
         * @return the value field
         */
        public synchronized int getValue() {
            return this.value;
        }

        /**
         * The method sets the move field.
         *
         * @param move the update value for the move field
         */
        private synchronized void setMove(final Square move) {
            this.move = move;
        }

        /**
         * The method sets the value field.
         *
         * @param value the update value for the value field
         */
        private synchronized void setValue(final int value) {
            this.value = value;
        }

        /**
         * Returns the {@code this} reference after setting the new {@code move} field.
         *
         * @param move the move assigned to the search node
         * @return     the {@code this} reference
         */
        public SearchNode.Builder withMove(final Square move) {
            setMove(move);
            return this;
        }

        /**
         * Returns the {@code this} reference after setting the new {@code value} field.
         *
         * @param value the value assigned to the search node
         * @return      the {@code this} reference
         */
        public SearchNode.Builder withValue(final int value) {
            setValue(value);
            return this;
        }

    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameter {@code move} can be null.
     *
     * @param  move  the square where to put the disc
     * @param  value the game position value
     * @return        a new search node
     */
    public static SearchNode valueOf(final Square move, final int value) {
        return new SearchNode(move, value);
    }

    /** The move field. */
    private final Square move;

    /** The value field. */
    private final int value;

    /**
     * Class constructor.
     *
     * @param move  the square selected as move
     * @param value the position value associated with the move
     **/
    private SearchNode(final Square move, final int value) {
        this.move = move;
        this.value = value;
    }

    /**
     * Getter method for move field.
     *
     * @return the node's move
     **/
    public Square move() { return move; }

    /**
     * Getter method for value field.
     *
     * @return the node's value
     **/
    public int value() { return value; }

    /**
     * Returns a new node having the value sign negated.
     *
     * @return the new node negated
     */
    public SearchNode negated() { return new SearchNode(move, -value); }

    /**
     * Returns a String representing the {@code Node} object.
     * <p>
     * The format is: {@code [move=b4, value=567]}
     *
     * @return a string showing the minimax's node move and value fields
     */
    @Override
    public String toString() {
        return "[move=" + move + ", value=" + value + "]";
    }

}
