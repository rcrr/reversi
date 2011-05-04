/*
 *  SearchNodeBuilder.java
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

/**
 * A search node builder is a facility to generate search node instances for testing.
 * <p>
 * {@code SearchNodeBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class SearchNodeBuilder {

    /** The move field. */
    private Square move;

    /** The value field. */
    private int value;

    /**
     * The class constructor.
     */
    public SearchNodeBuilder() {
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
    public SearchNodeBuilder withMove(final Square move) {
        setMove(move);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code value} field.
     *
     * @param value the value assigned to the search node
     * @return      the {@code this} reference
     */
    public SearchNodeBuilder withValue(final int value) {
        setValue(value);
        return this;
    }

}
