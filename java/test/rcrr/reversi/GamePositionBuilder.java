/*
 *  GamePositionBuilder.java
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
 * A game position builder is a facility to generate game position instances for testing.
 * <p>
 * {@code GamePositionBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class GamePositionBuilder {

    /** The board field. */
    private Board board;

    /** The player field. */
    private Player player;

    /**
     * The class constructor.
     */
    public GamePositionBuilder() {
        this.board = Board.emptyBoard();
        this.player = Player.BLACK;
    }

    /**
     * Returns a new instance of a game position object.
     *
     * @return the game position instance as prepared by the current game position's builder
     */
    public synchronized GamePosition build() {
        return GamePosition.valueOf(board, player);
    }

    /**
     * The method returns the board field.
     *
     * @return the board field
     */
    public synchronized Board getBoard() {
        return this.board;
    }

    /**
     * The method returns the player field.
     *
     * @return the player field
     */
    public synchronized Player getPlayer() {
        return this.player;
    }

    /**
     * The method sets the board field.
     *
     * @param board the update value for the board field
     */
    private synchronized void setBoard(final Board board) {
        this.board = board;
    }

    /**
     * The method sets the player field.
     *
     * @param player the update value for the player field
     */
    private synchronized void setPlayer(final Player player) {
        this.player = player;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code board} field.
     * <p>
     * The {@code board} parameter cannot be null.
     *
     * @param board the board assigned to the game position
     * @return      the {@code this} reference
     */
    public GamePositionBuilder withBoard(final Board board) {
        setBoard(board);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code player} field.
     *
     * @param player the player assigned to the game position
     * @return       the {@code this} reference
     */
    public GamePositionBuilder withPlayer(final Player player) {
        setPlayer(player);
        return this;
    }

}
