/*
 *  GamePosition.java
 *
 *  Copyright (c) 2010 Roberto Corradini. All rights reserved.
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
 * A game position is a value object joining the board
 * and the moving player.
 * <p>
 * {@code GamePosition} is immutable.
 */
public final class GamePosition {

    /**
     * Static factory that returns a new initial game position.
     * <p>
     * The returned game position has the board set with the four central
     * disk, and the black player has to move.
     *
     * @return a new initial game position as required by international game's rules
     */
    public static GamePosition initialGamePosition() {
        return valueOf(Board.initialBoard(), Player.BLACK);
    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameter {@code board} cannot be null.
     * Parameter {@code player} can be null when the board doesn't allow
     * any legal move.
     *
     * @param  board  the board state
     * @param  player the player that has to move
     * @return        a new game position
     * @throws NullPointerException when board parameter is null,
     *                              or when player is null and legal moves are still available
     */
    public static GamePosition valueOf(final Board board, final Player player) {
        if (board == null) { throw new NullPointerException("Parameter board cannot be null."); }
        if ((player == null) && board.hasAnyPlayerAnyLegalMove()) {
            throw new NullPointerException("Parameter player cannot be null when there are still valid moves.");
        }
        return new GamePosition(board, player);
    }

    /** The board field. */
    private final Board board;

    /** The player field. */
    private final Player player;

    /**
     * Class constructor.
     * <p>
     * Parameter {@code board} must be not null.
     * Parameter {@code player} must be not null when there are still valid moves.
     *
     * @param board  the board state
     * @param player the player that has to move
     */
    private GamePosition(final Board board, final Player player) {
        assert (board != null) : "Parameter board cannot be null.";
        assert ((player != null) || !board.hasAnyPlayerAnyLegalMove())
            : "Parameter player cannot be null when there are still valid moves.";
        this.board = board;
        this.player = player;
    }

    /**
     * Returns the board field.
     *
     * @return the game state board
     */
    public Board board() {
        return board;
    }

    /**
     * Returns the player field.
     *
     * @return the game state player
     */
    public Player player() {
        return player;
    }

    /**
     * Returns if the game state admit one or more legal moves.
     * <p>
     * When the game position has a null player it returns false.
     *
     * @return {@code true} if the player has at last one legal move
     */
    public boolean hasAnyLegalMove() {
        if (player() == null) return false;
        return board.hasAnyLegalMove(player);
    }

    /**
     * Returns if either one of the two players has any legal move.
     *
     * @return {@code true} if anyone can play a move
     */
    public boolean hasAnyPlayerAnyLegalMove() {
        return board.hasAnyPlayerAnyLegalMove();
    }

    /**
     * Returns the boolean value telling if the move, done by the position's player, is legal.
     * If the position has a null player the return value is always false.
     * A null square parameter is not allowed, in such a case the method throws a
     * {@code NullPointerException}.
     *
     * @param move the square where to put the new disk
     * @return     {@code true} true if the move is legal, otherwise false
     * @throws NullPointerException if parameter {@code move} is null
     */
    public boolean isLegal(final Square move) {
        if (move == null) {
            throw new NullPointerException("Parameter move must be not null.");
        }
        boolean result = false;
        if (player() == null) {
            result =  false;
        } else {
            result =  board.isLegal(move, player());
        }
        return result;
    }

}
