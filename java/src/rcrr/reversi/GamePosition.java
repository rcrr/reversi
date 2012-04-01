/*
 *  GamePosition.java
 *
 *  Copyright (c) 2010, 2012 Roberto Corradini. All rights reserved.
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
     * A game position builder is a facility to generate game position instances for testing.
     * <p>
     * {@code GamePositionBuilder} is mutable, and it is thread-safe.
     * The object status is guarded by a lock on {@code this}.
     */
    /**
     * An instance of this class encapsulates the information needed to instantiate
     * and initialize a game position. That process is triggered when the {@code build()}
     * method is called.
     * <p>
     * The builder properties and the respectives initializations are:
     * <ul>
     *   <li>{@code board = Board.emptyBoard()}</li>
     *   <li>{@code player = Player.BLACK}</li>
     * </ul>
     * <p>
     * The {@code Builder} class is mutable, and it is thread-safe.
     * The object status is guarded by a lock on {@code this}.
     */
    public static final class Builder {

        /** The board field. */
        private Board board;

        /** The player field. */
        private Player player;

        /**
         * Construct a new builder.
         */
        public Builder() {
            this.board = EnumMapBoard.emptyBoard();
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
         *
         * @param board the board assigned to the game position
         * @return      the {@code this} reference
         */
        public GamePosition.Builder withBoard(final Board board) {
            setBoard(board);
            return this;
        }

        /**
         * Returns the {@code this} reference after setting the new {@code player} field.
         *
         * @param player the player assigned to the game position
         * @return       the {@code this} reference
         */
        public GamePosition.Builder withPlayer(final Player player) {
            setPlayer(player);
            return this;
        }

    }

    /** Prime number 17. */
    private static final int PRIME_NUMBER_17 = 17;

    /** Prime number 31. */
    private static final int PRIME_NUMBER_31 = 31;

    /**
     * Static factory that returns a new initial game position.
     * <p>
     * The returned game position has the board set with the four central
     * disk, and the black player has to move.
     *
     * @return a new initial game position as required by international game's rules
     */
    public static GamePosition initialGamePosition() {
        return valueOf(BoardFactoryHolder.getInstance().boardFactory().initialBoard(), Player.BLACK);
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

    /** Lazily initialized, cached hashCode. */
    private transient volatile int hashCode = 0;

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
     * Returns true if the specified object is equal to this game position.
     * Two game positions are equal when they have the same player and two equal boards.
     *
     * @param object the object to compare to
     * @return {@code true} when the {@code object} parameter is an instance of
     *         the {@code GamePosition} class, when the players are the same, and
     *         the boards are equal.
     */
    @Override
    public boolean equals(final Object object) {
        if (object == this) { return true; }
        if (!(object instanceof GamePosition)) { return false; }
        final GamePosition position = (GamePosition) object;
        if (player() != position.player()) { return false; }
        if (!board().equals(position.board())) { return false; }
        return true;
    }

    /**
     * Returns a hash code for this game position.
     *
     * @return a hash code for this game position
     */
    @Override
    public int hashCode() {
        if (hashCode == 0) {
            int result = PRIME_NUMBER_17;
            result = PRIME_NUMBER_31 * result + board().hashCode();
            result = PRIME_NUMBER_31 * result + player().hashCode();
            hashCode = result;
        }
        return hashCode;
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
        if (player() == null) { return false; }
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
