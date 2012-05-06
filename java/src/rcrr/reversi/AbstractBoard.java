/*
 *  AbstractBoard.java
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

package rcrr.reversi;

import java.util.List;
import java.util.ArrayList;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;

public abstract class AbstractBoard implements Board {

    /**
     * An instance of this class is used as the proxy that enable the board serialization.
     */
    private static final class SerializationProxy implements Serializable {

        /** The serialVersionUID requested by the specification for serialization. */
        private static final long serialVersionUID = 2193788367767667846L;;

        /**
         * The bitboard field.
         * @serial
         */
        private final long[] bitboard;

        /**
         * Class constructor.
         *
         * @param board the board instance to be serialized
         */
        SerializationProxy(final Board board) {
            this.bitboard = BoardUtils.mapToBitboard(BoardUtils.squares(board));
        }

        /**
         * The method {@code readResolve()} is the real implementation for the board constructor
         * when it cames to deserialization of boards.
         * The methods checks that the bitboard array is composed by two entries and that there
         * are not duplicated one position held by the two entries.
         *
         * @return the deserialized board object
         */
        private Object readResolve() {
            if (this.bitboard.length != 2) {
                throw new IllegalArgumentException("Class field bitboard has the wrong length.");
            }
            if ((bitboard[0] & bitboard[1]) != 0L) {
                throw new IllegalArgumentException("Class field bitboard has invalid values.");
            }
            return EnumMapBoard.valueOf(BoardUtils.bitboardToMap(this.bitboard));
        }

    }

    /** Prime number 17. */
    private static final int PRIME_NUMBER_17 = 17;

    /** Prime number 37. */
    private static final int PRIME_NUMBER_37 = 37;

    /** Lazily initialized, cached hashCode. */
    private transient volatile int hashCode = 0;

    /**
     * {@inheritDoc}
     */
    public int countDifference(final Player player) {
        if (player == null) { throw new NullPointerException("Parameter player must be not null."); }
        return countPieces(player.color()) - countPieces(player.opponent().color());
    }

    /**
     * Returns the disk count for the color.
     *
     * @param color the color for which the disk count is computed
     * @return      the disk count
     * @throws NullPointerException if parameter {@code color} is null
     */
    public abstract int countPieces(SquareState color);

    /**
     * Returns the {@code SquareState} value for the given board's square.
     * <p>
     * When {@code square} is {@code null} the method has to return {@code SquareState.OUTER} value.
     *
     * @param  square the board square to retrieve the state value
     * @return        the square state
     */
    public abstract SquareState get(Square square);

    /**
     * Returns if the player has any legal move given the board state.
     * <p>
     * Parameter {@code player} must be not {@code null}.
     *
     * @param player the player
     * @return       {@code true} if the player has any legal move, otherwise {@code false}
     * @throws NullPointerException if parameter {@code player} is null
     */
    public boolean hasAnyLegalMove(final Player player) {
        if (player == null) { throw new NullPointerException("parameter player must be not null."); }
        boolean hasAnyLegalMove = false;
        for (Square move : Square.values()) {
            if (isLegal(move, player)) {
                hasAnyLegalMove = true;
                break;
            }
        }
        return hasAnyLegalMove;
    }

    /**
     * Returns true if either black or white player has any legal move.
     *
     * @return {@code true} if either player has a legal move
     */
    public boolean hasAnyPlayerAnyLegalMove() {
        boolean hasAnyPlayerAnyLegalMove = false;
        for (Player player : Player.values()) {
            if (hasAnyLegalMove(player)) {
                hasAnyPlayerAnyLegalMove = true;
                break;
            }
        }
        return hasAnyPlayerAnyLegalMove;
    }

    /**
     * Returns the boolean value telling if the move, done by the specified player, is legal.
     * <p>
     * Parameter {@code move} must be not {@code null}.
     * Parameter {@code player} must be not {@code null}.
     *
     * @param move   the square where to put the new disk
     * @param player the player moving
     * @return       true if the move is legal, otherwise false
     * @throws NullPointerException if parameter {@code move} or {@code player} is null
     */
    public abstract boolean isLegal(Square move, Player player);

    /**
     * Returns a list holding the legal moves that the {@code player} can
     * do at the board position. When no moves are available to the player
     * the method returns an empty list.
     *
     * @param player the player
     * @return       the moves available to the player
     * @throws NullPointerException if parameter {@code player} is null
     */
    public List<Square> legalMoves(final Player player) {
        if (player == null) { throw new NullPointerException("parameter player must be not null."); }
        final List<Square> legalMoves = new ArrayList<Square>();
        for (final Square move : Square.values()) {
            if (isLegal(move, player)) { legalMoves.add(move); }
        }
        return legalMoves;
    }

    /**
     * Returns a new updated board to reflect move by player. This static factory executes a game move
     * to the board and returns a new one, reflecting the move. The original board is not modified.
     * <p>
     * A null value for player is not allowed, a {@code NullPointerException} is thrown in such a case.
     * <p>
     * A null value for move is allowed, and moreover is the only valid value
     * acceptable by the method, when the player has not any legal move.
     * Otherwise a null move is forbidden, and a {@code NullPointerException} is risen.
     * <p>
     * The method does check if the move is legal. It throws an {@code IllegalArgumentException} in case it is not.
     *
     * @param  move   the board square where to put the disk
     * @param  player the disk color to put on the board
     * @return        a new {@code Board} reflecting the move made
     * @throws NullPointerException     if parameter {@code move} or {@code player} is null
     * @throws IllegalArgumentException if the {@code move} by {@code player} is illegal
     */
    public abstract Board makeMove(Square move, Player player);

    /**
     * Returns a formatted string showing a 2d graphical represention of the board.
     *
     * @return a string being a 2d representation of the board
     */
    public String printBoard() {
        final StringBuilder sb = new StringBuilder();
        sb.append("    a b c d e f g h ");
        for (final Row r : Row.values()) {
            sb.append("\n ").append(r.label()).append("  ");
            for (final Column c : Column.values()) {
                String p = get(Square.getInstance(r, c)).symbol();
                sb.append(p).append(" ");
            }
        }
        sb.append("\n");
        return sb.toString();
    }

    /**
     * Returns a formatted string showing a 2d graphical composed view
     * of the board and the disk count.
     * <p>
     * The method joins the printBoard() and the printCount() output,
     * setting the second on the right of the first board's row.
     *
     * @return a string being a 2d representation of the board with the disk count
     */
    public String printBoardWithCount() {
        final StringBuilder sbBoardWithCount = new StringBuilder();
        final String sBoard = printBoard();
        final String sCount = printCount();
        final String[] lines = sBoard.split("\n");
        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];
            sbBoardWithCount.append(line);
            if (i == 0) { sbBoardWithCount.append(sCount); }
            sbBoardWithCount.append("\n");
        }
        return (sbBoardWithCount.toString());
    }

    /**
     * Returns a formatted string, giving the two player disk count and their difference.
     *
     * @return a string showing the two player's count
     */
    public String printCount() {
        final int cb = countPieces(SquareState.BLACK);
        final int cw = countPieces(SquareState.WHITE);
        final int cd = cb - cw;
        return "[@=" + cb + " 0=" + cw + " (" + cd + ")]";
    }

    /**
     * Returns true if the specified object is equal to this board.
     * Two boards are equal when they have the same disk's configuration.
     *
     * @param object the object to compare to
     * @return {@code true} when the {@code object} parameter is an instance of
     *         the {@code Board} class and when the disks' position are the same.
     */
    @Override
    public boolean equals(final Object object) {
        if (object == this) { return true; }
        if (!(object instanceof Board)) { return false; }
        final Board board = (Board) object;
        for (final Square sq : Square.values()) {
            if (get(sq) != board.get(sq)) { return false; }
        }
        return true;
    }

    /**
     * Returns a hash code for this board.
     *
     * @return a hash code for this board
     */
    @Override
    public int hashCode() {
        if (hashCode == 0) {
            int result = PRIME_NUMBER_17;
            for (final Square sq : Square.values()) {
                result = PRIME_NUMBER_37 * result + get(sq).ordinal();
            }
            hashCode = result;
        }
        return hashCode;
    }

    /**
     * The {@code writeReplace()} method for the serialization proxy pattern.
     * <p>
     * The method return a newly created instance of the class {@code EnumMapBoard.SerializationProxy}.
     * This instance is then serialized instead of the actual board object.
     * <p>
     * See the book: <i>"Bloch, Joshua. Effective Java Second Edition. Addison-Wesley, 2008"</i>.
     *
     * @return a new seialization proxy for the board object
     */
    Object writeReplace() {
        return new SerializationProxy(this);
    }

    /**
     * The {@code readObject()} method for the serialization proxy pattern.
     * <p>
     * The method cannot be invoked. It always throws a new exception.
     * <p>
     * See the book: <i>"Bloch, Joshua. Effective Java Second Edition. Addison-Wesley, 2008"</i>.
     *
     * @param stream the object input stream
     * @throws InvalidObjectException always
     */
    private void readObject(final ObjectInputStream stream) throws InvalidObjectException {
        throw new InvalidObjectException("Proxy required");
    }

}
