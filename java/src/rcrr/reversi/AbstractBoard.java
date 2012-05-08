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

/**
 * The class provides an implementation for most methods defined by the {@code Board} interface.
 * <p>
 * It also gives the serialization machinery for a format common for all the class that derive from it.
 * In order to use this service the subclass has to implement the {@code Serializable} interface and
 * to define the {@code writeReplace()} method. For instance a simple implementation is:
 * <pre>
 *    Object writeReplace() {
 *        return super.writeReplace();
 *    }
 * </pre>
 */
public abstract class AbstractBoard implements Board {

    /**
     * An instance of this class is used as the proxy that enable the board serialization.
     */
    private static final class SerializationProxy implements Serializable {

        /** The serialVersionUID requested by the specification for serialization. */
        private static final long serialVersionUID = 2193788367767667846L;

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

    /** Class constructor. */
    AbstractBoard() { }

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
     * {@inheritDoc}
     */
    public abstract int countPieces(SquareState color);

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
     * {@inheritDoc}
     */
    public abstract SquareState get(Square square);

    /**
     * {@inheritDoc}
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
     * {@inheritDoc}
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
     * {@inheritDoc}
     */
    public abstract boolean isLegal(Square move, Player player);

    /**
     * {@inheritDoc}
     */
    public List<Square> legalMoves(final Player player) {
        if (player == null) { throw new NullPointerException("Parameter player must be not null."); }
        final List<Square> legalMoves = new ArrayList<Square>();
        for (final Square move : Square.values()) {
            if (isLegal(move, player)) { legalMoves.add(move); }
        }
        return legalMoves;
    }

    /**
     * {@inheritDoc}
     */
    public abstract Board makeMove(Square move, Player player);

    /**
     * {@inheritDoc}
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
     * {@inheritDoc}
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
     * {@inheritDoc}
     */
    public String printCount() {
        final int cb = countPieces(SquareState.BLACK);
        final int cw = countPieces(SquareState.WHITE);
        final int cd = cb - cw;
        return "[@=" + cb + " 0=" + cw + " (" + cd + ")]";
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
