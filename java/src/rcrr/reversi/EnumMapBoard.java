/*
 *  EnumMapBoard.java
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

/*
 *                                                                                                 1         1         1
 *       1         2         3         4         5         6         7         8         9         0         1         2
 * 456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
 */

package rcrr.reversi;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.Serializable;

/**
 * A board is an instance of a board game position. It is the state that a board
 * has regardless of the player that has to move or the time spent or remaining to each player.
 * <p>
 * A {@code EnumMapBoard} object holds the information of the state of each board's square.
 * The board state is kept into a map holding the state of the sixtyfour squares.
 * <p>
 * Two boards are equal when they represent the same position. It is up to the implementation
 * if leverage the immutability property and to cache the existing boards instead of creating
 * new ones.
 * <p>
 * {@code EnumMapBoard} is immutable.
 * <p>
 * @see Square
 */
public final class EnumMapBoard extends AbstractBoard implements Serializable {

    /**
     * An instance of this class encapsulates the information needed to instantiate
     * and initialize a board object. That process is triggered when the {@code build()}
     * method is called.
     * <p>
     * The builder has one property, the {@code squares} field. It is initialized as follow:
     * <ul>
     *   <li>{@code squares = EnumMapBoard.emptyBoardSquares()}</li>
     * </ul>
     * <p>
     * The {@code Builder} class is mutable, and it is thread-safe.
     * The object status is guarded by a lock on {@code this}.
     */
    public static final class Builder {

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
        public Builder() {
            this.squares = EnumMapBoard.emptyBoardSquares();
        }

        /**
         * Construct a new builder using as starting values the state extracted
         * from the {@code board} parameter.
         *
         * @param board the default state
         */
        public Builder(final Board board) {
            final EnumMap<Square, SquareState> squares = new EnumMap<Square, SquareState>(Square.class);
            for (final Square sq : Square.values()) {
                squares.put(sq,board.get(sq));
            }
            this.squares = squares;
            // this.squares = new EnumMap<Square, SquareState>(board.squares());
        }

        /**
         * Returns a new instance of a board object.
         *
         * @return the board instance as prepared by the current board's builder
         */
        public synchronized Board build() {
            return EnumMapBoard.valueOf(squares);
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
        public EnumMapBoard.Builder withSquaresLiteral(final Integer... squaresLiteral) {
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
        public EnumMapBoard.Builder withSquare(final Square square, final SquareState squareState) {
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
        public EnumMapBoard.Builder withSquares(final Map<Square, SquareState> squares) {
            setSquares(squares);
            return this;
        }
    }

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
        SerializationProxy(final EnumMapBoard board) {
            this.bitboard = BoardUtils.mapToBitboard(board.squares());
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

    /**
     * A static factory for the class that returns a new empty board.
     *
     * @return a new empty board
     */
    public static Board emptyBoard() {
        return valueOf(emptyBoardSquares());
    }

    /**
     * A static factory for the class that returns a board filled
     * by sixtyfour discs having the color set by the {@code player} parameter.
     * <p>
     * Parameter {@code player} cannot be {@code null}.
     *
     * @param player it selects the color of the sixtyfour discs
     * @return       a new board filled by sixtyfour discs
     */
    public static Board fillWithColor(final Player player) {
        if (player == null) { throw new NullPointerException("Parameter color cannot be null."); }
        final Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(Square.class);
        for (Square sq : Square.values()) {
            sm.put(sq, player.color());
        }
        return valueOf(sm);
    }

    /**
     * A static factory for the class that returns a new initial board.
     *
     * @return a new initial board
     */
    public static Board initialBoard() {
        Map<Square, SquareState> sm = emptyBoardSquares();
        sm.put(Square.D4, SquareState.WHITE);
        sm.put(Square.E4, SquareState.BLACK);
        sm.put(Square.D5, SquareState.BLACK);
        sm.put(Square.E5, SquareState.WHITE);
        return valueOf(sm);
    }

    /**
     * Base static factory for the class.
     * <p>
     * {@code squares} must be not null, and must have an entry for every board square.
     * Given that the map cannot have duplicate keys, its size must be equal to the number
     * of class instances defined by the {@code Square} enum.
     *
     * @param  squares the map of squares
     * @return         a new board having as state the given square map
     * @throws NullPointerException     if parameter {@code squares} is null
     * @throws IllegalArgumentException if the {@code squares} is not complete
     */
    public static Board valueOf(final Map<Square, SquareState> squares) {
        if (squares == null) { throw new NullPointerException("Parameter squares cannot be null."); }
        if (squares.size() != Square.values().length) {
            throw new IllegalArgumentException("Parameter squares size is not consistent."
                                               + " squares.size()=" + squares.size()
                                               + " expected value: " + Square.values().length);
        }
        if (squares.containsKey(null)) {
            throw new NullPointerException("Parameter squares cannot have null keys. squares=" + squares);
        }
        if (squares.containsValue(null)) {
            throw new NullPointerException("Parameter squares cannot have null values. squares=" + squares);
        }
        return new EnumMapBoard(squares);
    }

    /**
     * Returns a new squares map being filled by empty values.
     *
     * @return a new map having and empty square state value for each
     *         square in the board
     */
    static Map<Square, SquareState> emptyBoardSquares() {
        Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(Square.class);
        for (Square sq : Square.values()) {
            sm.put(sq, SquareState.EMPTY);
        }
        return sm;
    }

    /** Lazily initialized, cached hashCode. */
    private transient volatile int hashCode = 0;

    /** The squares field. */
    private final transient Map<Square, SquareState> squares;

    /**
     * Class constructor.
     * <p>
     * {@code squares} must be not null, and must have a size equal to
     * the number of squares, as defined by the {@code Square} enum.
     *
     * @param  squares the squares field
     */
    private EnumMapBoard(final Map<Square, SquareState> squares) {
        assert (squares != null) : "Parameter squares cannot be null.";
        assert (squares.size() == Square.values().length) : "Parameter squares size is not consistent."
            + " squares.size()=" + squares.size()
            + " expected value: " + Square.values().length;
        assert (!squares.containsKey(null)) : "Parameter squares cannot contains null keys.";
        assert (!squares.containsValue(null)) : "Parameter squares cannot contains null values.";
        this.squares = Collections.unmodifiableMap(new EnumMap<Square, SquareState>(squares));
    }

    /**
     * Returns the disk count for the color.
     *
     * @param color the color for which the disk count is computed
     * @return the disk count
     * @throws NullPointerException if parameter {@code color} is null
     */
    public int countPieces(final SquareState color) {
        if (color == null) {
            throw new NullPointerException("parameter color must be not null.");
        }
        int count = 0;
        for (SquareState ss : squares.values()) {
            if (ss == color) { count++; }
        }
        return count;
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
        final EnumMapBoard board = (EnumMapBoard) object;
        for (Square sq : Square.values()) {
            if (squares.get(sq) != board.squares.get(sq)) { return false; }
        }
        return true;
    }

    /**
     * Returns the {@code SquareState} value for the given board's square.
     * <p>
     * When {@code square} is {@code null} the method returns {@code SquareState.OUTER} value.
     *
     * @param  square the board square to retrieve the state value
     * @return        the square state
     */
    public SquareState get(final Square square) {
        return (square == null) ? SquareState.OUTER : squares.get(square);
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
            Square[] squareArray = Square.values();
            for (int i = 0; i < squareArray.length; i++) {
                SquareState ss = squares.get(squareArray[i]);
                int k = 0;
                switch (ss) {
                case EMPTY: k = 0; break;
                case BLACK: k = 1; break;
                case WHITE: k = 2; break;
                default: k = 0; // this should never happens.
                }
                result = PRIME_NUMBER_37 * result + k;
            }
            hashCode = result;
        }
        return hashCode;
    }

    /**
     * Returns the boolean value telling if the move, done by the
     * specified player, is legal.
     * <p>
     * Parameter {@code move} must be not {@code null}.
     * Parameter {@code player} must be not {@code null}.
     *
     * @param move   the square where to put the new disk
     * @param player the player moving
     * @return       true if the move is legal, otherwise false
     * @throws NullPointerException if parameter {@code move} or {@code player} is null
     */
    public boolean isLegal(final Square move, final Player player) {
        if (move == null) {
            throw new NullPointerException("Parameter move must be not null.");
        }
        if (player == null) {
            throw new NullPointerException("Parameter player must be not null.");
        }
        if (get(move) != SquareState.EMPTY) { return false; }
        for (Direction dir : Direction.values()) {
            if (wouldFlip(move, player, dir) != null) { return true; }
        }
        return false;
    }

    /**
     * Returns a new updated board to reflect move by player. This static
     * factory executes a game move to the board and returns a new one,
     * reflecting the move. The original board is not modified.
     * <p>
     * A null value for player is not allowed, a {@code NullPointerException}
     * is thrown in such a case.
     * <p>
     * A null value for move is allowed, and moreover is the only valid value
     * acceptable by the method, when the player has not any legal move.
     * Otherwise a null move is forbidden, and a {@code NullPointerException}
     * is risen.
     * <p>
     * The method does check if the move is legal. It throws an
     * {@code IllegalArgumentException} in case it is not.
     *
     * @param  move   the board square where to put the disk
     * @param  player the disk color to put on the board
     * @return        a new {@code Board} reflecting the move made
     * @throws NullPointerException     if parameter {@code move}
     *                                  or {@code player} is null
     * @throws IllegalArgumentException if the {@code move}
     *                                  by {@code player} is illegal
     */
    public Board makeMove(final Square move, final Player player) {
        if (player == null) {
            throw new NullPointerException("Parameter player must be not null.");
        }
        if (move == null) {
            if (hasAnyLegalMove(player)) {
                throw new NullPointerException("Parameter move must be not null when a legal one is available.");
            } else {
                return this;
            }
        }
        if (!isLegal(move, player)) {
            throw new IllegalArgumentException("The move<"
                                               + move + "> by player<"
                                               + player + "> is illegal.");
        }
        Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(squares);
        sm.put(move, player.color());
        for (Direction dir : Direction.values()) {
            Square bracketer = wouldFlip(move, player, dir);
            if (bracketer != null) {
                for (Square c = move.neighbors().get(dir); true; c = c.neighbors().get(dir)) {
                    if (c == bracketer) { break; }
                    sm.put(c, player.color());
                }
            }
        }
        return valueOf(sm);
    }

    /**
     * Returns the bracketing square or null if it is missing.
     * The method does not check that the move is legal and that the square parameter
     * is one step from move in the given direction.
     * <p>
     * The method should be private. It is not private to enable unit testing.
     * <p>
     * Parameters square, player, and dir must be not null. Java assert statements check
     * against this case to occur.
     *
     * @param square the square obtained moving by one from the move in the given direction
     * @param player the player
     * @param dir    the direction
     * @return       the bracketing square, or null if it is not found
     */
    private Square findBracketingPiece(final Square square, final Player player, final Direction dir) {
        assert (square != null) : "Argument square must be not null";
        assert (player != null) : "Argument player must be not null";
        assert (dir != null) : "Argument dir must be not null";
        if (get(square) == player.color()) {
            return square;
        } else if (get(square) == player.opponent().color()) {
            Square next = square.neighbors().get(dir);
            if (next != null) { return findBracketingPiece(next, player, dir); }
        }
        return null;
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

    /**
     * Returns the squares field.
     *
     * @return the map representing the squares of the board
     */
    private Map<Square, SquareState> squares() {
        return this.squares;
    }

    /**
     * Returns the bracketing square or null if it is not found.
     * The method does not check that the move is legal.
     * <p>
     * The method should be private. It is not private to enable unit testing.
     * <p>
     * Parameters move, player, and dir must be not null. Java assert statements check
     * against this case to occur.
     *
     * @param move   the square where to move
     * @param player the player
     * @param dir    the direction
     * @return       the bracketing square, or null if it is not found
     */
    private Square wouldFlip(final Square move, final Player player, final Direction dir) {
        assert (move != null) : "Argument square must be not null";
        assert (player != null) : "Argument player must be not null";
        assert (dir != null) : "Argument dir must be not null";
        Square neighbor = move.neighbors().get(dir);
        Square bracketing = null;
        if (get(neighbor) == player.opponent().color()) {
            Square next = neighbor.neighbors().get(dir);
            if (next != null) { bracketing = findBracketingPiece(next, player, dir); }
        }
        return bracketing;
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
    private Object writeReplace() {
        return new SerializationProxy(this);
    }

}
