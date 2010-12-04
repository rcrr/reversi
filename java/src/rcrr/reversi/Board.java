/*
 *  Board.java
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

/*
 *                                                                                                 1         1         1
 *       1         2         3         4         5         6         7         8         9         0         1         2
 * 456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
 */

package rcrr.reversi;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Collections;
import java.util.EnumMap;

/**
 * A board is an instance of a board game position. It is the state that a board
 * has regardless of the player that has to move or the time spent or remaining to each player.
 * <p>
 * A {@code Board} object holds the information of the state of each board's square.
 * The board state is kept into a map holding the state of the sixtyfour squares.
 * <p>
 * Two boards are equal when they represent the same position. It is up to the implementation
 * if leverage the immutability property and to cache the existing boards instead of creating
 * new ones.
 * <p>
 * {@code Board} is immutable.
 * <p>
 * @see Square
 */
public final class Board {

    /** Lazily initialized, cached hashCode. */
    private volatile int hashCode = 0;

    /** The squares field. */
    private final Map<Square, SquareState> squares;

    /**
     * Class constructor.
     * <p>
     * {@code squareMap} must be not null, and must have a size equal to
     * the number of squares, as defined by the {@code Square} enum.
     *
     * @param  squareMap the sqares field
     */
    private Board(final Map<Square, SquareState> squareMap) {
        assert (squareMap != null) : "Parameter squareMap cannot be null.";
        assert (squareMap.size() == Square.values().length) : "Parameter squareMap size is not consistent."
            + " squareMap.size()=" + squareMap.size()
            + " expected value: " + Square.values().length;
        final EnumMap<Square, SquareState> squareEnumMap = (squareMap instanceof EnumMap)
            ? (EnumMap<Square, SquareState>) squareMap
            : new EnumMap<Square, SquareState>(squareMap);
        this.squares = Collections.unmodifiableMap(squareEnumMap);
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
     * Base static factory for the class.
     * <p>
     * {@code squareMap} must be not null, and must have an entry for every board square.
     * Given that the map cannot have duplicate keys, its size must be equal to the number
     * of class instances defined by the {@code Square} enum.
     *
     * @param  squareMap the map of squares
     * @return           a new board having as state the given square map
     * @throws NullPointerException     if parameter {@code squareMap} is null
     * @throws IllegalArgumentException if the {@code squareMap} is not complete
     */
    public static Board valueOf(final Map<Square, SquareState> squareMap) {
        if (squareMap == null) { throw new NullPointerException("Parameter squareMap cannot be null."); }
        if (squareMap.size() != Square.values().length) {
            throw new IllegalArgumentException("Parameter squareMap size is not consistent."
                                               + " squareMap.size()=" + squareMap.size()
                                               + " expected value: " + Square.values().length);
        }
        if (squareMap.containsKey(null)) {
            throw new NullPointerException("Parameter squareMap cannot have null keys. squareMap=" + squareMap);
        }
        return new Board(squareMap);
    }

    /**
     * A static factory for the class that returns a new empty board.
     *
     * @return a new empty board
     */
    public static Board emptyBoard() {
        return valueOf(emptyBoardSquares());
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
     * Returns the boolean value telling if the move, done by the
     * specified player, is legal.
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
     * Returns the disk difference between the player and her opponent.
     *
     * @param player the player
     * @return       the disk count difference
     * @throws NullPointerException if parameter {@code player} is null
     */
    public int countDifference(final Player player) {
        if (player == null) { throw new NullPointerException("parameter player must be not null."); }
        return countPieces(player.color()) - countPieces(player.opponent().color());
    }

    /**
     * Returns a formatted string showing a 2d graphical represention of the board.
     *
     * @return a string being a 2d representation of the board
     */
    public String printBoard() {
        StringBuilder sb = new StringBuilder();
        sb.append("    a b c d e f g h ");
        for (Row r : Row.values()) {
            sb.append("\n " + r.label() + "  ");
            for (Column c : Column.values()) {
                int idx = (r.ordinal() * 8) + c.ordinal();
                String p = get(Square.getInstance(idx)).symbol();
                sb.append(p + " ");
            }
        }
        sb.append("\n");
        return sb.toString();
    }

    /**
     * Returns a formatted string, giving the two player disk count and their difference.
     *
     * @return a string showing the two player's count
     */
    public String printCount() {
        int cb = countPieces(SquareState.BLACK);
        int cw = countPieces(SquareState.WHITE);
        int cd = cb - cw;
        return "[@=" + cb + " 0=" + cw + " (" + cd + ")]";
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
        StringBuilder sbBoardWithCount = new StringBuilder();
        String sBoard = printBoard();
        String sCount = printCount();
        String[] lines = sBoard.split("\n");
        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];
            sbBoardWithCount.append(line);
            if (i == 0) { sbBoardWithCount.append(sCount); }
            sbBoardWithCount.append("\n");
        }
        return (sbBoardWithCount.toString());
    }

    /**
     * Returns the next player: the one that has to play next, given the current one.
     * When the opponent player has at last one legal move she is
     * the next player. When she doesn't have it, the method checks if
     * the current has at last one legal move, if the check is positive, the current player is
     * also the next one. When neither player has a legal move the method
     * returns null.
     *
     * @param current the current player
     * @return        the next player that has to play a move
     * @throws NullPointerException if parameter {@code current} is null
     */
    public Player nextToPlay(final Player current) {
        if (current == null) { throw new NullPointerException("parameter current must be not null."); }
        Player opponent = current.opponent();
        Player next = null;
        if (hasAnyLegalMove(opponent)) {
            next = opponent;
        } else if (hasAnyLegalMove(current)) {
            next = current;
        }
        return next;
    }

    /**
     * Returns if the player has any legal move given the board state.
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
        List<Square> legalMoves = new ArrayList<Square>();
        for (Square move : Square.values()) {
            if (isLegal(move, player)) { legalMoves.add(move); }
        }
        return legalMoves;
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
        Board board = (Board) object;
        for (Square sq : Square.values()) {
            if (squares.get(sq) != board.squares.get(sq)) { return false; }
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
            int result = 17;
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
                result = 37 * result + k;
            }
            hashCode = result;
        }
        return hashCode;
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
    Square wouldFlip(final Square move, final Player player, final Direction dir) {
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
    Square findBracketingPiece(final Square square, final Player player, final Direction dir) {
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
     * Returns a new squares map being filled by empty values.
     *
     * @return a new map having and empty square state value for each
     *         square in the board
     */
    private static Map<Square, SquareState> emptyBoardSquares() {
        Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(Square.class);
        for (Square sq : Square.values()) {
            sm.put(sq, SquareState.EMPTY);
        }
        return sm;
    }

}
