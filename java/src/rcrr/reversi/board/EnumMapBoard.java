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

package rcrr.reversi.board;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

/**
 * A board concrete implementation.
 * <p>
 * A {@code EnumMapBoard} object holds the information of the state of each board's square.
 * The board state is kept into a map holding the state of the sixtyfour squares.
 * <p>
 * {@code EnumMapBoard} is immutable.
 * <p>
 * @see Square
 */
public final class EnumMapBoard extends AbstractBoard {

    /** Caches the square enum values in a local array. */
    private static final Square[] SQUARE_VALUES = Square.values();

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
    static Board valueOf(final Map<Square, SquareState> squares) {
        BoardUtils.checkForConsistencyTheSquareMap(squares);
        return new EnumMapBoard(squares);
    }

    /**
     * Lazily initialized, cached legalMoves.
     * In case of a multi-threadd use must be applied a ReadWriteLock on this field.
     */
    private final transient Map<Player, List<Square>> legalMovesForPlayer
        = new EnumMap<Player, List<Square>>(Player.class);

    /** The squares field. */
    private final transient EnumMap<Square, SquareState> squares;

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
        assert (squares.size() == SQUARE_VALUES.length) : "Parameter squares size is not consistent."
            + " squares.size()=" + squares.size()
            + " expected value: " + SQUARE_VALUES.length;
        assert (!squares.containsKey(null)) : "Parameter squares cannot contains null keys.";
        assert (!squares.containsValue(null)) : "Parameter squares cannot contains null values.";
        this.squares = new EnumMap<Square, SquareState>(squares);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int countPieces(final SquareState color) {
        if (color == null) {
            throw new NullPointerException("Parameter color must be not null.");
        }
        int count = 0;
        for (SquareState ss : squares.values()) {
            if (ss == color) { count++; }
        }
        return count;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SquareState get(final Square square) {
        return (square == null) ? SquareState.OUTER : squares.get(square);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isLegal(final Square move, final Player player) {
        isLegalInvariantsAreSatisfied(move, player);
        if (get(move) != SquareState.EMPTY) { return false; }
        for (final Direction dir : move.capableToFlipDirections()) {
            if (wouldFlip(move, player, dir) != null) { return true; }
        }
        return false;
    }

    /**
     * Returns a list holding the legal moves that the {@code player} can
     * do at the board position. When no moves are available to the player
     * the method returns an empty list.
     *
     * This version overrides the method defined by the {@code AbstractBoard} class.
     * It memoizes the value calculated.
     *
     * @param player the player
     * @return       the moves available to the player
     * @throws NullPointerException if parameter {@code player} is null
     */
    @Override
    public List<Square> legalMoves(final Player player) {
        if (player == null) { throw new NullPointerException("Parameter player must be not null."); }
        List<Square> cached = this.legalMovesForPlayer.get(player);
        if (cached != null) { return cached; }
        final List<Square> legalMoves = new ArrayList<Square>();
        for (final Square move : SQUARE_VALUES) {
            if (isLegal(move, player)) { legalMoves.add(move); }
        }
        cached = Collections.unmodifiableList(legalMoves);
        this.legalMovesForPlayer.put(player, cached);
        return cached;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Board makeMove(final Square move, final Player player) {
        makeMoveInvariantsAreSatisfied(move, player);
        final Map<Square, SquareState> sm = squares.clone();
        sm.put(move, player.color());
        for (final Direction dir : move.capableToFlipDirections()) {
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
            final Square next = square.neighbors().get(dir);
            if (next != null) { return findBracketingPiece(next, player, dir); }
        }
        return null;
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
        final Square neighbor = move.neighbors().get(dir);
        Square bracketing = null;
        if (get(neighbor) == player.opponent().color()) {
            final Square next = neighbor.neighbors().get(dir);
            if (next != null) { bracketing = findBracketingPiece(next, player, dir); }
        }
        return bracketing;
    }

}
