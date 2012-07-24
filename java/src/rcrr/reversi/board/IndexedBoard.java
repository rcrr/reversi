/*
 *  IndexedBoard.java
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

package rcrr.reversi.board;

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.EnumSet;

/**
 * A board concrete implementation.
 * <p>
 * A {@code IndexedBoard} object holds the information of the state of each board's square.
 * The board state is kept into a map holding the state of the sixtyfour squares.
 * <p>
 * {@code IndexedBoard} is immutable.
 * <p>
 *
 * @see Square
 */
public final class IndexedBoard extends AbstractBoard {

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
        return new IndexedBoard(squares);
    }

    static Board valueOf(final int[] indexes) {
        return new IndexedBoard(indexes);
    }

    /**
     * Lazily initialized, cached legalMoves.
     * In case of a multi-threadd use must be applied a ReadWriteLock on this field.
     */
    private final transient Map<Player, Set<Square>> legalMovesForPlayer
        = new EnumMap<Player, Set<Square>>(Player.class);

    /**
     * Verify that the transient modifier is really neaded.
     */
    private final transient int[] indexes;

    /**
     * Class constructor.
     * <p>
     * {@code squares} must be not null, and must have a size equal to
     * the number of squares, as defined by the {@code Square} enum.
     *
     * @param  squares the squares field
     */
    private IndexedBoard(final Map<Square, SquareState> squares) {
        assert (squares != null) : "Parameter squares cannot be null.";
        assert (squares.size() == Square.values().length) : "Parameter squares size is not consistent."
            + " squares.size()=" + squares.size()
            + " expected value: " + Square.values().length;
        assert (!squares.containsKey(null)) : "Parameter squares cannot contains null keys.";
        assert (!squares.containsValue(null)) : "Parameter squares cannot contains null values.";
        this.indexes = computeIndexes(squares);
    }

    private IndexedBoard(final int[] indexes) {
        this.indexes = indexes;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SquareState get(final Square square) {
        if (square == null) { return SquareState.OUTER; }
        final int squareOrdinal = square.ordinal();
        final int rowNumber = squareOrdinal / 8;
        final int ordinalPositionInRow = squareOrdinal % 8;
        final LineState lineState = LineState.valueOf(8, indexes[rowNumber]);
        final SquareState color = lineState.configuration().get(ordinalPositionInRow);
        return color;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isLegal(final Square move, final Player player) {
        if (move == null) {
            throw new NullPointerException("Parameter move must be not null.");
        }
        if (player == null) {
            throw new NullPointerException("Parameter player must be not null.");
        }

        Set<Square> cached = this.legalMovesForPlayer.get(player);
        if (cached == null) {
            legalMoves(player);
        }
        return this.legalMovesForPlayer.get(player).contains(move);
    }

    /**
     * Returns a list holding the legal moves that the {@code player} can
     * do at the board position. When no moves are available to the player
     * the method returns an empty list.
     * <p>
     * This version overrides the method defined by the {@code AbstractBoard} class.
     * It memoizes the value calculated.
     * <p>
     * A Read/Write lock is should/could be implemented for the cache management.
     *
     * @param player the player
     * @return       the moves available to the player
     * @throws NullPointerException if parameter {@code player} is null
     */
    @Override
    public List<Square> legalMoves(final Player player) {
        if (player == null) { throw new NullPointerException("Parameter player must be not null."); }
        Set<Square> cached = this.legalMovesForPlayer.get(player);
        if (cached == null) {
            final Set<Square> legalMovesAsSet = EnumSet.noneOf(Square.class);
            for (final Line line : Line.values()) {
                final LineIndex li = LineIndex.valueOf(line, getIndex(player, line));
                legalMovesAsSet.addAll(li.legalMoves().keySet());
            }
            cached = Collections.unmodifiableSet(legalMovesAsSet);
            this.legalMovesForPlayer.put(player, cached);
        }
        return new ArrayList<Square>(cached);
    }

    /**
     * Should be moved in LineIndex .....
     */
    private int getIndex(final Player player, final Line line) {
        return (player == Player.BLACK) ? indexes[line.ordinal()]: LineIndex.valueOf(line, indexes[line.ordinal()]).flip().index();
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
    @Override
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

        /**
         * Black is ok. White is not. Indexes must be accessed using getIndex().
         * Finally the indexes must be flipped back.
         *
         */
        final int[] newIndexes = new int[indexes.length];

        final List<LineIndexMove> moveAddendums = new ArrayList<LineIndexMove>();
        for (final Line line : Line.linesForSquare(move)) {
            final LineIndex li = LineIndex.valueOf(line, getIndex(player, line));
            for (final Map.Entry<Square, LineIndex> entry : li.legalMoves().entrySet()) {
                final Square sq = entry.getKey();
                final int moveOrdinalPosition = line.squares().indexOf(sq);
                final LineIndexMove lim = LineIndexMove.valueOf(li, moveOrdinalPosition);
                if (sq == move) { moveAddendums.add(lim); }
            }
        }

        final int[] addedDiscDeltas = new int[indexes.length];
        for (final Line line : Line.linesForSquare(move)) {
            final int ordinal = line.squares().indexOf(move);
            addedDiscDeltas[line.ordinal()] = Line.squarePositionInLineBase3Coefficient(ordinal);
        }

        for (final Line line : Line.values()) {
            final int i = line.ordinal();
            newIndexes[i] = getIndex(player, line) + addedDiscDeltas[i];
            for (int j = 0; j < moveAddendums.size(); j++) {
                newIndexes[i] += moveAddendums.get(j).getDeltas()[i];
            }
        }

        /** Flip indexes if player is White */
        if (player == Player.WHITE) {
            int k = 0;
            for (final Line line : Line.values()) {
                newIndexes[k] = LineIndex.valueOf(line, newIndexes[k]).flip().index();
                k++;
            }
        }
        /** */

        return valueOf(newIndexes);
    }

    /**
     * The {@code writeReplace()} method for the serialization proxy pattern.
     * <p>
     * The method return a newly created instance of the class {@code AbstractBoard.SerializationProxy}.
     * This instance is then serialized instead of the actual board object.
     * <p>
     * See the book: <i>"Bloch, Joshua. Effective Java Second Edition. Addison-Wesley, 2008"</i>.
     *
     * @return a new seialization proxy for the board object
     */
    @Override
    Object writeReplace() {
        return super.writeReplace();
    }

    /**
     * Returns the indexes field.
     *
     * @return the array representing the indexes of the board
     */
    public int[] indexes() {
        return this.indexes;
    }

    public static final int[] computeIndexes(final Map<Square, SquareState> squares) {
        final int[] transientIndexes = new int[Line.NUMBER_OF];
        for (final Square sq : Square.values()) {
            for (final Line line : Line.linesForSquare(sq)) {
                final int color = squares.get(sq).ordinal();
                final int squarePosition = line.squares().indexOf(sq);
                transientIndexes[line.ordinal()] += color * Line.squarePositionInLineBase3Coefficient(squarePosition);
            }
        }
        return transientIndexes;
    }

    public static final EnumMap<Square, SquareState> computeSquares(final int[] indexes) {
        final EnumMap<Square, SquareState> sqs = new EnumMap<Square, SquareState>(Square.class);
        for (int iRow = 0; iRow < 8; iRow++) {
            for (int iSquare = 0; iSquare < 8; iSquare++) {
                final LineState lineState = LineState.valueOf(8, indexes[iRow]);
                final SquareState color = lineState.configuration().get(iSquare);
                sqs.put(Square.values()[(iRow * 8) + iSquare], color);
            }
        }
        return sqs;
    }

}
