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

    /** Caches the square enum values in a local array. */
    private static final Square[] SQUARE_VALUES = Square.values();

    /** Caches the line enum values in a local array. */
    private static final Line[] LINE_VALUES = Line.values();

    private static final EnumMap<Square, int[]> MOVE_DISC_DELTA_MAP = computeMoveDiscDeltas();

    public static int[] computeIndexes(final Map<Square, SquareState> squares) {
        final int[] transientIndexes = new int[Line.NUMBER_OF];
        for (final Square sq : SQUARE_VALUES) {
            for (final Line line : Line.linesForSquare(sq)) {
                final int color = squares.get(sq).ordinal();
                final int squarePosition = line.squares().indexOf(sq);
                transientIndexes[line.ordinal()] += color * Line.squarePositionInLineBase3Coefficient(squarePosition);
            }
        }
        return transientIndexes;
    }

    public static EnumMap<Square, SquareState> computeSquares(final int[] indexes) {
        final EnumMap<Square, SquareState> sqs = new EnumMap<Square, SquareState>(Square.class);
        for (int iRow = 0; iRow < 8; iRow++) {
            for (int iSquare = 0; iSquare < 8; iSquare++) {
                final LineState lineState = LineState.valueOf(8, indexes[iRow]);
                final SquareState color = lineState.configuration().get(iSquare);
                sqs.put(SQUARE_VALUES[(iRow * 8) + iSquare], color);
            }
        }
        return sqs;
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
    static Board valueOf(final Map<Square, SquareState> squares) {
        BoardUtils.checkForConsistencyTheSquareMap(squares);
        return new IndexedBoard(computeIndexes(squares));
    }

    static Board valueOf(final int[] indexes) {
        return new IndexedBoard(indexes);
    }

    private static EnumMap<Square, int[]> computeMoveDiscDeltas() {
        final EnumMap<Square, int[]> deltaMap = new EnumMap<Square, int[]>(Square.class);
        for (final Square move : SQUARE_VALUES) {
            final int[] deltas = new int[Line.NUMBER_OF];
            for (final Line line : Line.linesForSquare(move)) {
                final int ordinal = line.squares().indexOf(move);
                deltas[line.ordinal()] = Line.squarePositionInLineBase3Coefficient(ordinal);
            }
            deltaMap.put(move, deltas);
        }
        return deltaMap;
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
     * {@code indexes} must be not null, and must have a length equal to
     * the number of indexes, as defined by the {@code Line} enum.
     *
     * @param indexes the indexes field
     */
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
        final Set<Square> cached = this.legalMovesForPlayer.get(player);
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
            for (final Line line : LINE_VALUES) {
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
     * {@inheritDoc}
     */
    @Override
    public Board makeMove(final Square move, final Player player) {

        makeMoveInvariantAreSatisfied(move, player);

        final int[] newIndexes = new int[Line.NUMBER_OF];

        // We can think to make an optimized map to square --> Line --> index LineIndexMove .....
        final List<LineIndexMove> moveAddendums = new ArrayList<LineIndexMove>();
        for (final Line line : Line.linesForSquare(move)) {
            final LineIndex li = LineIndex.valueOf(line, getIndex(player, line));
            for (final Square sq : li.legalMoves().keySet()) {
                if (sq == move) {
                    final int moveOrdinalPosition = line.squares().indexOf(sq);
                    final LineIndexMove lim = LineIndexMove.valueOf(li, sq);
                    moveAddendums.add(lim);
                }
            }
        }

        final int[] moveDiscDeltas = MOVE_DISC_DELTA_MAP.get(move);
        for (final Line line : LINE_VALUES) {
            final int i = line.ordinal();
            newIndexes[i] = getIndex(player, line) +  moveDiscDeltas[i];
            for (final LineIndexMove lim : moveAddendums) {
                newIndexes[i] += lim.deltas()[i];
            }
        }

        // must be a method ... and LineIndex must have a flip method ...
        /** Flip indexes if player is White */
        if (player == Player.WHITE) {
            int k = 0;
            for (final Line line : LINE_VALUES) {
                newIndexes[k] = LineIndex.valueOf(line, newIndexes[k]).flip().index();
                k++;
            }
        }

        return valueOf(newIndexes);
    }

    /**
     * Returns the indexes field.
     *
     * @return the array representing the indexes of the board
     */
    public int[] indexes() {
        return this.indexes;
    }

}
