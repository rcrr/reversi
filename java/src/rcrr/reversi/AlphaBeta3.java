/*
 *  AlphaBeta3.java
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

import java.util.Comparator;
import java.util.Map;
import java.util.HashMap;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.List;

/**
 * The {@code AlphaBeta3} class implements {@code DecisionRule} and provides
 * an implementation of the search method applying the alpha-beta pruning
 * algorithm.
 * <p>
 * The alpha-beta family of algorithms is described by the wikipedia
 * page: <a href="http://en.wikipedia.org/wiki/Alpha-beta_pruning" target="_blank">alpha-beta pruning</a>.
 */
public final class AlphaBeta3 extends AbstractDecisionRule {

    private static final class MoveData {
        private final int value;
        private final GamePosition position;
        MoveData(final GamePosition position, final int value) {
            this.value = value;
            this.position = position;
        }
        GamePosition position() { return this.position; }
        int value() { return this.value; }
    }

    private static final class ValueComparator implements Comparator<Square> {

        private final Map<Square, MoveData> values;

        ValueComparator(final Map<Square, MoveData> values) {
            this.values = values;
        }

        /**
         * Compares its two arguments for order.
         * Returns a negative integer, zero, or a positive integer as the first argument
         * is less than, equal to, or greater than the second.
         * <p>
         * The order is given by the value of the new game position associated to the given square.
         * Higher value comes first. Two squares having the same value are then sorted by their
         * natural order.
         * <p>
         * Parameters {@code sq0}, and {@code sq1} must be not null.
         *
         * @param sq0 the first square object to be compared
         * @param sq1 the second square object to be compared
         * @return    a negative integer, zero, or a positive integer as the first argument
         *            is less than, equal to, or greater than the second
         */
        public int compare(final Square sq0,
                           final Square sq1) {
            assert (sq0 != null) : "Parameter sq0 cannot be null.";
            assert (sq1 != null) : "Parameter sq1 cannot be null.";
            final int v0 = values.get(sq0).value();
            final int v1 = values.get(sq1).value();
            if (v0 == v1) {
                return sq0.compareTo(sq1);
            } else if (v0 > v1) {
                return -1;
            } else {
                return +1;
            }
        }
    }

    private SortedMap<Square, MoveData> dynamicSortedLegalMoves(final Player player,
                                                                final Board board,
                                                                final EvalFunction ef) {
        final List<Square> moves = board.legalMoves(player);
        final Map<Square, MoveData> values = new HashMap<Square, MoveData>();
        for (Square move : moves) {
            final GamePosition position = GamePosition.valueOf(board.makeMove(move, player), player.opponent());
            final MoveData data = new MoveData(position, -ef.eval(position));
            values.put(move, data);
        }
        final SortedMap<Square, MoveData> results = new TreeMap<Square, MoveData>(new ValueComparator(values));
        results.putAll(values);
        return results;
    }

    /**
     * Class static factory.
     *
     * @return a new alphabeta instance
     */
    public static AlphaBeta3 getInstance() {
        return new AlphaBeta3();
    }

    /** Class constructor. */
    private AlphaBeta3() { };

    /**
     * Implemented by means of the alpha-beta algorithm.
     *
     * @param position the reached game position
     * @param ply      the search depth
     * @param ef       the evaluation function
     * @return         a new search node
     */
    public SearchNode search(final GamePosition position,
                             final int ply,
                             final EvalFunction ef) {
        return multiLevelOrderingSearch(position, LOSING_VALUE, WINNING_VALUE, ply, ef, 0);
    }

    /**
     * Implemented by means of the alpha-beta algorithm applying a dynamic ordering
     * of the moves.
     *
     * @param position    the game position
     * @param achievable  the upper bound
     * @param cutoff      the lower bound
     * @param ply         the search depth
     * @param ef          the evaluation function
     * @param efNodeValue the node value as returned by the evaluation function
     * @return            a new search node
     */
    private SearchNode multiLevelOrderingSearch(final GamePosition position,
                                                final int achievable,
                                                final int cutoff,
                                                final int ply,
                                                final EvalFunction ef,
                                                final int efNodeValue) {
        SearchNode node;
        final Board board = position.board();
        final Player player = position.player();
        final Player opponent = player.opponent();
        if (ply == 0) {
            node = SearchNode.valueOf(null, efNodeValue);
        } else {
            SortedMap<Square, MoveData> moves = dynamicSortedLegalMoves(player, board, ef);
            if (moves.isEmpty()) {
                if (board.hasAnyLegalMove(opponent)) {
                    node = multiLevelOrderingSearch(GamePosition.valueOf(board, opponent),
                                                    -cutoff, -achievable, ply - 1, ef, -efNodeValue).negated();
                } else {
                    node = SearchNode.valueOf(null, finalValue(board, player));
                }
            } else {
                node = SearchNode.valueOf(moves.firstKey(), achievable);
                outer: for (Square move : moves.keySet()) {
                    int val = multiLevelOrderingSearch(moves.get(move).position(),
                                                       -cutoff, -node.value(),
                                                       ply - 1,
                                                       ef,
                                                       -moves.get(move).value())
                        .negated().value();
                    if (val > node.value()) {
                        node = SearchNode.valueOf(move, val);
                    }
                    if (node.value() >= cutoff) { break outer; }
                }
            }
        }
        return node;
    }

}
