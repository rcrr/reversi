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
        final GamePosition position() { return this.position; }
        final int value() { return this.value; }
    }

    private static final class ValueComparator<Square> implements Comparator<Square> {
        private final Map<Square, MoveData> values;
        ValueComparator(final Map<Square, MoveData> values) {
            this.values = values;
        }
        @SuppressWarnings(value = "unchecked")
        public int compare(final Object object1,
                           final Object object2) {
            if (object1 == object2) { return 0; }
            rcrr.reversi.Square sq0 = (rcrr.reversi.Square) object1;
            rcrr.reversi.Square sq1 = (rcrr.reversi.Square) object2;
            final int v0 = values.get(sq0).value();
            final int v1 = values.get(sq1).value();
            if (v0 == v1) {
                final int ordinalSq0 = sq0.ordinal();
                final int ordinalSq1 = sq1.ordinal();
                return (ordinalSq0 > ordinalSq1) ? -1: +1;
            }
            if (v0 > v1) {
                return -1;
            } else {
                return +1;
            }
        }
    }

    private final SortedMap<Square, MoveData> dynamicSortedLegalMoves(final Player player, final Board board, final EvalFunction ef) {
        final List<Square> moves = board.legalMoves(player);
        final Map<Square, MoveData> values = new HashMap<Square, MoveData>();
        for (Square move : moves) {
            final GamePosition position = GamePosition.valueOf(board.makeMove(move, player), player.opponent());
            final MoveData data = new MoveData(position, - ef.eval(position));
            values.put(move, data);
        }
        final SortedMap<Square, MoveData> results = new TreeMap<Square, MoveData>(new ValueComparator<Square>(values));
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
     * @param position   the game position
     * @param achievable the upper bound
     * @param cutoff     the lower bound
     * @param ply        the search depth
     * @param ef         the evaluation function
     * @return a new search node
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
                                                       - moves.get(move).value())
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
