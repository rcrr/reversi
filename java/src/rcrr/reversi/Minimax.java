/*
 *  Minimax.java
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

package rcrr.reversi;

import java.util.List;

/**
 * Minimax provides some static methods that return {@code Strategy}
 * objects. These searcher methods differ for the algorithm implementation.
 * <p>
 * The Minimax family of algorithms is described by the wikipedia
 * page: <a href="http://en.wikipedia.org/wiki/Minimax">Minimax</a>.
 * <p>
 */
public final class Minimax extends AbstractDecisionRule {

    /** Class constructor. */
    private Minimax() { };

    /**
     * Class static factory.
     *
     * @return a new minimax instance
     */
    public static Minimax getInstance() {
        return new Minimax();
    }

    /**
     * The minimax function.
     * <p>
     * Polishing:
     * <ul>
     *   <li>ply == 0 and finalState cases has to be merged.</li>
     *   <li>standard case is a bit ugly.</li>
     *   <li>pass should be included into the standard case.</li>
     * </ul>
     *
     * @param player     the player that has the move
     * @param board      the reached board
     * @param achievable not used
     * @param cutoff     not used
     * @param ply        the search depth reached
     * @param ef         the evaluation function
     * @return           a node in the search tree
     */
    public SearchNode search(final Player player,
                             final Board board,
                             final int achievable,
                             final int cutoff,
                             final int ply,
                             final EvalFunction ef) {
        SearchNode node;
        final Player opponent = player.opponent();
        if (ply == 0) {
            node = new SearchNode(null, ef.eval(GamePosition.valueOf(board, player)));
        } else {
            List<Square> moves = board.legalMoves(player);
            if (moves.isEmpty()) {
                if (board.hasAnyLegalMove(opponent)) {
                    node = search(opponent, board, 0, 0, ply - 1, ef).negated();
                } else {
                    node = new SearchNode(null, finalValue(board, player));
                }
            } else {
                node = new SearchNode(null, Integer.MIN_VALUE);
                for (Square move : moves) {
                    int value = search(opponent, board.makeMove(move, player),
                                       0, 0,
                                       ply - 1, ef).negated().value();
                    if (value > node.value()) {
                        node = new SearchNode(move, value);
                    }
                }
            }
        }
        return node;
    }

}
