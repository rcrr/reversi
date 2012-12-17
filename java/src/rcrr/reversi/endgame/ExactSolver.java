/*
 *  ExactSolver.java
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

package rcrr.reversi.endgame;

import java.util.List;

import rcrr.reversi.CountDifference;
import rcrr.reversi.EvalFunction;
import rcrr.reversi.SearchNode;
import rcrr.reversi.GamePosition;

import rcrr.reversi.board.Player;
import rcrr.reversi.board.Board;
import rcrr.reversi.board.Square;

/**
 * Exact solver searches the end of the game for an exact outcome.
 */
public class ExactSolver {

    /**
     * Returns the board final value.
     *
     * @param board  the final board
     * @param player the player for wich the value is calulated
     * @return       the game final value
     */
    protected static int finalValue(final Board board, final Player player) {
        return board.countDifference(player);
    }

    public ExactSolver() { }

    public boolean dummy() { return true; }

    public SearchNode solve(final GamePosition position) {
        final EvalFunction ef = new CountDifference();
        return solveImpl(position.player(), position.board(), -64, +64, 60, ef);
    }

    /**
     * Implemented by means of the alpha-beta algorithm.
     *
     * @param player     the player having the move
     * @param board      the board
     * @param achievable the search window lower bound (also know as alpha)
     * @param cutoff     the search window upper bound (also know as beta)
     * @param ply        the search depth
     * @param ef         the evaluation function
     * @return a new search node
     */
    public SearchNode solveImpl(final Player player,
                                final Board board,
                                final int achievable,
                                final int cutoff,
                                final int ply,
                                final EvalFunction ef) {
        SearchNode node;
        final Player opponent = player.opponent();
        if (ply == 0) {
            node = SearchNode.valueOf(null, ef.eval(GamePosition.valueOf(board, player)));
        } else {
            final List<Square> moves = board.legalMoves(player);
            if (moves.isEmpty()) {
                if (board.hasAnyLegalMove(opponent)) {
                    node = solveImpl(opponent, board, -cutoff, -achievable, ply - 1, ef).negated();
                } else {
                    node = SearchNode.valueOf(null, finalValue(board, player));
                }
            } else {
                node = SearchNode.valueOf(moves.get(0), achievable);
                outer: for (Square move : moves) {
                    Board board2 = board.makeMove(move, player);
                    int val = solveImpl(opponent, board2, -cutoff, -node.value(), ply - 1, ef).negated().value();
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
