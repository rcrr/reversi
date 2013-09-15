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
import java.util.Set;
import java.util.HashSet;

import rcrr.reversi.EvalEndgame;
import rcrr.reversi.EvalFunction;
import rcrr.reversi.SearchNode;
import rcrr.reversi.GamePosition;

import rcrr.reversi.board.Player;
import rcrr.reversi.board.Board;
import rcrr.reversi.board.SquareState;
import rcrr.reversi.board.Square;
import rcrr.reversi.board.BoardBuilder;

/**
 * Exact solver searches the end of the game for an exact outcome.
 */
public class ExactSolver {

    /** Error code 1. */
    private static final int ERROR_CODE_1 = 1;

    private static long nodeCount = 0;
    private static long leafCount = 0;

    private static EvalFunction evalEndgame = new EvalEndgame();

    /**
     * Returns the board final value.
     *
     * @param gp the game position to be evaluated
     * @return   the game final value
     */
    protected static int finalValue(final GamePosition gp) {
        return evalEndgame.eval(gp);
    }

    final private GamePosition root;

    public ExactSolver(final GamePosition root) {
        this.root  = root;
    }

    public SearchNode solve() {
        final SearchNode result = solveImpl(root.player(), root.board(), -64, +64, 60);
        System.out.println("[nodeCount=" + nodeCount + ", leafCount=" + leafCount + "]");
        return result;
    }

    /**
     * Implemented by means of the alpha-beta algorithm.
     *
     * @param player     the player having the move
     * @param board      the board
     * @param achievable the search window lower bound (also know as alpha)
     * @param cutoff     the search window upper bound (also know as beta)
     * @param ply        the search depth
     * @return           a new search node
     */
    public SearchNode solveImpl(final Player player,
                                final Board board,
                                final int achievable,
                                final int cutoff,
                                final int ply) {
        nodeCount++;
        final GamePosition gp = GamePosition.valueOf(board, player);
        SearchNode node;
        final Player opponent = player.opponent();
        final List<Square> moves = board.legalMoves(player);
        if (moves.isEmpty()) {
            if (board.hasAnyLegalMove(opponent)) {
                node = solveImpl(opponent, board, -cutoff, -achievable, ply - 1).negated();
            } else {
                leafCount++;
                node = SearchNode.valueOf(null, finalValue(gp));
            }
        } else {
            node = SearchNode.valueOf(moves.get(0), achievable);
            outer: for (final Square move : moves) {
                final Board board2 = board.makeMove(move, player);
                final int val = solveImpl(opponent, board2, -cutoff, -node.value(), ply - 1).negated().value();
                if (val > node.value()) {
                    node = SearchNode.valueOf(move, val);
                }
                    if (node.value() >= cutoff) { break outer; }
            }
        }
        return node;
    }

    /**
     * The main entry point for the Exact Solver program.
     *
     * @param args an array having two elements: [black's strategy, white's strategy]
     */
    public static void main(final String[] args) {
        if (args == null || args.length != 0) {
            System.out.println("Argument list error: blackStrategy and whiteStrategy must be provided.");
            usage();
            System.exit(ERROR_CODE_1);
        }

        System.out.printf("FF0_40:\nBlack to move, Turner vs Monnom, Bruxelles 1997.\n%s\n", FFO_40.board().printBoard());

        final SearchNode result = new ExactSolver(FFO_40).solve();

        System.out.printf("%s\n", result);
        
    }

    /**
     * Print the usage message.
     */
    private static void usage() {
        System.out.println("usage: java rcrr.reversi.endgame.ExactSolver");
    }

    /**
     * FFO position #40, black to move, Turner vs Monnom, Bruxelles 1997.
     * Principal Variation, PV: a2 b1 c1 -- b6 c7 a7 b7 b8 d7 f8 c6 f7 g7
     * Final score is +38
     */
    final static GamePosition FFO_40 = new GamePosition.Builder()
        .withBoard(new BoardBuilder()
                   .withSquaresLiteral(2, 0, 0, 2, 2, 2, 2, 1,
                                       0, 2, 2, 2, 2, 2, 2, 1,
                                       2, 2, 1, 1, 2, 2, 2, 1,
                                       2, 2, 1, 2, 2, 2, 1, 1,
                                       2, 2, 2, 2, 2, 2, 1, 1,
                                       0, 0, 0, 2, 2, 2, 2, 1,
                                       0, 0, 0, 0, 2, 0, 0, 1,
                                       0, 0, 0, 0, 0, 0, 0, 0)
                   .build())
        .withPlayer(Player.BLACK)
        .build();


}
