/*
 *  AbstractDecisionRule.java
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

/**
 * This abstract class provide a searcher implementation
 * as required by the {@code DecisionRule} interface, leaving
 * to the extending classes the requirement to implement the
 * other methods defined by the interface (e.g. the search method).
 */
public abstract class AbstractDecisionRule implements DecisionRule {

    /**
     * The winning value.
     * <p>
     * Integer.MAX_VALUE as defined by the java language specification is
     * equal to {@code 2^31-1}, or {@code 2,147,483,647}.
     * <p>
     * {@code WINNING_VALUE} is than set to {@code +2,000,000,000},
     * that is large enought to leave space for an evaluation function
     * to have "intermediate" values.
     */
    protected static final int WINNING_VALUE = +2000000000;

    /**
     * The losing value.
     * <p>
     * {@code LOSING_VALUE} is set to {@code -2,000,000,000},
     */
    protected static final int LOSING_VALUE = -2000000000;

    /**
     * Returns the board final value.
     * <p>
     * Should be part of any eval function (or a service).
     *
     * @param board  the final board
     * @param player the player for wich the value is calulated
     * @return       the game final value
     */
    protected static int finalValue(final Board board, final Player player) {
        assert (board != null) : "Parameter board must be not null";
        assert (player != null) : "Parameter player must be not null";
        switch (Integer.signum(board.countDifference(player))) {
        case -1: return LOSING_VALUE;
        case  0: return 0;
        case +1: return WINNING_VALUE;
        default: throw new RuntimeException("Unreachable condition found. player=" + player + ", board=" + board);
        }
    }

    /**
     * Returns a {@code Strategy} that maximixes the value obtained
     * applying the evaluation function to the avalilable legal moves.
     * <p>
     * The method is equivalent to call a searcher giving a one ply depth
     * search.
     *
     * @param ef evaluation function
     * @return   a strategy maximizing the value of the legal moves
     */
    public static Strategy maximizer(final EvalFunction ef) {
        return new Strategy() {
            public Move move(final GameSnapshot gameSnapshot) {
                if (gameSnapshot == null) {
                    throw new NullPointerException("Parameter gameSnapshot cannot be null.");
                }
                if (!gameSnapshot.hasAnyLegalMove()) {
                    return Move.valueOf(Move.Action.PASS);
                }
                int value = LOSING_VALUE;
                Square move = null;
                Player player = gameSnapshot.player();
                Board board = gameSnapshot.board();
                for (Square tentativeMove : board.legalMoves(player)) {
                    GamePosition gp = GamePosition.valueOf(board.makeMove(tentativeMove, player), player);
                    int moveValue = ef.eval(gp);
                    if (moveValue > value) {
                        value = moveValue;
                        move = tentativeMove;
                    }
                }
                if (move == null) {
                    return Move.valueOf(Move.Action.PASS);
                } else {
                    return Move.valueOf(move);
                }
            }
        };
    }

    /** Class constructor. */
    public AbstractDecisionRule() { }

    /**
     * Returns a new strategy that searches ply levels deep
     * applying the ef evaluation function.
     *
     * @param ply the search depth reached
     * @param ef  the evaluation function
     * @return a strategy
     */
    public final Strategy searcher(final int ply, final EvalFunction ef) {
        if (ply <= 0) { throw new IllegalArgumentException("Parameter ply must be greather than zero. ply=" + ply); }
        if (ef == null) { throw new NullPointerException("Parameter ef must be not null."); }
        return new Strategy() {
            public Move move(final GameSnapshot gameSnapshot) {
                if (gameSnapshot == null) {
                    throw new NullPointerException("Parameter gameSnapshot cannot be null.");
                }
                if (!gameSnapshot.hasAnyLegalMove()) {
                    return Move.valueOf(Move.Action.PASS);
                }
                SearchNode node = search(gameSnapshot.position(),
                                         ply,
                                         ef);
                return Move.valueOf(node.move());
            }
        };
    }

}
