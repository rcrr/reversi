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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

/**
 * Minimax provides some static methods that return {@code Strategy}
 * objects. These searcher methods differ for the algorithm implementation.
 * <p>
 * The Minimax family of algorithms is described by the wikipedia
 * page: <a href="http://en.wikipedia.org/wiki/Minimax">Minimax</a>.
 * <p>
 * <ul>
 *   <li>{@code minimaxSearcher}</li>
 *   <li>{@code alphabetaSearcher}</li>
 * </ul>
 * Javadocs, Unit tests, and semplification are under construction.
 * <p>
 * Must be transformed to be immutable and not instantiable.
 */
public final class Minimax {

    /** 
     * The winning value.
     * Integer.MAX_VALUE = 2^31-1 = 2,147,483,647
     * Leaving enough space and having an easy to recognize 
     * number leads to a value of 2000000032.
     */
    private static final int WINNING_VALUE = + 2000000032;

    /** The losing value. */
    private static final int LOSING_VALUE = - 2000000032;

    /** The move field. */
    private Square move;

    /** The value field. Must be transformed into int. */
    private int value;

    private Square getMove() { return move; }
    private void setMove(Square move) { this.move = move; }
    private int getValue() { return value; }
    private void setValue(int value) { this.value = value; }

    private Minimax(Square move, Integer value) {
	setMove(move);
	setValue(value);
    }

    private Minimax minus() {
	return new Minimax(getMove(), - getValue());
    }

    /**
     * Returns a String representing the {@code Minimax} object.
     * <p>
     * The format is: {@code [move=b4, value=567]}
     * 
     * @return a string showing the minimax's node move and value fields
     */
    @Override
    public String toString() {
	return "[move=" + move + ", value=" + value + "]";
    }

    /**
     * The minimax function.
     */
    static Node minimax(final Player player, final Board board, final int ply, final EvalFunction ef) {
	Node node;
	final Player opponent = player.opponent();
	if (ply == 0) {
	    node = new Node(null, ef.eval(player, board));
	} else {
	    List<Square> moves = board.legalMoves(player);
	    if (moves.isEmpty()) {
		if (board.hasAnyLegalMove(opponent)) {
		    node = minimax(opponent, board, ply - 1, ef).minus();
		} else {
		    node = new Node(null, finalValue(board, player));
		}
	    } else {
		node = new Node(null, Integer.MIN_VALUE);
		for (Square move : moves) {
		    int value = minimax(opponent, board.makeMove(move, player), ply - 1, ef).minus().value();
		    if (value > node.value()) {
			node = new Node(move, value);
		    }
		}
	    }
	}
	return node;
    }

    /**
     * The minimax searcher function.
     *
     * @param ply the depth of the search
     * @param ef  the evaluation function
     * @return    a new {@code Strategy} instance
     * @throws    IllegalArgumentException when the parameter ply is less that one
     */
    public static Strategy minimaxSearcher(final int ply, final EvalFunction ef) {
	if (ply <= 0) throw new IllegalArgumentException("Parameter ply must be greather than zero. ply=" + ply);
	if (ef == null) throw new NullPointerException("Parameter ef must not null. ef=" + ef);
	return new Strategy() {
	    public Square move(GameState gameState) {
		Node node = minimax(gameState.player(), gameState.board(), ply, ef);
		return node.move();
	    }
	};
    }

    /**
     * The alpha-beta function.
     */
    static Minimax alphabeta(Player player, Board board, int achievable, int cutoff, int ply, EvalFunction ef) {
	Minimax ab = null;
	Player opponent = player.opponent();
	if (ply == 0) {
	    ab = new Minimax(null, ef.eval(player, board));
	} else {
	    List<Square> moves = board.legalMoves(player);
	    if (moves.isEmpty()) {
		if (board.hasAnyLegalMove(opponent)) {
		    ab = alphabeta(opponent, board, - cutoff, - achievable, ply - 1, ef).minus();
		} else {
		    ab = new Minimax(null, finalValue(board, player));
		}
	    } else {
		ab = new Minimax(moves.get(0), achievable);
		outer: for (Square move : moves) {
		    Board board2 = board.makeMove(move, player);
		    int val = alphabeta(opponent, board2, - cutoff, - ab.getValue(), ply - 1, ef).minus().getValue();
		    if (val > ab.getValue()) {
			ab.setValue(val);
			ab.setMove(move);
		    }
		    if (ab.getValue() >= cutoff) break outer;
		}
	    }
	}
	return ab;
    }

    /**
     * The alpha-beta searcher function.
     */
    public static Strategy alphabetaSearcher(final int ply, final EvalFunction ef) {
	return new Strategy() {
	    public Square move(GameState gameState) {
		Minimax ab = alphabeta(gameState.player(), gameState.board(), LOSING_VALUE, WINNING_VALUE, ply, ef);
		return ab.getMove();
	    }
	};
    }

    /**
     * Must check that parameters are not null.
     */
    private static int finalValue(final Board board, final Player player) {
	switch (Integer.signum(board.countDifference(player))) {
	case -1: return LOSING_VALUE;
	case  0: return 0;
	case +1: return WINNING_VALUE;
	default: throw new RuntimeException("Unreachable condition found. player=" + player + ", board=" + board);
	}
    }

    /**
     * Should be moved into a StrategyUtils ideal class.
     * This is anyhow a better place than Board.
     *
     * should be tested. And tested.
     * Transform the List into an Array.
     */
    public static Strategy maximizer(final EvalFunction ef) {
	return new Strategy() {
	    public Square move(GameState gameState) {
		Player player = gameState.player();
		Board board = gameState.board();
		List<Square> moves = board.legalMoves(player);
		List<Integer> scores = new ArrayList<Integer>();
		for (Square move : moves) {
		    Board newBoard = board.copyBoard();
		    newBoard.makeMove(move, player);
		    Integer score = ef.eval(player, newBoard);
		    scores.add(score);
		}
		Integer best = Collections.max(scores);
		return moves.get(scores.indexOf(best));
	    }
	};
    }

    private static class Node {
	private final Square move;
	private final int value;
	Node(final Square move, final int value) {
	    this.move = move;
	    this.value = value;
	}
	Square move() { return move; }
	int value() { return value; }
	Node minus() { return new Node(move, - value); }
    } 

}
