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

    /**
     * The minimax function.
     * <p>
     * Polishing:
     * - ply == 0 and finalState cases has to be merged.
     * - standard case is a bit ugly.
     * - pass should be included into the standard case.
     */
    private static Node minimax(final Player player, final Board board, final int ply, final EvalFunction ef) {
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
		if (!gameState.hasAnyLegalMove()) return null;
		Node node = minimax(gameState.player(), gameState.board(), ply, ef);
		return node.move();
	    }
	};
    }

    /**
     * The alpha-beta function.
     * <p>
     * Polish as per the minimax method, and write a complete javadoc.
     */
    private static Node alphabeta(Player player, Board board, int achievable, int cutoff, int ply, EvalFunction ef) {
	Node ab;
	Player opponent = player.opponent();
	if (ply == 0) {
	    ab = new Node(null, ef.eval(player, board));
	} else {
	    List<Square> moves = board.legalMoves(player);
	    if (moves.isEmpty()) {
		if (board.hasAnyLegalMove(opponent)) {
		    ab = alphabeta(opponent, board, - cutoff, - achievable, ply - 1, ef).minus();
		} else {
		    ab = new Node(null, finalValue(board, player));
		}
	    } else {
		ab = new Node(moves.get(0), achievable);
		outer: for (Square move : moves) {
		    Board board2 = board.makeMove(move, player);
		    int val = alphabeta(opponent, board2, - cutoff, - ab.value(), ply - 1, ef).minus().value();
		    if (val > ab.value()) {
			ab = new Node(move, val);
		    }
		    if (ab.value() >= cutoff) break outer;
		}
	    }
	}
	return ab;
    }

    /**
     * The alpha-beta searcher function.
     * <p>
     * It has to be refactor merging alphabetaSearcher and minimaxSearcher into one single searcher interface.
     */
    public static Strategy alphabetaSearcher(final int ply, final EvalFunction ef) {
	if (ply <= 0) throw new IllegalArgumentException("Parameter ply must be greather than zero. ply=" + ply);
	if (ef == null) throw new NullPointerException("Parameter ef must not null. ef=" + ef);
	return new Strategy() {
	    public Square move(GameState gameState) {
		if (!gameState.hasAnyLegalMove()) return null;
		Node ab = alphabeta(gameState.player(), gameState.board(), LOSING_VALUE, WINNING_VALUE, ply, ef);
		return ab.move();
	    }
	};
    }

    /**
     * Returns the board final value.
     * <p>
     * Should be part of any eval function (or a service).
     *
     * @param board  the final board
     * @param player the player for wich the value is calulated
     * @return       the game final value
     */
    private static int finalValue(final Board board, final Player player) {
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
	    public Square move(GameState gameState) {
		int value = LOSING_VALUE;
		Square move = null;
		Player player = gameState.player();
		Board board = gameState.board();
		for (Square tentativeMove : board.legalMoves(player)) {
		    int moveValue = ef.eval(player, board.makeMove(tentativeMove, player));
		    if (moveValue > value) {
			value = moveValue;
			move = tentativeMove;
		    }
		}
		return move;
	    }
	};
    }

    /**
     * A static inner class of Minimax, representing one node into the
     * search tree.
     * <p>
     * {@code Node} is immutable.
     * <p>
     * Has to be fully investigated if:
     * - register also the (board, player) tuple.
     * - organize nodes into a real tree (taken from a library or here developed).
     * - organize the value field as a "stack" of values obtained deepening the search.
     */
    private static class Node {

	/** The move field. */
	private final Square move;

	/** The value field. */
	private final int value;

	/** Private constructor. */
	private Node(final Square move, final int value) {
	    this.move = move;
	    this.value = value;
	}
	
	/** Getter method for move field. */
	private Square move() { return move; }

	/** Getter method for value field. */
	private int value() { return value; }

	/** Returns a new node having the value sign changed. */
	private Node minus() { return new Node(move, - value); }
	
	/**
	 * Returns a String representing the {@code Node} object.
	 * <p>
	 * The format is: {@code [move=b4, value=567]}
	 * 
	 * @return a string showing the minimax's node move and value fields
	 */
	@Override
	public String toString() {
	    return "[move=" + move + ", value=" + value + "]";
	}

    } 

}
