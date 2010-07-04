/*
 *  BoardState.java
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
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

import java.io.PrintStream;

public class BoardState {

    public static final List<Integer> ALL_SQUARES = Square.allSquares();

    public static final List<List<Integer>> NEIGHBOR_TABLE = neighborTable();

    private static List<List<Integer>> neighborTable() {
	List<List<Integer>> nt = new ArrayList<List<Integer>>(100);
	for (int i=0; i<100; i++) nt.add(new ArrayList<Integer>());
	for (Integer square : ALL_SQUARES) {
	    List<Integer> nl = new ArrayList<Integer>();
	    for (Direction dir : Direction.values()) {
		Integer neighbor = square + dir.delta();
		if (isValid(neighbor)) nl.add(neighbor);
	    }
	    nt.set(square, nl);
	}
	return nt;
    }

    public static List<Integer> neighbors(Integer square) {
	return NEIGHBOR_TABLE.get(square);
    }

    public static final int WINNING_VALUE = Integer.MAX_VALUE;
    public static final int LOSING_VALUE = - Integer.MAX_VALUE;

    private List<SquareState> squares;

    private BoardState() {
	this.squares = new ArrayList<SquareState>();
	for (int i=0; i<100; i++) {
	    if (ALL_SQUARES.contains(i)) {
		squares.add(SquareState.EMPTY);
	    } else {
		squares.add(SquareState.OUTER);
	    }
	}
    }

    public BoardState copyBoard() {
	BoardState bs = emptyBoard();
	for (Integer i : ALL_SQUARES) {
	    bs.set(i, squares.get(i));
	}
	return bs;
    }

    private void setInitialDisks() {
	squares.set(44, SquareState.WHITE);
	squares.set(45, SquareState.BLACK);
	squares.set(54, SquareState.BLACK);
	squares.set(55, SquareState.WHITE);
    }

    public static BoardState emptyBoard() { return new BoardState(); }

    public static BoardState initialBoard() {
	BoardState ib = new BoardState();
	ib.setInitialDisks();
	return ib;
    }

    public SquareState get(Integer index) {
	return squares.get(index);
    }

    public void set(Integer index, SquareState ss) {
	    squares.set(index, ss);
    }

    public Integer count(SquareState player) {
	int count = 0;
	for (int i=0; i<100; i++) {
	    if (squares.get(i) == player) count++;
	}
	return new Integer(count);
    }

    public Integer countDifference(SquareState player) {
	return count(player) - count(SquareState.opponent(player));
    }

    public void print() {
	print(System.out);
    }

    public void print(PrintStream ps) {
	Integer cb = count(SquareState.BLACK);
	Integer cw = count(SquareState.WHITE);
	Integer cd = cb - cw;
	ps.print("    1 2 3 4 5 6 7 8 [@=" + cb + " 0=" + cw + " (" + cd + ")]");
	for (int row=1; row<9; row++) {
	    ps.print("\n " + row + "  ");
	    for (int col=1; col<9; col++) {
		int idx = (row * 10) + col;
		String p = squares.get(idx).toString();
		ps.print(p + " ");
	    }
	}
	ps.print("\n\n");
    }

    public Integer wouldFlip(Integer move, SquareState player, Direction dir) {
	int c = move + dir.delta();
	Integer bp = null;
	if (get(c) == SquareState.opponent(player)) {
	    bp = findBracketingPiece(c + dir.delta(), player, dir);
	}
	return bp;
    }

    public Integer findBracketingPiece(Integer square, SquareState player, Direction dir) {
	if (get(square) == player) {
	    return square;
	} else if (get(square) == SquareState.opponent(player)) {
	    return findBracketingPiece(square + dir.delta(), player, dir);
	} else {
	    return null;
	}
    }

    public static Boolean isValid(Integer move) {
	return ALL_SQUARES.contains(move);
    }

    public Boolean isLegal(Integer move, SquareState player) {
	if (squares.get(move) != SquareState.EMPTY) return false;
	if (!(player == SquareState.BLACK || player == SquareState.WHITE)) return false;
	for (Direction dir : Direction.values()) {
	    if (wouldFlip(move, player, dir) != null) return true;
	}
 	return false;
    }

    public void makeMove(Integer move, SquareState player) {
	set(move, player);
	for (Direction dir : Direction.values()) {
	    makeFlips(move, player, dir);
	}
    }

    public void makeFlips(Integer move, SquareState player, Direction dir) {
	Integer bracketer = wouldFlip(move, player, dir);
	if (bracketer != null) {
	    for (int c = move + dir.delta(); true; c = c + dir.delta()) {
		if (c == bracketer) break;
		set(c, player);
	    }
	}
    }

    public SquareState nextToPlay(SquareState previousPlayer, PrintStream ps) {
	SquareState opponent = SquareState.opponent(previousPlayer);
	SquareState next = null;
	if (anyLegalMove(opponent)) {
	    next = opponent;
	} else if (anyLegalMove(previousPlayer)) {
	    next = previousPlayer;
	    if (ps != null) {
		ps.print("\n" + opponent + " has no moves and must pass.\n");
	    }
	}
	return next;
    }

    public Boolean anyLegalMove (SquareState player) {
	Boolean b = false;
	for (Integer move : ALL_SQUARES) {
	    if (isLegal(move, player)) {
		b = true;
		break;
	    }
	}
	return b;
    }

    public Integer getMove(Strategy strategy, SquareState player, PrintStream ps) {
	if (ps != null) print(ps);
	Integer move = strategy.move(player, copyBoard());
	if (isValid(move) && isLegal(move, player)) {
	    if (ps != null) {
		ps.print("\n" + player.name() + " moves to " + move + "\n");
		makeMove(move, player);
	    }
	    return move;
	} else {
	    if (ps != null) ps.print("Illegal move: " + move + "\n");
	    return getMove(strategy, player, ps);
	}
    }

    public static Integer reversi(Strategy blStrategy, Strategy whStrategy, PrintStream ps) {
	BoardState board = initialBoard();
	Strategy strategy = null;
	Boolean gameOver = false;
	for (SquareState player = SquareState.BLACK;
	     player != null;
	     player = board.nextToPlay(player, ps)) {
	    if (player == SquareState.BLACK) {
		strategy = blStrategy;
	    } else {
		strategy = whStrategy;
	    }
	    board.getMove(strategy, player, ps);
	}
	if (ps != null) {
	    ps.print("\nThe Game is over. Final result:\n\n");
	    board.print(ps);
	}
	return board.countDifference(SquareState.BLACK);
    }

    public List<Integer> legalMoves(SquareState player) {
	List<Integer> legalMoves = new ArrayList<Integer>();
	for (Integer move : ALL_SQUARES) {
	    if (isLegal(move, player)) legalMoves.add(move);
	}
	return legalMoves;
    }

    public static Strategy maximizer(final EvalFunction ef) {
	return new Strategy() {
	    public Integer move(SquareState player, BoardState board) {
		List<Integer> moves = board.legalMoves(player);
		List<Integer> scores = new ArrayList<Integer>();
		for (Integer move : moves) {
		    BoardState newBoard = board.copyBoard();
		    newBoard.makeMove(move, player);
		    Integer score = ef.eval(player, newBoard);
		    scores.add(score);
		}
		Integer best = Collections.max(scores);
		return moves.get(scores.indexOf(best));
	    }
	};
    }

    public Integer finalValue(SquareState player) {
	Integer value = null;
	switch (Integer.signum(countDifference(player))) {
	case -1: value = LOSING_VALUE; break;
	case  0: value = 0; break;
	case +1: value = WINNING_VALUE; break;
	}
	return value;
    }

    private static void usage() {
	System.out.println("usage: java rcrr.reversi.BoardState blackStrategy whiteStrategy");
	System.out.println("\t Where blackStrategy and whiteStrategy are two classes");
	System.out.println("\t that implements the rcrr.reversi.Strategy interface.");
    }
	
    public static void main(String[] args) {
	if (args == null || args.length != 2) {
	    System.out.println("Argument list error: blackStrategy and whiteStrategy must be provided.");
	    usage();
	    System.exit(1);
	}
	Strategy s[] = new Strategy[]{null, null};
	for (int i=0; i<2; i++) {
	    Object o = null;
	    try {
		Class<?> c = Class.forName(args[i]);
		o = c.newInstance();
	    } catch (ClassNotFoundException e) {
		System.out.println("Exception e: " + e);
		usage();
		System.exit(2);
	    } catch (InstantiationException e) {
		System.out.println("Exception e: " + e);
		usage();
		System.exit(3);
	    } catch (IllegalAccessException e) {
		System.out.println("Exception e: " + e);
		usage();
		System.exit(4);
	    }
	    try {
		s[i] = (Strategy) o;
	    } catch (ClassCastException e) {
		System.out.println("Exception e: " + e);
		usage();
		System.exit(5);
	    }
	}
	reversi(s[0], s[1], System.out);
    }

}