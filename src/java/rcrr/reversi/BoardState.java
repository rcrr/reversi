 /*
    Copyright (c) 2010 Roberto Corradini

    This file is part of the reversi program
    http://github.com/rcrr/reversi

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 3, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
    or visit the site <http://www.gnu.org/licenses/>.
*/

/*

Still missing:
 - reversi

Then the connection with the swing GUI and the printing facility.
And then refactoring, documentation, javadocs.

*/


package rcrr.reversi;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

import java.io.PrintStream;

public class BoardState {

    public static final List<Integer> allSquares = Arrays.asList(11,12,13,14,15,16,17,18,
								 21,22,23,24,25,26,27,28,
								 31,32,33,34,35,36,37,38,
								 41,42,43,44,45,46,47,48,
								 51,52,53,54,55,56,57,58,
								 61,62,63,64,65,66,67,68,
								 71,72,73,74,75,76,77,78,
								 81,82,83,84,85,86,87,88);

    private List<SquareState> squares;

    private BoardState() {
	this.squares = new ArrayList<SquareState>();
	for (int i=0; i<100; i++) {
	    if (allSquares.contains(i)) {
		squares.add(SquareState.EMPTY);
	    } else {
		squares.add(SquareState.OUTER);
	    }
	}
    }

    public BoardState copyBoard() {
	BoardState bs = emptyBoard();
	for (Integer i : allSquares) {
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

    public Boolean isValid(Integer move) {
	return allSquares.contains(move);
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
	for (Integer move : allSquares) {
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
	for (Integer move : allSquares) {
	    if (isLegal(move, player)) legalMoves.add(move);
	}
	return legalMoves;
    }
	
    public static void main(String[] args) {
	if (args == null || args.length != 2) {
	    System.out.println("Argument list error ...");
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
		System.exit(2);
	    } catch (InstantiationException e) {
		System.out.println("Exception e: " + e);
		System.exit(3);
	    } catch (IllegalAccessException e) {
		System.out.println("Exception e: " + e);
		System.exit(4);
	    }
	    try {
		s[i] = (Strategy) o;
	    } catch (ClassCastException e) {
		System.out.println("Exception e: " + e);
		System.exit(5);
	    }
	}
	reversi(s[0], s[1], System.out);
    }

}