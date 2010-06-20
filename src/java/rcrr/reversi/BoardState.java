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

    public Boolean wouldFlip(Integer move, SquareState player, Direction dir) {
	int c = move + dir.delta();
	if (get(c) == SquareState.opponent(player) &&
	    null != findBracketingPiece(c + dir.delta(), player, dir)) {
	    return true;
	} else {
	    return false;
	}
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
	    if (wouldFlip(move, player, dir)) return true;
	}
 	return false;
    }

    public List<Integer> legalMoves(SquareState player) {
	List<Integer> legalMoves = new ArrayList<Integer>();
	for (Integer move : allSquares) {
	    if (isLegal(move, player)) legalMoves.add(move);
	}
	return legalMoves;
    }
	
    public static void main(String[] args) {
	System.out.println("BoardState: Start.");
	BoardState bs = BoardState.emptyBoard();
	System.out.println("BoardState: bs = " + bs);
	for (Integer i : allSquares) {
	    System.out.println("square[" + i + "]: " + bs.get(i));
	}
	
	bs = BoardState.initialBoard();
	bs.set(43, SquareState.BLACK);
	bs.set(44, SquareState.BLACK);
	BoardState bs1 = bs.copyBoard();
	for (int i = 0; i<100; i++) {
	    System.out.println("square[" + i + "]: " + bs1.get(i));
	}
	bs1.print();

	List<Integer> lm = bs1.legalMoves(SquareState.WHITE);
	System.out.println("lm: " + lm);
        
	System.out.println("BoardState: Stop.");
    }

}