/*
 *  AbstractBoard.java
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

abstract class AbstractBoard implements Board {

    abstract public SquareState get(Integer index);

    abstract List<SquareState> squares();
 
   /** Test ok*/
    public Integer wouldFlip(Integer move, Player player, Direction dir) {
	int c = move + dir.delta();
	Integer bp = null;
	if (get(c) == player.opponent().color()) {
	    bp = findBracketingPiece(c + dir.delta(), player, dir);
	}
	return bp;
    }

   /** Test ok*/
    public Integer findBracketingPiece(Integer square, Player player, Direction dir) {
	if (get(square) == player.color()) {
	    return square;
	} else if (get(square) == player.opponent().color()) {
	    return findBracketingPiece(square + dir.delta(), player, dir);
	} else {
	    return null;
	}
    }

   /** Test ok*/
    public Integer countPieces(SquareState color) {
	int count = 0;
	for (int i=0; i<100; i++) {
	    if (get(i) == color) count++;
	}
	return new Integer(count);
    }

   /** Test ok*/
    public Integer countDifference(Player player) {
	return countPieces(player.color()) - countPieces(player.opponent().color());
    }

   /** Should go out of Board Class*/
    public void print() {
	print(System.out, null);
    }

   /** Should go out of Board Class*/
    public void print(PrintStream ps, Clock clock) {
	Integer cb = countPieces(SquareState.BLACK);
	Integer cw = countPieces(SquareState.WHITE);
	Integer cd = cb - cw;
	ps.print("    a b c d e f g h [@=" + cb + " 0=" + cw + " (" + cd + ")]");
	for (int row=1; row<9; row++) {
	    ps.print("\n " + row + "  ");
	    for (int col=1; col<9; col++) {
		int idx = (row * 10) + col;
		String p = get(idx).toString();
		ps.print(p + " ");
	    }
	}
	if (clock != null) {
	    ps.print("\n");
	    ps.print("\tClock: " + clock);
	}
	ps.print("\n\n");
    }

   /** Test ok*/
    public static Boolean isValid(Integer move) {
	return Square.ALL_SQUARES.contains(move);
    }

   /** Test ok*/
    public Boolean isLegal(Integer move, Player player) {
	if (get(move) != SquareState.EMPTY) return false;
	if (!(player.color() == SquareState.BLACK || player.color() == SquareState.WHITE)) return false;
	for (Direction dir : Direction.values()) {
	    if (wouldFlip(move, player, dir) != null) return true;
	}
 	return false;
    }

   /** Test ok*/
    public Player nextToPlay(Player previousPlayer, PrintStream ps) {
	Player opponent = previousPlayer.opponent();
	Player next = null;
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

   /** Test ok*/
    public Boolean anyLegalMove (Player player) {
	Boolean b = false;
	for (Integer move : Square.ALL_SQUARES) {
	    if (isLegal(move, player)) {
		b = true;
		break;
	    }
	}
	return b;
    }

    // Should be moved in another class.
    public static Game getMove(BoardState b, Strategy strategy, Player player, PrintStream ps, Clock clock) throws GameOverException {
	if (ps != null) b.print(ps, clock);
	long t0 = System.currentTimeMillis();
	Integer move = strategy.move(player, b.copyBoard());
	long t1 = System.currentTimeMillis();
	clock = clock.setTime(player, t1 - t0);
	if (b.isValid(move) && b.isLegal(move, player)) {
	    if (ps != null) {
		ps.print("\n" + player.name() + " moves to " + Square.getSquare(move).getDisplayName() + "\n");
	    }
	    BoardState b1 = b.makeMove(move, player);
	    return Game.valueOf(b1, b1.nextToPlay(player, null), clock);
	} else {
	    if (ps != null) ps.print("Illegal move: " + move + "\n");
	    return getMove(b, strategy, player, ps, clock);
	}
    }

   /** Test ok*/
    public List<Integer> legalMoves(Player player) {
	List<Integer> legalMoves = new ArrayList<Integer>();
	for (Integer move : Square.ALL_SQUARES) {
	    if (isLegal(move, player)) legalMoves.add(move);
	}
	return legalMoves;
    }

    // Should be moved in another class.
    public static Strategy maximizer(final EvalFunction ef) {
	return new Strategy() {
	    public Integer move(Player player, BoardState board) {
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

   /** Should be moved out of Board Class*/
    public Integer finalValue(Player player) {
	Integer value = null;
	switch (Integer.signum(countDifference(player))) {
	case -1: value = Reversi.LOSING_VALUE; break;
	case  0: value = 0; break;
	case +1: value = Reversi.WINNING_VALUE; break;
	}
	return value;
    }

}