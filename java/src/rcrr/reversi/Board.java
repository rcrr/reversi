/*
 *  Board.java
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
import java.util.EnumMap;

import java.io.PrintStream;

// To do:
// - change the List into an Map<Square, SquareState>
// - Remove the Abstract Class and the Interface. Move all the code into a Board Class.
// - remove the copyBoard() method (Board is now immutable ...)
// - javadoc ....
// - complete an exhaustive test suite
// - polish, polish, polish ....

public final class Board {

    private final List<SquareState> squares;

    List<SquareState> squares() { throw new UnsupportedOperationException(); }

    /** Test ok */
    public SquareState get(Integer index) {
	return squares.get(index);
    }

    private Board(List<SquareState> ssl) {
	this.squares = Collections.unmodifiableList(new ArrayList<SquareState>(ssl));
    }

    /** Test ok */
    public static Board valueOf(List<SquareState> ssl) {
	return new Board(ssl);
    }

    private static List<SquareState> emptyBoardList() {
	List<SquareState> ssl = new ArrayList<SquareState>();
	for (int i=0; i<100; i++) {
	    if (Square.ALL_SQUARES.contains(i)) {
		ssl.add(SquareState.EMPTY);
	    } else {
		ssl.add(SquareState.OUTER);
	    }
	}
	return ssl;
    }

    public static Board emptyBoard() {
	return valueOf(emptyBoardList());
    }

    public static Board initialBoard() {
	List<SquareState> ssl = emptyBoardList();
	ssl.set(44, SquareState.WHITE);
	ssl.set(45, SquareState.BLACK);
	ssl.set(54, SquareState.BLACK);
	ssl.set(55, SquareState.WHITE);
	return valueOf(ssl);
    }
    
    /** Why not just return this!*/
    public Board copyBoard() {
	return valueOf(squares);
    }

    /**
     * Returns a new updated board to reflect move by player. This static
     * factory executes a game move to the board and returns a new one, reflecting
     * the move. The original board is not modified.
     *
     * @param  move   an integer that points to the board square where to put the disk
     * @param  player the disk color to put on the board
     * @return a new {@code Board} reflecting the move made.
     */
    public Board makeMove(Integer move, Player player) {
	List<SquareState> ssl = new ArrayList<SquareState>(squares);
	ssl.set(move, player.color());
	for (Direction dir : Direction.values()) {
	    Integer bracketer = wouldFlip(move, player, dir);
	    if (bracketer != null) {
		for (int c = move + dir.delta(); true; c = c + dir.delta()) {
		    if (c == bracketer) break;
		    ssl.set(c, player.color());
		}
	    }
	}
	return valueOf(ssl);
    }
    
    /** Test ok*/
    Integer wouldFlip(Integer move, Player player, Direction dir) {
	int c = move + dir.delta();
	Integer bp = null;
	if (get(c) == player.opponent().color()) {
	    bp = findBracketingPiece(c + dir.delta(), player, dir);
	}
	return bp;
    }

    /** Test ok*/
    public Boolean isLegal(Integer move, Player player) {
	if (get(move) != SquareState.EMPTY) return false;
	for (Direction dir : Direction.values()) {
	    if (wouldFlip(move, player, dir) != null) return true;
	}
 	return false;
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

    /** Test ok*/
    public List<Integer> legalMoves(Player player) {
	List<Integer> legalMoves = new ArrayList<Integer>();
	for (Integer move : Square.ALL_SQUARES) {
	    if (isLegal(move, player)) legalMoves.add(move);
	}
	return legalMoves;
    }

}