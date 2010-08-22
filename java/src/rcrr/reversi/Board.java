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
// - javadoc ....
// - complete an exhaustive test suite
// - polish, polish, polish ....

// makeMove has to check that the move is legal.
// findBracketingPiece must be private
// wouldFlip must be private

// makeMove, isLegal, legalMoves need an extensive test suit

// equal and hash method has to be written

public final class Board {

    private final Map<Square, SquareState> squares;

    // Has to be verified that returning OUTER on null is ok.
    /** Test ok */
    public SquareState get(Square sq) {
	if (sq == null) return SquareState.OUTER;
	return squares.get(sq);
    }

    // must be verified if the Map object is copied too much!
    private Board(Map<Square, SquareState> sm) {
	this.squares = Collections.unmodifiableMap(new EnumMap<Square, SquareState>(sm));
    }

    /** Test ok */
    public static Board valueOf(Map<Square, SquareState> sm) {
	return new Board(sm);
    }

    private static Map<Square, SquareState> emptyBoardSquares() {
	Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(Square.class);
	for (Square sq : Square.values()) {
	    sm.put(sq, SquareState.EMPTY);   
	}
	return sm;
    }

    public static Board emptyBoard() {
	return valueOf(emptyBoardSquares());
    }

    public static Board initialBoard() {
	Map<Square, SquareState> sm = emptyBoardSquares();
	sm.put(Square.getSquare(44), SquareState.WHITE);
	sm.put(Square.getSquare(45), SquareState.BLACK);
	sm.put(Square.getSquare(54), SquareState.BLACK);
	sm.put(Square.getSquare(55), SquareState.WHITE);
	return valueOf(sm);
    }
    
    /**
     * Returns a copy of the object.
     *<p>
     * Being the {@code Board} object immutable, the method returns a reference
     * of the object itself. The method does not appear a lot useful, but it helps
     * to remain consistent with the implementation of some clients.
     *
     * @return a copy of the {@code Board}
     */
    public Board copyBoard() {
	return this;
    }

    /**
     * Returns a new updated board to reflect move by player. This static
     * factory executes a game move to the board and returns a new one, reflecting
     * the move. The original board is not modified.
     * Currently it does not check if the move is legal. It must do it, and
     * throw an IllegalArgumentValue in case it is not.
     *
     * @param  move   the board square where to put the disk
     * @param  player the disk color to put on the board
     * @return a new {@code Board} reflecting the move made
     */
    public Board makeMove(Square move, Player player) {
	Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(squares);
	sm.put(move, player.color());
	for (Direction dir : Direction.values()) {
	    Square bracketer = wouldFlip(move, player, dir);
	    if (bracketer != null) {
		for (int c = move.position() + dir.delta(); true; c = c + dir.delta()) {
		    if (c == bracketer.position()) break;
		    sm.put(Square.getSquare(c), player.color());
		}
	    }
	}
	return valueOf(sm);
    }
    
    /** Test ok*/
    Square wouldFlip(Square move, Player player, Direction dir) {
	//System.out.println("wouldFlip: move=" + move + ", player=" + player + ", dir=" + dir);
	int c = move.position() + dir.delta();
	Square sc = Square.getSquare(c);
	//System.out.println("sc=" + sc);
	Square bp = null;
	if (get(sc) == player.opponent().color()) {
	    bp = findBracketingPiece(Square.getSquare(c + dir.delta()), player, dir);
	}
	return bp;
    }
    
    /** Test ok*/
    public Boolean isLegal(Square move, Player player) {
	if (get(move) != SquareState.EMPTY) return false;
	for (Direction dir : Direction.values()) {
	    if (wouldFlip(move, player, dir) != null) return true;
	}
 	return false;
    }

    /** Test ok*/
    Square findBracketingPiece(Square square, Player player, Direction dir) {
	//System.out.println("findBracketingPiece: square=" + square + ", player=" + player + ", dir=" + dir);
	if (get(square) == player.color()) {
	    return square;
	} else if (get(square) == player.opponent().color()) {
	    return findBracketingPiece(Square.getSquare(square.position() + dir.delta()), player, dir);
	} else {
	    return null;
	}
    }

    /** Test ok*/
    public Integer countPieces(SquareState color) {
	int count = 0;
	for (SquareState ss : squares.values()) {
	    if (ss == color) count++;
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
		String p = get(Square.getSquare(idx)).toString();
		ps.print(p + " ");
	    }
	}
	if (clock != null) {
	    ps.print("\n");
	    ps.print("\tClock: " + clock);
	}
	ps.print("\n\n");
    }

    // should be removed!
    /** Test ok*/
    public static Boolean isValid(Square move) {
	if (move != null) { return true; } else { return false; }
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
	for (Square move : Square.values()) {
	    if (isLegal(move, player)) {
		b = true;
		break;
	    }
	}
	return b;
    }

    /** Test ok*/
    public List<Square> legalMoves(Player player) {
	List<Square> legalMoves = new ArrayList<Square>();
	for (Square move : Square.values()) {
	    if (isLegal(move, player)) legalMoves.add(move);
	}
	return legalMoves;
    }

}