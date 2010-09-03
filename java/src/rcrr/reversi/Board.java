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
// - javadoc ....
// - complete an exhaustive test suite
// - polish, polish, polish ....

// makeMove has to check that the move is legal.
// findBracketingPiece must be private
// wouldFlip must be private

// makeMove, isLegal, legalMoves need an extensive test suit

// equal and hashCode() method has to be written

/**
 * An instance of a board game position, or state.
 * <p>
 * A {@code Board} object holds the information of the state of each board's square.
 * <p>
 * {@code Board} is immutable.
 */
public final class Board {

    /** The squares field. */
    private final Map<Square, SquareState> squares;

    // must be verified if the Map object is copied too much!
    private Board(Map<Square, SquareState> sm) {
	this.squares = Collections.unmodifiableMap(new EnumMap<Square, SquareState>(sm));
    }

    /**
     * Returns the {@code SquareState} value for the given board's square.
     * <p>
     * When {@code square} is {@code null} the method returns {@code SquareState.OUTER} value.
     *
     * @return the square state
     */
    public SquareState get(Square square) {
	SquareState ss;
	if (square == null) ss = SquareState.OUTER;
	else ss = squares.get(square);
	return ss;
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
	sm.put(Square.D4, SquareState.WHITE);
	sm.put(Square.E4, SquareState.BLACK);
	sm.put(Square.D5, SquareState.BLACK);
	sm.put(Square.E5, SquareState.WHITE);
	return valueOf(sm);
    }
    
    /**
     * Returns a copy of the board object.
     * <p>
     * Being the {@code Board} object immutable, the method returns a reference
     * of the object itself. The method does not appear a lot useful, but it helps
     * to remain consistent with the implementation of some clients.
     *
     * @return a copy of the board
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
     * @return        a new {@code Board} reflecting the move made
     * @throws NullPointerException if parameter {@code move} or {@code player} is null
     */
    public Board makeMove(Square move, Player player) {
	if (move == null || player == null) {
	    throw new NullPointerException("Parameters move and " +
					       "player must be not null. player=" +
					       player + ", move=" + move);
	}
	Map<Square, SquareState> sm = new EnumMap<Square, SquareState>(squares);
	sm.put(move, player.color());
	for (Direction dir : Direction.values()) {
	    Square bracketer = wouldFlip(move, player, dir);
	    if (bracketer != null) {
		for (Square c = move.neighbors().get(dir); true; c = c.neighbors().get(dir)) {
		    if (c == bracketer) break;
		    sm.put(c, player.color());
		}
	    }
	}
	return valueOf(sm);
    }
    
    /**
     * Returns the bracketing square or null if it is not found.
     * The method does not check that the move is legal.
     * <p>
     * The method should be private. It is not private to enable unit testing.
     * <p>
     * Parameters move, player, and dir must be not null. Java assert statements check
     * against this case to occur.
     *
     * @param move   the square where to move
     * @param player the player
     * @param dir    the direction
     * @return       the bracketing square, or null if it is not found
     */
    Square wouldFlip(Square move, Player player, Direction dir) {
	assert (move != null) : "Argument square must be not null";
	assert (player != null) : "Argument player must be not null";
	assert (dir != null) : "Argument dir must be not null";
	Square neighbor = move.neighbors().get(dir);
	Square bracketing = null;
	if (get(neighbor) == player.opponent().color()) {
	    Square next = neighbor.neighbors().get(dir);
	    if (next != null) bracketing = findBracketingPiece(next, player, dir);
	}
	return bracketing;
    }
    
    /**
     * Returns the boolean value telling if the move, done by the
     * specified player, is legal.
     *
     * @param move   the square where to put the new disk
     * @param player the player moving
     * @return       true if the move is legal, otherwise false
     * @throws NullPointerException if parameter {@code move} or {@code player} is null
     */
    public boolean isLegal(Square move, Player player) {
	if (move == null || player == null) {
	    throw new NullPointerException("Parameters move and " +
					       "player must be not null. player=" +
					       player + ", move=" + move);
	}
	if (get(move) != SquareState.EMPTY) return false;
	for (Direction dir : Direction.values()) {
	    if (wouldFlip(move, player, dir) != null) return true;
	}
 	return false;
    }

    /**
     * Returns the bracketing square or null if it is missing.
     * The method does not check that the move is legal and that the square parameter
     * is one step from move in the given direction.
     * <p>
     * The method should be private. It is not private to enable unit testing.
     * <p>
     * Parameters square, player, and dir must be not null. Java assert statements check
     * against this case to occur.
     *
     * @param square the square obtained moving by one from the move in the given direction
     * @param player the player
     * @param dir    the direction
     * @return       the bracketing square, or null if it is not found
     */
    Square findBracketingPiece(Square square, Player player, Direction dir) {
	assert (square != null) : "Argument square must be not null";
	assert (player != null) : "Argument player must be not null";
	assert (dir != null) : "Argument dir must be not null";
	if (get(square) == player.color()) {
	    return square;
	} else if (get(square) == player.opponent().color()) {
	    Square next = square.neighbors().get(dir);
	    if (next != null) return findBracketingPiece(next, player, dir);
	}
	return null;
    }

    /**
     * Returns the disk count for the color.
     *
     * @param color the color for which the disk count is computed
     * @return the disk count
     * @throws NullPointerException if parameter {@code color} is null
     */
    public int countPieces(SquareState color) {
	if (color == null) throw new NullPointerException("parameter color must be not null. color=" + color);
	int count = 0;
	for (SquareState ss : squares.values()) {
	    if (ss == color) count++;
	}
	return count;
    }

    /**
     * Returns the disk difference between the player and her opponent.
     *
     * @param player the player
     * @return       the disk count difference
     * @throws NullPointerException if parameter {@code player} is null
     */
    public int countDifference(Player player) {
	if (player == null) throw new NullPointerException("parameter player must be not null. player=" + player);
	return countPieces(player.color()) - countPieces(player.opponent().color());
    }

    /** Should go out of Board Class. */
    public void print() {
	print(System.out, null);
    }

    /** Should go out of Board Class. */
    public void print(PrintStream ps, Clock clock) {
	Integer cb = countPieces(SquareState.BLACK);
	Integer cw = countPieces(SquareState.WHITE);
	Integer cd = cb - cw;
	ps.print("    a b c d e f g h [@=" + cb + " 0=" + cw + " (" + cd + ")]");
	
	for (Row r : Row.values()) {
	    ps.print("\n " + r.label() + "  ");
	    for (Column c : Column.values()) {
		int idx = (r.ordinal() * 8) + c.ordinal();
		String p = get(Square.getInstance(idx)).symbol();
		ps.print(p + " ");
	    }
	}

	if (clock != null) {
	    ps.print("\n");
	    ps.print("\tClock: " + clock);
	}
	ps.print("\n\n");
    }

    /**
     * Returns the next player: the one that has to play next, given the current one.
     * When the opponent player has at last one legal move she is
     * the next player. When she does't have it, the method checks if
     * the current has at last one legal move, if the check is positive, the current player is 
     * also the next one. When neither player has a legal move the method
     * returns null.
     *
     * @param current the current player
     * @return        the next player that has to play a move
     * @throws NullPointerException if parameter {@code current} is null
     */
    public Player nextToPlay(Player current) {
	if (current == null) throw new NullPointerException("parameter current must be not null. current=" + current);
	Player opponent = current.opponent();
	Player next = null;
	if (hasAnyLegalMove(opponent)) {
	    next = opponent;
	} else if (hasAnyLegalMove(current)) {
	    next = current;
	}
	return next;
    }

    /**
     * Returns if the player has any legal move given the board state.
     *
     * @param player the player
     * @return       {@code true} if the player has any legal move, otherwise {@code false}
     * @throws NullPointerException if parameter {@code player} is null
     */
    public boolean hasAnyLegalMove (Player player) {
	if (player == null) throw new NullPointerException("parameter player must be not null. player=" + player);
	boolean b = false;
	for (Square move : Square.values()) {
	    if (isLegal(move, player)) {
		b = true;
		break;
	    }
	}
	return b;
    }

    /**
     * Returns a list holding the legal moves that the {@code player} can
     * do at the board position. When no moves are available to the player
     * the method returns an empty list.
     *
     * @param player the player
     * @return       the moves available to the player
     * @throws NullPointerException if parameter {@code player} is null
     */
    public List<Square> legalMoves(Player player) {
	if (player == null) throw new NullPointerException("parameter player must be not null. player=" + player);
	List<Square> legalMoves = new ArrayList<Square>();
	for (Square move : Square.values()) {
	    if (isLegal(move, player)) legalMoves.add(move);
	}
	return legalMoves;
    }

}