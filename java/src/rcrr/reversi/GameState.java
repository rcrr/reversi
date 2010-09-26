/*
 *  GameState.java
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
 * An instance of a game state.
 * <p>
 * A {@code GameState} object holds the information of the state of the game.
 * It is a value object composed by three fields:
 * <ol>
 *   <li>the game board</li>
 *   <li>the player that has to move</li>
 *   <li>the current clock</li>
 * </ol>
 * <p>
 * {@code GameState} is immutable.
 * <p>
 * The {@code player} field can be {@code null} only when no legal moves are available to either player.
 */
public class GameState {

    /** The board field. */
    private final Board board;

    /** The player field. */
    private final Player player;

    /** The clock field. */
    private final Clock clock;

    /**
     * Private constructor.
     * <p>
     * Parameters {@code board} and {@code clock} must be not null.
     *
     * @param board  the board state
     * @param player the player that has to move
     * @param clock  the current clock
     */
    private GameState(Board board, Player player, Clock clock) {
	assert (board != null) : "Parameter board cannot be null. board=" + board;
	assert (clock != null) : "Parameter clock cannot be null. clock=" + clock;
	assert ((player != null) ||
		!board.hasAnyPlayerAnyLegalMove()) : "Parameter player cannot be null when there are still valid moves. player=" + player;
	this.board = board;
	this.player = player;
	this.clock = clock;
    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameters {@code board} and {@code clock} must be not null.
     * Parameter {@code player} can be null.
     *
     * @param  board  the board state
     * @param  player the player that has to move
     * @param  clock  the current clock
     * @return        a new game state
     * @throws NullPointerException when board or clock parameters are null,
     *                              or when player is null and legal moves are still there
     */
    public static GameState valueOf(Board board, Player player, Clock clock) {
	if (board == null) throw new NullPointerException("Parameter board cannot be null. board=" + board);
	if (clock == null) throw new NullPointerException("Parameter clock cannot be null. clock=" + clock);
	if ((player == null) && board.hasAnyPlayerAnyLegalMove())
	    throw new NullPointerException("Parameter player cannot be null when there are still valid moves. player=" + player);
	GameState gs = new GameState(board, player, clock);
	return gs;
    }

    /**
     * Static factory that returns a new initial game's state.
     * <p>
     * The returned game's state has the board set with the four central
     * disk, the black player has to move, and each player has the assigned 
     * time in minutes in their own clock.
     *
     * @param minutes the time assigned to each player in minutes
     * @return        a new initial game as required by international game's rures
     */
    public static GameState initialGameState(int minutes) {
	return valueOf(Board.initialBoard(), Player.BLACK, Clock.initialClock(minutes));
    }

    /**
     * Returns if the game state admit one or more legal moves.
     *
     * @return {@code true} if the player has at last one legal move
     */
    public boolean hasAnyLegalMove() {
	return board.hasAnyLegalMove(player);
    }

    /**
     * Returns if either one of the two players has any legal move.
     *
     * @return {@code true} if anyone can play a move.
     */
    public boolean hasAnyPlayerAnyLegalMove() {
	return board.hasAnyPlayerAnyLegalMove();
    }

    /**
     * Returns the board field.
     *
     * @return the game state board
     */
    public Board board() {
	return board;
    }

    /**
     * Returns the player field.
     *
     * @return the game state player
     */
    public Player player() {
	return player;
    }

    /**
     * Returns the clock field.
     *
     * @return the game state clock
     */
    public Clock clock() {
	return clock;
    }

    /**
     * Returns a formatted string showing a 2d graphical composed view
     * of the game state. It shows the board, the disk count, and the clock.
     * <p>
     * The method joins the printBoardWithCount() and the printClock() output,
     * setting the second on the right of the last board's row.
     *
     * @return a string being a 2d representation of the game
     */
    public String printGameState() {
	StringBuilder sbGameState = new StringBuilder();
	String sBoard = board.printBoardWithCount();
	String sClock = clock.printClock();
	String[] lines = sBoard.split("\n");
	for (int i=0; i<lines.length; i++) {
	    String line = lines[i];
	    sbGameState.append(line);
	    if (i == 8) sbGameState.append(sClock);
	    sbGameState.append("\n");
	}
	{
	    Player p = player();
	    if (p != null) {
		sbGameState.append(" Next to play: " + p + ", legal moves: " + board().legalMoves(p) + "\n");
	    } else {
		sbGameState.append(" No player has any legal move. The game is over.");
	    }
	}
	return (sbGameState.toString());
    }

}
