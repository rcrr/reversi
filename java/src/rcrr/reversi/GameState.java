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

import java.io.PrintStream;

public class GameState {

    private final Clock clock;
    private final Board board;
    private final Player player;

    private GameState(Board board, Player player, Clock clock) {
	this.clock = clock;
	this.board = board;
	this.player = player;
    }

    static GameState valueOf(Board board, Player player, Clock clock) {
	GameState gs = new GameState(board, player, clock);
	return gs;
    }

    public Clock getClock() {
	return clock;
    }

    public Board getBoard() {
	return board;
    }

    public Player getPlayer() {
	return player;
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
	return (sbGameState.toString());
    }

}
