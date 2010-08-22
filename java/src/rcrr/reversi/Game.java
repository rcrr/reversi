/*
 *  Game.java
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

public class Game {
    private final Clock clock;
    private final Board board;
    private final Player player;

    private Game(Board board, Player player, Clock clock) {
	this.clock = clock;
	this.board = board;
	this.player = player;
    }

    static Game valueOf(Board board, Player player, Clock clock) {
	Game gs = new Game(board, player, clock);
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

    /*
      Has to be copletely rewritten!
      Player should be renamed into Color (or PlayerColor to avoid collision with the java Color class)
      Player should then be a new class having all the Player attribute, mostly the Strategy ....
      All the printing shold be eradicated from the base data classes
      the new API shold be somthing like:
      game g;
      g.getMove();
     */
    public static Game getMoveX(Board b, Strategy strategy, Player player, PrintStream ps, Clock clock) throws GameOverException {
	if (ps != null) b.print(ps, clock);
	long t0 = System.currentTimeMillis();
	Square move = strategy.move(player, b.copyBoard());
	long t1 = System.currentTimeMillis();
	clock = clock.setTime(player, t1 - t0);
	if (b.isValid(move) && b.isLegal(move, player)) {
	    if (ps != null) {
		ps.print("\n" + player.name() + " moves to " + move.getDisplayName() + "\n");
	    }
	    Board b1 = b.makeMove(move, player);
	    return Game.valueOf(b1, b1.nextToPlay(player, null), clock);
	} else {
	    if (ps != null) ps.print("Illegal move: " + move + "\n");
	    return getMoveX(b, strategy, player, ps, clock);
	}
    }


}
