/*
 *  Reversi.java
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

// Notes:
// - Which practice is best when it comes to write unit test for UI?
//   After a brief search on google the best so far tool to investigate
//   on is UISpec4J (http://www.uispec4j.org).

// To do:
// move away the WINNING and LOSING values.
// javadocs
// junit tests
// run the two strategy with an object having a flexible collection of settings
//   this is close to rewrite the Strategy-Player association ....
//   The Strategy as designed so far is not able to persist between moves.

package rcrr.reversi;

import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.io.PrintStream;


/**
 * {@code Reversi} is the main entry point for the program.
 * <p>
 * Documentation has to be completed.
 * Testing has to be completed.
 */
public class Reversi {

    /** Private constructor. Not used so far. */
    private Reversi() {
	throw new UnsupportedOperationException();
    }

    /**
     * Plays a game returning the score.
     * <p>
     * Documentation has to be completed.
     * Testing has to be completed.
     * It has to be fully refactored.
     *
     * @param blStrategy ...
     * @param whStrategy
     * @param ps
     * @param minutes
     * 
     * @return           the game score
     */
    public static int reversi(Strategy blStrategy, Strategy whStrategy, PrintStream ps, int minutes) {
	GameState gs = GameState.initialGameState(minutes);
	Game game = Game.valueOf(Arrays.asList(gs));
	try {
	    for (Player player = gs.player();
		 player != null;
		 player = gs.board().nextToPlay(player)) {
		if (ps != null) ps.print(gs.printGameState());
		gs = getMoveY(gs, ((player == Player.BLACK) ? blStrategy : whStrategy), ps);
		game.add(gs);
		if (ps != null) {
		    if (gs.board().nextToPlay(player) == player) ps.print("\n" + player.opponent() + " has no moves and must pass.\n");
		}
	    }
	} catch (GameOverException goe) {
	    // to be completed .....
	}
	if (ps != null) {
	    ps.print(gs.printGameState());
	}
	return gs.board().countDifference(Player.BLACK);
    }

    /*
      Has to be copletely rewritten!
      Player should be renamed into Color (or PlayerColor to avoid collision with the java Color class)
      Player should then be a new class having all the Player attribute, mostly the Strategy ....
      All the printing should be eradicated from the base data classes
      the new API shold be somthing like:
      game g;
      g.getMove();
     */
    public static GameState getMoveY(GameState gs, Strategy strategy, PrintStream ps) throws GameOverException {
	Player player = gs.player();
	Board b = gs.board();
	Clock clock = gs.clock();
	long t0 = System.currentTimeMillis();
	Square move = strategy.move(gs);
	long t1 = System.currentTimeMillis();
	clock = clock.setTime(player, t1 - t0);
	if (b.isLegal(move, player)) {
	    if (ps != null) {
		ps.print("\n" + player.name() + " moves to " + move.label() + "\n");
	    }
	    Board b1 = b.makeMove(move, player);
	    return GameState.valueOf(b1, b1.nextToPlay(player), clock);
	} else {
	    if (ps != null) ps.print("Illegal move: " + move + "\n");
	    return getMoveY(gs, strategy, ps);
	}
    }

    /**
     * The main entry point for the Reversi Program.
     *
     * @param args an array having two elements: [black's strategy, white's strategy]
     */
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
	reversi(s[0], s[1], System.out, 30);
    }

    /** returns the usage message */
    private static void usage() {
	System.out.println("usage: java rcrr.reversi.Reversi blackStrategy whiteStrategy");
	System.out.println("\t Where blackStrategy and whiteStrategy are two classes");
	System.out.println("\t that implements the rcrr.reversi.Strategy interface.");
    }

}