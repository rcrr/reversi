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
// - The clock should run asynchronously. See ScheduledThreadPoolExecutor.

package rcrr.reversi;

import java.util.Arrays;
import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.io.PrintStream;

import org.joda.time.Period;
import org.joda.time.Duration;

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
     * @param gameDuration
     * 
     * @return           the game score
     */
    public static int reversi(Strategy blStrategy, Strategy whStrategy, PrintStream ps, Duration gameDuration) {
	Game game = Game.initialGame(blStrategy, whStrategy, gameDuration, ps);
	game.play();
	return game.countDiscDifference();
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
	reversi(s[0], s[1], System.out, Period.minutes(30).toStandardDuration());
    }

    /** returns the usage message */
    private static void usage() {
	System.out.println("usage: java rcrr.reversi.Reversi blackStrategy whiteStrategy");
	System.out.println("\t Where blackStrategy and whiteStrategy are two classes");
	System.out.println("\t that implements the rcrr.reversi.Strategy interface.");
    }

}