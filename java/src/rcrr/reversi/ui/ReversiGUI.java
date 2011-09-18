/*
 *  ReversiGUI.java
 *
 *  Copyright (c) 2011 Roberto Corradini. All rights reserved.
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

package rcrr.reversi.ui;

import rcrr.reversi.Strategy;
import rcrr.reversi.Actor;
import rcrr.reversi.Game;

import java.util.List;
import java.util.ListIterator;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;

import java.io.PrintStream;

import org.joda.time.Period;
import org.joda.time.Duration;

import java.io.PrintWriter;
import java.io.StringWriter;

/**
 * The reversi class {@code ReversiGUI} is the main entry point for the program.
 * <p>
 * Documentation has to be completed.
 * Testing has to be completed.
 */
public final class ReversiGUI {

    /** Error code 1. */
    private static final int ERROR_CODE_1 = 1;

    /** Error code 2. */
    private static final int ERROR_CODE_2 = 2;

    /** Error code 3. */
    private static final int ERROR_CODE_3 = 3;

    /** Error code 4. */
    private static final int ERROR_CODE_4 = 4;

    /** Error code 5. */
    private static final int ERROR_CODE_5 = 5;

    /** Default game duration in minutes. */
    private static final int DEFAULT_GAME_DURATION_IN_MINUTES = 30;

    /** Class constructor. Not used so far. */
    private ReversiGUI() {
        throw new UnsupportedOperationException();
    }

    /**
     * Plays a game returning the score.
     * <p>
     * Documentation has to be completed.
     * Testing has to be completed.
     * It has to be fully refactored.
     *
     * @param blStrategy   the black's strategy
     * @param whStrategy   the white's strategy
     * @param gameDuration the game duration assigned to both players
     *
     * @return           the game score
     */
    public static int reversi(final Strategy blStrategy,
                              final Strategy whStrategy,
                              final Duration gameDuration) {

        /** Must be revised!!!! */
        final Actor black = Actor.valueOf("Black Actor", blStrategy);
        final Actor white = Actor.valueOf("White Actor", whStrategy);
        final Game game = Game.initialGame(black, white, gameDuration, null);
        game.play();
        return game.countDiscDifference();
    }

    /**
     * The main entry point for the Reversi Program.
     *
     * @param args an array having two elements: [black's strategy, white's strategy]
     */
    public static void main(final String[] args) {
        if (args == null || args.length != 2) {
            System.out.println("Argument list error: blackStrategy and whiteStrategy must be provided.");
            usage();
            System.exit(ERROR_CODE_1);
        }
        Strategy[] s = new Strategy[]{null, null};
        for (int i = 0; i < s.length; i++) {
            Object o = null;
            try {
                Class<?> c = Class.forName(args[i]);
                o = c.newInstance();
            } catch (ClassNotFoundException e) {
                System.out.println("Exception e: " + e);
                usage();
                System.exit(ERROR_CODE_2);
            } catch (InstantiationException e) {
                System.out.println("Exception e: " + e);
                usage();
                System.exit(ERROR_CODE_3);
            } catch (IllegalAccessException e) {
                System.out.println("Exception e: " + e);
                usage();
                System.exit(ERROR_CODE_4);
            }
            try {
                s[i] = (Strategy) o;
            } catch (ClassCastException e) {
                System.out.println("Exception e: " + e);
                usage();
                System.exit(ERROR_CODE_5);
            }
        }
        reversi(s[0], s[1],
                Period.minutes(DEFAULT_GAME_DURATION_IN_MINUTES).toStandardDuration());
    }

    /**
     * Print the usage message.
     */
    private static void usage() {
        System.out.println("usage: java rcrr.reversi.ui.ReversiGUI blackStrategy whiteStrategy");
        System.out.println("\t Where blackStrategy and whiteStrategy are two classes");
        System.out.println("\t that implements the rcrr.reversi.Strategy interface.");
    }

}
