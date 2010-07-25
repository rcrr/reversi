/*
 *  Clock.java
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

import java.text.DecimalFormat;
import java.text.NumberFormat;

/**
 * Clock Class defines the Game Clock. It has two values, the
 * Black's remaining time, and the White's one, expressed in
 * milliseconds.
 * Clock is an immutable class, since there is no way to change
 * its state after construction.
 */
public final class Clock {

    private static final long DEFAULT_GAME_TIME_IN_MINUTES = 30;
    private static final long TIME_UNITS_PER_SECOND = 1000;
    private static final long SECONDS_PER_MINUTE = 60;

    private static final NumberFormat TIME_FORMATTER = new DecimalFormat("##00");

    private final long blackTime;
    private final long whiteTime;

    /**
     * Class constructor.
     * <p>
     * This constructor creates a Clock object given the Black's time and the White's
     * one.
     *
     * @param  blackTime the Black's remaining time
     * @param  whiteTime the White's remaining time
     */
    private Clock(final long blackTime, final long whiteTime) {
	this.blackTime = blackTime;
	this.whiteTime = whiteTime;
    }

    /**
     * Class static factory. Returns a new Clock with the given initial values
     * assigned to each individual player.
     *
     * @param  blackTime the game's time assigned to the Black player in milliseconds
     * @param  whiteTime the game's time assigned to the White player in milliseconds
     * @return           a new Clock having Black's and White's time set to the
     *                   given parameters
     */
    public static Clock valueOf(final Long blackTime, final Long whiteTime) {
	return new Clock (blackTime, whiteTime);
    }

    /**
     * Class static factory. Returns a new Clock with the given initial value
     * assigned to both players.
     *
     * @param  gameTimeInMinutes the game's time in minutes assigned to the two players
     * @return                   a new Clock having Black's and White's time set to
     *                           the same given value
     */
    public static Clock initialClock(final Long gameTimeInMinutes) {
	final long tm = (gameTimeInMinutes == null) ? DEFAULT_GAME_TIME_IN_MINUTES : gameTimeInMinutes;
	final long t = SECONDS_PER_MINUTE * TIME_UNITS_PER_SECOND * tm;
	return new Clock (t, t);
    }

    /**
     * Returns a new Clock object generated subtracting the deltaTime value from
     * the specified player remaining clock time.
     *
     * @param  player    the player from which to take away the specified time
     * @param  deltaTime the amount of time in milliseconds to subtract from the 
     *                   player's clock time
     * @return           a new updated Clock
     */
    public Clock setTime(final SquareState player, final Long deltaTime) throws GameOverException {
	switch(player) {
	case BLACK:
	    final long bRemainingTime = blackTime - deltaTime;
	    if (bRemainingTime < 0) throw new GameOverException("BLACK player exceded his game time limit.");
	    return new Clock(bRemainingTime, whiteTime);
	case WHITE:
	    final long wRemainingTime = whiteTime - deltaTime;
	    if (wRemainingTime < 0) throw new GameOverException("WHITE player exceded his game time limit.");
	    return new Clock(blackTime, wRemainingTime);
	}
	// throw new Exception("Only BLACK and WHITE players are supported by Clock.");
	System.out.println("Only BLACK and WHITE players are supported by Clock.");
	System.exit(1);
	return null;
    }

    /**
     * Returns a Long value that represents the Clock time remaining to
     * the specified player parameter.
     *
     * @param  player the player for which the remaining time is queried 
     * @return        the player remaining time in milliseconds
     */
    public Long getTime(SquareState player) {
	return (player == SquareState.BLACK) ? blackTime : whiteTime;
    }

    /**
     * Returns a String in the format mm:ss corresponding to
     * the given time in milliseconds, where:
     *  mm is the amount of minuts
     *  ss is the amount of seconds
     *
     * @param  time time in milliseconds 
     * @return      a formatted string with minutes and seconds
     */
    private static String timeString(long time) {
	long rTime = Math.round(time / TIME_UNITS_PER_SECOND);
	long minutes = (long) Math.floor(rTime / SECONDS_PER_MINUTE);
	long seconds = rTime - (minutes * SECONDS_PER_MINUTE);
	return TIME_FORMATTER.format(minutes) + ":" + TIME_FORMATTER.format(seconds);
    }

    /**
     * Returns a String representing the Clock object.
     *
     * @return a String representing the clock
     */
    @Override public String toString() {
	return "[ " + SquareState.BLACK + "=" + timeString(blackTime) + ", " + 
	    SquareState.WHITE + "=" + timeString(whiteTime) + " ]";
    }

    public static void main(String[] args) {
	System.out.println("Clock .... init.");
	Clock c = Clock.valueOf( 1000L * 60L * 30L, 4000L );
	Long time = c.getTime(SquareState.BLACK);
	System.out.println("time=" + time + ", c=" + c);
    }

}