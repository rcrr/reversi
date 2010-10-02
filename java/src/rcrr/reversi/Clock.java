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

import java.util.Map;
import java.util.EnumMap;
import java.util.Collections;

import java.text.DecimalFormat;
import java.text.NumberFormat;

import org.joda.time.Duration;

/**
 * The {@code Clock} class defines the game clock. It has two values, the
 * Black's remaining time, and the White's one, expressed in
 * milliseconds.
 * <p>
 * {@code Clock} is immutable.
 * <p>
 * To create a new {@code Clock} there are two static factories
 * available. The first is:
 * <pre>
 * {@code
 * Clock c = Clock.valueOf(1000, 1000);
 * }
 * </pre>
 * while the second is:
 * <pre>
 * {@code
 * Clock c = Clock.initialClock(10);
 * }
 * </pre>
 * Another way to get a new {@code Clock} is to call the {@code setTime}
 * method. For instance let say that we have already a {@code Clock} and we want to
 * subtract one full second from the Black player:
 * <pre>
 * {@code
 * Clock updated = c.setTime(Player.BLACK, 1000);
 * }
 * </pre>
 */
public final class Clock {
    
    private static final long TIME_UNITS_PER_SECOND = 1000;
    private static final long SECONDS_PER_MINUTE = 60;

    private static final NumberFormat TIME_FORMATTER = new DecimalFormat("##00");

    /**
     * Players' clock time in milliseconds
     */
    private final Map<Player, Duration> playersGameDuration;

    /**
     * Class constructor.
     * <p>
     * This constructor creates a Clock object given the Black's time and the White's
     * one.
     *
     * @param  blackTime the Black's remaining time
     * @param  whiteTime the White's remaining time
     */
    private Clock(final Map<Player, Duration> durationMap) {
	assert (durationMap != null) : "Parameter durationMap cannot be null. durationMap=" + durationMap;
	assert (durationMap.size() == Player.values().length) : "Parameter durationMap size is not consistent." +
	    " durationMap.size()=" + durationMap.size() +
	    ", expected value: " + Player.values().length;
	EnumMap<Player, Duration> durationEnumMap = (durationMap instanceof EnumMap) ?
	    (EnumMap<Player, Duration>) durationMap : new EnumMap<Player, Duration>(durationMap);
	this.playersGameDuration = Collections.unmodifiableMap(durationEnumMap);
    }

    /**
     * Class static factory. Returns a new Clock with the given initial values
     * assigned to each individual player.
     *
     * @param  blackTime the game's time assigned to the Black player in milliseconds
     * @param  whiteTime the game's time assigned to the White player in milliseconds
     * @return           a new {@code Clock} having Black's and White's time set to the
     *                   given parameters
     */
    public static Clock valueOf(final Map<Player, Duration> durationMap) {
	if (durationMap == null) throw new NullPointerException("Parameter durationMap cannot be null. durationMap=" + durationMap);
	if (durationMap.size() != Player.values().length)
	    throw new IllegalArgumentException("Parameter durationMap size is not consistent." +
					       " durationMap.size()=" + durationMap.size() +
					       " expected value: " + Player.values().length);
	if (durationMap.containsKey(null))
	    throw new NullPointerException("Parameter durationMap cannot have null keys. durationMap=" + durationMap);
	return new Clock(durationMap);
    }

    /**
     * Class static factory. Returns a new Clock with the given initial value
     * assigned to both players.
     *
     * @param  gameTimeInMinutes the game's time in minutes assigned to the two players
     * @return                   a new {@code Clock} having Black's and White's time set to
     *                           the same given value
     * @throws IllegalArgumentException if gameTimeInMinutes is lesser than 1, or greather than 60. 
     */
    public static Clock initialClock(final int gameTimeInMinutes) {
	if (gameTimeInMinutes <= 0 || gameTimeInMinutes > 60)
	    throw new IllegalArgumentException("Parameter gameTimeInMinutes must be between 1 and 60. value=" + 
					       gameTimeInMinutes);
	final long t = SECONDS_PER_MINUTE * TIME_UNITS_PER_SECOND * gameTimeInMinutes;
	Map<Player, Duration> m = new EnumMap<Player, Duration>(Player.class);
	m.put(Player.BLACK, new Duration(t));
	m.put(Player.WHITE, new Duration(t));
	return valueOf(m);
    }

    /**
     * Returns a new Clock object generated subtracting the deltaTime value from
     * the specified player remaining clock time.
     *
     * @param  player    the player from which to take away the specified time
     * @param  deltaTime the amount of time in milliseconds to subtract from the 
     *                   player's clock time
     * @return           a new updated {@code Clock}
     * @throws NullPointerException     if the player parameter is null 
     * @throws IllegalArgumentException if a player different from BLACK or WHITE is passed as parameter
     */
    public Clock setTime(final Player player, final Duration delta) {
	if (player == null) throw new NullPointerException("Parameter player connot be null. player=" + player);
	if (delta == null) throw new NullPointerException("Parameter delta connot be null. delta=" + delta);
	if (delta.isShorterThan(Duration.ZERO)) throw new IllegalArgumentException("Parameter delta cannot be negative. delta=" + delta);
	final Duration actual = playersGameDuration.get(player);
	final Duration tmp = actual.minus(delta);
	final Duration remaining = (tmp.isLongerThan(Duration.ZERO)) ? tmp : Duration.ZERO;
	final Map<Player, Duration> m = new EnumMap<Player, Duration>(playersGameDuration);
	m.put(player, remaining);
	return valueOf(m);
    }

    /**
     * Returns a formatted string, showing the two player clocks.
     *
     * @return a string showing the two player's clocks
     */
    public String printClock() {
	return "[" + Player.BLACK.symbol() + "=" + timeString(getTime(Player.BLACK)) + ", " + 
	    Player.WHITE.symbol() + "=" + timeString(getTime(Player.WHITE)) + "]";
    }

    /**
     * Returns a {@code long} value that represents the player's remaining time
     * in milliseconds as registered by the {@code Clock} instance. 
     *
     * @param  player the player for which the remaining time is queried 
     * @return        the player remaining time in milliseconds
     * @throws NullPointerException if the player parameter is null
     */
    public Duration getTime(Player player) {
	if (player == null) throw new NullPointerException("Parameter player connot be null. player=" + player);
	return playersGameDuration.get(player);
    }

    /**
     * Returns a {@code String} representing the {@code Clock} object.
     *
     * @return a {@code String} representing the clock
     */
    @Override public String toString() {
	return "[" + Player.BLACK + "=" + timeString(getTime(Player.BLACK)) + ", " + 
	    Player.WHITE + "=" + timeString(getTime(Player.WHITE)) + "]";
    }

    /**
     * Returns a String in the format mm:ss corresponding to
     * the given time in milliseconds, where:
     *  - mm is the amount of minuts
     *  - ss is the amount of seconds
     *
     * @param  time time in milliseconds 
     * @return      a formatted {@code String} with minutes and seconds
     */
    private static String timeString(Duration duration) {
	long time = duration.getMillis();
	long rTime = Math.round(time / TIME_UNITS_PER_SECOND);
	long minutes = (long) Math.floor(rTime / SECONDS_PER_MINUTE);
	long seconds = rTime - (minutes * SECONDS_PER_MINUTE);
	return TIME_FORMATTER.format(minutes) + ":" + TIME_FORMATTER.format(seconds);
    }

}