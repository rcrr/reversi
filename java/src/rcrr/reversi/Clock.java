/*
 *  Clock.java
 *
 *  Copyright (c) 2010, 2011 Roberto Corradini. All rights reserved.
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

import org.joda.time.DateTimeConstants;
import org.joda.time.Duration;

/**
 * The {@code Clock} class defines the different clocks used in the Reversi game.
 * It has two properties, the black's remaining time, and the white's one.
 * The remaining times are received and returned by clock's methods by
 * means of duration objects, as defined by the Joda-Time {@code Duration} class.
 * <p>
 * {@code Clock} is immutable.
 * <p>
 * Three static factories are available In order to create a new clock instance.
 * The first is:
 * <pre>
 * {@code
 * Clock c = Clock.valueOf(blackDuration, whiteDuration);
 * }
 * </pre>
 * where the parameters {@code blackDuration} and {@code whiteDuration} are of type {@code Duration}.
 * The second factory is:
 * <pre>
 * {@code
 * Clock c = Clock.valueOf(durations);
 * }
 * </pre>
 * where the {@code durations} parameter is a map defined having objects of the {@code Player} class as keys
 * and objects of the {@code Duration} class as values.
 * And the third factory is:
 * <pre>
 * {@code
 * Clock c = Clock.initialClock(gameDuration);
 * }
 * </pre>
 * where the {@code gameDuration} parameter is the time assigned equally to both players.
 * <p>
 * Another way to create a new clock instance is by calling the {@code set}
 * method. It returns a new clock subtracting the provided delta time
 * to the specified player. For instance let say that we have already a {@code Clock c} and that we want to
 * subtract one full second from the black player:
 * <pre>
 * {@code
 * Clock c;
 * ...
 * Duration amount = Seconds.ONE.toStandardDuration();
 * Clock updated = c.set(Player.BLACK, amount);
 * }
 * </pre>
 * where {@code Seconds.ONE.toStandardDuration()} parameter is
 * a one second duration object belonging to the {@code Duration} class.
 */
public final class Clock {

    /** It is really needed? */
    private static final NumberFormat TIME_FORMATTER = new DecimalFormat("##00");

    /** Prime number 17. */
    private static final int PRIME_NUMBER_17 = 17;

    /** Prime number 37. */
    private static final int PRIME_NUMBER_37 = 37;

    /**
     * Class static factory that returns a new clock object with the given initial value
     * assigned equally to both players.
     * <p>
     * Parameter {@code initialDuration} must be not {@code null}, and must be not negative.
     *
     * @param  initialDuration the game's duration assigned to the two players
     * @return                 a new clock having black's and white's time duration
     *                         set to the same given value
     * @throws NullPointerException     if parameter {@code initialDuration} is {@code null}
     * @throws IllegalArgumentException if parameter {@code initialDuration} is shorter than a zero duration
     */
    public static Clock initialClock(final Duration initialDuration) {
        if (initialDuration == null) { throw new NullPointerException("Parameter initialDuration cannot be null."); }
        if (initialDuration.isShorterThan(Duration.ZERO)) {
            throw new IllegalArgumentException("Parameter initialDuration cannot be negative.");
        }
        return valueOf(transientDurations(initialDuration, initialDuration));
    }

    /**
     * Class static factory that returns a new clock object constructed using the given map.
     * <p>
     * Parameter {@code durations} must satisfy the following conditions:
     * <ul>
     *   <li>Must be not {@code null}</li>
     *   <li>Must have a number of entries equal to the players' count</li>
     *   <li>Must not have {@code null} keys</li>
     *   <li>Must not have {@code null} values</li>
     *   <li>Must not have negative duration values</li>
     * </ul>
     *
     * @param durations the game's time assigned to the players
     * @return          a new clock having black's and white's time set to the
     *                  given durations
     * @throws NullPointerException     when parameter {@code durations} is {@code null},
     *                                  or it has a {@code null} key,
     *                                  or it has a {@code null} value
     * @throws IllegalArgumentException when parameter {@code durations} size is different from the number
     *                                  of players, or duration values are negative
     */
    public static Clock valueOf(final Map<Player, Duration> durations) {
        if (durations == null) {
            throw new NullPointerException("Parameter durations cannot be null.");
        }
        if (durations.size() != Player.values().length) {
            throw new IllegalArgumentException("Parameter durations size is not consistent."
                                               + " durations.size()=" + durations.size()
                                               + " expected value: " + Player.values().length);
        }
        if (durations.containsKey(null)) {
            throw new NullPointerException("Parameter durations cannot have null keys.");
        }
        if (durations.containsValue(null)) {
            throw new NullPointerException("Parameter durations cannot have null values.");
        }
        for (Duration duration : durations.values()) {
            if (duration.isShorterThan(Duration.ZERO)) {
                throw new IllegalArgumentException("Parameter durations cannot have negative values."
                                                   + " durations=" + durations);
            }
        }
        return new Clock(durations);
    }

    /**
     * Returns a String representing the clock.
     * The format is mm:ss corresponding to the given time in milliseconds, where:
     *  - mm is the amount of minutes
     *  - ss is the amount of seconds
     *
     * @param  duration time in milliseconds
     * @return          a formatted {@code String} with minutes and seconds
     */
    private static String timeString(final Duration duration) {
        long time = duration.getMillis();
        long rTime = time / DateTimeConstants.MILLIS_PER_SECOND;
        long minutes = (long) Math.floor(rTime / DateTimeConstants.SECONDS_PER_MINUTE);
        long seconds = rTime - (minutes * DateTimeConstants.SECONDS_PER_MINUTE);
        return TIME_FORMATTER.format(minutes) + ":" + TIME_FORMATTER.format(seconds);
    }

    /**
     * Returns a new map object with the given duaration values
     * assigned to each individual player.
     *
     * @param  blackDuration the game's time assigned to the Black player
     * @param  whiteDuration the game's time assigned to the White player
     * @return               a new map having black's and white's time set to the
     *                       given parameters
     */
    private static Map<Player, Duration> transientDurations(final Duration blackDuration,
                                                            final Duration whiteDuration) {
        Map<Player, Duration> transientDurations = new EnumMap<Player, Duration>(Player.class);
        transientDurations.put(Player.BLACK, blackDuration);
        transientDurations.put(Player.WHITE, whiteDuration);
        return transientDurations;
    }

    /**
     * The durations field.
     * It stores each players' clock time as a Joda-Time Duration.
     * Internally Durations store the time in milliseconds.
     */
    private final Map<Player, Duration> durations;

    /** Lazily initialized, cached hashCode. */
    private volatile int hashCode = 0;

    /**
     * Class constructor.
     * <p>
     * This constructor creates a Clock object given the Black's time and the White's
     * one.
     *
     * @param durations a map having the remaining time duration assigned to each player.
     */
    private Clock(final Map<Player, Duration> durations) {
        assert (durations != null) : "Parameter durations cannot be null.";
        assert (durations.size() == Player.values().length)
            : "Parameter durations size is not consistent."
            + " durations.size()=" + durations.size()
            + ", expected value: " + Player.values().length;
        assert (!durations.containsKey(null)) : "Parameter durations cannot contains null keys.";
        assert (!durations.containsValue(null)) : "Parameter durations cannot contains null values.";
        this.durations = Collections.unmodifiableMap(new EnumMap<Player, Duration>(durations));
    }

    /**
     * Returns a new clock object generated subtracting the delta value from
     * the specified player remaining clock time.
     * <p>
     * When the delta parameter is greather than the player's actual time
     * the updated value is set to zero.
     * <p>
     * Parameter {@code player} must be not {@code null}.
     * Parameter {@code delta} must be not {@code null}, and must be not negative.
     *
     * @param  player the player from which to take away the specified time
     * @param  delta  the amount of time in milliseconds to subtract from the
     *                player's clock time
     * @return        a new updated clock
     * @throws NullPointerException     if the player or delta parameter is null
     * @throws IllegalArgumentException if the delta parameter is negative.
     */
    public Clock decrement(final Player player, final Duration delta) {
        if (player == null) { throw new NullPointerException("Parameter player connot be null"); }
        if (delta == null) { throw new NullPointerException("Parameter delta connot be null."); }
        if (delta.isShorterThan(Duration.ZERO)) {
            throw new IllegalArgumentException("Parameter delta cannot be negative.");
        }
        final Duration actual = this.durations.get(player);
        final Duration updated = (actual.isLongerThan(delta)) ? actual.minus(delta) : Duration.ZERO;
        return valueOf(set(player, updated));
    }

    /**
     * Returns true if the specified object is equal to this clock.
     * Two clockss are equal when they have the same remaining times.
     *
     * @param object the object to compare to
     * @return {@code true} when the {@code object} parameter is an instance of
     *         the {@code Clock} class and when the time remaining to both
     *         players is equal.
     */
    @Override
    public boolean equals(final Object object) {
        if (object == this) { return true; }
        if (!(object instanceof Clock)) { return false; }
        Clock clock = (Clock) object;
        for (Player player : Player.values()) {
            if (!get(player).equals(clock.get(player))) { return false; }
        }
        return true;
    }

    /**
     * Returns a {@code Duration} value that represents the player's remaining time
     * as registered by the clock instance.
     * <p>
     * Parameter {@code player} must be not {@code null}.
     *
     * @param  player the player for which the remaining time is queried
     * @return        the player remaining duration
     * @throws NullPointerException if the player parameter is null
     */
    public Duration get(final Player player) {
        if (player == null) {
            throw new NullPointerException("Parameter player connot be null.");
        }
        return this.durations.get(player);
    }

    /**
     * Returns a hash code value for the clock object.
     *
     * @return a hash code value for this clock
     */
    @Override
    public int hashCode() {
        if (hashCode == 0) {
            int result = PRIME_NUMBER_17;
            for (Duration duration : this.durations.values()) {
                int k = duration.hashCode();
                result = PRIME_NUMBER_37 * result + k;
            }
            hashCode = result;
        }
        return hashCode;
    }

    /**
     * Returns a {@code String} representing the {@code Clock} object.
     *
     * @return a {@code String} representing the clock
     */
    @Override public String toString() {
        return "[" + Player.BLACK + "=" + timeString(get(Player.BLACK)) + ", "
            + Player.WHITE + "=" + timeString(get(Player.WHITE)) + "]";
    }

    /**
     * Returns a formatted string, showing the two player clocks.
     *
     * @return a string showing the two player's clocks
     */
    public String printClock() {
        return "[" + Player.BLACK.symbol() + "=" + timeString(get(Player.BLACK)) + ", "
            + Player.WHITE.symbol() + "=" + timeString(get(Player.WHITE)) + "]";
    }

    /**
     * Returns a new durations map, having the duration associated to player updated
     * with the given value.
     *
     * @param player  the player to apply the update
     * @param updated the new duration value
     * @return        a new durations map
     */
    private Map<Player, Duration> set(final Player player, final Duration updated) {
        assert (player != null) : "Parameter player cannot be null.";
        assert (updated != null) : "Parameter updated cannot be null.";
        final Map<Player, Duration> transientDurations = new EnumMap<Player, Duration>(this.durations);
        transientDurations.put(player, updated);
        return transientDurations;
    }

}
