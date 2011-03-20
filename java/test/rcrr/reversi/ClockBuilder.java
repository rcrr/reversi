/*
 *  ClockBuilder.java
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

package rcrr.reversi;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.EnumMap;

import org.joda.time.Duration;

/**
 * A clock builder is a facility to generate clock instances for testing.
 * <p>
 * {@code ClockBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class ClockBuilder {

    /** The durations field. */
    private Map<Player, Duration> durations;

    /**
     * The class constructor.
     */
    public ClockBuilder() {
        this.durations = new EnumMap<Player, Duration>(Player.class);
        this.durations.put(Player.BLACK, CommonFixtures.A_DURATION);
        this.durations.put(Player.WHITE, CommonFixtures.A_DURATION);
    }

    /**
     * Returns a new instance of a clock object.
     *
     * @return the clock instance as prepared by the current clock's builder
     */
    public synchronized Clock build() {
        return Clock.valueOf(durations);
    }

    /**
     * The method returns the player's duration.
     *
     * @param player the player to whom get the remaining duration
     * @return       the remaning duration for the given player
     */
    private synchronized Duration get(final Player player) {
        return this.durations.get(player);
    }

    /**
     * The method assigns a duration to the given player.
     *
     * @param player   the player to whom set the remaning duration
     * @param duration the assigning duration
     */
    private synchronized void put(final Player player, final Duration duration) {
        this.durations.put(player, duration);
    }

    /**
     * The setter method for the durations field.
     *
     * @param durations the update for the durations field
     */
    private synchronized void setDurations(final Map<Player, Duration> durations) {
        this.durations = durations;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code duration}
     * value to the {@code player} entry.
     *
     * @param player   the player to whom assign the remaining duration
     * @param duration the assigning duration
     * @return         the {@code this} reference
     */
    public ClockBuilder withDuration(final Player player, final Duration duration) {
        put(player, duration);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code durations}
     * field value.
     *
     * @param durations the field hosting the two players remaning durations 
     * @return          the {@code this} reference
     */
    public ClockBuilder withDurations(final Map<Player, Duration> durations) {
        setDurations(durations);
        return this;
    }
}
