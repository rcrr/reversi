/*
 *  ClockFixtures.java
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

import org.joda.time.Duration;
import org.joda.time.Period;

/**
 * The class host a number of predefined clocks.
 * <p>
 * The {@code Clock} class defines immutable objects thus {@code ClockFixtures}
 * implements clock instances as public static shared objects. Tests can
 * freely share the instances without any modification issue.
 */
public final class ClockFixtures {

    /** One minute duration. */
    private static final Duration ONE_MINUTE_DURATION = durationValueOfSeconds(60);

    /** Fiftynine seconds duration. */
    private static final Duration FIFTYNINE_SECONDS_DURATION = durationValueOfSeconds(59);

    /** One second duration. */
    private static final Duration ONE_SECOND_DURATION = durationValueOfSeconds(1);

    /** Twenty one seconds duration. */
    private static final Duration TWENTYONE_SECONDS_DURATION = durationValueOfSeconds(21);

    /** Sixteen seconds duration. */
    private static final Duration SIXTEEN_SECONDS_DURATION = durationValueOfSeconds(16);

    /**
     * Returns a duration object corresponding to the value of the {@code seconds} parameter.
     *
     * @param seconds the number of seconds given to the duration object
     * @return        the corresponding duration object
     */
    private static Duration durationValueOfSeconds(final int seconds) {
        return Period.seconds(seconds).toStandardDuration();
    }

    /** A generic clock instance. */
    public static final Clock AN_INSTANCE = new Clock.Builder().build();

    /** The {@code null} clock. */
    public static final Clock NULL = null;

    /**
     * Both players have one minute left.
     * <p>
     * {@code [BLACK=01:00, WHITE=01:00]}
     */
    public static final Clock ONE_MINUTE_LEFT_TO_BOTH_PLAYERS
        = new Clock.Builder()
        .withDuration(Player.BLACK, ONE_MINUTE_DURATION)
        .withDuration(Player.WHITE, ONE_MINUTE_DURATION)
        .build();

    /**
     * Black has 59 seconds left, while white has 60 ones.
     * <p>
     * {@code [BLACK=00:59, WHITE=01:00]}
     */
    public static final Clock BLACK_HAS_59_SECONDS_WHITE_60
        = new Clock.Builder()
        .withDuration(Player.BLACK, FIFTYNINE_SECONDS_DURATION)
        .withDuration(Player.WHITE, ONE_MINUTE_DURATION)
        .build();

    /**
     * Black has 1 second left, while white has 60 ones.
     * <p>
     * {@code [BLACK=00:01, WHITE=01:00]}
     */
    public static final Clock BLACK_HAS_1_SECOND_WHITE_60
        = new Clock.Builder()
        .withDuration(Player.BLACK, ONE_SECOND_DURATION)
        .withDuration(Player.WHITE, ONE_MINUTE_DURATION)
        .build();

    /**
     * Black has 21 seconds left, while white has 16 ones.
     * <p>
     * {@code [BLACK=00:21, WHITE=00:16]}
     * <p>
     * This clock fixture is equal to {@code EQL_TEST_B}
     *
     * @see ClockFixtures#EQL_TEST_B
     */
    public static final Clock EQL_TEST_A
        = new Clock.Builder()
        .withDuration(Player.BLACK, TWENTYONE_SECONDS_DURATION)
        .withDuration(Player.WHITE, SIXTEEN_SECONDS_DURATION)
        .build();

    /**
     * Black has 21 seconds left, while white has 16 ones.
     * <p>
     * {@code [BLACK=00:21, WHITE=00:16]}
     * <p>
     * This clock fixture is equal to {@code EQL_TEST_A}
     *
     * @see ClockFixtures#EQL_TEST_A
     */
    public static final Clock EQL_TEST_B
        = new Clock.Builder()
        .withDuration(Player.BLACK, TWENTYONE_SECONDS_DURATION)
        .withDuration(Player.WHITE, SIXTEEN_SECONDS_DURATION)
        .build();

    /** Class constructor. */
    private ClockFixtures() { }

}
