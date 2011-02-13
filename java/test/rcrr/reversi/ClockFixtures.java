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
    private static final Duration ONE_MINUTE_DURATION = Period.minutes(1).toStandardDuration();

    /** Fiftynine seconds duration. */
    private static final Duration FIFTYNINE_SECONDS_DURATION = Period.seconds(59).toStandardDuration();

    /** One second duration. */
    private static final Duration ONE_SECOND_DURATION = Period.seconds(1).toStandardDuration();

    /** A generic clock instance. */
    public static final Clock AN_INSTANCE = Clock.initialClock(ONE_MINUTE_DURATION);

    /** The null clock. */
    public static final Clock NULL = null;

    /** Both players have one minute left. */
    public static final Clock ONE_MINUTE_LEFT_TO_BOTH_PLAYERS = Clock.initialClock(ONE_MINUTE_DURATION);

    /** Black has 59 seconds left, while white has 60 ones. */
    public static final Clock BLACK_HAS_59_SECONDS_WHITE_60
        = Clock.valueOf(FIFTYNINE_SECONDS_DURATION, ONE_MINUTE_DURATION);

    /** Black has 1 second left, while white has 60 ones. */
    public static final Clock BLACK_HAS_1_SECOND_WHITE_60
        = Clock.valueOf(ONE_SECOND_DURATION, ONE_MINUTE_DURATION);

    /** Class constructor. */
    private ClockFixtures() { }

}
