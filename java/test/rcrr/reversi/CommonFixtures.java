/*
 *  CommonFixtures.java
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

import java.io.PrintStream;

import org.joda.time.DateTime;
import org.joda.time.Instant;
import org.joda.time.Duration;
import org.joda.time.Period;

/**
 * The class hosts a number of predefined common immutable objects.
 */
public final class CommonFixtures {

    /** Year 2011. */
    private static final int YEAR_2011 = 2011;

    /** January, as the first month of the year. */
    private static final int MONTH_JAN = 1;

    /** The first day of the month. */
    private static final int DAY_OF_MONTH_01 = 1;

    /** The first hour of the day. */
    private static final int HOUR_OF_DAY_01 = 1;

    /** The minute zero of the hour. */
    private static final int MINUTE_OF_HOUR_00 = 0;

    /** The second zero of the minute. */
    private static final int SECOND_OF_MINUTE_00 = 0;

    /** The first millisecond of the second. */
    private static final int MILLIS_OF_SECOND_001 = 1;

    /** The first minute of the hour. */
    private static final int MINUTE_OF_HOUR_01 = 1;

    /** The first second of the minute. */
    private static final int SECOND_OF_MINUTE_01 = 1;

    /** The null duration. */
    public static final Duration NULL_DURATION = null;

    /** A generic duration. */
    public static final Duration A_DURATION = Period.minutes(1).toStandardDuration();

    /** A ten minutes duration. */
    public static final Duration TEN_MINUTES_DURATION = Period.minutes(10).toStandardDuration();

    /** A one minute duration. */
    public static final Duration ONE_MINUTE_DURATION = Period.minutes(1).toStandardDuration();

    /**
     * The null instant.
     */
    public static final Instant NULL_INSTANT = null;

    /**
     * A generic instant.
     */
    public static final Instant AN_INSTANT
        = new DateTime(1969,
                       5,
                       18,
                       5,
                       30,
                       0,
                       1).toInstant();

    /**
     * An instant corresponding to the first millisecond of year 2011.
     */
    public static final Instant INSTANT_FIRST_MILLISEC_OF_YEAR_2011
        = new DateTime(YEAR_2011,
                       MONTH_JAN,
                       DAY_OF_MONTH_01,
                       HOUR_OF_DAY_01,
                       MINUTE_OF_HOUR_00,
                       SECOND_OF_MINUTE_00,
                       MILLIS_OF_SECOND_001).toInstant();

    /**
     * An instant corresponding to the first millisecond of year 2011,
     * plus a second.
     */
    public static final Instant INSTANT_FIRST_MILLISEC_OF_YEAR_2011_PLUS_A_SECOND
        = new DateTime(YEAR_2011,
                       MONTH_JAN,
                       DAY_OF_MONTH_01,
                       HOUR_OF_DAY_01,
                       MINUTE_OF_HOUR_00,
                       SECOND_OF_MINUTE_01,
                       MILLIS_OF_SECOND_001).toInstant();

    /**
     * An instant corresponding to the first millisecond of year 2011,
     * plus a minute.
     */
    public static final Instant INSTANT_FIRST_MILLISEC_OF_YEAR_2011_PLUS_A_MINUTE
        = new DateTime(YEAR_2011,
                       MONTH_JAN,
                       DAY_OF_MONTH_01,
                       HOUR_OF_DAY_01,
                       MINUTE_OF_HOUR_01,
                       SECOND_OF_MINUTE_00,
                       MILLIS_OF_SECOND_001).toInstant();

    /** The null print stream. */
    public static final PrintStream NULL_PRINT_STREAM = null;

    /** Class constructor. */
    private CommonFixtures() { }

}
