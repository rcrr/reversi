/*
 *  MoveRecordFixtures.java
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

import org.joda.time.Instant;

/**
 * The class host a number of predefined move records.
 * <p>
 * The {@code MoveRecord} class defines immutable objects thus {@code MoveRecordFixtures}
 * implements move record instances as public static shared objects. Tests can
 * freely share the instances without any modification issue.
 */
public final class MoveRecordFixtures {

    /**
     * A generic instance.
     */
    public static final MoveRecord AN_INSTANCE = new MoveRecordBuilder()
        .build();

    /**
     * The null instance.
     */
    public static final MoveRecord NULL = null;

    /**
     * An instance used to test the getter methods. It is build as follow:
     * <p>
     * The move is {@code MoveFixtures.A1}.
     * <p>
     * The clock is {@code ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS}.
     * <p>
     * The timestamp is {@code CommonFixtures.INSTANT_FIRST_MILLISEC_OF_YEAR_2011}.
     *
     * @see MoveFixtures#A1
     * @see ClockFixtures#ONE_MINUTE_LEFT_TO_BOTH_PLAYERS
     * @see CommonFixtures#INSTANT_FIRST_MILLISEC_OF_YEAR_2011
     */
    public static final MoveRecord GETTER_TEST_CASES = new MoveRecordBuilder()
        .withMove(MoveFixtures.A1)
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withTimestamp(CommonFixtures.INSTANT_FIRST_MILLISEC_OF_YEAR_2011)
        .build();

    /** Class constructor. */
    private MoveRecordFixtures() { }

}
