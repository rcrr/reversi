/*
 *  MoveRegisterFixtures.java
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

/**
 * The class host a number of predefined move registers.
 * <p>
 * The {@code MoveRegister} class defines immutable objects thus {@code MoveRegisterFixtures}
 * implements move register instances as public static shared objects. Tests can
 * freely share the instances without any modification issue.
 */
public final class MoveRegisterFixtures {

    /**
     * The null instance.
     */
    public static final MoveRegister NULL = null;

    /**
     * A generic instance.
     */
    public static final MoveRegister AN_INSTANCE = new MoveRegisterBuilder()
        .withRegister(Arrays.asList(MoveRecordFixtures.AN_INSTANCE))
        .build();

    /**
     * An instance having three records.
     * <p>
     * The instance is described as follow:
     * <ul>
     *  <li><i>1st move record</i> {@code [[PUT_DISC; A1]; [BLACK=01:00, WHITE=01:00]; 2011-01-01T00:00:00.001Z]}</li>
     *  <li><i>2nd move record</i> {@code [[PASS; null];   [BLACK=00:59, WHITE=01:00]; 2011-01-01T00:00:01.001Z]}</li>
     *  <li><i>3rd move record</i> {@code [[PUT_DISC; B3]; [BLACK=00:01, WHITE=01:00]; 2011-01-01T00:01:00.001Z]}</li>
     * </ul>
     */
    public static final MoveRegister THREE_RECORDS = new MoveRegisterBuilder()
        .withRegister(Arrays.asList(new MoveRecordBuilder()
                                    .withMove(MoveFixtures.A1)
                                    .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
                                    .withTimestamp(CommonFixtures.INSTANT_FIRST_MILLISEC_OF_YEAR_2011)
                                    .build(),
                                    new MoveRecordBuilder()
                                    .withMove(MoveFixtures.PASS)
                                    .withClock(ClockFixtures.BLACK_HAS_59_SECONDS_WHITE_60)
                                    .withTimestamp(CommonFixtures.INSTANT_FIRST_MILLISEC_OF_YEAR_2011_PLUS_A_SECOND)
                                    .build(),
                                    new MoveRecordBuilder()
                                    .withMove(MoveFixtures.B3)
                                    .withClock(ClockFixtures.BLACK_HAS_1_SECOND_WHITE_60)
                                    .withTimestamp(CommonFixtures.INSTANT_FIRST_MILLISEC_OF_YEAR_2011_PLUS_A_MINUTE)
                                    .build()))
        .build();

    /**
     * An empty instance.
     */
    public static final MoveRegister EMPTY = MoveRegister.empty();

    /** Class constructor. */
    private MoveRegisterFixtures() { }

}
