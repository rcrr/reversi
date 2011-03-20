/*
 *  MoveRecordTest.java
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

import org.junit.Test;
import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for the {@code MoveRecord} class.
 *
 * @see MoveRecord
 */
public class MoveRecordTest {

    /** Class constructor. */
    public MoveRecordTest() { }

    /**
     * Tests the clock getter method.
     *
     * @see MoveRecord#clock()
     */
    @Test
    public final void testClock() {
        assertThat("MoveRecord's clock for MoveRecordFixtures.GETTER_TEST_CASES",
                   MoveRecordFixtures.GETTER_TEST_CASES.clock(),
                   is(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS));
    }

    /**
     * Tests the move getter method.
     *
     * @see MoveRecord#move()
     */
    @Test
    public final void testMove() {
        assertThat("MoveRecord's move for MoveRecordFixtures.GETTER_TEST_CASES",
                   MoveRecordFixtures.GETTER_TEST_CASES.move(),
                   is(MoveFixtures.A1));
    }

    /**
     * Tests the timestamp getter method.
     *
     * @see MoveRecord#timestamp()
     */
    @Test
    public final void testTimestamp() {
        assertThat("MoveRecord's timestamp for MoveRecordFixtures.GETTER_TEST_CASES",
                   MoveRecordFixtures.GETTER_TEST_CASES.timestamp(),
                   is(CommonFixtures.INSTANT_FIRST_MILLISEC_OF_YEAR_2011));
    }

    /**
     * Tests the toString method.
     *
     * @see MoveRecord#toString()
     */
    @Test
    public final void testToString() {
        assertThat("MoveRecord's toString for MoveRecordFixtures.GETTER_TEST_CASES",
                   MoveRecordFixtures.GETTER_TEST_CASES.toString(),
                   is("[[PUT_DISC; A1]; [BLACK=01:00, WHITE=01:00]; 2011-01-01T00:00:00.001Z]"));
    }

    /**
     * Tests the valueOfAtCurrentTime factory when parameter {@code move} is null.
     *
     * @see MoveRecord#valueOfAtCurrentTime(Move, Clock)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOfAtCurrentTime_boundaryConditions_c1() {
        MoveRecord.valueOfAtCurrentTime(MoveFixtures.NULL, ClockFixtures.AN_INSTANCE);
    }

    /**
     * Tests the valueOfAtCurrentTime factory when parameter {@code clock} is null.
     *
     * @see MoveRecord#valueOfAtCurrentTime(Move, Clock)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOfAtCurrentTime_boundaryConditions_c2() {
        MoveRecord.valueOfAtCurrentTime(MoveFixtures.AN_INSTANCE, ClockFixtures.NULL);
    }

    /**
     * Tests the valueOfAtCurrentTime creation method.
     *
     * @see MoveRecord#valueOfAtCurrentTime(Move, Clock)
     */
    @Test
    public final void testValueOfAtCurrentTime() {
        assertThat("MoveRecord.valueOfAtCurrentTime(MoveFixtures.AN_INSTANCE,"
                   + " ClockFixtures.AN_INSTANCE) must be an instance of MoveRecord class.",
                   MoveRecord.valueOfAtCurrentTime(MoveFixtures.AN_INSTANCE,
                                                   ClockFixtures.AN_INSTANCE),
                   instanceOf(MoveRecord.class));
    }

    /**
     * Tests the valueOf factory when parameter {@code move} is null.
     *
     * @see MoveRecord#valueOf(Move, Clock, Instant)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_c1() {
        MoveRecord.valueOf(MoveFixtures.NULL, ClockFixtures.AN_INSTANCE, CommonFixtures.AN_INSTANT);
    }

    /**
     * Tests the valueOf factory when parameter {@code clock} is null.
     *
     * @see MoveRecord#valueOf(Move, Clock, Instant)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_c2() {
        MoveRecord.valueOf(MoveFixtures.AN_INSTANCE, ClockFixtures.NULL, CommonFixtures.AN_INSTANT);
    }

    /**
     * Tests the valueOf factory when parameter {@code timestamp} is null.
     *
     * @see MoveRecord#valueOf(Move, Clock, Instant)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_c3() {
        MoveRecord.valueOf(MoveFixtures.AN_INSTANCE, ClockFixtures.AN_INSTANCE, CommonFixtures.NULL_INSTANT);
    }

    /**
     * Tests the valueOf creation method.
     *
     * @see MoveRecord#valueOf(Move, Clock, Instant)
     */
    @Test
    public final void testValueOf() {
        assertThat("MoveRecord.valueOf(MoveFixtures.AN_INSTANCE, ClockFixtures.AN_INSTANCE, CommonFixtures.AN_INSTANT)"
                   + " must be an instance of MoveRecord class.",
                   MoveRecord.valueOf(MoveFixtures.AN_INSTANCE,
                                      ClockFixtures.AN_INSTANCE,
                                      CommonFixtures.AN_INSTANT),
                   instanceOf(MoveRecord.class));
    }

}
