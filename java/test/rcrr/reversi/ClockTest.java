/*
 *  ClockTest.java
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

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.instanceOf;

import java.util.Map;
import java.util.EnumMap;
import java.util.HashMap;

import org.joda.time.Duration;
import org.joda.time.Period;
import org.joda.time.PeriodType;

/**
 * Test Suite for {@code Clock} class.
 * <p>
 * Several thing to do:
 * <ul>
 *   <li>Comment assert statement.</li>
 *   <li>Boundary conditions are not checked.</li>
 *   <li>Make a Builder and Fixtures.</li>
 *   <li>Complete javadocs and style.</li>
 * </ul>
 */
public class ClockTest {

    /** Number of milliseconds equivalent to a minute. */
    private static final long MILLISECOND_PER_MINUTE = 60000;

    /** Class constructor. */
    public ClockTest() { }

    @Test
    public final void testReviewReminder() {
        fail("The Test Suite must be reviewed!");
    }

    /**
     * Tests the {@code decrement(Player, Duration)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see Clock#decrement(Player, Duration)
     */
    @Test(expected = NullPointerException.class)
    public final void testDecrement_boundaryConditions_checkNullParameter_player() {
        new ClockBuilder().build()
            .decrement(Player.NULL, CommonFixtures.A_DURATION);
    }

    /**
     * Tests the {@code decrement(Player, Duration)} method when parameter
     * {@code delta} is {@code null}.
     *
     * @see Clock#decrement(Player, Duration)
     */
    @Test(expected = NullPointerException.class)
    public final void testDecrement_boundaryConditions_checkNullParameter_delta() {
        new ClockBuilder().build()
            .decrement(Player.AN_INSTANCE, CommonFixtures.NULL_DURATION);
    }

    /**
     * Tests the {@code decrement(Player, Duration)} method when parameter
     * {@code delta} is negative.
     *
     * @see Clock#decrement(Player, Duration)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testDecrement_boundaryConditions_checkNegativeParameter_delta() {
        new ClockBuilder().build()
            .decrement(Player.AN_INSTANCE, new Duration(-1L));
    }

    /**
     * Tests the {@code decrement(Player, Duration)} method.
     * <p>
     * It test the method when the delta duration is shorter than the
     * available time.
     *
     * @see Clock#decrement(Player, Duration)
     */
    @Test
    public void testDecrement() {

        Player player = Player.AN_INSTANCE;
        Duration before = new Duration(300L);
        Duration delta = new Duration(100L);
        Duration after = new Duration(200L);

        assertThat("Starting from a player having 300L, subtracting 100L,"
                   + " the returned clock must have a duration of 200L left to the player.",
                   new ClockBuilder()
                   .withDuration(player, before)
                   .build()
                   .decrement(player, delta)
                   .get(player),
                   is(after));

        assertThat("Starting from a both players having 300L, subtracting 100L from one,"
                   + " the returned clock must have a duration of 200L left to the other player.",
                   new ClockBuilder()
                   .withDuration(player, before)
                   .withDuration(player.opponent(), before)
                   .build()
                   .decrement(player, delta)
                   .get(player.opponent()),
                   is(before));
    }

    /**
     * Tests the {@code decrement(Player, Duration)} method.
     * <p>
     * It test the method when the delta duration is longer than the
     * available time.
     *
     * @see Clock#decrement(Player, Duration)
     */
    @Test
    public void testDecrement_whenDeltaIsLongerThanTheAvailableTime() {

        Player player = Player.AN_INSTANCE;
        Duration before = new Duration(300L);
        Duration delta = new Duration(500L);
        Duration after = Duration.ZERO;

        assertThat("Starting from a player having 300L, subtracting 500L,"
                   + " the returned clock must have a duration of 0L left to the player.",
                   new ClockBuilder()
                   .withDuration(player, before)
                   .build()
                   .decrement(player, delta)
                   .get(player),
                   is(after));
    }

    /**
     * Tests the {@code decrement(Player, Duration)} method.
     * <p>
     * It test the method when the delta duration is equal to the
     * available time.
     *
     * @see Clock#decrement(Player, Duration)
     */
    @Test
    public void testDecrement_whenDeltaIsEqualToTheAvailableTime() {

        Player player = Player.AN_INSTANCE;
        Duration before = new Duration(300L);
        Duration delta = new Duration(300L);
        Duration after = Duration.ZERO;

        assertThat("Starting from a player having 300L, subtracting 300L,"
                   + " the returned clock must have a duration of 0L left to the player.",
                   new ClockBuilder()
                   .withDuration(player, before)
                   .build()
                   .decrement(player, delta)
                   .get(player),
                   is(after));
    }

    /**
     * Tests the {@code equals(Object)} method when the objects are different.
     * <p>
     * The test runs the following assertions:
     * <p>
     * <ul>
     *   <li>{@code ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS}
     *       is not {@code ClockFixtures.NULL}</li>
     *   <li>{@code ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS}
     *       is not {@code new Object()}</li>
     *   <li>{@code ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS}
     *       is not {@code ClockFixtures.BLACK_HAS_1_SECOND_WHITE_60}</li>
     * </ul>
     *
     * @see Clock#equals(Object)
     */
    @Test
    public final void testEquals_whenObjectIsDifferent() {
        assertThat("ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS"
                   + " must not be equal to ClockFixtures.NULL.",
                   ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS,
                   is(not(ClockFixtures.NULL)));
        assertThat("ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS must not be equal to a new Object().",
                   ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS,
                   is(not(new Object())));
        assertThat("ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS"
                   + " must not be equal to ClockFixtures.BLACK_HAS_1_SECOND_WHITE_60.",
                   ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS,
                   is(not(ClockFixtures.BLACK_HAS_1_SECOND_WHITE_60)));
    }

    /**
     * Tests the {@code equals(Object)} method when the two objects are the same.
     * <p>
     * The test runs the following assertions:
     * <p>
     * <ul>
     *   <li>{@code ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS}
     *       is equal to {@code ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS}</li>
     *   <li>{@code ClockFixtures.EQL_TEST_A}
     *       is equal to {@code ClockFixtures.EQL_TEST_A}</li>
     *   <li>{@code ClockFixtures.EQL_TEST_B}
     *       is equal to {@code ClockFixtures.EQL_TEST_B}</li>
     * </ul>
     *
     * @see Clock#equals(Object)
     * @see ClockFixtures#ONE_MINUTE_LEFT_TO_BOTH_PLAYERS
     * @see ClockFixtures#EQL_TEST_A
     * @see ClockFixtures#EQL_TEST_B
     */
    @Test
    public final void testEquals_whenObjectIsTheSameObject() {
        assertThat("ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS"
                   + " must be equal to ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS.",
                   ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS,
                   is(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS));
        assertThat("ClockFixtures.EQL_TEST_A must be equal to ClockFixtures.EQL_TEST_A.",
                   ClockFixtures.EQL_TEST_A,
                   is(ClockFixtures.EQL_TEST_A));
        assertThat("ClockFixtures.EQL_TEST_B must be equal to ClockFixtures.EQL_TEST_B.",
                   ClockFixtures.EQL_TEST_B,
                   is(ClockFixtures.EQL_TEST_B));
    }

    /**
     * Tests the {@code equals(Object)} method.
     * <p>
     * The test checks that the two clocks have not the seme identity,
     * then it runs the following assertions:
     * <p>
     * <ul>
     *   <li>{@code ClockFixtures.EQL_TEST_A.equals(ClockFixtures.EQL_TEST_A)} is true.</li>
     *   <li>{@code ClockFixtures.EQL_TEST_B.equals(ClockFixtures.EQL_TEST_B)} is true.</li>
     *   <li>{@code ClockFixtures.EQL_TEST_A.equals(ClockFixtures.EQL_TEST_B)} is true.</li>
     *   <li>{@code ClockFixtures.EQL_TEST_B.equals(ClockFixtures.EQL_TEST_A)} is true.</li>
     * </ul>
     *
     * @see Board#equals(Object)
     * @see ClockFixtures#EQL_TEST_A
     * @see ClockFixtures#EQL_TEST_B
     */
    @Test
    public final void testEquals_whenObjectIsNotTheSameObject_andIsEqual() {

        /** Checks that the two object are really not the same. */
        assertFalse("ClockFixtures.EQL_TEST_A and ClockFixtures.EQL_TEST_B"
                    + " must be two different object, otherwise the test is fouled.",
                    ClockFixtures.EQL_TEST_A == ClockFixtures.EQL_TEST_B);

        assertTrue("ClockFixtures.EQL_TEST_A.equals(ClockFixtures.EQL_TEST_A)"
                   + " must return true.",
                   ClockFixtures.EQL_TEST_A.equals(ClockFixtures.EQL_TEST_A));
        assertTrue("ClockFixtures.EQL_TEST_B.equals(ClockFixtures.EQL_TEST_B)"
                   + " must return true.",
                   ClockFixtures.EQL_TEST_B.equals(ClockFixtures.EQL_TEST_B));
        assertTrue("ClockFixtures.EQL_TEST_A.equals(ClockFixtures.EQL_TEST_B)"
                   + " must return true.",
                   ClockFixtures.EQL_TEST_A.equals(ClockFixtures.EQL_TEST_B));
        assertTrue("ClockFixtures.EQL_TEST_B.equals(ClockFixtures.EQL_TEST_A)"
                   + " must return true.",
                   ClockFixtures.EQL_TEST_B.equals(ClockFixtures.EQL_TEST_A));
    }

    /**
     * Tests the {@code get(Player)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see Clock#get(Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testGet_boundaryConditions_checkNullParameter_player() {
        new ClockBuilder().build()
            .get(Player.NULL);
    }

    /**
     * Tests the {@code get(Player)} method.
     * <p>
     * <p>
     * The test runs the following assertion:
     * <ul>
     *   <li>{@code new ClockBuilder().withDuration(Player.BLACK, new Duration(3)).build().get(Player.BLACK)}
     *       must return an object equal to {@code new Duration(3)}.</li>
     * </ul>
     *
     * @see Clock#get(Player)
     */
    @Test
    public void testGet() {
        assertThat("new ClockBuilder()"
                   + ".withDuration(Player.BLACK, new Duration(3))"
                   + ".build()"
                   + ".get(Player.BLACK)"
                   + " must return an object equal to a new Duration(3) instance.",
                   new ClockBuilder()
                   .withDuration(Player.BLACK, new Duration(3))
                   .build()
                   .get(Player.BLACK),
                   is(new Duration(3)));
    }

    /**
     * Tests the {@code initialClock(Duration)} factory when parameter
     * {@code initialDuration} is {@code null}.
     *
     * @see initialClock#get(Duration)
     */
    @Test(expected = NullPointerException.class)
    public final void testInitialClock_boundaryConditions_checkNullParameter_initialDuration() {
        Clock.initialClock(CommonFixtures.NULL_DURATION);
    }

    /**
     * Tests the {@code initialClock(Duration)} factory.
     *
     * @see initialClock#get(Duration)
     */
    @Test
    public void testInitialClock() {

        final Duration duration = new Duration(300L);

        assertThat("The clock created using Clock.initialClock(duration)"
                   + " must be equal to the one created assigning to black and white"
                   + " players the same duration amounts.",
                   Clock.initialClock(duration),
                   is(new ClockBuilder()
                      .withDuration(Player.BLACK, duration)
                      .withDuration(Player.WHITE, duration)
                      .build()));
    }

    @Test
    public void testPrintClock() {
        Map<Player, Duration> m = new EnumMap<Player, Duration>(Player.class);
        m.put(Player.BLACK, new Duration(900000));
        m.put(Player.WHITE, new Duration(1));
        Clock c = Clock.valueOf(m);
        assertEquals("[@=15:00, O=00:00]", c.printClock());
    }

    @Test
    public void testToString() {
        Map<Player, Duration> m = new EnumMap<Player, Duration>(Player.class);
        m.put(Player.BLACK, new Duration(900000));
        m.put(Player.WHITE, new Duration(1));
        Clock c = Clock.valueOf(m);
        assertEquals("[BLACK=15:00, WHITE=00:00]", c.toString());
    }

    /**
     * Test the {@code valueOf(Map<Player, Duration)} factory.
     * <p>
     * The factory receives the durations parameter, and any further change to it
     * must not be reflected to the returned clock instance.
     *
     * @see Clock#valueOf(Map)
     */
    @Test
    public final void testValueOf_durationsMustBeUnchangeable() {

        final Map<Player, Duration> changeable = new EnumMap<Player, Duration>(Player.class);
        changeable.put(Player.BLACK, CommonFixtures.A_DURATION);
        changeable.put(Player.WHITE, CommonFixtures.TEN_MINUTES_DURATION);
        final Clock instance = Clock.valueOf(changeable);
        changeable.put(Player.WHITE, CommonFixtures.A_DURATION);

        assertThat("The clock instance must be not affected by a"
                   + " change in the duratioons parameter.",
                   instance.get(Player.WHITE), is(CommonFixtures.TEN_MINUTES_DURATION));
    }


    /**
     * Test the {@code valueOf(Map<Player, Duration)} factory.
     * <p>
     * The factory receives the durations parameter, it cannot contains null values.
     *
     * @see Clock#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_durationsMustNotContainNullValues() {
        final Map<Player, Duration> corruptedDurations = new HashMap<Player, Duration>();
        corruptedDurations.put(Player.BLACK, CommonFixtures.A_DURATION);
        corruptedDurations.put(Player.WHITE, CommonFixtures.NULL_DURATION);
        Clock.valueOf(corruptedDurations);
    }

    /**
     * Test the {@code valueOf(Map<Player, Duartion>)} factory.
     * <p>
     * The factory receives the durations parameter, it cannot contains null keys.
     *
     * @see Clock#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_durationsMustNotContainNullKeys() {
        final Map<Player, Duration> corruptedDurations = new HashMap<Player, Duration>();
        corruptedDurations.put(Player.BLACK, CommonFixtures.A_DURATION);
        corruptedDurations.put(Player.NULL, CommonFixtures.A_DURATION);
        Clock.valueOf(corruptedDurations);
    }

    /**
     * Test the {@code valueOf(Map<Player, Duartion>)} factory.
     * <p>
     * The factory receives the durations parameter, it cannot contain negative values.
     *
     * @see Clock#valueOf(Map)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testValueOf_durationsMustNotContainNegativeValues() {
        final Map<Player, Duration> corruptedDurations = new HashMap<Player, Duration>();
        corruptedDurations.put(Player.BLACK, new Duration(-1L));
        corruptedDurations.put(Player.WHITE, CommonFixtures.A_DURATION);
        Clock.valueOf(corruptedDurations);
    }

    /**
     * Test the {@code valueOf(Map<Player, Duartion>)} factory.
     * <p>
     * The factory receives the durations parameter, it must have a number
     * of entries equal to the players count.
     *
     * @see Clock#valueOf(Map)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testValueOf_durationsMustHaveTwoEntries() {
        final Map<Player, Duration> corruptedDurations = new HashMap<Player, Duration>();
        corruptedDurations.put(Player.BLACK, CommonFixtures.A_DURATION);
        Clock.valueOf(corruptedDurations);
    }

    /**
     * Test the {@code valueOf(Map<Player, Duartion>)} factory.
     * <p>
     * The factory receives the durations parameter, it cannot be null.
     *
     * @see Clock#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_durationsCannotBeNull() {
        final Map<Player, Duration> nullDurations = null;
        Clock.valueOf(nullDurations);
    }

    @Test
    public void testValueOf() {
        final Duration lb = new Duration(1);
        final Duration lw = new Duration(1000*30*60+1);
        Map<Player, Duration> m = new EnumMap<Player, Duration>(Player.class);
        m.put(Player.BLACK, lb);
        m.put(Player.WHITE, lw);

        Duration tb;
        Duration tw;

        /** Tests the first static factory. */
        final Clock c1 = Clock.valueOf(m);
        tb = c1.get(Player.BLACK);
        tw = c1.get(Player.WHITE);
        assertEquals(lb, tb);
        assertEquals(lw, tw);

    }
    
}
