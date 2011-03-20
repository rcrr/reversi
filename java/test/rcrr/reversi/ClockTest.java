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

import org.junit.*;
import static org.junit.Assert.*;

import static org.hamcrest.CoreMatchers.*;

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

    private static long MILLISECOND_PER_MINUTE = 60000;

    @Test
    public final void testReviewReminder() {
        fail("The Test Suite must be reviewed!");
    }

    @Test
    public void testGet() {
        Map<Player, Duration> m = new EnumMap<Player, Duration>(Player.class);
        m.put(Player.BLACK, new Duration(900000));
        m.put(Player.WHITE, new Duration(1));
        Clock c = Clock.valueOf(m);
        try {
            c.get(null);
            fail("An exception must be risen.");
        } catch (NullPointerException npe) {
            assertTrue(true);
        }
        assertEquals(new Duration(1), c.get(Player.WHITE));
    }

    @Test
    public void testInitialClock() {
        int minutes = 1;
        Duration d = Period.minutes(minutes).toStandardDuration();
        Clock c = Clock.initialClock(d);
        Duration bd = c.get(Player.BLACK);
        Duration wd = c.get(Player.WHITE);
        assertEquals(bd.toPeriod(PeriodType.time()).toStandardMinutes().getMinutes(), minutes);
        assertEquals(wd.toPeriod(PeriodType.time()).toStandardMinutes().getMinutes(), minutes);
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
    public void testSet() {
        Map<Player, Duration> m = new EnumMap<Player, Duration>(Player.class);
        m.put(Player.BLACK, new Duration(100));
        m.put(Player.WHITE, new Duration(100));
        Clock c = Clock.valueOf(m);
        Duration delta = new Duration(10);
        Clock updated = c.set(Player.BLACK, delta);
        assertEquals(c.get(Player.WHITE), updated.get(Player.WHITE));
        assertEquals(delta, c.get(Player.BLACK).minus(updated.get(Player.BLACK)));
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

        /** Tests the second static factory. */
        final Clock c2 = Clock.valueOf(lb, lw);
        tb = c2.get(Player.BLACK);
        tw = c2.get(Player.WHITE);
        assertEquals(lb, tb);
        assertEquals(lw, tw);
    }
    
}
