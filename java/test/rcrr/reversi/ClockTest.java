/*
 *  ClockTest.java
 *
 *  Copyright (c) 2010 Roberto Corradini. All rights reserved.
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

import java.util.Map;
import java.util.EnumMap;

import org.joda.time.Duration;
import org.joda.time.Period;
import org.joda.time.PeriodType;

public class ClockTest {

    private static long MILLISECOND_PER_MINUTE = 60000;

    @Test
    public void testValueOf() {
	Duration lb = new Duration(1);
	Duration lw = new Duration(1000*30*60+1);
	Map<Player, Duration> m = new EnumMap<Player, Duration>(Player.class);
	m.put(Player.BLACK, lb);
	m.put(Player.WHITE, lw);
	Clock c = Clock.valueOf(m);
	Duration tb = c.getTime(Player.BLACK);
	Duration tw = c.getTime(Player.WHITE);
        assertEquals(lb, tb);
        assertEquals(lw, tw);
    }

    @Test
    public void testInitialClock() {
	int minutes = 1;
	Duration d = Period.minutes(minutes).toStandardDuration();
	Clock c = Clock.initialClock(d);
	Duration bd = c.getTime(Player.BLACK);
	Duration wd = c.getTime(Player.WHITE);
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
    public void testSetTime() {
	Map<Player, Duration> m = new EnumMap<Player, Duration>(Player.class);
	m.put(Player.BLACK, new Duration(100));
	m.put(Player.WHITE, new Duration(100));
	Clock c = Clock.valueOf(m);
	Duration delta = new Duration(10);
	Clock updated = c.setTime(Player.BLACK, delta);
        assertEquals(c.getTime(Player.WHITE), updated.getTime(Player.WHITE));
	assertEquals(delta, c.getTime(Player.BLACK).minus(updated.getTime(Player.BLACK)));
    }

    @Test
    public void testGetTime() {
	Map<Player, Duration> m = new EnumMap<Player, Duration>(Player.class);
	m.put(Player.BLACK, new Duration(900000));
	m.put(Player.WHITE, new Duration(1));
	Clock c = Clock.valueOf(m);
	try {
	    c.getTime(null);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}
	assertEquals(new Duration(1), c.getTime(Player.WHITE));
    }

    @Test
    public void testToString() {
	Map<Player, Duration> m = new EnumMap<Player, Duration>(Player.class);
	m.put(Player.BLACK, new Duration(900000));
	m.put(Player.WHITE, new Duration(1));
	Clock c = Clock.valueOf(m);
	assertEquals("[BLACK=15:00, WHITE=00:00]", c.toString());
    }
    
}