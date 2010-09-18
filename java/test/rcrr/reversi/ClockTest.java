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

public class ClockTest {

    private static long MILLISECOND_PER_MINUTE = 60000;

    @Test
    public void testValueOf() {
	long lb = 1;
	long lw = 1000*30*60+1;
	Clock c = Clock.valueOf(lb, lw);
	long tb = c.getTime(Player.BLACK);
	long tw = c.getTime(Player.WHITE);
        assertEquals(lb, tb);
        assertEquals(lw, tw);
    }

    @Test
    public void testInitialClock() {
	int seconds = 1;
	Clock c = Clock.initialClock(seconds);
	long tb = c.getTime(Player.BLACK);
	long tw = c.getTime(Player.WHITE);
        assertEquals(seconds * MILLISECOND_PER_MINUTE, tb);
	assertEquals(seconds * MILLISECOND_PER_MINUTE, tw);
    }

    @Test
    public void testPrintClock() {
	Clock c = Clock.valueOf(900000, 1);
	assertEquals("[@=15:00, O=00:00]", c.printClock());
    }

    @Test
    public void testSetTime() {
	long tb = 100;
	long tw = 100;
	long delta = 10;
	Clock c = Clock.valueOf(tb, tw);
	Clock updated = null;
	try {
	    updated = c.setTime(Player.BLACK, delta);
	} catch (GameOverException goe) {
	    fail(); // TODO: has to be changed.
	}
        assertEquals(c.getTime(Player.WHITE), updated.getTime(Player.WHITE));
	assertEquals(delta, c.getTime(Player.BLACK) - updated.getTime(Player.BLACK));
    }

    @Test
    public void testGetTime() {
	Clock c = Clock.valueOf(900000, 1);
	assertEquals(1, c.getTime(Player.WHITE));
    }

    @Test
    public void testToString() {
	Clock c = Clock.valueOf(900000, 1);
	assertEquals("[BLACK=15:00, WHITE=00:00]", c.toString());
    }

    @Test
    public void testDefaultGameTimeInMinutes() {
	assertEquals(30, Clock.defaultGameTimeInMinutes());
    }
    
}