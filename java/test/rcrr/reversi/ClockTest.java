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

    private static Long MILLISECOND_PER_MINUTE = 60000L;

    @Test public void valueOf() {
	Long lb = Long.valueOf(1);
	Long lw = Long.valueOf(1000*30*60+1);
	Clock c = Clock.valueOf(lb, lw);
	Long tb = c.getTime(Player.BLACK);
	Long tw = c.getTime(Player.WHITE);
        assertEquals(lb, tb);
        assertEquals(lw, tw);
    }

    @Test public void initialClock() {
	Long time = Long.valueOf(1);
	Clock c = Clock.initialClock(time);
	Long tb = c.getTime(Player.BLACK);
	Long tw = c.getTime(Player.WHITE);
        assertEquals(Long.valueOf(time * MILLISECOND_PER_MINUTE), tb);
	assertEquals(Long.valueOf(time * MILLISECOND_PER_MINUTE), tw);
    }

    @Test public void setTime() {
	Long tb = Long.valueOf(100);
	Long tw = Long.valueOf(100);
	Long delta = Long.valueOf(10);
	Clock c = Clock.valueOf(tb, tw);
	Clock updated = null;
	try {
	    updated = c.setTime(Player.BLACK, delta);
	} catch (GameOverException goe) {
	}
        assertEquals(c.getTime(Player.WHITE), updated.getTime(Player.WHITE));
	assertEquals(delta, Long.valueOf(c.getTime(Player.BLACK) - updated.getTime(Player.BLACK)));
    }
    
}