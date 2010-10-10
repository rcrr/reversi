/*
 *  GamePositionTest.java
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

public class GamePositionTest {

    @Test
    public void testValueOf() {

	/**
	 * Tests if the valueOf method throws a NullPointerException when
	 * the passed board is null.
	 */
	try {
	    GamePosition.valueOf(null, Player.BLACK);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	/**
	 * Tests if the valueOf method throws a NullPointerException when
	 * the passed player is null, and there are available moves.
	 */
	try {
	    GamePosition.valueOf(Board.initialBoard(), null);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}
 
	/**
	 * Tests if the valueOf method doesn't throw a NullPointerException when
	 * the passed player is null, but no player has legal moves.
	 */
	try {
	    GamePosition.valueOf(Board.emptyBoard(), null);
	    assertTrue(true);
	} catch (NullPointerException npe) {
	    fail("An exception must not be risen.");
	}

	Board b = Board.initialBoard();
	Player p = Player.BLACK;
	GamePosition gs = GamePosition.valueOf(b, p);
	assertEquals(b, gs.board());
	assertEquals(p, gs.player());
    }

}