/*
 *  GameStateTest.java
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

// To do:
// write all tests

package rcrr.reversi;

import org.junit.*;
import static org.junit.Assert.*;

public class GameStateTest {

    @Test
    public void testValueOf() {
	boolean thrown;

	/**
	 * Tests if the valueOf method throws a NullPointerException when
	 * the passed board is null.
	 */
	thrown = false;
	try {
	    GameState.valueOf(null, Player.BLACK, Clock.initialClock(1L));
	} catch (NullPointerException npe) {
	    thrown = true;
	}
	assertTrue(thrown);

	/**
	 * Tests if the valueOf method throws a NullPointerException when
	 * the passed player is null, and there are available moves.
	 */
	thrown = false;
	try {
	    GameState.valueOf(Board.initialBoard(), null, Clock.initialClock(1L));
	} catch (NullPointerException npe) {
	    thrown = true;
	}
	assertTrue(thrown);

	/**
	 * Tests if the valueOf method doesn't throw a NullPointerException when
	 * the passed player is null, but no player has legal moves.
	 */
	GameState.valueOf(Board.emptyBoard(), null, Clock.initialClock(1L));

	/**
	 * Tests if the valueOf method throws a NullPointerException when
	 * the passed clock is null.
	 */
	thrown = false;
	try {
	    GameState.valueOf(Board.emptyBoard(), Player.BLACK, null);
	} catch (NullPointerException npe) {
	    thrown = true;
	}
	assertTrue(thrown);

	Board b = Board.initialBoard();
	Player p = Player.BLACK;
	Clock c = Clock.initialClock(30L);
	GameState gs = GameState.valueOf(b, p, c);
	assertEquals(b, gs.board());
	assertEquals(p, gs.player());
	assertEquals(c, gs.clock());
    }

    @Test
    public void testPrintGameState() {
	Board b = Board.initialBoard();
	Player p = Player.BLACK;
	Clock c = Clock.initialClock(30L);
	GameState gs = GameState.valueOf(b, p, c);
	StringBuilder initialGameState = new StringBuilder();
	initialGameState.append("    a b c d e f g h [@=2 0=2 (0)]\n");
	initialGameState.append(" 1  . . . . . . . . \n");
	initialGameState.append(" 2  . . . . . . . . \n");
	initialGameState.append(" 3  . . . . . . . . \n");
	initialGameState.append(" 4  . . . O @ . . . \n");
	initialGameState.append(" 5  . . . @ O . . . \n");
	initialGameState.append(" 6  . . . . . . . . \n");
	initialGameState.append(" 7  . . . . . . . . \n");
	initialGameState.append(" 8  . . . . . . . . [@=30:00, O=30:00]\n");
	initialGameState.append(" Next to play: BLACK, legal moves: [D3, C4, F5, E6]\n");
	assertEquals(initialGameState.toString(), gs.printGameState());
    }

    /**
     * Tests the class getter methods.
     * Same test used in the testValueOf test.
     */
    @Test
    public void testGetters() {
	Board b = Board.initialBoard();
	Player p = Player.BLACK;
	Clock c = Clock.initialClock(30L);
	GameState gs = GameState.valueOf(b, p, c);
	assertEquals(b, gs.board());
	assertEquals(p, gs.player());
	assertEquals(c, gs.clock());
    }

    /**
     * Has to be written.
     */
    @Test
    public void testInitialGameState() {
	assertEquals(true, true);
    }


}