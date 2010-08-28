/*
 *  PlayerTest.java
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

public class PlayerTest {

    @Test
    public void testDescription() {
	assertEquals("The Black player", Player.BLACK.description());
	assertEquals("The White player", Player.WHITE.description());
    }

    @Test
    public void testColor() {
	assertEquals(SquareState.BLACK, Player.BLACK.color());
	assertEquals(SquareState.WHITE, Player.WHITE.color());
    }

    @Test
    public void testSymbol() {
	assertEquals("@", Player.BLACK.symbol());
	assertEquals("O", Player.WHITE.symbol());
    }

    @Test
    public void testOpponent() {
	assertEquals(Player.WHITE, Player.BLACK.opponent());
	assertEquals(Player.BLACK, Player.WHITE.opponent());
    }
    
}