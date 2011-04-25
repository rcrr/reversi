/*
 *  PlayerTest.java
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

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code Player} enum.
 *
 * @see Player
 */
public class PlayerTest {

    /** Class constructor. */
    public PlayerTest() { }

    /**
     * Tests the {@code description()} method.
     *
     * @see Player#description()
     */
    @Test
    public final void testDescription() {
        assertThat("Player.BLACK.description() must return The Black player.",
                   Player.BLACK.description(),
                   is("The Black player"));
        assertThat("Player.WHITE.description() must return The White player.",
                   Player.WHITE.description(),
                   is("The White player"));
    }

    /**
     * Tests the {@code color()} method.
     *
     * @see Player#color()
     */
    @Test
    public final void testColor() {
        assertThat("Player.BLACK.color() is SquareState.BLACK.",
                   Player.BLACK.color(),
                   is(SquareState.BLACK));
        assertThat("Player.WHITE.color() is SquareState.WHITE.",
                   Player.WHITE.color(),
                   is(SquareState.WHITE));
    }

    /**
     * Tests the {@code symbol()} method.
     *
     * @see Player#symbol()
     */
    @Test
    public final void testSymbol() {
        assertThat("Player.BLACK.symbol() is @.",
                   Player.BLACK.symbol(),
                   is("@"));
        assertThat("Player.WHITE.symbol() is O.",
                   Player.WHITE.symbol(),
                   is("O"));
    }

    /**
     * Tests the {@code opponent()} method.
     *
     * @see Player#opponent()
     */
    @Test
    public final void testOpponent() {
        assertThat("Player.BLACK.opponent() is Player.WHITE.",
                   Player.BLACK.opponent(),
                   is(Player.WHITE));
        assertThat("Player.WHITE.opponent() is Player.BLACK.",
                   Player.WHITE.opponent(),
                   is(Player.BLACK));
    }

}
