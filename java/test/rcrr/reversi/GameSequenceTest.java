/*
 *  GameSequenceTest.java
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

import java.util.List;
import java.util.ArrayList;

import org.junit.*;
import static org.junit.Assert.*;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.matchers.JUnitMatchers.*;

public class GameSequenceTest {

    private static final List<GameSnapshot> NULL_SEQUENCE = null;

    private static final List<GameSnapshot> EMPTY_SEQUENCE = new ArrayList<GameSnapshot>();

    private static final List<GameSnapshot> NULL_KEY_SEQUENCE;

    static {
        GameSnapshot NULL_GAME_SNAPSHOT = null;
        NULL_KEY_SEQUENCE = new ArrayList<GameSnapshot>();
        NULL_KEY_SEQUENCE.add(NULL_GAME_SNAPSHOT);
    }

    /**
     * Test to be written.
     */
    @Test
    public void testAdd() {
        assertTrue("Test to be written.", false);
    }

    /**
     * Test to be written.
     */
    @Test
    public void testGet() {
        assertTrue("Test to be written.", false);
    }

    /**
     * Test to be written.
     */
    @Test
    public void testInitialGameSequence() {
        assertTrue("Test to be written.", false);
    }

    /**
     * Test to be written.
     */
    @Test
    public void testLast() {
        assertTrue("Test to be written.", false);
    }

    /**
     * Test to be written.
     */
    @Test
    public void testSize() {
        assertTrue("Test to be written.", false);
    }

    /**
     * Tests the valueOf factory when parameter {@code sequence} is null.
     *
     * @see GameSequence#valueOf(List)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_null() {
        GameSequence.valueOf(NULL_SEQUENCE);
    }

    /**
     * Tests the valueOf factory when parameter {@code sequence} is null.
     *
     * @see GameSequence#valueOf(List)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_nullKey() {
        GameSequence.valueOf(NULL_KEY_SEQUENCE);
    }

    /**
     * Tests the valueOf factory when parameter {@code sequence} is null.
     *
     * @see GameSequence#valueOf(List)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testValueOf_boundaryConditions_empty() {
        GameSequence.valueOf(EMPTY_SEQUENCE);
    }

    /**
     * Test to be written.
     */
    @Test
    public void testValueOf() {
        assertTrue("Test to be written.", false);
    }

}
