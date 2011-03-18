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
import java.util.Arrays;

import org.joda.time.Duration;

import org.junit.*;
import static org.junit.Assert.*;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.matchers.JUnitMatchers.*;

public class GameSequenceTest {

    private static final List<GameSnapshot> NULL_SEQUENCE = null;

    private static final List<GameSnapshot> EMPTY_SEQUENCE = new ArrayList<GameSnapshot>();

    private static final List<GameSnapshot> NULL_KEY_SEQUENCE
        = Arrays.asList(GameSnapshotFixtures.AN_INSTANCE,
                        GameSnapshotFixtures.NULL,
                        GameSnapshotFixtures.AN_INSTANCE);

    private static final List<GameSnapshot> A_SEQUENCE
        = Arrays.asList(GameSnapshotFixtures.AN_INSTANCE);

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
     * Tests the initialGameSequence method when parameter {@code gameDuration} is null.
     *
     * @see GameSequence#initialGameSequence(Duration)
     */
    @Test(expected = NullPointerException.class)
    public final void testInitialGameSequence_boundaryConditions_null() {
        GameSequence.initialGameSequence(CommonFixtures.NULL_DURATION);
    }

    /**
     * Tests the initialGameSequence method.
     *
     * @see GameSequence#initialGameSequence(Duration)
     */
    @Test
    public void testInitialGameSequence() {
        assertThat("GameSequence.initialGameSequence(CommonFixtures.ONE_MINUTE_DURATION) must be an instance of GameSequence class.",
                   GameSequence.initialGameSequence(CommonFixtures.ONE_MINUTE_DURATION),
                   instanceOf(GameSequence.class));
    }

    /**
     * Test to be written.
     */
    @Test
    public void testLast() {
        assertTrue("Test to be written.", false);
    }

    /**
     * Tests the size method.
     *
     * @see GameSequence#size()
     */
    @Test
    public void testSize() {
        assertThat("GameSequenceFixtures.THREE_SNAPSHOTS has three game snapshots.",
                   GameSequenceFixtures.THREE_SNAPSHOTS.size(),
                   is(3));
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
     * Test the valueOf factory.
     *
     * @see GameSequence#valueOf(List)
     */
    @Test
    public void testValueOf() {
        assertThat("GameSequence.valueOf(A_SEQUENCE) must be an instance of GameSequence class.",
                   GameSequence.valueOf(A_SEQUENCE), instanceOf(GameSequence.class));
    }

}
