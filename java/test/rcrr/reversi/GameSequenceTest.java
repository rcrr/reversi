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

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for the {@code GameSequence} class.
 *
 * @see GameSequence
 */
public class GameSequenceTest {

    private static final List<GameSnapshot> NULL_SEQUENCE = null;

    private static final List<GameSnapshot> EMPTY_SEQUENCE = new ArrayList<GameSnapshot>();

    private static final List<GameSnapshot> NULL_VALUE_SEQUENCE
        = Arrays.asList(GameSnapshotFixtures.AN_INSTANCE,
                        GameSnapshotFixtures.NULL,
                        GameSnapshotFixtures.AN_INSTANCE);

    private static final List<GameSnapshot> A_SEQUENCE
        = Arrays.asList(GameSnapshotFixtures.AN_INSTANCE);

    /**
     * Test to be written.
     */
    @Test
    public final void testAdd() {
        assertTrue("Test to be written.", false);
    }

    /**
     * Test to be written.
     */
    @Test
    public final void testGet() {
        assertTrue("Test to be written.", false);
    }

    /**
     * Tests the {@code initialGameSequence(Duration)} factory when
     * parameter {@code gameDuration} is {@code null}.
     *
     * @see GameSequence#initialGameSequence(Duration)
     */
    @Test(expected = NullPointerException.class)
    public final void testInitialGameSequence_boundaryConditions_null() {
        GameSequence.initialGameSequence(CommonFixtures.NULL_DURATION);
    }

    /**
     * Tests the {@code initialGameSequence(Duration)} factory.
     *
     * @see GameSequence#initialGameSequence(Duration)
     */
    @Test
    public final void testInitialGameSequence() {
        assertThat("GameSequence.initialGameSequence(CommonFixtures.A_DURATION)"
                   + " must be an instance of GameSequence class.",
                   GameSequence.initialGameSequence(CommonFixtures.A_DURATION),
                   instanceOf(GameSequence.class));
    }

    /**
     * Tests the {@code last()} method.
     *
     * @see GameSequence#last()
     */
    @Test
    public final void testLast() {
        assertThat("GameSequenceFixtures.THREE_SNAPSHOTS last game snapshot"
                   + " must be GameSnapshotFixtures.G00_S02.",
                   GameSequenceFixtures.THREE_SNAPSHOTS.last(),
                   is(GameSnapshotFixtures.G00_S02));
    }

    /**
     * Tests the {@code size()} method.
     *
     * @see GameSequence#size()
     */
    @Test
    public final void testSize() {
        assertThat("GameSequenceFixtures.THREE_SNAPSHOTS has three game snapshots.",
                   GameSequenceFixtures.THREE_SNAPSHOTS.size(),
                   is(3));
    }

    /**
     * Tests the {@code valueOf(List<GameSnapshot>)} factory when parameter
     * {@code sequence} is {@code null}.
     *
     * @see GameSequence#valueOf(List)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_null() {
        GameSequence.valueOf(NULL_SEQUENCE);
    }

    /**
     * Tests the {@code valueOf(List<GameSnapshot>)} factory when parameter
     * {@code sequence} contains a null value.
     *
     * @see GameSequence#valueOf(List)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_nullValue() {
        GameSequence.valueOf(NULL_VALUE_SEQUENCE);
    }

    /**
     * Tests the {@code valueOf(List<GameSnapshot>)} factory when parameter
     * {@code sequence} is empty.
     *
     * @see GameSequence#valueOf(List)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testValueOf_boundaryConditions_empty() {
        GameSequence.valueOf(EMPTY_SEQUENCE);
    }

    /**
     * Test the {@code valueOf(List<GameSnapshot>)} factory.
     *
     * @see GameSequence#valueOf(List)
     */
    @Test
    public final void testValueOf() {
        assertThat("GameSequence.valueOf(A_SEQUENCE) must be an instance of GameSequence class.",
                   GameSequence.valueOf(A_SEQUENCE), instanceOf(GameSequence.class));
    }

    /**
     * Test the {@code valueOf(List<GameSnapshot>)} factory.
     * <p>
     * The factory receives the sequence parameter, and any further change to it
     * must not be reflected to the returned game sequence instance.
     *
     * @see GameSequence#valueOf(List)
     */
    @Test
    public final void testValueOf_sequenceMustBeUnchanceable() {

        final List<GameSnapshot> changeable = new ArrayList<GameSnapshot>();
        changeable.add(GameSnapshotFixtures.G00_S00);
        final GameSequence instance = GameSequence.valueOf(changeable);
        changeable.add(GameSnapshotFixtures.G00_S01);

        assertThat("The game snapshot instance must be not affected by a"
                   + " change in the sequence parameter.",
                   instance.last(), is(GameSnapshotFixtures.G00_S00));
    }

}
