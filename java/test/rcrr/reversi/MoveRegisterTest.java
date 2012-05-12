/*
 *  MoveRegisterTest.java
 *
 *  Copyright (c) 2011 Roberto Corradini. All rights reserved.
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

import rcrr.reversi.board.Board;
import rcrr.reversi.board.BoardFixtures;
import rcrr.reversi.board.BoardBuilder;
import rcrr.reversi.board.Player;
import rcrr.reversi.board.Square;
import rcrr.reversi.board.SquareState;

import org.junit.Test;
import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for the {@code MoveRegister} class.
 *
 * @see MoveRegister
 */
public class MoveRegisterTest {

    /** The null register field value. */
    private static final List<MoveRecord> NULL_RECORDS = null;

    /** Class constructor. */
    public MoveRegisterTest() { }

    /**
     * Tests the {@code last()} method.
     *
     * @see MoveRegister#last()
     */
    @Test
    public final void testLast() {
        assertThat("MoveRegisterFixtures.THREE_RECORDS has to return a record"
                   + " having MoveFixtures.B3 as move.",
                   MoveRegisterFixtures.THREE_RECORDS.last().move(),
                   is(MoveFixtures.B3));
    }

    /**
     * Tests the {@code push(MoveRecord)} method when parameter {@code record} is {@code null}.
     *
     * @see MoveRegister#push(MoveRecord)
     */
    @Test(expected = NullPointerException.class)
    public final void testPush_boundaryConditions_nullParameter() {
        MoveRegisterFixtures.EMPTY.push(MoveRecordFixtures.NULL);
    }

    /**
     * Tests that the {@code push(MoveRecord)} method returns a {@code MoveRegister} instance.
     *
     * @see MoveRegister#push(MoveRecord)
     */
    @Test
    public final void testPush_returnsAMoveRecordInstance() {
        assertThat("MoveRegisterFixtures.EMPTY.push(MoveRecordFixtures.AN_INSTANCE)"
                   + " must return an instance of MoveRegister class.",
                   MoveRegisterFixtures.EMPTY.push(MoveRecordFixtures.AN_INSTANCE),
                   instanceOf(MoveRegister.class));
    }

    /**
     * Tests that the {@code push(MoveRecord)} method returns a {@code MoveRegister} instance having
     * a size incremented by one.
     *
     * @see MoveRegister#push(MoveRecord)
     */
    @Test
    public final void testPush_returnsALongerInstance() {
        assertThat("MoveRegisterFixtures.EMPTY.push(MoveRecordFixtures.AN_INSTANCE).size()"
                   + " must return a size value equal to 1.",
                   MoveRegisterFixtures.EMPTY.push(MoveRecordFixtures.AN_INSTANCE).size(),
                   is(1));
    }

    /**
     * Tests that the {@code push(MoveRecord)} method returns a {@code MoveRegister} instance having
     * as last element the pushed record.
     *
     * @see MoveRegister#push(MoveRecord)
     */
    @Test
    public final void testPush_returnsAConsistentLastElement() {
        assertThat("MoveRegisterFixtures.EMPTY.push(MoveRecordFixtures.AN_INSTANCE).last()"
                   + " must return a record equal to MoveRecordFixtures.AN_INSTANCE.",
                   MoveRegisterFixtures.EMPTY.push(MoveRecordFixtures.AN_INSTANCE).last(),
                   is(MoveRecordFixtures.AN_INSTANCE));
    }

    /**
     * Tests the {@code size()} method when the move register is empty.
     *
     * @see MoveRegister#size()
     */
    @Test
    public final void testSize_whenEmpty() {
        assertThat("MoveRegisterFixtures.EMPTY has no records.",
                   MoveRegisterFixtures.EMPTY.size(),
                   is(0));
    }

    /**
     * Tests the {@code size()} method when the move register is not empty.
     *
     * @see MoveRegister#size()
     */
    @Test
    public final void testSize_whenNotEmpty() {
        assertThat("MoveRegisterFixtures.THREE_RECORDS has three records.",
                   MoveRegisterFixtures.THREE_RECORDS.size(),
                   is(3));
    }

    /**
     * Tests the {@code toString()} method when the move register is not empty.
     *
     * @see MoveRegister#toString()
     */
    @Test
    public final void testToString_whenNotEmpty() {
        assertThat("Method applied to MoveRegisterFixtures.THREE_RECORDS.",
                   MoveRegisterFixtures.THREE_RECORDS.toString(),
                   is("[[PUT_DISC; A1]; [BLACK=01:00, WHITE=01:00]; 2011-01-01T00:00:00.001Z]\n"
                      + "[[PASS; null]; [BLACK=00:59, WHITE=01:00]; 2011-01-01T00:00:01.001Z]\n"
                      + "[[PUT_DISC; B3]; [BLACK=00:01, WHITE=01:00]; 2011-01-01T00:01:00.001Z]\n"));
    }

    /**
     * Tests the {@code toString()} method when the move register is empty.
     *
     * @see MoveRegister#toString()
     */
    @Test
    public final void testToString_whenEmpty() {
        assertThat("Method applied to MoveRegisterFixtures.EMPTY.",
                   MoveRegisterFixtures.EMPTY.toString(),
                   is("[EMPTY MoveRegister]\n"));
    }

    /**
     * Tests the {@code isEmpty()} method when the move record is empty.
     * <p>
     * The test runs against the {@link MoveRegisterFixtures#EMPTY} fixture.
     *
     * @see MoveRegister#isEmpty()
     */
    @Test
    public final void testIsEmpty_whenItIs() {
        assertThat("MoveRegisterFixtures.EMPTY is empty.",
                   MoveRegisterFixtures.EMPTY.isEmpty(),
                   is(true));
    }

    /**
     * Tests the {@code isEmpty()} method when the move record is not empty.
     * <p>
     * The test runs against the {@link MoveRegisterFixtures#THREE_RECORDS} fixture.
     *
     * @see MoveRegister#isEmpty()
     */
    @Test
    public final void testIsEmpty_whenItIsNot() {
        assertThat("MoveRegisterFixtures.THREE_RECORDS is not empty.",
                   MoveRegisterFixtures.THREE_RECORDS.isEmpty(),
                   is(false));
    }

    /**
     * Tests the {@code empty(Player)} factory.
     *
     * @see MoveRegister#empty(Player)
     */
    @Test
    public final void testEmpty() {
        assertThat("MoveRegister.empty() must return an instance of MoveRegister class.",
                   MoveRegister.empty(Player.AN_INSTANCE), instanceOf(MoveRegister.class));
    }

    /**
     * Tests the {@code valueOf(List<MoveRecord>, Player)} factory when
     * parameter {@code register} is {@code null}.
     *
     * @see MoveRegister#valueOf(List, Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_nullParameter() {
        MoveRegister.valueOf(NULL_RECORDS, Player.AN_INSTANCE);
    }

    /**
     * Tests the {@code valueOf(List<MoveRecord>, Player)} factory.
     * It tests if the factory returns an instance of {@code MoveRegister}.
     *
     * @see MoveRegister#valueOf(List, Player)
     */
    @Test
    public final void testValueOf() {
        final List<MoveRecord> records = new ArrayList<MoveRecord>();
        records.add(MoveRecordFixtures.AN_INSTANCE);
        assertThat("MoveRegister.valueOf(records)"
                   + " must return an instance of MoveRegister class.",
                   MoveRegister.valueOf(records, Player.AN_INSTANCE),
                   instanceOf(MoveRegister.class));
    }

    /**
     * Test the {@code valueOf(List<MoveRecord>, Player)} factory.
     * <p>
     * The factory receives the register parameter, and any further change to it
     * must not be reflected to the returned move register instance.
     *
     * @see MoveRegister#valueOf(List, Player)
     */
    @Test
    public final void testValueOf_registerMustBeUnchangeable() {

        final List<MoveRecord> changeable = new ArrayList<MoveRecord>();
        changeable.add(MoveRecordFixtures.R00);
        final MoveRegister instance = MoveRegister.valueOf(changeable, Player.AN_INSTANCE);
        changeable.add(MoveRecordFixtures.R01);

        assertThat("The move register instance must be not affected by a"
                   + " change in the records parameter.",
                   instance.last(), is(MoveRecordFixtures.R00));

        assertThat("The move register instance must be not affected by a"
                   + " change in the records parameter.",
                   instance.size(), is(1));
    }

}
