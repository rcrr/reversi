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

import org.junit.*;
import static org.junit.Assert.*;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.matchers.JUnitMatchers.*;

/**
 * Test Suite for the {@code MoveRegister} class.
 *
 * @see MoveRegister
 */
public class MoveRegisterTest {

    /** Class constructor. */
    public MoveRegisterTest() { }

    /**
     * Tests the last method.
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
     * Tests the push method when parameter {@code record} is null.
     *
     * @see MoveRegister#push(MoveRecord)
     */
    @Test(expected = NullPointerException.class)
    public final void testPush_boundaryConditions_c1() {
        MoveRegisterFixtures.EMPTY.push(MoveRecordFixtures.NULL);
    }

    /**
     * Tests that the push method returns a {@code MoveRegister} instance.
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
     * Tests that the push method returns a {@code MoveRegister} instance having
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
     * Tests that the push method returns a {@code MoveRegister} instance having
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
     * Tests the size method when the move register is empty.
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
     * Tests the size method when the move register is not empty.
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
     * Tests the toString method when the move register is not empty.
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
     * Tests the toString method when the move register is empty.
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
     * Tests the isEmpty method when the move record is empty.
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
     * Tests the isEmpty method when the move record is not empty.
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
     * Tests the empty creation method.
     *
     * @see MoveRegister#empty()
     */
    @Test
    public final void testEmpty() {
        assertThat("MoveRegister.empty() must return an instance of MoveRegister class.",
                   MoveRegister.empty(), instanceOf(MoveRegister.class));
    }

}
