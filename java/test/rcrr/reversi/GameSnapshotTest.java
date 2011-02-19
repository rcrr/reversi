/*
 *  GameSnapshotTest.java
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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

import org.joda.time.Duration;
import org.joda.time.Period;

public class GameSnapshotTest {

    /**
     * Tests the class getter methods. Must be removed.
     */
    @Test
    public void testGetters() {
	GamePosition gp = GamePosition.initialGamePosition();
	Clock c = Clock.initialClock(Period.minutes(30).toStandardDuration());
	MoveRegister reg = MoveRegister.empty();
	GameSnapshot gs = GameSnapshot.valueOf(gp, c, reg);
	assertEquals(gp, gs.position());
	assertEquals(c, gs.clock());
    }

    /**
     * Has to be written.
     */
    @Test
    public void testBoard() {
	assertTrue("Test has to be written.", false);
    }

    /**
     * Has to be written.
     */
    @Test
    public void testClock() {
	assertTrue("Test has to be written.", false);
    }

    /**
     * Has to be written.
     */
    @Test
    public void testCountDiscDifference() {
	assertTrue("Test has to be written.", false);
    }

    /**
     * Has to be written.
     */
    @Test
    public void testHasAnyLegalMove() {
	assertTrue("Test has to be written.", false);
    }

    /**
     * Has to be written.
     */
    @Test
    public void testHasAnyPlayerAnyLegalMove() {
	assertTrue("Test has to be written.", false);
    }

    /**
     * Tests the initialGameSnapshot method when parameter {@code gameDuration} is null.
     *
     * @see GameSnapshot#initialGameSnapshot(Duration)
     */
    @Test(expected = NullPointerException.class)
    public final void testInitialGameSnapshot_boundaryConditions_checkNullParameter() {
        GameSnapshot.initialGameSnapshot(CommonFixtures.NULL_DURATION);
    }

    /**
     * Tests the initialGameSnapshot factory.
     *
     * @see GameSnapshot#initialGameSnapshot(Duration)
     */
    @Test
    public void testInitialGameSnapshot() {
        GameSnapshot initialGameSnapshot = GameSnapshot.initialGameSnapshot(CommonFixtures.ONE_MINUTE_DURATION);

        assertThat("GameSnapshot.initialGameSnapshot()"
                   + " must return an instance of GameSnapshot class.",
                   initialGameSnapshot,
                   instanceOf(GameSnapshot.class));

        assertThat("GameSnapshot.initialGameSnapshot()"
                   + " must have an INITIAL board.",
                   initialGameSnapshot.position().board(),
                   is(BoardFixtures.INITIAL));

        assertThat("GameSnapshot.initialGameSnapshot()"
                   + " must have a black player.",
                   initialGameSnapshot.position().player(),
                   is(Player.BLACK));

        assertThat("GameSnapshot.initialGameSnapshot()"
                   + " must have a clock having one minute for each player.",
                   initialGameSnapshot.clock(),
                   is(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS));
    }

    /**
     * Has to be written.
     */
    @Test
    public void testPlayer() {
	assertTrue("Test has to be written.", false);
    }

    /**
     * Has to be written.
     */
    @Test
    public void testPosition() {
	assertTrue("Test has to be written.", false);
    }

    @Test
    public void testValueOf() {

	/**
	 * Tests if the valueOf method throws a NullPointerException when
	 * the passed position is null.
	 */
	try {
	    GameSnapshot.valueOf(null, Clock.initialClock(Period.minutes(1).toStandardDuration()), MoveRegister.empty());
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	/**
	 * Tests if the valueOf method throws a NullPointerException when
	 * the passed clock is null.
	 */
	try {
	    GameSnapshot.valueOf(GamePosition.initialGamePosition(), null, MoveRegister.empty());
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	/**
	 * Tests if the valueOf method throws a NullPointerException when
	 * the passed move register is null.
	 */
	try {
	    GameSnapshot.valueOf(GamePosition.initialGamePosition(), Clock.initialClock(Period.minutes(1).toStandardDuration()), null);
	    fail("An exception must be risen.");
	} catch (NullPointerException npe) {
	    assertTrue(true);
	}

	GamePosition gp = GamePosition.initialGamePosition();
	Clock c = Clock.initialClock(Period.minutes(30).toStandardDuration());
	MoveRegister reg = MoveRegister.empty();
	GameSnapshot gs = GameSnapshot.valueOf(gp, c, reg);
	assertEquals(gp, gs.position());
	assertEquals(c, gs.clock());
    }

    @Test
    public void testPrintGameSnapshot() {
	GamePosition gp = GamePosition.initialGamePosition();
	Clock c = Clock.initialClock(Period.minutes(30).toStandardDuration());
	MoveRegister reg = MoveRegister.empty();
	GameSnapshot gs = GameSnapshot.valueOf(gp, c, reg);
	StringBuilder initialGameSnapshot = new StringBuilder();
	initialGameSnapshot.append("    a b c d e f g h [@=2 0=2 (0)]\n");
	initialGameSnapshot.append(" 1  . . . . . . . . \n");
	initialGameSnapshot.append(" 2  . . . . . . . . \n");
	initialGameSnapshot.append(" 3  . . . . . . . . \n");
	initialGameSnapshot.append(" 4  . . . O @ . . . \n");
	initialGameSnapshot.append(" 5  . . . @ O . . . \n");
	initialGameSnapshot.append(" 6  . . . . . . . . \n");
	initialGameSnapshot.append(" 7  . . . . . . . . \n");
	initialGameSnapshot.append(" 8  . . . . . . . . [@=30:00, O=30:00]\n");
	initialGameSnapshot.append(" Next to play: BLACK, legal moves: [D3, C4, F5, E6]\n");
	assertEquals(initialGameSnapshot.toString(), gs.printGameSnapshot());
    }

}