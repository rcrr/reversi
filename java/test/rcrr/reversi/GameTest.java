/*
 *  GameTest.java
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
import static org.junit.Assert.assertTrue;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

import org.joda.time.Duration;
import org.joda.time.Period;

import org.joda.time.DateTime;

/**
 * Test Suite for the {@code Game} class.
 *
 * @see Game
 */
public class GameTest {

    /** Class constructor. */
    public GameTest() { }

    /**
     * To be completed.
     */
    @Test
    public void testAreThereAvailableMoves() {
        assertTrue("To be implemented.", false);
    }

    /**
     * Tests the {@code board()} method.
     *
     * @see Game#board()
     */
    @Test
    public final void testBoard() {
        assertThat("GameFixtureFactoriess.threeSnapshots().board()"
                   + " must be equal to the here built board.",
                   GameFixtureFactories.threeSnapshots().board(),
                   is(new BoardBuilder()
                      .withSquaresLiteral(0, 0, 0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 1, 0, 0, 0, 0,
                                          0, 0, 0, 1, 1, 0, 0, 0,
                                          0, 0, 2, 2, 2, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 0,
                                          0, 0, 0, 0, 0, 0, 0, 0)
                      .build()));
    }

    /**
     * Tests the {@code clock()} method.
     * <p>
     * The clock returned by {@code GameFixtureFactories.threeSnapshots().clock()}
     * must be equal to a clock having the following representation:
     * {@code [BLACK=00:59, WHITE=00:55]}.
     *
     * @see Game#clock()
     * @see GameFixtureFactories#threeSnapshots()
     */
    @Test
    public void testClock() {
        assertThat("GameFixtureFactoriess.threeSnapshots().clock()"
                   + " must be equal to the here built clock.",
                   GameFixtureFactories.threeSnapshots().clock(),
                   is(new ClockBuilder()
                      .withDuration(Player.BLACK, Period.seconds(59).toStandardDuration())
                      .withDuration(Player.WHITE, Period.seconds(55).toStandardDuration())
                      .build()));
    }

    /**
     * To be completed.
     */
    @Test
    public void testCountDiscDifference() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testHasOpponentPassed() {
        assertTrue("To be implemented.", false);
    }

    /**
     * Tests the {@code initialGame(Actor, Actor, Duration, PrintStream)} method when parameter
     * {@code black} is {@code null}.
     *
     * @see Game#initialGame(Actor, Actor, Duration, PrintStream)
     */
    @Test(expected = NullPointerException.class)
    public final void testInitialGame_boundaryConditions_checkNullParameter_black() {
        Game.initialGame(ActorFixtures.NULL,
                         new ActorBuilder().build(),
                         CommonFixtures.A_DURATION,
                         CommonFixtures.NULL_PRINT_STREAM);
    }

    /**
     * Tests the {@code initialGame(Actor, Actor, Duration, PrintStream)} method when parameter
     * {@code white} is {@code null}.
     *
     * @see Game#initialGame(Actor, Actor, Duration, PrintStream)
     */
    @Test(expected = NullPointerException.class)
    public final void testInitialGame_boundaryConditions_checkNullParameter_white() {
        Game.initialGame(new ActorBuilder().build(),
                         ActorFixtures.NULL,
                         CommonFixtures.A_DURATION,
                         CommonFixtures.NULL_PRINT_STREAM);
    }

    /**
     * Tests the {@code initialGame(Actor, Actor, Duration, PrintStream)} method when parameter
     * {@code gameDuration} is {@code null}.
     *
     * @see Game#initialGame(Actor, Actor, Duration, PrintStream)
     */
    @Test(expected = NullPointerException.class)
    public final void testInitialGame_boundaryConditions_checkNullParameter_gameDuration() {
        Game.initialGame(new ActorBuilder().build(),
                         new ActorBuilder().build(),
                         CommonFixtures.NULL_DURATION,
                         CommonFixtures.NULL_PRINT_STREAM);
    }

    /**
     * Tests the {@code initialGame(Actor, Actor, Duration, PrintStream)} method.
     * <p>
     * The game returned by {@code initialGame(Actor, Actor, Duration, PrintStream)}
     * must be an object of class {@code Game}
     *
     * @see Game#initialGame(Actor, Actor, Duration, PrintStream)
     */
    @Test
    public void testInitialGame() {
        Game initialGame = Game.initialGame(new ActorBuilder().build(),
                                            new ActorBuilder().build(),
                                            CommonFixtures.A_DURATION,
                                            CommonFixtures.NULL_PRINT_STREAM);

        assertThat("Game.initialGame(Actor, Actor, Duration, PrintStream)"
                   + " must return an instance of Game class.",
                   initialGame,
                   instanceOf(Game.class));
    }

    /**
     * To be completed.
     */
    @Test
    public void testLastGameSnapshot() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testMove() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testPlay() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testPlayer() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testSequence() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testValidateMove() {

        System.out.println(GameSnapshotFixtures.G00_S00.printGameSnapshot());
        System.out.println(GameSnapshotFixtures.G00_S01.printGameSnapshot());
        System.out.println(GameSnapshotFixtures.G00_S02.printGameSnapshot());


        assertTrue("To be implemented.", false);
    }

    /**
     * Tests the {@code newInstance(ActorsPair, GameSequence, PrintStream)} method when parameter
     * {@code actors} is {@code null}.
     *
     * @see Game#newInstance(ActorsPair, GameSequence, PrintStream)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_checkNullParameter_actors() {
        Game.newInstance(ActorsPairFixtures.NULL,
                         new GameSequenceBuilder().build(),
                         CommonFixtures.NULL_PRINT_STREAM);
    }

    /**
     * Tests the {@code newInstance(ActorsPair, GameSequence, PrintStream)} method when parameter
     * {@code sequence} is {@code null}.
     *
     * @see Game#newInstance(ActorsPair, GameSequence, PrintStream)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_checkNullParameter_sequence() {
        Game.newInstance(new ActorsPairBuilder().build(),
                         GameSequenceFixtures.NULL,
                         CommonFixtures.NULL_PRINT_STREAM);
    }

    /**
     * Tests the {@code newInstance(ActorsPair, GameSequence, PrintStream)} method.
     * <p>
     * The game returned by {@code newInstance(ActorsPair, GameSequence, PrintStream)}
     * must be an object of class {@code Game}
     *
     * @see Game#newInstance(ActorsPair, GameSequence, PrintStream)
     */
    @Test
    public void testValueOf() {
        Game instance = Game.newInstance(new ActorsPairBuilder().build(),
                                         new GameSequenceBuilder().build(),
                                         CommonFixtures.NULL_PRINT_STREAM);

        assertThat("Game.newInstance(ActorsPair, GameSequence, PrintStream)"
                   + " must return an instance of Game class.",
                   instance,
                   instanceOf(Game.class));
    }

}
