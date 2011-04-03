/*
 *  GameSnapshotTest.java
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
import static org.junit.Assert.assertEquals;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

import org.joda.time.Period;

/**
 * Test Suite for the {@code GameSnapshot} class.
 *
 * @see GameSnapshot
 */
public class GameSnapshotTest {

    /** Class constructor. */
    public GameSnapshotTest() { }

    /**
     * Tests that the board used to assemble the GamePosition instance,
     * then used by the GamePosition constructor, is equal to
     * the one returned by the board method.
     */
    @Test
    public final void testBoard() {
        assertThat("The board used to assemble the GamePosition,"
                   + " then used into the GameSnapshot, must be equal"
                   + " to the one returned by the board method.",
                   new GameSnapshotBuilder()
                   .withPosition(new GamePositionBuilder()
                                 .withBoard(BoardFixtures.AN_INSTANCE)
                                 .build())
                   .build()
                   .board(),
                   is(BoardFixtures.AN_INSTANCE));
    }

    /**
     * Tests the {@code clock()} method.
     *
     * @see GameSnapshot#clock()
     */
    @Test
    public final void testClock() {
        assertThat("The clock used by the constructor of the"
                   + " GameSnapshot instance, must be equal"
                   + " to the one returned by the clock method.",
                   new GameSnapshotBuilder()
                   .withClock(ClockFixtures.AN_INSTANCE)
                   .build()
                   .clock(),
                   is(ClockFixtures.AN_INSTANCE));
    }

    /**
     * Tests the {@code countDiscDifference()} method.
     * <p>
     * The method run on {@code GameSnapshotFixtures.BLACK_HAS_TO_PASS}
     * must return a count of {@code -2}.
     *
     * @see GameSnapshotFixtures#BLACK_HAS_TO_PASS
     */
    @Test
    public final void testCountDiscDifference() {
        assertThat("The disc count returned by CountDiscDifference on"
                   + " GameSnapshotFixtures.BLACK_HAS_TO_PASS must be -2.",
                   GameSnapshotFixtures.BLACK_HAS_TO_PASS.countDiscDifference(),
                   is(-2));
    }

    /**
     * Tests the {@code hasAnyLegalMove()} method.
     * <p>
     * The method run on {@code GameSnapshotFixtures.BLACK_HAS_TO_PASS}
     * must return {@code false}.
     *
     * @see GameSnapshotFixtures#BLACK_HAS_TO_PASS
     */
    @Test
    public final void testHasAnyLegalMove_on_BLACK_HAS_TO_PASS() {
        assertThat("GameSnapshotFixtures.BLACK_HAS_TO_PASS has no legal moves.",
                   GameSnapshotFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(),
                   is(false));
    }

    /**
     * Tests the {@code hasAnyLegalMove()} method.
     * <p>
     * The method run on {@code GameSnapshotFixtures.EARLY_GAME_B_9_MOVES}
     * must return {@code true}.
     *
     * @see GameSnapshotFixtures#EARLY_GAME_B_9_MOVES
     */
    @Test
    public final void testHasAnyLegalMove_on_EARLY_GAME_B_9_MOVES() {
        assertThat("GameSnapshotFixtures.EARLY_GAME_B_9_MOVES has legal moves.",
                   GameSnapshotFixtures.EARLY_GAME_B_9_MOVES.hasAnyLegalMove(),
                   is(true));
    }

    /**
     * Tests the {@code hasAnyPlayerAnyLegalMove()} method.
     * <p>
     * The method run on {@code GameSnapshotFixtures.BLACK_HAS_TO_PASS}
     * must return {@code true}.
     *
     * @see GameSnapshotFixtures#BLACK_HAS_TO_PASS
     */
    @Test
    public final void testHasAnyPlayerAnyLegalMove_on_BLACK_HAS_TO_PASS() {
        assertThat("GameSnapshotFixtures.BLACK_HAS_TO_PASS has no legal moves,"
                   + " but the other player, the white, has ones.",
                   GameSnapshotFixtures.BLACK_HAS_TO_PASS.hasAnyPlayerAnyLegalMove(),
                   is(true));
    }

    /**
     * Tests the {@code hasAnyPlayerAnyLegalMove()} method.
     * <p>
     * The method run on {@code GameSnapshotFixtures.EARLY_GAME_B_9_MOVES}
     * must return {@code true}.
     *
     * @see GameSnapshotFixtures#EARLY_GAME_B_9_MOVES
     */
    @Test
    public final void testHasAnyPlayerAnyLegalMove_on_EARLY_GAME_B_9_MOVES() {
        assertThat("GameSnapshotFixtures.EARLY_GAME_B_9_MOVES has legal moves.",
                   GameSnapshotFixtures.EARLY_GAME_B_9_MOVES.hasAnyPlayerAnyLegalMove(),
                   is(true));
    }

    /**
     * Tests the {@code hasAnyPlayerAnyLegalMove()} method.
     * <p>
     * The method run on a complete board and must return {@code false}.
     * The board is tested associated with the black, the white and the null player.
     *
     * @see GamePositionFixtures#FINAL_B37_W27_N
     * @see GamePositionFixtures#FINAL_B37_W27_B
     * @see GamePositionFixtures#FINAL_B37_W27_W
     */
    @Test
    public final void testHasAnyPlayerAnyLegalMove_on_FINAL_B37_W27() {
        assertThat("GameSnapshotFixtures.FINAL_B37_W27_N has no legal moves.",
                   new GameSnapshotBuilder()
                   .withPosition(GamePositionFixtures.FINAL_B37_W27_N)
                   .build()
                   .hasAnyPlayerAnyLegalMove(),
                   is(false));
        assertThat("GameSnapshotFixtures.FINAL_B37_W27_B has no legal moves.",
                   new GameSnapshotBuilder()
                   .withPosition(GamePositionFixtures.FINAL_B37_W27_B)
                   .build()
                   .hasAnyPlayerAnyLegalMove(),
                   is(false));
        assertThat("GameSnapshotFixtures.FINAL_B37_W27_W has no legal moves.",
                   new GameSnapshotBuilder()
                   .withPosition(GamePositionFixtures.FINAL_B37_W27_W)
                   .build()
                   .hasAnyPlayerAnyLegalMove(),
                   is(false));
    }

    /**
     * Tests the {@code initialGameSnapshot(Duration)} method when parameter
     * {@code gameDuration} is {@code null}.
     *
     * @see GameSnapshot#initialGameSnapshot(Duration)
     */
    @Test(expected = NullPointerException.class)
    public final void testInitialGameSnapshot_boundaryConditions_checkNullParameter() {
        GameSnapshot.initialGameSnapshot(CommonFixtures.NULL_DURATION);
    }

    /**
     * Tests that the {@code initialGameSnapshot(Duration)} factory returns
     * a {@code GameSnapshot} object.
     *
     * @see GameSnapshot#initialGameSnapshot(Duration)
     */
    @Test
    public final void testInitialGameSnapshot_returnsAnInstanceOfGameSnapshot() {
        GameSnapshot initialGameSnapshot = GameSnapshot.initialGameSnapshot(CommonFixtures.A_DURATION);

        assertThat("GameSnapshot.initialGameSnapshot(Duration)"
                   + " must return an instance of GameSnapshot class.",
                   initialGameSnapshot,
                   instanceOf(GameSnapshot.class));
    }

    /**
     * Tests that the {@code initialGameSnapshot(Duration)} factory returns
     * an instance having as player the black one.
     *
     * @see GameSnapshot#initialGameSnapshot(Duration)
     */
    @Test
    public final void testInitialGameSnapshot_returnsAnObjectHavingTheBlackPlayer() {
        GameSnapshot initialGameSnapshot = GameSnapshot.initialGameSnapshot(CommonFixtures.A_DURATION);

        assertThat("GameSnapshot.initialGameSnapshot(Duration)"
                   + " must have the black player.",
                   initialGameSnapshot.player(),
                   is(Player.BLACK));
    }

    /**
     * Tests that the {@code initialGameSnapshot(Duration)} factory returns
     * an instance having a board equal to the initial board.
     *
     * @see GameSnapshot#initialGameSnapshot(Duration)
     */
    @Test
    public final void testInitialGameSnapshot_returnsAnObjectHavingTheInitialBoard() {
        GameSnapshot initialGameSnapshot = GameSnapshot.initialGameSnapshot(CommonFixtures.A_DURATION);

        assertThat("GameSnapshot.initialGameSnapshot(Duration)"
                   + " must have an INITIAL board.",
                   initialGameSnapshot.board(),
                   is(BoardFixtures.INITIAL));
    }

    /**
     * Tests that the {@code initialGameSnapshot(Duration)} factory returns
     * an instance having a clock defined by the {@code gameDuration} parameter.
     *
     * @see GameSnapshot#initialGameSnapshot(Duration)
     */
    @Test
    public final void testInitialGameSnapshot_returnsAnInstanceHavingAppropriateClock() {
        GameSnapshot initialGameSnapshot = GameSnapshot.initialGameSnapshot(CommonFixtures.ONE_MINUTE_DURATION);

        assertThat("GameSnapshot.initialGameSnapshot(CommonFixtures.ONE_MINUTE_DURATION)"
                   + " must have a clock having one minute for each player.",
                   initialGameSnapshot.clock(),
                   is(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS));
    }

    /**
     * Tests that the {@code initialGameSnapshot(Duration)} factory returns
     * an instance having an empty move register.
     *
     * @see GameSnapshot#initialGameSnapshot(Duration)
     */
    @Test
    public final void testInitialGameSnapshot_returnsAnInstanceHavingAnEmptyMoveRegister() {
        GameSnapshot initialGameSnapshot = GameSnapshot.initialGameSnapshot(CommonFixtures.A_DURATION);

        assertThat("GameSnapshot.initialGameSnapshot(Duration)"
                   + " must have an empty move register",
                   initialGameSnapshot.register()
                   .isEmpty(),
                   is(true));
    }

    /**
     * Tests the {@code player()} method.
     */
    @Test
    public final void testPlayer() {
        assertThat("The player returned by the player method must be equal"
                   + " to the one used to build the referred game position.",
                   new GameSnapshotBuilder()
                   .withPosition(new GamePositionBuilder()
                                 .withPlayer(Player.WHITE)
                                 .build())
                   .build()
                   .player(),
                   is(Player.WHITE));
    }

    /**
     * Tests the {@code position()} method.
     */
    @Test
    public final void testPosition() {
        assertThat("The game position returned by the position method"
                   + " must be equal to the one passed to the constructor.",
                   new GameSnapshotBuilder()
                   .withPosition(GamePositionFixtures.AN_INSTANCE)
                   .build()
                   .position(),
                   is(GamePositionFixtures.AN_INSTANCE));
    }

    /**
     * Tests the {@code valueOf(GamePosition, Clock, MoveRegister)} factory when
     * parameter {@code position} is {@code null}.
     *
     * @see GameSnapshot#valueOf(GamePosition, Clock, MoveRegister)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_c1() {
        GameSnapshot.valueOf(GamePositionFixtures.NULL,
                             ClockFixtures.AN_INSTANCE,
                             MoveRegisterFixtures.AN_INSTANCE);
    }

    /**
     * Tests the {@code valueOf(GamePosition, Clock, MoveRegister)} factory when
     * parameter {@code clock} is {@code null}.
     *
     * @see GameSnapshot#valueOf(GamePosition, Clock, MoveRegister)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_c2() {
        GameSnapshot.valueOf(GamePositionFixtures.AN_INSTANCE,
                             ClockFixtures.NULL,
                             MoveRegisterFixtures.AN_INSTANCE);
    }

    /**
     * Tests the {@code valueOf(GamePosition, Clock, MoveRegister)} factory when
     * parameter {@code register} is {@code null}.
     *
     * @see GameSnapshot#valueOf(GamePosition, Clock, MoveRegister)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_c3() {
        GameSnapshot.valueOf(GamePositionFixtures.AN_INSTANCE,
                             ClockFixtures.AN_INSTANCE,
                             MoveRegisterFixtures.NULL);
    }

    /**
     * Tests the {@code valueOf(GamePosition, Clock, MoveRegister)} factory.
     * It tests if the factory returns an instance of {@code GameSnapshot}.
     *
     * @see GameSnapshot#valueOf(GamePosition, Clock, MoveRegister)
     */
    @Test
    public final void testValueOf() {
        assertThat("GameSnapshot.valueOf(GamePositionFixtures.AN_INSTANCE,"
                   + " ClockFixtures.AN_INSTANCE, MoveRegisterFixtures.AN_INSTANCE)"
                   + " must return an instance of GameSnapshot class.",
                   GameSnapshot.valueOf(GamePositionFixtures.AN_INSTANCE,
                                        ClockFixtures.AN_INSTANCE,
                                        MoveRegisterFixtures.AN_INSTANCE),
                   instanceOf(GameSnapshot.class));
    }

    /**
     * Tests the {@code printGameSnapshot()} method.
     *
     * @see GameSnapshot#printGameSnapshot()
     */
    @Test
    public final void testPrintGameSnapshot() {
        GamePosition gp = GamePosition.initialGamePosition();
        Clock c = Clock.initialClock(Period.minutes(30).toStandardDuration());
        MoveRegister reg = MoveRegister.empty();
        GameSnapshot gs = GameSnapshot.valueOf(gp, c, reg);
        StringBuilder initialGameSnapshot = new StringBuilder();
        initialGameSnapshot.append("[EMPTY MoveRegister]\n");
        initialGameSnapshot.append("    a b c d e f g h [@=2 0=2 (0)]\n");
        initialGameSnapshot.append(" 1  . . . . . . . . \n");
        initialGameSnapshot.append(" 2  . . . . . . . . \n");
        initialGameSnapshot.append(" 3  . . . . . . . . \n");
        initialGameSnapshot.append(" 4  . . . O @ . . . \n");
        initialGameSnapshot.append(" 5  . . . @ O . . . \n");
        initialGameSnapshot.append(" 6  . . . . . . . . \n");
        initialGameSnapshot.append(" 7  . . . . . . . . \n");
        initialGameSnapshot.append(" 8  . . . . . . . . [@=30:00, O=30:00]\n");
        initialGameSnapshot.append(" Next to play: BLACK, legal moves: [d3, c4, f5, e6]\n");
        assertEquals(initialGameSnapshot.toString(), gs.printGameSnapshot());
    }

}
