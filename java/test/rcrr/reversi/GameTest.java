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
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

import org.joda.time.Period;

/**
 * Test Suite for the {@code Game} class.
 *
 * @see Game
 */
public class GameTest {

    /** Class constructor. */
    public GameTest() { }

    /**
     * Tests the {@code areThereAvailableMoves()} method.
     * <p>
     * The value returned by sending the {@code areThereAvailableMoves()} message
     * to a game object having,
     * a sequence field having as last element,
     * a game snapshot defined by {@code GameSnapshotFixtures.BLACK_HAS_TO_PASS}
     * must be equal to true.
     *
     * @see Game#countDiscDifference()
     * @see GameSnapshotFixtures#BLACK_HAS_TO_PASS
     */
    @Test
    public final void testAreThereAvailableMoves() {
        assertThat("",
                   new GameBuilder()
                   .withSequence(new GameSequenceBuilder()
                                 .withSnapshots(GameSnapshotFixtures.BLACK_HAS_TO_PASS)
                                 .build())
                   .build()
                   .areThereAvailableMoves(),
                   is(true));
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
    public final void testClock() {
        assertThat("GameFixtureFactoriess.threeSnapshots().clock()"
                   + " must be equal to the here built clock.",
                   GameFixtureFactories.threeSnapshots().clock(),
                   is(new ClockBuilder()
                      .withDuration(Player.BLACK, Period.seconds(59).toStandardDuration())
                      .withDuration(Player.WHITE, Period.seconds(55).toStandardDuration())
                      .build()));
    }

    /**
     * Tests the {@code countDiscDifference()} method.
     * <p>
     * The value returned by sending the {@code countDiscDifference()} message
     * to a game object having,
     * a sequence field having as last element,
     * a game snapshot having as position field,
     * a game position having as board field,
     * a board defined by {@code BoardFixtures.FINAL_B37_W27}
     * must be equal to +10.
     *
     * @see Game#countDiscDifference()
     * @see BoardFixtures#FINAL_B37_W27
     */
    @Test
    public final void testCountDiscDifference() {
        assertThat("",
                   new GameBuilder()
                   .withSequence(new GameSequenceBuilder()
                                 .withSnapshots(new GameSnapshotBuilder()
                                                .withPosition(new GamePositionBuilder()
                                                              .withBoard(BoardFixtures.FINAL_B37_W27)
                                                              .build())
                                                .build())
                                 .build())
                   .build()
                   .countDiscDifference(),
                   is(+10));
    }

    /**
     * The test return always a fail. The PASS use case must be handled differently.
     */
    @Test
    public final void testHasOpponentPassed() {
        fail("The method hasOpponentPassed must relate on the MoveRegister last MoveRecord."
             + " It instead releates to the comparison with the previous player."
             + " It happens because passing moves are not part of the game sequence."
             + " This happens because the PAIP design was so conceived. It must be fixed.");
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
    public final void testInitialGame() {
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
     * Tests the {@code lastGameSnapshot()} method.
     *
     * @see Game#lastGameSnapshot()
     */
    @Test
    public final void testLastGameSnapshot() {
        assertThat("The game snapshot last added to game sequence set into"
                   + " the sequence field in the Game instance, must be equal"
                   + " to the one returned by the lastGameSnapshot method.",
                   new GameBuilder()
                   .withSequence(new GameSequenceBuilder()
                                 .withSnapshots(new GameSnapshotBuilder().build(),
                                                new GameSnapshotBuilder().build(),
                                                GameSnapshotFixtures.AN_INSTANCE)
                                 .build())
                   .build()
                   .lastGameSnapshot(),
                   is(GameSnapshotFixtures.AN_INSTANCE));
    }

    /**
     * Tests the {@code move()} method, verifying that the program handle
     * the PASS move.
     *
     * @see Game#move()
     */
    @Test
    public final void testMove_checkPassActionWhenNotLegal() {

        Strategy strategy = new Strategy() {
                private int index = -1;
                private final Move[] moves
                    = {Move.valueOf(Move.Action.PASS),
                       Move.valueOf(Square.E6)};
                public Move move(final GameSnapshot snapshot) {
                    index++;
                    return moves[index];
                }
            };

        Game game = new GameBuilder()
            .withSequence(GameSequenceFixtures.THREE_SNAPSHOTS)
            .withActors(new ActorsPairBuilder()
                        .withActor(Player.BLACK,
                                   new ActorBuilder()
                                   .withStrategy(strategy)
                                   .build())
                        .build())
            .build();

        game.move();

        //System.out.println(game.print());

        assertThat("The game ..... must have the WHITE player.",
                   game.player(),
                   is(Player.WHITE));

        assertThat("game.lastGameSnapshot().register().get(0).move()"
                   + " must be Move.valueOf(Move.Action.PASS)).",
                   game.lastGameSnapshot().register().get(0).move(),
                   is(Move.valueOf(Move.Action.PASS)));
    }

    /**
     * Tests the {@code move()} method, verifying that the program handle
     * the PASS move.
     *
     * @see Game#move()
     */
    @Test
    public final void testMove_checkPassActionWhenLegal() {

        Strategy strategy = new Strategy() {
                private int index = -1;
                private final Move[] moves
                    = {Move.valueOf(Square.A1),
                       Move.valueOf(Move.Action.PASS)};
                public Move move(final GameSnapshot snapshot) {
                    index++;
                    return moves[index];
                }
            };

        Game game = new GameBuilder()
            .withSequence(new GameSequenceBuilder()
                          .withSnapshot(GameSnapshotFixtures.BLACK_HAS_TO_PASS)
                          .build())
            .withActors(new ActorsPairBuilder()
                        .withActor(Player.BLACK,
                                   new ActorBuilder()
                                   .withStrategy(strategy)
                                   .build())
                        .build())
            .build();

        game.move();

        assertThat("The game ..... must have the WHITE player.",
                   game.player(),
                   is(Player.WHITE));

        assertThat("game.lastGameSnapshot().register().last().move()"
                   + " must be Move.valueOf(Move.Action.PASS)).",
                   game.lastGameSnapshot().register().last().move(),
                   is(Move.valueOf(Move.Action.PASS)));
    }

    /**
     * Tests the {@code move()} method, verifying that the program does not go
     * out of memory when the provided move is ever illegal.
     *
     * @see Game#move()
     */
    @Test
    public final void testMove_preventStackOverflow() {

        Strategy strategy = new Strategy() {
                public Move move(final GameSnapshot snapshot) {
                    return Move.valueOf(Square.A1);
                }
            };

        Game game = new GameBuilder()
            .withSequence(GameSequenceFixtures.THREE_SNAPSHOTS)
            .withActors(new ActorsPairBuilder()
                        .withActor(Player.BLACK,
                                   new ActorBuilder()
                                   .withStrategy(strategy)
                                   .build())
                        .build())
            .build();

        game.move();

        assertThat("The game ..... must have the NULL player.",
                   game.player(),
                   is(Player.NULL));

        assertThat("The game.countDiscDifference() must be -64.",
                   game.countDiscDifference(),
                   is(-64));
    }

    /**
     * Tests the {@code move()} method.
     * <p>
     * The strategy and the game must be put into dedicate auxiliary methods.
     * Several condition must be tested.
     * The strategy must return moves by mean of an hardcoded list.
     * When assigned an illegal move, result is a stack overflow.
     *
     * @see Game#move()
     */
    @Test
    public final void testMove() {

        Strategy strategy = new Strategy() {
                public Move move(final GameSnapshot snapshot) {
                    return Move.valueOf(Square.E6);
                }
            };

        Game game = new GameBuilder()
            .withSequence(GameSequenceFixtures.THREE_SNAPSHOTS)
            .withActors(new ActorsPairBuilder()
                        .withActor(Player.BLACK,
                                   new ActorBuilder()
                                   .withStrategy(strategy)
                                   .build())
                        .build())
            .build();

        game.move();

        assertThat("The game ..... must have the WHITE player.",
                   game.player(),
                   is(Player.WHITE));
    }

    /**
     * Tests the {@code move()} method, verifying that the game clock is
     * updated correctly also when a player send illegal moves.
     *
     * @see Game#move()
     */
    @Test
    public final void testMove_checksThatTheClockIsUpdatedWhenAnIllegalMoveIsGiven() {

        final long moveTime = 100;

        Strategy strategy = new Strategy() {
                private int index = -1;
                private final Square[] moves = {Square.A1, Square.E6};
                public Move move(final GameSnapshot snapshot) {
                    try {
                        Thread.sleep(moveTime);
                    } catch (java.lang.InterruptedException ie) {
                        throw new RuntimeException(ie);
                    }
                    index++;
                    return Move.valueOf(moves[index]);
                }
            };

        Game game = new GameBuilder()
            .withSequence(GameSequenceFixtures.THREE_SNAPSHOTS)
            .withActors(new ActorsPairBuilder()
                        .withActor(Player.BLACK,
                                   new ActorBuilder()
                                   .withStrategy(strategy)
                                   .build())
                        .build())
            .build();

        game.move();

        MoveRegister register = game.lastGameSnapshot().register();
        long blackClockResidualDurationBeforeMoving
            = GameSequenceFixtures.THREE_SNAPSHOTS.last().clock().get(Player.BLACK).getMillis();
        long blackClockResidualDurationHeldByFirstMoveRecord
            = game.lastGameSnapshot().register().get(0).clock().get(Player.BLACK).getMillis();
        long blackClockResidualDurationHeldBySecondMoveRecord
            = game.lastGameSnapshot().register().get(1).clock().get(Player.BLACK).getMillis();
        long blackClockResidualDurationOnTheNextGameSnapshot
            = game.clock().get(Player.BLACK).getMillis();

        assertThat("The move register size must be 2.",
                   register.size(),
                   is(2));

        assertTrue("The blackClockResidualDurationOnTheNextGameSnapshot must be equal to"
                   + " blackClockResidualDurationHeldBySecondMoveRecord.",
                   blackClockResidualDurationOnTheNextGameSnapshot
                   == blackClockResidualDurationHeldBySecondMoveRecord);

        assertTrue("blackClockResidualDurationHeldBySecondMoveRecord + moveTime"
                   + " must be less or equal to blackClockResidualDurationHeldByFirstMoveRecord.",
                   blackClockResidualDurationHeldBySecondMoveRecord + moveTime
                   <= blackClockResidualDurationHeldByFirstMoveRecord);

        assertTrue("blackClockResidualDurationHeldByFirstMoveRecord + moveTime"
                   + " must be less or equal to blackClockResidualDurationBeforeMoving.",
                   blackClockResidualDurationHeldByFirstMoveRecord + moveTime
                   <= blackClockResidualDurationBeforeMoving);

    }

    /**
     * The test has to be written. It must be split into several cases.
     * Before moving forward two changes should be prepared:
     *  (1) the proper game clock management
     *  (2) the PASS and FORFAIT use cases.
     */
    @Test
    public final void testPlay() {
        fail("The method play must take into consideration the proper management"
             + " of the game clock and the PASS use case.");
    }

    /**
     * Tests the {@code player()} method.
     *
     * @see Game#player()
     */
    @Test
    public final void testPlayer() {
        assertThat("The game snapshot last added to game sequence set into"
                   + " the sequence field in the Game instance, must be equal"
                   + " to the one returned by the lastGameSnapshot method.",
                   new GameBuilder()
                   .withSequence(new GameSequenceBuilder()
                                 .withSnapshots(new GameSnapshotBuilder()
                                                .withPosition(new GamePositionBuilder()
                                                              .withPlayer(Player.WHITE)
                                                              .build())
                                                .build())
                                 .build())
                   .build()
                   .player(),
                   is(Player.WHITE));
    }

    /**
     * Tests the {@code sequence()} method.
     *
     * @see Game#sequence()
     */
    @Test
    public final void testSequence() {
        assertThat("The sequence used by the constructor of the"
                   + " Game instance, must be equal"
                   + " to the one returned by the sequence method.",
                   new GameBuilder()
                   .withSequence(GameSequenceFixtures.AN_INSTANCE)
                   .build()
                   .sequence(),
                   is(GameSequenceFixtures.AN_INSTANCE));
    }

    /**
     * Tests the {@code validateMove(Move)} method when parameter
     * {@code square} is {@code null}.
     *
     * @see Game#validateMove(Move)
     */
    @Test(expected = NullPointerException.class)
    public final void testValidateMove_boundaryConditions_checkNullParameter_move() {
        new GameBuilder().build().validateMove(Move.NULL);
    }

    /**
     * Tests the {@code validateMove(Move)} method.
     *
     * @see Game#validateMove(Move)
     */
    @Test
    public final void testValidateMove_threeSnapshots_E6() {
        assertThat("The PUT_DISC move to square E6 is allowed."
                   + " GameFixtureFactories.threeSnapshots().validateMove(Square.E6)"
                   + " must return true.",
                   GameFixtureFactories.threeSnapshots().validateMove(Move.valueOf(Square.E6)),
                   is(true));
    }

    /**
     * Tests the {@code validateMove(Move)} method.
     *
     * @see Game#validateMove(Move)
     */
    @Test
    public final void testValidateMove_threeSnapshots_A1() {
        assertThat("The PUT_DISC move to square A1 is not allowed."
                   + " GameFixtureFactories.threeSnapshots().validateMove(Square.A1)"
                   + " must return false.",
                   GameFixtureFactories.threeSnapshots().validateMove(Move.valueOf(Square.A1)),
                   is(false));
    }

    /**
     * Tests the {@code newInstance(ActorsPair, GameSequence, PrintStream)} method when parameter
     * {@code actors} is {@code null}.
     *
     * @see Game#newInstance(ActorsPair, GameSequence, PrintStream)
     */
    @Test(expected = NullPointerException.class)
    public final void testNewInstance_boundaryConditions_checkNullParameter_actors() {
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
    public final void testNewInstance_boundaryConditions_checkNullParameter_sequence() {
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
    public final void testNewInstance() {
        Game instance = Game.newInstance(new ActorsPairBuilder().build(),
                                         new GameSequenceBuilder().build(),
                                         CommonFixtures.NULL_PRINT_STREAM);

        assertThat("Game.newInstance(ActorsPair, GameSequence, PrintStream)"
                   + " must return an instance of Game class.",
                   instance,
                   instanceOf(Game.class));
    }

}
