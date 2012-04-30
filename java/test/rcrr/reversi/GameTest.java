/*
 *  GameTest.java
 *
 *  Copyright (c) 2010, 2011, 2012 Roberto Corradini. All rights reserved.
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

import java.util.Set;
import java.util.HashSet;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

import org.joda.time.Period;
import org.joda.time.Duration;

/**
 * Test Suite for the {@code Game} class.
 *
 * @see Game
 */
public class GameTest {

    /**
     * Returns a new strategy constructed using he move array.
     *
     * @param moves the move array
     * @return      a new strategy
     */
    private static Strategy newStrategyValueOfMoveArray(final Move... moves) {
        return new Strategy() {
            private int index = -1;
            public Move move(final GameSnapshot snapshot) {
                index++;
                return moves[index];
            }
        };
    }

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
                   new Game.Builder()
                   .withSequence(new GameSequence.Builder()
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
                   is(new Clock.Builder()
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
                   new Game.Builder()
                   .withSequence(new GameSequence.Builder()
                                 .withSnapshots(new GameSnapshot.Builder()
                                                .withPosition(new GamePosition.Builder()
                                                              .withBoard(BoardFixtures.FINAL_B37_W27)
                                                              .build())
                                                .build())
                                 .build())
                   .build()
                   .countDiscDifference(),
                   is(+10));
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
                         new Actor.Builder().build(),
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
        Game.initialGame(new Actor.Builder().build(),
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
        Game.initialGame(new Actor.Builder().build(),
                         new Actor.Builder().build(),
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
        Game initialGame = Game.initialGame(new Actor.Builder().build(),
                                            new Actor.Builder().build(),
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
                   new Game.Builder()
                   .withSequence(new GameSequence.Builder()
                                 .withSnapshots(new GameSnapshot.Builder().build(),
                                                new GameSnapshot.Builder().build(),
                                                GameSnapshotFixtures.AN_INSTANCE)
                                 .build())
                   .build()
                   .lastGameSnapshot(),
                   is(GameSnapshotFixtures.AN_INSTANCE));
    }


    /**
     * Tests that the {@code move()} method behave correctly when the game reaches a
     * position where one of the player has to PASS.
     * <p>
     * The game position used is as described by <i>PAIP 18.6 (pg 621)</i> when the
     * white plays C1 the black has to PASS.
     * <pre>
     * {@code
     * BLACK moves to b1
     *     a b c d e f g h [@=20 0=1 (19)]
     *  1  O @ . . . . . .
     *  2  . @ . . . @ @ .
     *  3  @ @ @ @ @ @ . .
     *  4  . @ . @ @ . . .
     *  5  @ @ @ @ @ @ . .
     *  6  . @ . . . . . .
     *  7  . . . . . . . .
     *  8  . . . . . . . . [@=29:59, O=29:59]
     *  Next to play: WHITE, legal moves: [c1, f6]
     * }
     * </pre>
     *
     * @see Game#move
     */
    @Test
    public final void testMove_passUseCase() {

        final GameSnapshot paip1862 = new GameSnapshot.Builder()
            .withPosition(new GamePosition.Builder()
                          .withBoard(new BoardBuilder()
                                     .withSquaresLiteral(2, 1, 0, 0, 0, 0, 0, 0,
                                                         0, 1, 0, 0, 0, 1, 1, 0,
                                                         1, 1, 1, 1, 1, 1, 0, 0,
                                                         0, 1, 0, 1, 1, 0, 0, 0,
                                                         1, 1, 1, 1, 1, 1, 0, 0,
                                                         0, 1, 0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0, 0, 0)
                                     .build())
                          .withPlayer(Player.WHITE)
                          .build())
            .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
            .withRegister(MoveRegisterFixtures.EMPTY)
            .build();

        final Strategy whStrategy = new Strategy() {
                private int index = -1;
                private final Move[] moves
                    = {Move.valueOf(Square.C1),
                       Move.valueOf(Square.B7)};
                public Move move(final GameSnapshot snapshot) {
                    index++;
                    return moves[index];
                }
            };

        final Strategy blStrategy = new Strategy() {
                private int index = -1;
                private final Move[] moves
                    = {Move.valueOf(Move.Action.PASS),
                       Move.valueOf(Square.C7)};
                public Move move(final GameSnapshot snapshot) {
                    index++;
                    return moves[index];
                }
            };

        final Game game = new Game.Builder()
            .withSequence(new GameSequence.Builder()
                          .withSnapshots(paip1862)
                          .build())
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK,
                                   new Actor.Builder()
                                   .withStrategy(blStrategy)
                                   .build())
                        .withActor(Player.WHITE,
                                   new Actor.Builder()
                                   .withStrategy(whStrategy)
                                   .build())
                        .build())
            .build();

        assertThat("The game paip1862 ..... must have the WHITE player.",
                   game.player(),
                   is(Player.WHITE));

        game.move();

        assertThat("After the following moves: W-C1."
                   + " The game paip1862 ..... must have the BLACK player.",
                   game.player(),
                   is(Player.BLACK));

        game.move();

        assertThat("After the following moves: W-C1, B-PASS."
                   + " The game paip1862 ..... must have the WHITE player.",
                   game.player(),
                   is(Player.WHITE));

        game.move();

        assertThat("After the following moves: W-C1, B-PASS, B-B7."
                   + " The game paip1862 ..... must have the WHITE player.",
                   game.player(),
                   is(Player.BLACK));

        game.move();

        assertThat("After the following moves: W-C1, B-PASS, B-B7, C-C7."
                   + " The game paip1862 ..... must have the WHITE player.",
                   game.player(),
                   is(Player.WHITE));


    }

    /**
     * Tests the {@code move()} method, verifying that the program handle
     * the end of the time for the player.
     *
     * @see Game#move()
     */
    @Test
    public final void testMove_checkEndOfTimeAfterMove() {

        final long moveTime = 100;
        final long clockTime = 10;

        Strategy strategy = new Strategy() {
                public Move move(final GameSnapshot snapshot) {
                    try {
                        Thread.sleep(moveTime);
                    } catch (java.lang.InterruptedException ie) {
                        throw new RuntimeException(ie);
                    }
                    return Move.valueOf(Square.D3);
                }
            };

        Game game = new Game.Builder()
            .withSequence(new GameSequence.Builder()
                          .withSnapshots(new GameSnapshot.Builder()
                                        .withClock(new Clock.Builder()
                                                   .withDuration(Player.BLACK, new Duration(clockTime))
                                                   .build())
                                        .withPosition(GamePositionFixtures.INITIAL)
                                        .withRegister(MoveRegisterFixtures.EMPTY)
                                        .build())
                          .build())
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK,
                                   new Actor.Builder()
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

        Game game = new Game.Builder()
            .withSequence(GameSequenceFixtures.THREE_SNAPSHOTS)
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK,
                                   new Actor.Builder()
                                   .withStrategy(strategy)
                                   .build())
                        .build())
            .build();

        game.move();

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

        Game game = new Game.Builder()
            .withSequence(new GameSequence.Builder()
                          .withSnapshots(GameSnapshotFixtures.BLACK_HAS_TO_PASS)
                          .build())
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK,
                                   new Actor.Builder()
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

        Game game = new Game.Builder()
            .withSequence(GameSequenceFixtures.THREE_SNAPSHOTS)
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK,
                                   new Actor.Builder()
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

        Game game = new Game.Builder()
            .withSequence(GameSequenceFixtures.THREE_SNAPSHOTS)
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK,
                                   new Actor.Builder()
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
                        Thread.sleep(2 * moveTime);
                    } catch (java.lang.InterruptedException ie) {
                        throw new RuntimeException(ie);
                    }
                    index++;
                    return Move.valueOf(moves[index]);
                }
            };

        Game game = new Game.Builder()
            .withSequence(GameSequenceFixtures.THREE_SNAPSHOTS)
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK,
                                   new Actor.Builder()
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
     * Tests the {@code play()} method.
     * <p>
     * The test runs the same game executed by {@code ReversiTest.testPAIP_18_4_0()},
     * first asking to the two appropriate strategies to calculate the respective moves.
     * Then two inline strategies are prepared having the list of the same moves, and
     * the game is played again.
     * <p>
     * The test checks that the final score is equal to +53, and that the final board is:
     * <pre>
     * {@code
     * . a b c d e f g h [@=53 0=0 (53)]
     * 1 @ @ @ @ @ @ @ @
     * 2 @ @ @ @ @ @ @ @
     * 3 @ @ @ @ @ @ @ @
     * 4 @ @ @ @ @ @ @ @
     * 5 @ @ @ @ @ @ @ @
     * 6 . . @ @ @ @ @ @
     * 7 . . . @ @ @ @ @
     * 8 . . . . @ @ . .
     * }
     * </pre>
     *
     * @see Game#play()
     * @see ReversiTest#testPAIP_18_4_0()
     */
    @Test
    public final void testPlay() {

        final Game game = new Game.Builder()
            .withSequence(GameSequenceFixtures.INITIAL)
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK,
                                   new Actor.Builder()
                                   .withStrategy(Minimax.getInstance().searcher(3, new CountDifference()))
                                   .build())
                        .withActor(Player.WHITE,
                                   new Actor.Builder()
                                   .withStrategy(Minimax.maximizer(new CountDifference()))
                                   .build())
                        .build())
            .build();

        final int gameResult = game.play();

        assertThat("The game played by the two strategies must have a final result equal to +53.",
                   gameResult,
                   is(+53));

        assertThat("The game played by the two strategies must have a defined final board.",
                   game.board(),
                   is(new BoardBuilder()
                      .withSquaresLiteral(1, 1, 1, 1, 1, 1, 1, 1,
                                          1, 1, 1, 1, 1, 1, 1, 1,
                                          1, 1, 1, 1, 1, 1, 1, 1,
                                          1, 1, 1, 1, 1, 1, 1, 1,
                                          1, 1, 1, 1, 1, 1, 1, 1,
                                          0, 0, 1, 1, 1, 1, 1, 1,
                                          0, 0, 0, 1, 1, 1, 1, 1,
                                          0, 0, 0, 0, 1, 1, 0, 0)
                      .build()));

        final Game recordedGame = new Game.Builder()
            .withSequence(GameSequenceFixtures.INITIAL)
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK,
                                   new Actor.Builder()
                                   .withStrategy(newStrategyValueOfMoveArray(Move.valueOf(Square.D3),
                                                                             Move.valueOf(Square.B3),
                                                                             Move.valueOf(Square.C4),
                                                                             Move.valueOf(Square.F3),
                                                                             Move.valueOf(Square.D1),
                                                                             Move.valueOf(Square.A1),
                                                                             Move.valueOf(Square.B5),
                                                                             Move.valueOf(Square.A5),
                                                                             Move.valueOf(Square.A2),
                                                                             Move.valueOf(Square.E6),
                                                                             Move.valueOf(Square.C1),
                                                                             Move.valueOf(Square.F1),
                                                                             Move.valueOf(Square.F4),
                                                                             Move.valueOf(Square.H1),
                                                                             Move.valueOf(Square.D2),
                                                                             Move.valueOf(Square.G2),
                                                                             Move.valueOf(Square.H3),
                                                                             Move.valueOf(Square.G5),
                                                                             Move.valueOf(Square.G4),
                                                                             Move.valueOf(Square.G6),
                                                                             Move.valueOf(Square.H6),
                                                                             Move.valueOf(Square.E7),
                                                                             Move.valueOf(Square.F8),
                                                                             Move.valueOf(Square.F6),
                                                                             Move.valueOf(Square.E8),
                                                                             Move.valueOf(Square.H7),
                                                                             Move.valueOf(Square.C6)))
                                   .build())
                        .withActor(Player.WHITE,
                                   new Actor.Builder()
                                   .withStrategy(newStrategyValueOfMoveArray(Move.valueOf(Square.C3),
                                                                             Move.valueOf(Square.B2),
                                                                             Move.valueOf(Square.E3),
                                                                             Move.valueOf(Square.E2),
                                                                             Move.valueOf(Square.A3),
                                                                             Move.valueOf(Square.B4),
                                                                             Move.valueOf(Square.A4),
                                                                             Move.valueOf(Square.C2),
                                                                             Move.valueOf(Square.C5),
                                                                             Move.valueOf(Square.F5),
                                                                             Move.valueOf(Square.E1),
                                                                             Move.valueOf(Square.B1),
                                                                             Move.valueOf(Square.G1),
                                                                             Move.valueOf(Square.F2),
                                                                             Move.valueOf(Square.D7),
                                                                             Move.valueOf(Square.G3),
                                                                             Move.valueOf(Square.H2),
                                                                             Move.valueOf(Square.D6),
                                                                             Move.valueOf(Square.H5),
                                                                             Move.valueOf(Square.H4),
                                                                             Move.valueOf(Move.Action.PASS),
                                                                             Move.valueOf(Square.F7),
                                                                             Move.valueOf(Move.Action.PASS),
                                                                             Move.valueOf(Square.G7),
                                                                             Move.valueOf(Move.Action.PASS),
                                                                             Move.valueOf(Move.Action.PASS)))
                                   .build())
                        .build())
            .build();

        final int recordedGameResult = recordedGame.play();

        assertThat("The game recorded move by move must have a final result equal to +53.",
                   recordedGameResult,
                   is(+53));

        assertThat("The game recorded move by move must have the same final board obtained by the two strategies.",
                   recordedGame.board(),
                   is(game.board()));

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
                   new Game.Builder()
                   .withSequence(new GameSequence.Builder()
                                 .withSnapshots(new GameSnapshot.Builder()
                                                .withPosition(new GamePosition.Builder()
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
                   new Game.Builder()
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
        new Game.Builder().build().validateMove(Move.NULL);
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
                         new GameSequence.Builder().build(),
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
        Game.newInstance(new ActorsPair.Builder().build(),
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
        Game instance = Game.newInstance(new ActorsPair.Builder().build(),
                                         new GameSequence.Builder().build(),
                                         CommonFixtures.NULL_PRINT_STREAM);

        assertThat("Game.newInstance(ActorsPair, GameSequence, PrintStream)"
                   + " must return an instance of Game class.",
                   instance,
                   instanceOf(Game.class));
    }

    /**
     * Tests the {@code randomGame(long)} method when parameter
     * {@code numberOfRandomPutDiscMoves} is smaller than 0.
     *
     * @see Game#randomGame(long)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testRandomGame_boundaryConditions_negative_numberOfRandomPutDiscMoves() {
        Game.randomGame(-1);
    }

    /**
     * Tests the {@code randomGame(long)} method when parameter
     * {@code numberOfRandomPutDiscMoves} is greater than 60.
     *
     * @see Game#randomGame(long)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testRandomGame_boundaryConditions_greaterThanSixty_numberOfRandomPutDiscMoves() {
        Game.randomGame(61);
    }

    /**
     * Tests the {@code randomGame(long)} method.
     * <p>
     * The number of possible position that can be reached from an initial
     * board running two moves is twelve. The black has four moves (bringing
     * to the same case) than the white has the three classical openings named
     * Perpendicular, Diagonal, and Parallel. Composing the two chances the reachable
     * boards are then twelve.
     * <p>
     * By running the test one undred thousand times the maximum number of random
     * games generated in order to obtain at last one board for each of the twelve
     * achievable positions is close to two hundred.
     * <p>
     * The test run one hundred times and checks that within the limit of two hundred
     * and fifty random position generated all the twelve cases are obtained. If not
     * the test fails.
     * <p>
     * It is statistically possible that the twelve positions are not filled within
     * two hundred and fifty chances, but it is really a small probability.  
     *
     * @see Game#randomGame(long)
     */
    @Test
    public final void testRandomGame_twoMoves() {

        final int twoMoves = 2;
        final int twoHundredAndFiftyTimes = 250;
        final int twelvePositions = 12;
        final int oneHundredRuns = 100;

        for (int j = 0; j < oneHundredRuns; j++) {
            Set<Board> boards = new HashSet<Board>();

            for (int i = 0; i < twoHundredAndFiftyTimes; i++) {
                boards.add(Game.randomGame(twoMoves).board());
                if (boards.size() == twelvePositions) {
                    assertTrue(true);
                    break;
                }
            }
            if (boards.size() != twelvePositions) {
                fail("The number of possible position reached after two moves is twelve.");
            }
        }
    }

    /**
     * Tests the {@code randomGame(long)} method.
     * <p>
     * When generating ten moves random game, the resulting boards must be all
     * difefrent. The case that two between one hundred are equal is very very low.
     *
     * @see Game#randomGame(long)
     */
    @Test
    public final void testRandomGame_tenMoves() {

        final int tenMoves = 10;
        final int oneHundredRuns = 100;

        Set<Board> boards = new HashSet<Board>();
        for (int i = 0; i < oneHundredRuns; i++) {
            boards.add(Game.randomGame(tenMoves).board());
        }

        assertThat("Given the very low probability of two boards equal between one"
                   + " hundred made by running ten random moves, boards.size() must"
                   + " be equal to is(oneHundredRuns).",
                   boards.size(),
                   is(oneHundredRuns));
    }

    /**
     * Tests the {@code randomGame(long)} method.
     * <p>
     * When generating a zero moves random game, the resulting board must be an initial board.
     *
     * @see Game#randomGame(long)
     */
    @Test
    public final void testRandomGame_zeroMoves() {
        assertThat("Game.randomGame(0).board() must be BoardFixtures.INITIAL.",
                   Game.randomGame(0).board(),
                   is(BoardFixtures.INITIAL));

    }

    /**
     * Tests the {@code randomGame(long)} method.
     * <p>
     * When generating a zero moves random game, the resulting board must be an initial board.
     *
     * @see Game#randomGame(long)
     */
    @Test
    public final void testRandomGame_sixtyMoves() {
        for (int i = 0; i < 100; i++) {
            assertThat("Game.randomGame(60).board().countPieces(SquareState.EMPTY) must be 0.",
                       Game.randomGame(60).board().countPieces(SquareState.EMPTY),
                       is(0));
        }
    }

}
