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
    private static final Strategy newStrategyValueOfMoveArray(final Move... moves) {
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

        final GameSnapshot paip1862 = new GameSnapshotBuilder()
            .withPosition(new GamePositionBuilder()
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

        final Game game = new GameBuilder()
            .withSequence(new GameSequenceBuilder()
                          .withSnapshots(paip1862)
                          .build())
            .withActors(new ActorsPairBuilder()
                        .withActor(Player.BLACK,
                                   new ActorBuilder()
                                   .withStrategy(blStrategy)
                                   .build())
                        .withActor(Player.WHITE,
                                   new ActorBuilder()
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

        Game game = new GameBuilder()
            .withSequence(new GameSequenceBuilder()
                          .withSnapshots(new GameSnapshotBuilder()
                                        .withClock(new ClockBuilder()
                                                   .withDuration(Player.BLACK, new Duration(clockTime))
                                                   .build())
                                        .withPosition(GamePositionFixtures.INITIAL)
                                        .withRegister(MoveRegisterFixtures.EMPTY)
                                        .build())
                          .build())
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
                          .withSnapshots(GameSnapshotFixtures.BLACK_HAS_TO_PASS)
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

        final Game game = new GameBuilder()
            .withSequence(GameSequenceFixtures.INITIAL)
            .withActors(new ActorsPairBuilder()
                        .withActor(Player.BLACK,
                                   new ActorBuilder()
                                   .withStrategy(Minimax.getInstance().searcher(3, new CountDifference()))
                                   .build())
                        .withActor(Player.WHITE,
                                   new ActorBuilder()
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

        final Game regcordedGame = new GameBuilder()
            .withSequence(GameSequenceFixtures.INITIAL)
            .withActors(new ActorsPairBuilder()
                        .withActor(Player.BLACK,
                                   new ActorBuilder()
                                   .withStrategy(newStrategyValueOfMoveArray(Move.valueOf(Square.D3),
                                                                             Move.valueOf(Square.B3)))
                                   .build())
                        .withActor(Player.WHITE,
                                   new ActorBuilder()
                                   .withStrategy(newStrategyValueOfMoveArray(Move.valueOf(Square.C3),
                                                                             Move.valueOf(Square.B2)))
                                   .build())
                        .build())
            .build();
        /*
    [junit]  Next to play: BLACK, legal moves: [b3, c4, f5, e6]
    [junit] 
    [junit] [[PUT_DISC; B3]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.327Z]
    [junit]     a b c d e f g h [@=5 0=2 (3)]
    [junit]  1  . . . . . . . . 
    [junit]  2  . . . . . . . . 
    [junit]  3  . @ @ @ . . . . 
    [junit]  4  . . . O @ . . . 
    [junit]  5  . . . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [b2, d2, e3, f4, c5, d6]
    [junit] 
    [junit] [[PUT_DISC; B2]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.330Z]
    [junit]     a b c d e f g h [@=4 0=4 (0)]
    [junit]  1  . . . . . . . . 
    [junit]  2  . O . . . . . . 
    [junit]  3  . @ O @ . . . . 
    [junit]  4  . . . O @ . . . 
    [junit]  5  . . . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [b1, c4, f5, e6]
    [junit] 
    [junit] [[PUT_DISC; C4]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.362Z]
    [junit]     a b c d e f g h [@=6 0=3 (3)]
    [junit]  1  . . . . . . . . 
    [junit]  2  . O . . . . . . 
    [junit]  3  . @ O @ . . . . 
    [junit]  4  . . @ @ @ . . . 
    [junit]  5  . . . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [a3, e3, b4, c5]
    [junit] 
    [junit] [[PUT_DISC; E3]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.364Z]
    [junit]     a b c d e f g h [@=4 0=6 (-2)]
    [junit]  1  . . . . . . . . 
    [junit]  2  . O . . . . . . 
    [junit]  3  . @ O O O . . . 
    [junit]  4  . . @ @ O . . . 
    [junit]  5  . . . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [a1, b1, c2, d2, e2, f2, f3, f4, f5, f6]
    [junit] 
    [junit] [[PUT_DISC; F3]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.465Z]
    [junit]     a b c d e f g h [@=9 0=2 (7)]
    [junit]  1  . . . . . . . . 
    [junit]  2  . O . . . . . . 
    [junit]  3  . @ @ @ @ @ . . 
    [junit]  4  . . @ @ @ . . . 
    [junit]  5  . . . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [e2, b4, c5]
    [junit] 
    [junit] [[PUT_DISC; E2]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.467Z]
    [junit]     a b c d e f g h [@=7 0=5 (2)]
    [junit]  1  . . . . . . . . 
    [junit]  2  . O . . O . . . 
    [junit]  3  . @ @ @ O @ . . 
    [junit]  4  . . @ @ O . . . 
    [junit]  5  . . . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [a1, b1, d1, f1, f2, f4, f5, f6]
    [junit] 
    [junit] [[PUT_DISC; D1]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.517Z]
    [junit]     a b c d e f g h [@=9 0=4 (5)]
    [junit]  1  . . . @ . . . . 
    [junit]  2  . O . . @ . . . 
    [junit]  3  . @ @ @ O @ . . 
    [junit]  4  . . @ @ O . . . 
    [junit]  5  . . . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [e1, c2, g2, a3, g3, b4, c5, c6]
    [junit] 
    [junit] [[PUT_DISC; A3]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.519Z]
    [junit]     a b c d e f g h [@=6 0=8 (-2)]
    [junit]  1  . . . @ . . . . 
    [junit]  2  . O . . @ . . . 
    [junit]  3  O O O O O @ . . 
    [junit]  4  . . @ @ O . . . 
    [junit]  5  . . . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [a1, a2, c2, d2, f2, f4, f5, e6, f6]
    [junit] 
    [junit] [[PUT_DISC; A1]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.560Z]
    [junit]     a b c d e f g h [@=9 0=6 (3)]
    [junit]  1  @ . . @ . . . . 
    [junit]  2  . @ . . @ . . . 
    [junit]  3  O O @ O O @ . . 
    [junit]  4  . . @ @ O . . . 
    [junit]  5  . . . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [b1, c1, e1, f1, g2, g3, b4, b5, c5, c6, d6, e6]
    [junit] 
    [junit] [[PUT_DISC; B4]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.561Z]
    [junit]     a b c d e f g h [@=7 0=9 (-2)]
    [junit]  1  @ . . @ . . . . 
    [junit]  2  . @ . . @ . . . 
    [junit]  3  O O @ O O @ . . 
    [junit]  4  . O O O O . . . 
    [junit]  5  . . . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [a2, d2, a5, b5, c5, f5, e6, f6]
    [junit] 
    [junit] [[PUT_DISC; B5]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.601Z]
    [junit]     a b c d e f g h [@=12 0=5 (7)]
    [junit]  1  @ . . @ . . . . 
    [junit]  2  . @ . . @ . . . 
    [junit]  3  O @ @ @ O @ . . 
    [junit]  4  . @ @ O O . . . 
    [junit]  5  . @ . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [c1, e1, c2, d2, g2, g3, a4, c5, c6, d6]
    [junit] 
    [junit] [[PUT_DISC; A4]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.602Z]
    [junit]     a b c d e f g h [@=10 0=8 (2)]
    [junit]  1  @ . . @ . . . . 
    [junit]  2  . @ . . @ . . . 
    [junit]  3  O @ @ @ O @ . . 
    [junit]  4  O O O O O . . . 
    [junit]  5  . @ . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [a5, c5, f5, e6, f6]
    [junit] 
    [junit] [[PUT_DISC; A5]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.623Z]
    [junit]     a b c d e f g h [@=12 0=7 (5)]
    [junit]  1  @ . . @ . . . . 
    [junit]  2  . @ . . @ . . . 
    [junit]  3  O @ @ @ O @ . . 
    [junit]  4  O @ O O O . . . 
    [junit]  5  @ @ . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [c1, e1, f1, a2, c2, d2, g2, g3, c5, a6, c6, d6, e6]
    [junit] 
    [junit] [[PUT_DISC; C2]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.624Z]
    [junit]     a b c d e f g h [@=9 0=11 (-2)]
    [junit]  1  @ . . @ . . . . 
    [junit]  2  . @ O . @ . . . 
    [junit]  3  O O O O O @ . . 
    [junit]  4  O @ O O O . . . 
    [junit]  5  @ @ . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [a2, d2, f4, f5, e6, f6]
    [junit] 
    [junit] [[PUT_DISC; A2]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.646Z]
    [junit]     a b c d e f g h [@=14 0=7 (7)]
    [junit]  1  @ . . @ . . . . 
    [junit]  2  @ @ O . @ . . . 
    [junit]  3  @ @ O O O @ . . 
    [junit]  4  @ @ @ O O . . . 
    [junit]  5  @ @ . @ O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [e1, f1, g2, g3, c5, a6, c6, d6]
    [junit] 
    [junit] [[PUT_DISC; C5]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.646Z]
    [junit]     a b c d e f g h [@=12 0=10 (2)]
    [junit]  1  @ . . @ . . . . 
    [junit]  2  @ @ O . @ . . . 
    [junit]  3  @ @ O O O @ . . 
    [junit]  4  @ @ O O O . . . 
    [junit]  5  @ @ O O O . . . 
    [junit]  6  . . . . . . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [d2, f4, f5, c6, d6, e6, f6]
    [junit] 
    [junit] [[PUT_DISC; E6]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.663Z]
    [junit]     a b c d e f g h [@=18 0=5 (13)]
    [junit]  1  @ . . @ . . . . 
    [junit]  2  @ @ O . @ . . . 
    [junit]  3  @ @ O O @ @ . . 
    [junit]  4  @ @ @ O @ . . . 
    [junit]  5  @ @ O @ @ . . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [f1, f2, g3, f4, f5, a6, d6, f6]
    [junit] 
    [junit] [[PUT_DISC; F5]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.664Z]
    [junit]     a b c d e f g h [@=15 0=9 (6)]
    [junit]  1  @ . . @ . . . . 
    [junit]  2  @ @ O . @ . . . 
    [junit]  3  @ @ O O @ @ . . 
    [junit]  4  @ @ @ O O . . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [c1, d2, f4, g4, g5, b6, c6, d6, f6]
    [junit] 
    [junit] [[PUT_DISC; C1]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.685Z]
    [junit]     a b c d e f g h [@=18 0=7 (11)]
    [junit]  1  @ . @ @ . . . . 
    [junit]  2  @ @ @ . @ . . . 
    [junit]  3  @ @ @ O @ @ . . 
    [junit]  4  @ @ @ O O . . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [b1, e1, f1, f2, g2, g3, a6, d7, e7, f7]
    [junit] 
    [junit] [[PUT_DISC; E1]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.686Z]
    [junit]     a b c d e f g h [@=16 0=10 (6)]
    [junit]  1  @ . @ @ O . . . 
    [junit]  2  @ @ @ . O . . . 
    [junit]  3  @ @ @ O O @ . . 
    [junit]  4  @ @ @ O O . . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [f1, f4, g4, g5, c6, d6, f6, g6]
    [junit] 
    [junit] [[PUT_DISC; F1]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.700Z]
    [junit]     a b c d e f g h [@=20 0=7 (13)]
    [junit]  1  @ . @ @ @ @ . . 
    [junit]  2  @ @ @ . @ . . . 
    [junit]  3  @ @ @ @ O @ . . 
    [junit]  4  @ @ @ O O . . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [b1, d2, g2, g3, d7, e7, f7]
    [junit] 
    [junit] [[PUT_DISC; B1]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.701Z]
    [junit]     a b c d e f g h [@=18 0=10 (8)]
    [junit]  1  @ O @ @ @ @ . . 
    [junit]  2  @ @ O . @ . . . 
    [junit]  3  @ @ @ O O @ . . 
    [junit]  4  @ @ @ O O . . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [d2, f4, g4, g5, c6, d6, f6]
    [junit] 
    [junit] [[PUT_DISC; F4]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.711Z]
    [junit]     a b c d e f g h [@=21 0=8 (13)]
    [junit]  1  @ O @ @ @ @ . . 
    [junit]  2  @ @ O . @ . . . 
    [junit]  3  @ @ @ O O @ . . 
    [junit]  4  @ @ @ @ @ @ . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [g1, f2, g2, g3, g5, a6, b6, d7, e7, f7]
    [junit] 
    [junit] [[PUT_DISC; G1]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.713Z]
    [junit]     a b c d e f g h [@=17 0=13 (4)]
    [junit]  1  @ O O O O O O . 
    [junit]  2  @ @ O . @ . . . 
    [junit]  3  @ @ @ O O @ . . 
    [junit]  4  @ @ @ @ @ @ . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [h1, d2, f2, g4, g5, b6, c6, d6, f6, g6]
    [junit] 
    [junit] [[PUT_DISC; H1]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.726Z]
    [junit]     a b c d e f g h [@=24 0=7 (17)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ O . @ . . . 
    [junit]  3  @ @ @ O O @ . . 
    [junit]  4  @ @ @ @ @ @ . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [f2, g2, g3, g5, a6, d7, e7, f7]
    [junit] 
    [junit] [[PUT_DISC; F2]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.727Z]
    [junit]     a b c d e f g h [@=22 0=10 (12)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ O . @ O . . 
    [junit]  3  @ @ @ O O O . . 
    [junit]  4  @ @ @ @ @ O . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [d2, g2, g3, g4, g5, b6, c6, d6, f6, g6]
    [junit] 
    [junit] [[PUT_DISC; D2]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.739Z]
    [junit]     a b c d e f g h [@=25 0=8 (17)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ O . . 
    [junit]  3  @ @ @ @ O O . . 
    [junit]  4  @ @ @ @ @ O . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . @ . . . 
    [junit]  7  . . . . . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [d7, e7, f7]
    [junit] 
    [junit] [[PUT_DISC; D7]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.740Z]
    [junit]     a b c d e f g h [@=24 0=10 (14)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ O . . 
    [junit]  3  @ @ @ @ O O . . 
    [junit]  4  @ @ @ @ @ O . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . O . . . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [g2, g3, g4, g5, b6, c6, d6, f6, g6, e7, f7]
    [junit] 
    [junit] [[PUT_DISC; G2]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.743Z]
    [junit]     a b c d e f g h [@=27 0=8 (19)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ . 
    [junit]  3  @ @ @ @ O @ . . 
    [junit]  4  @ @ @ @ @ O . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . O . . . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [g3]
    [junit] 
    [junit] [[PUT_DISC; G3]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.744Z]
    [junit]     a b c d e f g h [@=26 0=10 (16)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ . 
    [junit]  3  @ @ @ @ O O O . 
    [junit]  4  @ @ @ @ @ O . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . O . . . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [h3, g4, h4, g5, b6, c6, d6, f6, g6, e7, f7]
    [junit] 
    [junit] [[PUT_DISC; H3]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.749Z]
    [junit]     a b c d e f g h [@=30 0=7 (23)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ . 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ O . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . O . . . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [h2]
    [junit] 
    [junit] [[PUT_DISC; H2]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.750Z]
    [junit]     a b c d e f g h [@=29 0=9 (20)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ O 
    [junit]  3  @ @ @ @ @ @ O @ 
    [junit]  4  @ @ @ @ @ O . . 
    [junit]  5  @ @ O O O O . . 
    [junit]  6  . . . . O . . . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [g4, h4, g5, b6, c6, d6, f6, g6, e7, f7]
    [junit] 
    [junit] [[PUT_DISC; G5]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.755Z]
    [junit]     a b c d e f g h [@=35 0=4 (31)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ O 
    [junit]  3  @ @ @ @ @ @ O @ 
    [junit]  4  @ @ @ @ @ @ . . 
    [junit]  5  @ @ @ @ @ @ @ . 
    [junit]  6  . . . . O . . . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [g4, h4, d6]
    [junit] 
    [junit] [[PUT_DISC; D6]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.756Z]
    [junit]     a b c d e f g h [@=33 0=7 (26)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ O 
    [junit]  3  @ @ @ @ @ @ O @ 
    [junit]  4  @ @ @ @ @ O . . 
    [junit]  5  @ @ @ @ O @ @ . 
    [junit]  6  . . . O O . . . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [g4, h4, f6, e7, f7, c8, d8]
    [junit] 
    [junit] [[PUT_DISC; G4]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.757Z]
    [junit]     a b c d e f g h [@=36 0=5 (31)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ O 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ . 
    [junit]  5  @ @ @ @ O @ @ . 
    [junit]  6  . . . O O . . . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [h4, h5]
    [junit] 
    [junit] [[PUT_DISC; H5]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.758Z]
    [junit]     a b c d e f g h [@=34 0=8 (26)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ O 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ . 
    [junit]  5  @ @ @ @ O O O O 
    [junit]  6  . . . O O . . . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [f6, g6, h6, c7, e7, f7, c8, d8]
    [junit] 
    [junit] [[PUT_DISC; G6]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.759Z]
    [junit]     a b c d e f g h [@=37 0=6 (31)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ O 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ . 
    [junit]  5  @ @ @ @ O @ @ O 
    [junit]  6  . . . O O . @ . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [h4, f7]
    [junit] 
    [junit] [[PUT_DISC; H4]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.760Z]
    [junit]     a b c d e f g h [@=36 0=8 (28)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ O 
    [junit]  3  @ @ @ @ @ @ @ O 
    [junit]  4  @ @ @ @ @ @ @ O 
    [junit]  5  @ @ @ @ O @ @ O 
    [junit]  6  . . . O O . @ . 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [f6, h6, c7, e7, f7, c8, d8]
    [junit] 
    [junit] [[PUT_DISC; H6]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.761Z]
    [junit]     a b c d e f g h [@=41 0=4 (37)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ O @ @ @ 
    [junit]  6  . . . O O . @ @ 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: []
    [junit] 
    [junit] [[PASS; null]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.761Z]
    [junit]     a b c d e f g h [@=41 0=4 (37)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ O @ @ @ 
    [junit]  6  . . . O O . @ @ 
    [junit]  7  . . . O . . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [f6, c7, e7, f7, c8, d8]
    [junit] 
    [junit] [[PUT_DISC; E7]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.764Z]
    [junit]     a b c d e f g h [@=45 0=1 (44)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . . @ @ . @ @ 
    [junit]  7  . . . O @ . . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [f7]
    [junit] 
    [junit] [[PUT_DISC; F7]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.764Z]
    [junit]     a b c d e f g h [@=44 0=3 (41)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . . @ @ . @ @ 
    [junit]  7  . . . O O O . . 
    [junit]  8  . . . . . . . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [c8, d8, e8, f8, g8]
    [junit] 
    [junit] [[PUT_DISC; F8]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.765Z]
    [junit]     a b c d e f g h [@=46 0=2 (44)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . . @ @ . @ @ 
    [junit]  7  . . . O @ O . . 
    [junit]  8  . . . . . @ . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: []
    [junit] 
    [junit] [[PASS; null]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.765Z]
    [junit]     a b c d e f g h [@=46 0=2 (44)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . . @ @ . @ @ 
    [junit]  7  . . . O @ O . . 
    [junit]  8  . . . . . @ . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [f6, c7, g7, c8, d8, e8, g8]
    [junit] 
    [junit] [[PUT_DISC; F6]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.766Z]
    [junit]     a b c d e f g h [@=48 0=1 (47)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . . @ @ @ @ @ 
    [junit]  7  . . . O @ @ . . 
    [junit]  8  . . . . . @ . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: [g7]
    [junit] 
    [junit] [[PUT_DISC; G7]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.766Z]
    [junit]     a b c d e f g h [@=46 0=4 (42)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . . @ @ @ @ @ 
    [junit]  7  . . . O O O O . 
    [junit]  8  . . . . . @ . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [c8, d8, e8, g8, h8]
    [junit] 
    [junit] [[PUT_DISC; E8]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.766Z]
    [junit]     a b c d e f g h [@=49 0=2 (47)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . . @ @ @ @ @ 
    [junit]  7  . . . O @ @ O . 
    [junit]  8  . . . . @ @ . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: []
    [junit] 
    [junit] [[PASS; null]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.767Z]
    [junit]     a b c d e f g h [@=49 0=2 (47)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . . @ @ @ @ @ 
    [junit]  7  . . . O @ @ O . 
    [junit]  8  . . . . @ @ . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [c6, c7, h7, c8, d8, g8, h8]
    [junit] 
    [junit] [[PUT_DISC; H7]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.767Z]
    [junit]     a b c d e f g h [@=51 0=1 (50)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . . @ @ @ @ @ 
    [junit]  7  . . . O @ @ @ @ 
    [junit]  8  . . . . @ @ . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: []
    [junit] 
    [junit] [[PASS; null]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.768Z]
    [junit]     a b c d e f g h [@=51 0=1 (50)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . . @ @ @ @ @ 
    [junit]  7  . . . O @ @ @ @ 
    [junit]  8  . . . . @ @ . . [@=00:59, O=00:59]
    [junit]  Next to play: BLACK, legal moves: [c6, c7, c8, d8]
    [junit] 
    [junit] [[PUT_DISC; C6]; [BLACK=00:59, WHITE=00:59]; 2011-05-22T07:08:14.768Z]
    [junit]     a b c d e f g h [@=53 0=0 (53)]
    [junit]  1  @ @ @ @ @ @ @ @ 
    [junit]  2  @ @ @ @ @ @ @ @ 
    [junit]  3  @ @ @ @ @ @ @ @ 
    [junit]  4  @ @ @ @ @ @ @ @ 
    [junit]  5  @ @ @ @ @ @ @ @ 
    [junit]  6  . . @ @ @ @ @ @ 
    [junit]  7  . . . @ @ @ @ @ 
    [junit]  8  . . . . @ @ . . [@=00:59, O=00:59]
    [junit]  Next to play: WHITE, legal moves: []
         */

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
