/*
 *  GameSnapshotFixtures.java
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

import org.joda.time.DateTime;
import org.joda.time.Period;

/**
 * The class host a number of predefined game snapshots.
 * <p>
 * The {@code GameSnapshot} class defines immutable objects thus {@code GameSnapshotFixtures}
 * implements game snapshot instances as public static shared objects. Tests can
 * freely share the instances without any modification issue.
 */
public final class GameSnapshotFixtures {

    /** A generic game snapshot. */
    public static final GameSnapshot AN_INSTANCE = new GameSnapshotBuilder()
        .withPosition(new GamePositionBuilder()
                      .withBoard(new BoardBuilder()
                                 .withSquaresLiteral(0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 1, 0, 0, 0, 0,
                                                     0, 0, 0, 1, 1, 0, 1, 0,
                                                     0, 0, 2, 2, 2, 2, 2, 2,
                                                     0, 0, 0, 0, 1, 0, 1, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0)
                                 .build())
                      .withPlayer(Player.BLACK)
                      .build())
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withRegister(MoveRegisterFixtures.EMPTY)
        .build();

    /** Initial position, one minute left to both players. */
    public static final GameSnapshot INITIAL = GameSnapshot.initialGameSnapshot(CommonFixtures.ONE_MINUTE_DURATION);

    /**
     * Game snapshot <i>S00</i> taken from a game named <i>G00</i>.
     * <p>
     * {@code G00_S00.board()} returns a board described by:
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . . . . . .
     * 2 . . . . . . . .
     * 3 . . . . . . . .
     * 4 . . . O @ . . .
     * 5 . . . @ O . . .
     * 6 . . . . . . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     * {@code G00_S00.player()} returns: {@code Player.BLACK}.
     * <p>
     * {@code G00_S00.clock()} returns a clock described by: {@code [BLACK=01:00, WHITE=01:00]}.
     * <p>
     * {@code G00_S00.register()} returns a register described by:
     * {@code [EMPTY MoveRegister]}.
     **/
    public static final GameSnapshot G00_S00 = new GameSnapshotBuilder()
        .withPosition(new GamePositionBuilder()
                      .withBoard(new BoardBuilder()
                                 .withSquaresLiteral(0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 2, 1, 0, 0, 0,
                                                     0, 0, 0, 1, 2, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0)
                                 .build())
                      .withPlayer(Player.BLACK)
                      .build())
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withRegister(MoveRegisterFixtures.EMPTY)
        .build();

    /**
     * Game snapshot <i>S01</i> taken from a game named <i>G00</i>.
     * <p>
     * {@code G00_S01.board()} returns a board described by:
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . . . . . .
     * 2 . . . . . . . .
     * 3 . . . @ . . . .
     * 4 . . . @ @ . . .
     * 5 . . . @ O . . .
     * 6 . . . . . . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     * {@code G00_S01.player()} returns: {@code Player.WHITE}.
     * <p>
     * {@code G00_S01.clock()} returns a clock described by: {@code [BLACK=00:59, WHITE=01:00]}.
     * <p>
     * {@code G00_S01.register()} returns a register described by:
     * {@code [[PUT_DISC; D3]; [BLACK=00:59, WHITE=01:00]; 2011-03-17T08:03:00.001Z]}.
     **/
    public static final GameSnapshot G00_S01 = new GameSnapshotBuilder()
        .withPosition(new GamePositionBuilder()
                      .withBoard(new BoardBuilder()
                                 .withSquaresLiteral(0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 1, 0, 0, 0, 0,
                                                     0, 0, 0, 1, 1, 0, 0, 0,
                                                     0, 0, 0, 1, 2, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0)
                                 .build())
                      .withPlayer(Player.WHITE)
                      .build())
        .withClock(Clock.valueOf(Period.seconds(59).toStandardDuration(),
                                 Period.seconds(60).toStandardDuration()))
        .withRegister(new MoveRegisterBuilder()
                      .withRecords(new MoveRecordBuilder()
                                   .withMove(Move.valueOf(Move.Action.PUT_DISC, Square.D3))
                                   .withClock(Clock.valueOf(Period.seconds(59).toStandardDuration(),
                                                            Period.seconds(60).toStandardDuration()))
                                   .withTimestamp(new DateTime(2011,
                                                               03,
                                                               17,
                                                               9,
                                                               03,
                                                               0,
                                                               1).toInstant())
                                   .build())
                      .build())
        .build();

    /**
     * Game snapshot <i>S02</i> taken from a game named <i>G00</i>.
     * <p>
     * {@code G00_S02.board()} returns a board described by:
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . . . . . .
     * 2 . . . . . . . .
     * 3 . . . @ . . . .
     * 4 . . . @ @ . . .
     * 5 . . O O O . . .
     * 6 . . . . . . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     * {@code G00_S02.player()} returns: {@code Player.BLACK}.
     * <p>
     * {@code G00_S02.clock()} returns a clock described by: {@code [BLACK=00:59, WHITE=00:55]}.
     * <p>
     * {@code G00_S02.register()} returns a register described by:
     * <pre>
     * {@code
     * [[PUT_DISC; C5]; [BLACK=00:59, WHITE=00:55]; 2011-03-17T08:03:15.001Z]
     * [[PASS; null]; [BLACK=00:59, WHITE=00:58]; 2011-03-17T08:03:11.001Z]
     * [[PUT_DISC; A1]; [BLACK=00:59, WHITE=00:59]; 2011-03-17T08:03:10.001Z]
     * }.
     * </pre>
     **/
    public static final GameSnapshot G00_S02 = new GameSnapshotBuilder()
        .withPosition(new GamePositionBuilder()
                      .withBoard(new BoardBuilder()
                                 .withSquaresLiteral(0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 1, 0, 0, 0, 0,
                                                     0, 0, 0, 1, 1, 0, 0, 0,
                                                     0, 0, 2, 2, 2, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0,
                                                     0, 0, 0, 0, 0, 0, 0, 0)
                                 .build())
                      .withPlayer(Player.BLACK)
                      .build())
        .withClock(Clock.valueOf(Period.seconds(59).toStandardDuration(),
                                 Period.seconds(55).toStandardDuration()))
        .withRegister(new MoveRegisterBuilder()
                      .withRecords(new MoveRecordBuilder()
                                   .withMove(Move.valueOf(Move.Action.PUT_DISC, Square.A1))
                                   .withClock(Clock.valueOf(Period.seconds(59).toStandardDuration(),
                                                            Period.seconds(59).toStandardDuration()))
                                   .withTimestamp(new DateTime(2011,
                                                               03,
                                                               17,
                                                               9,
                                                               03,
                                                               10,
                                                               1).toInstant())
                                   .build(),
                                   new MoveRecordBuilder()
                                   .withMove(Move.valueOf(Move.Action.PASS, Square.NULL))
                                   .withClock(Clock.valueOf(Period.seconds(59).toStandardDuration(),
                                                            Period.seconds(58).toStandardDuration()))
                                   .withTimestamp(new DateTime(2011,
                                                               03,
                                                               17,
                                                               9,
                                                               03,
                                                               11,
                                                               1).toInstant())
                                   .build(),
                                   new MoveRecordBuilder()
                                   .withMove(Move.valueOf(Move.Action.PUT_DISC, Square.C5))
                                   .withClock(Clock.valueOf(Period.seconds(59).toStandardDuration(),
                                                            Period.seconds(55).toStandardDuration()))
                                   .withTimestamp(new DateTime(2011,
                                                               03,
                                                               17,
                                                               9,
                                                               03,
                                                               15,
                                                               1).toInstant())
                                   .build())
                      .build())
        .build();

    /** Minimax test case A, white player has to move, one minute left to both players. */
    public static final GameSnapshot MINIMAX_TEST_CASE_A = new GameSnapshotBuilder()
        .withPosition(GamePositionFixtures.MINIMAX_TEST_CASE_A)
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withRegister(MoveRegisterFixtures.EMPTY)
        .build();

    /** Minimax test case B, white player has to move, one minute left to both players. */
    public static final GameSnapshot MINIMAX_TEST_CASE_B = new GameSnapshotBuilder()
        .withPosition(GamePositionFixtures.MINIMAX_TEST_CASE_B)
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withRegister(MoveRegisterFixtures.EMPTY)
        .build();

    /**
     * Black has no legal moves, black player has to move, one minute left to both players.
     *
     * @see GamePositionFixtures#BLACK_HAS_TO_PASS
     * @see ClockFixtures#ONE_MINUTE_LEFT_TO_BOTH_PLAYERS
     * @see MoveRegisterFixtures#EMPTY
     */
    public static final GameSnapshot BLACK_HAS_TO_PASS = new GameSnapshotBuilder()
        .withPosition(GamePositionFixtures.BLACK_HAS_TO_PASS)
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withRegister(MoveRegisterFixtures.EMPTY)
        .build();

    /**
     * Early game snapshot identified by EARLY_GAME_B_9_MOVES.
     * Nine moves from the beginning.
     * White player has to move.
     */
    public static final GameSnapshot EARLY_GAME_B_9_MOVES = new GameSnapshotBuilder()
        .withPosition(new GamePositionBuilder()
                      .withBoard(BoardFixtures.EARLY_GAME_B_9_MOVES)
                      .withPlayer(Player.WHITE)
                      .build())
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withRegister(MoveRegisterFixtures.EMPTY)
        .build();

    /**
     * Game snapshot identified by EARLY_GAME_BC3_10_MOVES.
     * Ten moves from the beginning.
     * Black player has to move.
     * The game position has been generated from EARLY_GAME_B_9_MOVES by
     * moving the black to c3.
     */
    public static final GameSnapshot EARLY_GAME_BC3_10_MOVES = new GameSnapshotBuilder()
        .withPosition(new GamePositionBuilder()
                      .withBoard(BoardFixtures.EARLY_GAME_BC3_10_MOVES)
                      .withPlayer(Player.BLACK)
                      .build())
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withRegister(MoveRegisterFixtures.EMPTY)
        .build();

    /**
     * Game snapshot identified by EARLY_GAME_BC6_10_MOVES.
     * Ten moves from the beginning.
     * Black player has to move.
     * The game position has been generated from EARLY_GAME_B_9_MOVES by
     * moving the black to c6.
     */
    public static final GameSnapshot EARLY_GAME_BC6_10_MOVES = new GameSnapshotBuilder()
        .withPosition(new GamePositionBuilder()
                      .withBoard(BoardFixtures.EARLY_GAME_BC6_10_MOVES)
                      .withPlayer(Player.BLACK)
                      .build())
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withRegister(MoveRegisterFixtures.EMPTY)
        .build();

    /** The null game snapshot. */
    public static final GameSnapshot NULL = null;

    /** Class constructor. */
    private GameSnapshotFixtures() { }

}
