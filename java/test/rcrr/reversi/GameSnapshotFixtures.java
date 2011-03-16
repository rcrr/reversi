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

import java.util.Arrays;
import java.util.List;

/**
 * The class host a number of predefined game snapshots.
 * <p>
 * The {@code GameSnapshot} class defines immutable objects thus {@code GameSnapshotFixtures}
 * implements game snapshot instances as public static shared objects. Tests can
 * freely share the instances without any modification issue.
 */
public class GameSnapshotFixtures {

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

    /** Game snapshot S0. */
    public static final GameSnapshot S0 = new GameSnapshotBuilder()
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

    /** Game snapshot S1. */
    public static final GameSnapshot S1 = new GameSnapshotBuilder()
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
        .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
        .withRegister(MoveRegisterFixtures.EMPTY)
        .build();

    /** Initial position, one minute left to both players. */
    public static final GameSnapshot INITIAL = GameSnapshot.initialGameSnapshot(CommonFixtures.ONE_MINUTE_DURATION);

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

    /** Black has no legal moves, black player has to move, one minute left to both players. */
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
