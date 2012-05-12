/*
 *  GamePositionFixtures.java
 *
 *  Copyright (c) 2011, 2012 Roberto Corradini. All rights reserved.
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

import rcrr.reversi.board.Player;
import rcrr.reversi.board.BoardFixtures;

/**
 * The class host a number of predefined game positions.
 * <p>
 * The {@code GamePosition} class defines immutable objects thus {@code GamePositionFixtures}
 * implements game position instances as public static shared objects. Tests can
 * freely share the instances without any modification issue.
 */
public final class GamePositionFixtures {

    /** The null player. */
    private static final Player NULL_PLAYER = null;

    /**
     * A generic instance, black player has to move.
     * <p>
     * The board position is defined by {@code BoardFixtures.AN_INSTANCE}.
     * <p>
     * The player that has to move is: {@code Player.BLACK}.
     *
     * @see BoardFixtures#AN_INSTANCE
     * @see Player#BLACK
     */
    public static final GamePosition AN_INSTANCE = new GamePosition.Builder()
        .withBoard(BoardFixtures.AN_INSTANCE)
        .withPlayer(Player.BLACK)
        .build();

    /**
     * Black player has to move, and has no legal move.
     * <p>
     * The board position is defined by {@code BoardFixtures.BLACK_HAS_TO_PASS}.
     * <p>
     * The player that has to move is: {@code Player.BLACK}.
     *
     * @see BoardFixtures#BLACK_HAS_TO_PASS
     * @see Player#BLACK
     */
    public static final GamePosition BLACK_HAS_TO_PASS = new GamePosition.Builder()
        .withBoard(BoardFixtures.BLACK_HAS_TO_PASS)
        .withPlayer(Player.BLACK)
        .build();

    /**
     * Initial game position.
     * <p>
     * The board position is defined by {@code BoardFixtures.INITIAL}.
     * <p>
     * The player that has to move is: {@code Player.BLACK}.
     *
     * @see BoardFixtures#INITIAL
     * @see Player#BLACK
     */
    public static final GamePosition INITIAL = new GamePosition.Builder()
        .withBoard(BoardFixtures.INITIAL)
        .withPlayer(Player.BLACK)
        .build();

    /**
     * Final game position.
     * <p>
     * The board position is defined by {@code BoardFixtures.FINAL_B37_W27}.
     * <p>
     * The player that has to move is: {@code null}.
     *
     * @see BoardFixtures#FINAL_B37_W27
     */
    public static final GamePosition FINAL_B37_W27_N = new GamePosition.Builder()
        .withBoard(BoardFixtures.FINAL_B37_W27)
        .withPlayer(NULL_PLAYER)
        .build();

    /**
     * Final game position.
     * <p>
     * The board position is defined by {@code BoardFixtures.FINAL_B37_W27}.
     * <p>
     * The player that has to move is: {@code Player.BLACK}.
     *
     * @see BoardFixtures#FINAL_B37_W27
     * @see Player#BLACK
     */
    public static final GamePosition FINAL_B37_W27_B = new GamePosition.Builder()
        .withBoard(BoardFixtures.FINAL_B37_W27)
        .withPlayer(Player.BLACK)
        .build();

    /**
     * Final game position.
     * <p>
     * The board position is defined by {@code BoardFixtures.FINAL_B37_W27}.
     * <p>
     * The player that has to move is: {@code Player.WHITE}.
     *
     * @see BoardFixtures#FINAL_B37_W27
     * @see Player#WHITE
     */
    public static final GamePosition FINAL_B37_W27_W = new GamePosition.Builder()
        .withBoard(BoardFixtures.FINAL_B37_W27)
        .withPlayer(Player.WHITE)
        .build();

    /** Minimax test case A, white player has to move. */
    public static final GamePosition MINIMAX_TEST_CASE_A = new GamePosition.Builder()
        .withBoard(BoardFixtures.MINIMAX_TEST_CASE_A)
        .withPlayer(Player.WHITE)
        .build();

    /** Minimax test case B, white player has to move. */
    public static final GamePosition MINIMAX_TEST_CASE_B = new GamePosition.Builder()
        .withBoard(BoardFixtures.MINIMAX_TEST_CASE_B)
        .withPlayer(Player.WHITE)
        .build();

    /** The null game position. */
    public static final GamePosition NULL = null;

    /** Class constructor. */
    private GamePositionFixtures() { }

}
