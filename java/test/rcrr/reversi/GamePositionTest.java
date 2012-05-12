/*
 *  GamePositionTest.java
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

import rcrr.reversi.board.Player;
import rcrr.reversi.board.Square;
import rcrr.reversi.board.BoardFixtures;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

import static rcrr.reversi.GamePositionFixtures.INITIAL;
import static rcrr.reversi.GamePositionFixtures.AN_INSTANCE;

/**
 * Test Suite for the {@code GamePosition} class.
 *
 * @see GamePosition
 */
public class GamePositionTest {

    /** Class constructor. */
    public GamePositionTest() { }

    /**
     * Tests the board getter method.
     *
     * @see GamePosition#board()
     */
    @Test
    public final void testBoard() {
        assertThat("GamePosition's board for INITIAL is BoardFixtures.INITIAL",
                   INITIAL.board(), is(BoardFixtures.INITIAL));
    }

    /**
     * Tests the hasAnyLegalMove method.
     *
     * @see GamePosition#hasAnyLegalMove()
     */
    @Test
    public final void testHasAnyLegalMove() {
        assertThat("INITIAL must return true, black player has four legal moves.",
                   INITIAL.hasAnyLegalMove(),
                   is(true));
        assertThat("GamePositionFixtures.BLACK_HAS_TO_PASS must return false, black player has no legal moves.",
                   GamePositionFixtures.BLACK_HAS_TO_PASS.hasAnyLegalMove(),
                   is(false));
        assertThat("GamePositionFixtures.FINAL_B37_W27_N must return false, the game is over.",
                   GamePositionFixtures.FINAL_B37_W27_N.hasAnyLegalMove(),
                   is(false));
        assertThat("GamePositionFixtures.FINAL_B37_W27_B must return false, the game is over.",
                   GamePositionFixtures.FINAL_B37_W27_B.hasAnyLegalMove(),
                   is(false));
        assertThat("GamePositionFixtures.FINAL_B37_W27_W must return false, the game is over.",
                   GamePositionFixtures.FINAL_B37_W27_W.hasAnyLegalMove(),
                   is(false));
    }

    /**
     * Tests the hasAnyPlayerAnyLegalMove method.
     *
     * @see GamePosition#hasAnyPlayerAnyLegalMove()
     */
    @Test
    public final void testHasAnyPlayerAnyLegalMove() {
        assertThat("INITIAL must return true, black & white players have legal moves.",
                   INITIAL.hasAnyPlayerAnyLegalMove(),
                   is(true));
        assertThat("GamePositionFixtures.BLACK_HAS_TO_PASS must return true, white player has legal moves.",
                   GamePositionFixtures.BLACK_HAS_TO_PASS.hasAnyPlayerAnyLegalMove(),
                   is(true));
        assertThat("GamePositionFixtures.FINAL_B37_W27_N must return false, the game is over.",
                   GamePositionFixtures.FINAL_B37_W27_N.hasAnyPlayerAnyLegalMove(),
                   is(false));
        assertThat("GamePositionFixtures.FINAL_B37_W27_B must return false, the game is over.",
                   GamePositionFixtures.FINAL_B37_W27_B.hasAnyPlayerAnyLegalMove(),
                   is(false));
        assertThat("GamePositionFixtures.FINAL_B37_W27_W must return false, the game is over.",
                   GamePositionFixtures.FINAL_B37_W27_W.hasAnyPlayerAnyLegalMove(),
                   is(false));
    }

    /**
     * Tests the initialGamePosition factory.
     *
     * @see GamePosition#initialGamePosition()
     */
    @Test
    public final void testInitialGamePosition() {
        GamePosition initialGamePosition = GamePosition.initialGamePosition();

        assertThat("GamePosition.initialGamePosition()"
                   + " must return an instance of GamePosition class.",
                   initialGamePosition,
                   instanceOf(GamePosition.class));

        assertThat("GamePosition.initialGamePosition()"
                   + " must have an INITIAL board.",
                   initialGamePosition.board(),
                   is(BoardFixtures.INITIAL));

        assertThat("GamePosition.initialGamePosition()"
                   + " must have a black player.",
                   initialGamePosition.player(),
                   is(Player.BLACK));

    }

    /**
     * Tests the isLegat method when parameter {@code square} is null.
     *
     * @see GamePosition#isLegal(Square)
     */
    @Test(expected = NullPointerException.class)
    public final void testIsLegal_boundaryConditions_checkNullParameter() {
        AN_INSTANCE.isLegal(Square.NULL);
    }

    /**
     * Tests the isLegal method.
     *
     * @see GamePosition#isLegal(Square)
     */
    @Test
    public final void testIsLegal() {
        assertThat("Initial game has D3 as a legal move.",
                   GamePositionFixtures.INITIAL.isLegal(Square.D3),
                   is(true));
        assertThat("Initial game has E3 as an illegal move.",
                   GamePositionFixtures.INITIAL.isLegal(Square.E3),
                   is(false));
        assertThat("Initial game has D4 as an illegal move.",
                   GamePositionFixtures.INITIAL.isLegal(Square.D4),
                   is(false));

        assertThat("A final game position must return always false.",
                   GamePositionFixtures.FINAL_B37_W27_B.isLegal(Square.AN_INSTANCE),
                   is(false));
        assertThat("A final game position having a null player must return always false.",
                   GamePositionFixtures.FINAL_B37_W27_N.isLegal(Square.AN_INSTANCE),
                   is(false));
    }

    /**
     * Tests the player getter method.
     *
     * @see GamePosition#player()
     */
    @Test
    public final void testPlayer() {
        assertThat("GamePosition's player for INITIAL is Player.BLACK.",
                   INITIAL.player(), is(Player.BLACK));
    }

    /**
     * Tests if the {@code valueOf} method throws a {@code NullPointerException} when
     * the passed {@code board} is null.
     *
     * @see GamePosition#valueOf(Board, Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_boardIsNull() {
        GamePosition.valueOf(BoardFixtures.NULL, Player.AN_INSTANCE);
    }

    /**
     * Tests if the {@code valueOf} method throws a {@code NullPointerException} when
     * the passed {@code player} is null, in case there are legal moves for
     * either player.
     *
     * @see GamePosition#valueOf(Board, Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_playerIsNull_c1() {
        GamePosition.valueOf(BoardFixtures.INITIAL, Player.NULL);
    }

    /**
     * Tests if the {@code valueOf} method doesn't throw a {@code NullPointerException} when
     * the passed {@code player} is null, in case there aren't legal moves for
     * either player.
     * <p>
     * Said it in another way, when the game is over the player is allowed to be null.
     *
     * @see GamePosition#valueOf(Board, Player)
     */
    @Test
    public final void testValueOf_boundaryConditions_playerIsNull_c2() {
        try {
            assertThat("GamePosition.valueOf(BoardFixtures.FINAL_B37_W27, Player.NULL)"
                       + " must return an instance of GamePosition class.",
                       GamePosition.valueOf(BoardFixtures.FINAL_B37_W27, Player.NULL),
                       instanceOf(GamePosition.class));
        } catch (NullPointerException npe) {
            fail("GamePosition.valueOf(BoardFixtures.FINAL_B37_W27, Player.NULL)"
                 + " must not rise a NullPointerException exception.");
        }

    }

    /**
     * Tests if the {@code valueOf} method return an instance of
     * {@code GamePosition} class.
     *
     * @see GamePosition#valueOf(Board, Player)
     */
    @Test
    public final void testValueOf() {
        assertThat("GamePosition.valueOf(BoardFixtures.AN_INSTANCE, Player.AN_INSTANCE)"
                   + " must return an instance of GamePosition class.",
                   GamePosition.valueOf(BoardFixtures.AN_INSTANCE, Player.AN_INSTANCE),
                   instanceOf(GamePosition.class));
    }

}
