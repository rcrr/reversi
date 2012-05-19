/*
 *  AbstractBoardFactoryTest.java
 *
 *  Copyright (c) 2012 Roberto Corradini. All rights reserved.
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

package rcrr.reversi.board;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for any {@code BoardFactory} class implementation that derives from
 * the {@code AbstractBoardFactory} abstract class.
 */
public class AbstractBoardFactoryTest {

    /** Class constructor. */
    public AbstractBoardFactoryTest() { }

    /**
     * Tests the {@code emptyBoard()} factory.
     * <p>
     * The test runs two types of assertion.
     * The first relates to the {@code equals(Object)} method and checks that
     * the returned board is equal to {@code BoardFixtures.EMPTY}.
     * The second relates on the {@code get(Square)} method and checks that all
     * the squares of the returned board have a {@code SquereState.EMPTY} value.
     *
     * @see AbstractBoardFactory#emptyBoard()
     * @see BoardFixtures#EMPTY
     * @see AbstractBoard#equals(Object)
     * @see AbstractBoard#get(Square)
     */
    @Test
    public final void testEmptyBoard() {
        assertThat("BoardFactoryHolder.getInstance().boardFactory().emptyBoard() must return a board being equal to BoardFixtures.EMPTY.",
                   boardFactory().emptyBoard(),
                   is(BoardFixtures.EMPTY));

        final Board empty = BoardFactoryHolder.getInstance().boardFactory().emptyBoard();
        for (Square square : Square.values()) {
            assertThat("Each square state returned by the get method iterating on"
                       + " the squares of a board returned by EnumMapBoard.emptyBoard()"
                       + " must return a SquareState.EMPTY value.",
                       empty.get(square),
                       is(SquareState.EMPTY));
        }
    }

    /**
     * Tests the {@code fillWithColor(Player)} method when parameter
     * {@code player} is {@code null}.
     *
     * @see AbstractBoardFactory#fillWithColor(Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testFillWithColor_boundaryConditions_checkNullParameter_player() {
        boardFactory().fillWithColor(Player.NULL);
    }

    /**
     * Tests the {@code fillWithColor(Player)} method.
     * <p>
     * The test verifies that for each board square the state is the expected one.
     *
     * @see AbstractBoardFactory#fillWithColor(Player)
     */
    @Test
    public final void testFillWithColor() {
        for (final Player player : Player.values()) {
            final Board filled = boardFactory().fillWithColor(player);
            final SquareState expected = player.color();
            for (final Square square : Square.values()) {
                final SquareState actual = filled.get(square);
                assertEquals("Square:" + actual + " must be equal to " + expected + ".",
                             expected, actual);
            }
        }
    }

    /**
     * Tests the {@code initialBoard()} method.
     * <p>
     * The test runs two kind of assertions:
     * <p>
     * <ul>
     *   <li>{@code BoardFactoryHolder.getInstance().boardFactory().initialBoard()} is equal to {@code BoardFixtures.INITIAL}</li>
     *   <li>For each board square the state is checked against the expected one</li>
     * </ul>
     *
     * @see AbstractBoardFactory#initialBoard()
     * @see BoardFixtures#INITIAL
     */
    @Test
    public final void testInitialBoard() {
        assertEquals("BoardFactoryHolder.getInstance().boardFactory().initialBoard() must be equal to BoardFixtures.INITIAL.",
                     BoardFixtures.INITIAL, boardFactory().initialBoard());
        final Board initial = boardFactory().initialBoard();
        for (Square square : Square.values()) {
            SquareState actual = initial.get(square);
            SquareState expected;
            switch (square) {
            case D4: expected = SquareState.WHITE; break;
            case E4: expected = SquareState.BLACK; break;
            case D5: expected = SquareState.BLACK; break;
            case E5: expected = SquareState.WHITE; break;
            default: expected = SquareState.EMPTY; break;
            }
            assertEquals("Square:" + actual + " must be equal to " + expected + ".",
                         expected, actual);
        }
    }

    /**
     * Returns the jvm wide board factory instance.
     *
     * @return the default board factory
     */
    BoardFactory boardFactory() {
        return BoardFactoryHolder.getInstance().boardFactory();
    }

}
