/*
 *  IagoMethodsTest.java
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

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for the {@code Iago} class.
 *
 * @see Iago
 */
public class IagoMethodsTest {

    /** Class constructor. */
    public IagoMethodsTest() { }

    /**
     * Tests the {@code mobility(GamePosition)} method.
     *
     * @see Iago#mobility(GamePosition)
     */
    @Test
    public final void testMobility() {
        assertThat("new Iago().mobility(GamePositionFixtures.INITIAL).current() is 4.",
                   new Iago().mobility(GamePositionFixtures.INITIAL).current(),
                   is(4));
        assertThat("new Iago().mobility(GamePositionFixtures.INITIAL).potential() is 6.",
                   new Iago().mobility(GamePositionFixtures.INITIAL).potential(),
                   is(6));
        assertThat("new Iago().mobility(GamePositionFixtures.BLACK_HAS_TO_PASS).current() is 0.",
                   new Iago().mobility(GamePositionFixtures.BLACK_HAS_TO_PASS).current(),
                   is(0));
        assertThat("new Iago().mobility(GamePositionFixtures.BLACK_HAS_TO_PASS).potential() is 5.",
                   new Iago().mobility(GamePositionFixtures.BLACK_HAS_TO_PASS).potential(),
                   is(5));
    }

    /**
     * Tests the {@code edgeIndex(Player, Board, List)} method.
     *
     * @see Iago#edgeIndex(Player, Board, List)
     */
    @Test
    public final void testEdgeIndex() {
        assertThat("Iago.edgeIndex(Player.BLACK, BoardFixtures.INITIAL, Iago.TOP_EDGE) is 0.",
                   Iago.edgeIndex(Player.BLACK, BoardFixtures.INITIAL, Iago.TOP_EDGE),
                   is(0));
        assertThat("Iago.edgeIndex(Player.WHITE, BoardFixtures.INITIAL, Iago.TOP_EDGE) is 0.",
                   Iago.edgeIndex(Player.WHITE, BoardFixtures.INITIAL, Iago.TOP_EDGE),
                   is(0));

        final Board fullEdge = new Board.Builder().withSquaresLiteral(1, 1, 1, 1, 1, 1, 1, 1,
                                                                      0, 1, 0, 0, 0, 0, 1, 0,
                                                                      0, 0, 0, 0, 0, 0, 0, 0,
                                                                      0, 0, 0, 0, 0, 0, 0, 0,
                                                                      0, 0, 0, 0, 0, 0, 0, 0,
                                                                      0, 0, 0, 0, 0, 0, 0, 0,
                                                                      0, 0, 0, 0, 0, 0, 0, 0,
                                                                      0, 0, 0, 0, 0, 0, 0, 0).build();

        assertThat("When all the squares in the edge are occupied by the PLAYER the value must be 29524.",
                   Iago.edgeIndex(Player.BLACK, fullEdge, Iago.TOP_EDGE),
                   is(29524));
        assertThat("When all the squares in the edge are occupied by the OPPONENT the value must be 59048.",
                   Iago.edgeIndex(Player.WHITE, fullEdge, Iago.TOP_EDGE),
                   is(59048));

        assertThat("Iago.edgeIndex(Player.BLACK, BoardFixtures.BLACK_HAS_TO_PASS, Iago.TOP_EDGE) is 35290.",
                   Iago.edgeIndex(Player.BLACK, BoardFixtures.BLACK_HAS_TO_PASS, Iago.TOP_EDGE),
                   is(35290));
        assertThat("Iago.edgeIndex(Player.WHITE, BoardFixtures.BLACK_HAS_TO_PASS, Iago.TOP_EDGE) is 50816.",
                   Iago.edgeIndex(Player.WHITE, BoardFixtures.BLACK_HAS_TO_PASS, Iago.TOP_EDGE),
                   is(50816));
        assertThat("Iago.edgeIndex(Player.BLACK, BoardFixtures.BLACK_HAS_TO_PASS, Iago.BOTTOM_EDGE) is 39355.",
                   Iago.edgeIndex(Player.BLACK, BoardFixtures.BLACK_HAS_TO_PASS, Iago.BOTTOM_EDGE),
                   is(39355));
        assertThat("Iago.edgeIndex(Player.WHITE, BoardFixtures.BLACK_HAS_TO_PASS, Iago.BOTTOM_EDGE) is 49217.",
                   Iago.edgeIndex(Player.WHITE, BoardFixtures.BLACK_HAS_TO_PASS, Iago.BOTTOM_EDGE),
                   is(49217));
        assertThat("Iago.edgeIndex(Player.BLACK, BoardFixtures.BLACK_HAS_TO_PASS, Iago.LEFT_EDGE) is 34999.",
                   Iago.edgeIndex(Player.BLACK, BoardFixtures.BLACK_HAS_TO_PASS, Iago.LEFT_EDGE),
                   is(34999));
        assertThat("Iago.edgeIndex(Player.WHITE, BoardFixtures.BLACK_HAS_TO_PASS, Iago.LEFT_EDGE) is 50306.",
                   Iago.edgeIndex(Player.WHITE, BoardFixtures.BLACK_HAS_TO_PASS, Iago.LEFT_EDGE),
                   is(50306));
        assertThat("Iago.edgeIndex(Player.BLACK, BoardFixtures.BLACK_HAS_TO_PASS, Iago.RIGHT_EDGE) is 26224.",
                   Iago.edgeIndex(Player.BLACK, BoardFixtures.BLACK_HAS_TO_PASS, Iago.RIGHT_EDGE),
                   is(26224));
        assertThat("Iago.edgeIndex(Player.WHITE, BoardFixtures.BLACK_HAS_TO_PASS, Iago.RIGHT_EDGE) is 42638.",
                   Iago.edgeIndex(Player.WHITE, BoardFixtures.BLACK_HAS_TO_PASS, Iago.RIGHT_EDGE),
                   is(42638));
    }

}
