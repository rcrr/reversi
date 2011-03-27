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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

public class GameTest {

    /**
     * To be completed.
     */
    @Test
    public void testAreThereAvailableMoves() {
        assertTrue("To be implemented.", false);
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
     * To be completed.
     */
    @Test
    public void testClock() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testCountDiscDifference() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testHasOpponentPassed() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testInitialGame() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testLastGameSnapshot() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testMove() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testPlay() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testPlayer() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testSequence() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testValidateMove() {
        assertTrue("To be implemented.", false);
    }

    /**
     * To be completed.
     */
    @Test
    public void testValueOf() {
        assertTrue("To be implemented.", false);
    }

}
