/*
 *  DiagonalTest.java
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

import static org.junit.matchers.JUnitMatchers.hasItems;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code DiagonalLR} and {@code DiagonalRL} enums.
 */
public class DiagonalTest {

    /** Class constructor. */
    public DiagonalTest() { }

    /**
     * Tests the {@code squares()} method.
     *
     * @see DiagonalLR#squares()
     */
    @Test
    public final void testSquares_LR() {
        assertThat("DiagonalLR.A6_C8.squares() must return squares A6, B7, and C8.",
                   DiagonalLR.A6_C8.squares(),
                   hasItems(Square.A6,
                            Square.B7,
                            Square.C8));
    }

    /**
     * Tests the {@code squares()} method.
     *
     * @see DiagonalRL#squares()
     */
    @Test
    public final void testSquares_RL() {
        assertThat("DiagonalRL.H2_B8.squares() must return squares H2, G3, F4, E5, D6, C7, and B8.",
                   DiagonalRL.H2_B8.squares(),
                   hasItems(Square.H2,
                            Square.G3,
                            Square.F4,
                            Square.E5,
                            Square.D6,
                            Square.C7,
                            Square.B8));
    }

}
