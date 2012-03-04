/*
 *  WeightedSquaresBoundaryConditionsTest.java
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

package rcrr.reversi;

import org.junit.Test;

/**
 * Test Suite for {@code WeightedSquares} class.
 *
 * @see WeightedSquares
 */
public class WeightedSquaresBoundaryConditionsTest {

    private static final GamePosition NULL_GAME_POSITION = null;

    /**
     * Tests the {@code eval(GamePosition)} method when parameter
     * {@code gamePosition} is {@code null}.
     *
     * @see EvalFunction#eval(GamePosition)
     */
    @Test(expected = NullPointerException.class)
    public final void testEval_boundaryConditions_checkNullParameter_gamePosition() {
        new WeightedSquares().eval(NULL_GAME_POSITION);
    }

}
