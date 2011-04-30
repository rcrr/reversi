/*
 *  EvalFunctionTestUtils.java
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

import org.junit.Test;

import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Abstract Test for the {@code EvalFunction} interface.
 *
 * @see EvalFunction
 */
public abstract class EvalFunctionTestUtils {

    /** The testFailureMessage field. */
    private final String testFailureMessage;

    /** The gamePosition field. */
    private final GamePosition gamePosition;

    /** The expected value field. */
    private final Integer expectedValue;

    /** The eval function field. */
    private final EvalFunction fn;

    /**
     * Class constructor.
     *
     * @param testFailureMessage the test failure message
     * @param gamePosition       the gamePosition parameter passed to the evaluation function
     * @param fn                 the evaluation function
     * @param expectedValue      the expected value used by the unit test assertion
     */
    public EvalFunctionTestUtils(final String testFailureMessage,
                                 final GamePosition gamePosition,
                                 final EvalFunction fn,
                                 final Integer expectedValue) {
        this.testFailureMessage = testFailureMessage;
        this.gamePosition = gamePosition;
        this.fn = fn;
        this.expectedValue = expectedValue;
    }

    /**
     * Tests the {@code eval(GamePosition)} method.
     *
     * @see EvalFunction#eval(GamePosition)
     */
    @Test
    public final void testEval() {
        assertThat(testFailureMessage,
                   fn.eval(gamePosition),
                   is(expectedValue));
    }

}
