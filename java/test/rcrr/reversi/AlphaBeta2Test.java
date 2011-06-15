/*
 *  AlphaBeta2Test.java
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

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

/**
 * Test Suite for the {@code AlphaBeta2} class.
 * <p>
 * The test cases given by the data method are the same used by
 * {@code MinimaxTest}.
 */
@RunWith(Parameterized.class)
public class AlphaBeta2Test extends DecisionRuleTestUtils {

    /**
     * Class constructor.
     *
     * @param testFailureMessage the test failure message
     * @param snapshot           the snapshot parameter passed to the move function
     * @param expectedMove       the expected move used by the unit test assertion
     * @param ply                the depth of the search
     * @param fn                 the evaluation function
     */
    public AlphaBeta2Test(final String testFailureMessage,
                          final GameSnapshot snapshot,
                          final Move expectedMove,
                          final Integer ply,
                          final EvalFunction fn) {
        super(testFailureMessage,
              snapshot,
              AlphaBeta2.getInstance(),
              ply,
              fn,
              expectedMove);
    }

}
