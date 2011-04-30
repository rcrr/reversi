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
import static org.junit.Assert.*;

public abstract class EvalFunctionTestUtils {

    /** The board field. */
    private final Board board;

    /** The player field. */
    private final Player player;

    /** The expected value field. */
    private final Integer expectedValue;

    /** The eval function field. */
    private final EvalFunction fn;

    public EvalFunctionTestUtils(final Board board,
                                 final Player player,
                                 final EvalFunction fn,
                                 final Integer expectedValue) {
        this.board = board;
        this.player = player;
        this.fn = fn;
        this.expectedValue = expectedValue;
    }

    @Test
    public final void testEval() {
        assertEquals(expectedValue.intValue(), fn.eval(GamePosition.valueOf(board, player)));
    }

}
