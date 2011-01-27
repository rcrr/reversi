/*
 *  AlphaBetaTest.java
 *
 *  Copyright (c) 2010 Roberto Corradini. All rights reserved.
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

import java.util.Arrays;
import java.util.Collection;

import org.junit.*;
import static org.junit.Assert.*;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

@RunWith(Parameterized.class)
public class AlphaBetaTest extends DecisionRuleTestUtils {

    public AlphaBetaTest(GameSnapshot snapshot, Square expectedMove, Integer ply, EvalFunction ef) {
        super(snapshot, expectedMove, ply, ef);
        decisionRule = AlphaBeta.getInstance();
        strategy = decisionRule.searcher(ply, ef);
    }

    @Parameterized.Parameters
    public static Collection data() {
        return Arrays.asList(new Object[][] {

                /** ... */
                { GameSnapshotFixtures.INITIAL, Square.D3, 1, new CountDifference() },
                { GameSnapshotFixtures.INITIAL, Square.D3, 2, new CountDifference() },
                { GameSnapshotFixtures.INITIAL, Square.D3, 3, new CountDifference() },
                { GameSnapshotFixtures.INITIAL, Square.D3, 4, new CountDifference() },

            });
    }

}
