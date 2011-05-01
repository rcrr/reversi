/*
 *  DecisionRulesBoundaryConditionsTestUtils.java
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

/**
 * Abstract Test Suite for the {@code DecisionRule} interface
 * dedicated to check boundary conditions.
 *
 * @see DecisionRule
 */
public abstract class DecisionRuleBoundaryConditionsTestUtils {

    /** The decision rule field. */
    private final DecisionRule decisionRule;

    /**
     * Class constructor.
     *
     * @param decisionRule the decision rule used by tests
     */
    public DecisionRuleBoundaryConditionsTestUtils(final DecisionRule decisionRule) {
        this.decisionRule = decisionRule;
    }

    /**
     * Tests the {@code searcher(int, EvalFunction)} method when the
     * {@code ply} parameter is illegal.
     *
     * @see DecisionRule#searcher(int, EvalFunction)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testSearcher_boundaryConditions_checkPlyOutOfRange() {
        Strategy s = decisionRule.searcher(0, new CountDifference());
    }

    /**
     * Tests the {@code searcher(int, EvalFunction)} method when the
     * {@code fn} parameter is {@code null}.
     *
     * @see DecisionRule#searcher(int, EvalFunction)
     */
    @Test(expected = NullPointerException.class)
    public final void testSearcher_boundaryConditions_checkNullParameter_fn() {
        Strategy s = decisionRule.searcher(1, null);
    }

}
