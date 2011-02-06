/*
 *  MaximizeDifference.java
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

/**
 * A basic strategy that chooses the move maximizing the
 * disk count for the player.
 * It doesn't search.
 * When the higher score is shared by more than one move,
 * the move coming first, ordered by the table natural order
 * returned by Square.values(), is selected.
 */
public class MaximizeDifference implements Strategy {

    /** The strategy field. */
    private final Strategy maximizeDifference;

    /** Class constructor. */
    public MaximizeDifference() {
        maximizeDifference = AbstractDecisionRule.maximizer(new CountDifference());
    }

    /**
     * The strategy's move method.
     * <p>
     * Parameter {@code gameSnapshot} must be not null.
     *
     * @param gameSnapshot the game snapshot
     * @return             the strategy's move
     * @throws NullPointerException when the gameSnapshot parameter is null
     */
    public final Move move(final GameSnapshot gameSnapshot) {
        if (gameSnapshot == null) {
            throw new NullPointerException("Parameter gameSnapshot cannot be null.");
        }
        return maximizeDifference.move(gameSnapshot);
    }

}
