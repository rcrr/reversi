/*
 *  CountDifference.java
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
 * An {@code EvalFunction} implementation that counts the
 * difference between the player's disks and the opponent's ones.
 */
public class CountDifference implements EvalFunction {

    /**
     * Class constructor.
     */
    public CountDifference() { }

    /**
     * Implements the {@code EvalFunction} contract returning
     * the difference between the player'sdisks and the opponent's ones.
     *
     * @param position the game position to evaluate
     * @return the position value
     * @throws NullPointerException if parameter {@code position} is null
     */
    public final int eval(final GamePosition position) {
        if (position == null) { throw new NullPointerException("Parameter position cannot be null."); }
        return position.board().countDifference(position.player());
    }

}
