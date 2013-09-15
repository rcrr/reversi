/*
 *  EvalEndgame.java
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

import rcrr.reversi.board.Board;
import rcrr.reversi.board.Player;

/**
 * An {@code EvalFunction} implementation that counts the
 * difference between the player's disks and the opponent's ones,
 * assigning empty discs to the winner.
 */
public class EvalEndgame implements EvalFunction {

    /**
     * Class constructor.
     */
    public EvalEndgame() { }

    /**
     * Implements the {@code EvalFunction} contract returning
     * the difference between the player's disks and the opponent's ones,
     * assigning empty squares to the winner.
     *
     * @param position the game position to evaluate
     * @return         the position value
     * @throws NullPointerException if parameter {@code position} is null
     */
    public final int eval(final GamePosition position) {
        assert (position != null) : "Parameter position cannot be null.";

        final Board  b = position.board();
        final Player p = position.player();
        final Player o = p.opponent();

        final int pcount = b.countPieces(p.color());
        final int ocount = b.countPieces(o.color());

        final int difference = pcount - ocount; 
        final int empties = 64 - (pcount + ocount);

        return difference + ((difference > 0) ? +empties : -empties);

        //return position.board().countDifference(position.player());
    }

}
