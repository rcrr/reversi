/*
 *  ModifiedWeightedSquares.java
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

import java.util.Map;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

/**
 * An {@code EvalFunction} implementation that weights the value 
 * of each square owned by a color applying a static parameter.
 * <p>
 * The parameter table is taken from the corresponding one defined in
 * the {@link WeightedSquares} class, but is then modified as follow:
 * every time a corner is occupied, its three neighbor square weights
 * are recalculated, assigning a value of +5 to each one.
 * This setting is done with the underlying idea that the penalty
 * associated with the X and C squares has to be removed when
 * the corner is captured by either one of the two players.
 */
public class ModifiedWeightedSquares implements EvalFunction {

    /** The basic unmodified weights received from WeightedSquares. */
    private final static Map<Square, Integer> WEIGHTS = 
	Collections.unmodifiableMap(WeightedSquares.weights());

    /** The reference to the WeightedSquares evaluation function. */
    private final EvalFunction ws;

    /** Public constructor. */
    public ModifiedWeightedSquares() {
	ws = new WeightedSquares();
    }

    /** 
     * Computes the position evaluation according to the {@code ModifiedWeightedSquares}
     * implementation of the {@link EvalFunction} interface.
     */
    public int eval(Player player, Board board) {
	if (player == null) throw new NullPointerException ("Parameter player cannot be null."); 
	if (board == null) throw new NullPointerException ("Parameter board cannot be null."); 
	int value = ws.eval(player, board);
	for (Square corner : Square.corners()) {
	    if (board.get(corner) != SquareState.EMPTY) {
		for (Square c : corner.neighbors().values()) {
		    if (c != null && board.get(c) != SquareState.EMPTY) {
			int j = (board.get(c) == player.color()) ? 1 : -1;
			value += (j * (5 - WEIGHTS.get(c)));
		    }
		}
	    }
	}
	return value;
    }

}
