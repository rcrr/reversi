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

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

public class ModifiedWeightedSquares implements EvalFunction, Strategy {

    public final static List<Integer> WEIGHTS = WeightedSquares.WEIGHTS;
    public final static List<Integer> CORNERS = Arrays.asList(11, 18, 81, 88);

    private Strategy s;
    private EvalFunction ws;

    public ModifiedWeightedSquares() {
	s = Minimax.maximizer(this);
	ws = new WeightedSquares();
    }

    public Integer eval(Player player, Board board) {
	int w = ws.eval(player, board);
	for (Integer corner : CORNERS) {
	    if (board.get(corner) != SquareState.EMPTY) {
		for (Integer c : Square.neighbors(corner)) {
		    if (board.get(c) != SquareState.EMPTY) {
			int j = (board.get(c) == player.color()) ? 1 : -1;
			w += (j * (5 - WEIGHTS.get(c)));
		    }
		}
	    }
	}
	return w;
    }

    public Integer move(Player player, Board board) {
	return s.move(player, board);
    }

}
