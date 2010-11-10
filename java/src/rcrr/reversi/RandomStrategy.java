/*
 *  RandomStrategy.java
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

import java.util.Random;
import java.util.List;
import java.util.Arrays;

/**
 * A strategy that selects randomly a move among the legal ones.
 */
public class RandomStrategy implements Strategy {

    private final Random r;
    
    public RandomStrategy() {
	r = new Random();
    }

    public Move move(final GameSnapshot gameSnapshot) {
	if (!gameSnapshot.hasAnyLegalMove()) {
	    return Move.valueOf(Move.Action.PASS);
	} else {
	    Player player = gameSnapshot.player(); 
	    Board board = gameSnapshot.board();
	    List<Square> moves = board.legalMoves(player);
	    int index = r.nextInt(moves.size());
	    return Move.valueOf(moves.get(index));
	}
    }

}
