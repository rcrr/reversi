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

import rcrr.reversi.board.Board;
import rcrr.reversi.board.Player;
import rcrr.reversi.board.Square;

/**
 * A strategy that selects randomly a move among the legal ones.
 */
public class RandomStrategy implements Strategy {

    /** Random field. */
    private final Random random;

    /** Class constructor. */
    public RandomStrategy() {
        random = new Random();
    }

    /**
     * A random move generation. The selection is taken with equal
     * probability among the set of legal moves.
     *
     * @param gameSnapshot the game snapshot
     * @return             the move
     * @throws NullPointerException when the gameSnapshot parameter is null
     */
    public final Move move(final GameSnapshot gameSnapshot) {
        if (gameSnapshot == null) {
            throw new NullPointerException("Parameter gameSnapshot cannot be null.");
        }
        if (!gameSnapshot.hasAnyLegalMove()) {
            return Move.valueOf(Move.Action.PASS);
        } else {
            Player player = gameSnapshot.player();
            Board board = gameSnapshot.board();
            List<Square> moves = board.legalMoves(player);
            int index = random.nextInt(moves.size());
            return Move.valueOf(moves.get(index));
        }
    }

}
