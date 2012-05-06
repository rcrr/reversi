/*
 *  Board.java
 *
 *  Copyright (c) 2012 Roberto Corradini. All rights reserved.
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

/**
 * A board is an instance of a board game position. It is the state that a board
 * has regardless of the player that has to move or the time spent or remaining to each player.
 * <p>
 * A {@code Board} object holds the information of the state of each board's square.
 * <p>
 * Two boards are equal when they represent the same position. It is up to the implementation
 * if leverage the immutability property and to cache the existing boards instead of creating new ones.
 * <p>
 * @see Square SquareState Player BoardFactory
 */
public interface Board {

    /**
     * Returns the disk difference between the player and her opponent.
     * <p>
     * Parameter {@code player} must be not {@code null}.
     *
     * @param player the player
     * @return       the disk count difference
     * @throws NullPointerException if parameter {@code player} is {@code null}
     */
    int countDifference(Player player);

    SquareState get(Square square);
    List<Square> legalMoves(Player player);
    Board makeMove(Square move, Player player);
    boolean hasAnyLegalMove(Player player);
    boolean isLegal(Square move, Player player);
    boolean hasAnyPlayerAnyLegalMove();
    int countPieces(SquareState color);
    String printBoard();
    String printBoardWithCount();
    String printCount();
}
