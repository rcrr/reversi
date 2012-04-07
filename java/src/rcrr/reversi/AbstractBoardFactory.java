/*
 *  AbstractBoardFactory.java
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

import java.util.Map;

public abstract class AbstractBoardFactory implements BoardFactory {

    /**
     * Returns a new empty board.
     *
     * @return a new empty board
     */
    public Board emptyBoard() {
        return valueOf(BoardUtils.emptyBoardSquares());
    }

    /**
     * A static factory for the class that returns a board filled
     * by sixtyfour discs having the color set by the {@code player} parameter.
     * <p>
     * Parameter {@code player} cannot be {@code null}.
     *
     * @param player it selects the color of the sixtyfour discs
     * @return       a new board filled by sixtyfour discs
     */
    public Board fillWithColor(final Player player) {
        if (player == null) { throw new NullPointerException("Parameter color cannot be null."); }
        final Map<Square, SquareState> sm = BoardUtils.emptyBoardSquares();
        for (Square sq : Square.values()) {
            sm.put(sq, player.color());
        }
        return valueOf(sm);
    }

    /**
     * A static factory for the class that returns a new initial board.
     *
     * @return a new initial board
     */
    public Board initialBoard() {
        final Map<Square, SquareState> sm = BoardUtils.emptyBoardSquares();
        sm.put(Square.D4, SquareState.WHITE);
        sm.put(Square.E4, SquareState.BLACK);
        sm.put(Square.D5, SquareState.BLACK);
        sm.put(Square.E5, SquareState.WHITE);
        return valueOf(sm);
    }

    public abstract Board valueOf(final Map<Square, SquareState> squares);

}
