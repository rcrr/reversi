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

package rcrr.reversi.board;

import java.util.Map;

/**
 * This abstract board factory provides implementation methods for all the functions
 * defined by the {@code BoardFactory} interfece with the exception of the
 * {@code valueOf(Map<Square, SquareState>)} method. This is left to the concrete classes as
 * a way to specify he concrete {@code Board} implementation.
 *
 * @see Board
 */
public abstract class AbstractBoardFactory implements BoardFactory {

    /**
     * {@inheritDoc}
     */
    public Board emptyBoard() {
        return valueOf(BoardUtils.emptyBoardSquares());
    }

    /**
     * {@inheritDoc}
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
     * {@inheritDoc}
     */
    public Board initialBoard() {
        final Map<Square, SquareState> sm = BoardUtils.emptyBoardSquares();
        sm.put(Square.D4, SquareState.WHITE);
        sm.put(Square.E4, SquareState.BLACK);
        sm.put(Square.D5, SquareState.BLACK);
        sm.put(Square.E5, SquareState.WHITE);
        return valueOf(sm);
    }

    /**
     * {@inheritDoc}
     */
    public abstract Board valueOf(final Map<Square, SquareState> squares);

}
