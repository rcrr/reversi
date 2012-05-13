/*
 *  BoardFactory.java
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
 * A board factory provides methods that return board objects.
 * <p>
 * The factory is stateless.
 * 
 * @see Board
 */
public interface BoardFactory {

    /**
     * A factory that returns a new empty board.
     *
     * @return a new empty board
     */
    public Board emptyBoard();

    /**
     * A factory that returns a board filled
     * by sixtyfour discs having the color set by the {@code player} parameter.
     * <p>
     * Parameter {@code player} cannot be {@code null}.
     *
     * @param player it selects the color of the sixtyfour discs
     * @return       a new board filled by sixtyfour discs
     */
    public Board fillWithColor(final Player player);

    /**
     * A factory that returns a new initial board.
     *
     * @return a new initial board
     */
    public Board initialBoard();

    /**
     * Base factory for a board.
     * <p>
     * {@code squares} must be not null, and must have an entry for every board square.
     * Given that the map cannot have duplicate keys, its size must be equal to the number
     * of class instances defined by the {@code Square} enum.
     *
     * @param  squares the map of squares
     * @return         a new board having as state the given square map
     * @throws NullPointerException     if parameter {@code squares} is null
     * @throws IllegalArgumentException if the {@code squares} is not complete
     */
    public Board valueOf(final Map<Square, SquareState> squares);

}
