/*
 *  BitBoardFactory.java
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
 * The class is an abstract container for board's factories.
 */
public abstract class BitBoardFactory extends AbstractBoardFactory {

    /**
     * The class provides a concrete implementation for the board's factory interface returning
     * board objects of type {@code BitBoard0}.
     *
     * @see Board
     */
    public static final class Type0 extends BitBoardFactory {

        /**
         * {@inheritDoc}
         */
        public Board valueOf(final Map<Square, SquareState> squares) {
            return BitBoard0.valueOf(squares);
        }

    }

    /**
     * The class provides a concrete implementation for the board's factory interface returning
     * board objects of type {@code BitBoard1}.
     *
     * @see Board
     */
    public static final class Type1 extends BitBoardFactory {

        /**
         * {@inheritDoc}
         */
        public Board valueOf(final Map<Square, SquareState> squares) {
            return BitBoard1.valueOf(squares);
        }

    }

    /**
     * The class provides a concrete implementation for the board's factory interface returning
     * board objects of type {@code BitBoard2}.
     *
     * @see Board
     */
    public static final class Type2 extends BitBoardFactory {

        /**
         * {@inheritDoc}
         */
        public Board valueOf(final Map<Square, SquareState> squares) {
            return BitBoard2.valueOf(squares);
        }

    }

    /**
     * The class provides a concrete implementation for the board's factory interface returning
     * board objects of type {@code BitBoard3}.
     *
     * @see Board
     */
    public static final class Type3 extends BitBoardFactory {

        /**
         * {@inheritDoc}
         */
        public Board valueOf(final Map<Square, SquareState> squares) {
            return BitBoard3.valueOf(squares);
        }

    }

}
