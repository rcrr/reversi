/*
 *  WeightedSquares.java
 *
 *  Copyright (c) 2010, 2011 Roberto Corradini. All rights reserved.
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
import java.util.EnumMap;
import java.util.List;
import java.util.Arrays;
import java.util.Collections;

import rcrr.reversi.board.Board;
import rcrr.reversi.board.Player;
import rcrr.reversi.board.Square;
import rcrr.reversi.board.SquareState;

/**
 * An {@code EvalFunction} implementation that weights the value
 * of each square owned by a color applying a static parameter.
 * <p>
 * The parameter table is valued as follow:
 *
 * <pre>
 * {@code
 * .      a     b     c    d    e     f     g      h
 *   =================================================
 * 1 = +120 = -20 = +20 = +5 = +5 = +20 = -20 = +120 =
 *   =================================================
 * 2 =  -20 = -40 =  -5 = -5 = -5 =  -5 = -40 =  -20 =
 *   =================================================
 * 3 =  +20 =  -5 = +15 = +3 = +3 = +15 =  -5 =  +20 =
 *   =================================================
 * 4 =   +5 =  -5 =  +3 = +3 = +3 =  +3 =  -5 =   +5 =
 *   =================================================
 * 5 =   +5 =  -5 =  +3 = +3 = +3 =  +3 =  -5 =   +5 =
 *   =================================================
 * 6 =  +20 =  -5 = +15 = +3 = +3 = +15 =  -5 =  +20 =
 *   =================================================
 * 7 =  -20 = -40 =  -5 = -5 = -5 =  -5 = -40 =  -20 =
 *   =================================================
 * 8 = +120 = -20 = +20 = +5 = +5 = +20 = -20 = +120 =
 *   =================================================
 * }
 * </pre>
 *
 * The evaluation is performed summing all the player's squares
 * multiplied by the appropriete weight, and subtracting the
 * opponent's squares weighted the same way.
 *
 */
public class WeightedSquares implements EvalFunction {

    /** The static WEIGHTS map. */
    private static final Map<Square, Integer> WEIGHTS;

    /**
     * Initialization block:
     * . - sets and initializes the {@code WEIGHTS} map
     */
    static {
        final List<Integer> w = Arrays.asList(120, -20,  20,  5,  5,  20, -20, 120,
                                              -20, -40,  -5, -5, -5,  -5, -40, -20,
                                               20,  -5,  15,  3,  3,  15,  -5,  20,
                                                5,  -5,   3,  3,  3,   3,  -5,   5,
                                                5,  -5,   3,  3,  3,   3,  -5,   5,
                                               20,  -5,  15,  3,  3,  15,  -5,  20,
                                              -20, -40,  -5, -5, -5,  -5, -40, -20,
                                              120, -20,  20,  5,  5,  20, -20, 120);

        final Map<Square, Integer> wm = new EnumMap<Square, Integer>(Square.class);
        for (int idx = 0; idx < Square.values().length; idx++) {
            wm.put(Square.values()[idx], w.get(idx));
        }
        WEIGHTS = Collections.unmodifiableMap(wm);
    }

    /**
     * Returns the weights map.
     *
     * @return the weights map
     */
    public static Map<Square, Integer> weights() {
        return new EnumMap<Square, Integer>(WEIGHTS);
    }

    /** Class constructor. */
    public WeightedSquares() { }

    /**
     * Computes the position evaluation according to the {@code WeightedSquares}
     * implementation of the {@link EvalFunction} interface.
     *
     * @param position the game position to evaluate
     * @return         the board evaluation
     */
    public final int eval(final GamePosition position) {
        if (position == null) { throw new NullPointerException("Parameter position cannot be null."); }
        final Player player = position.player();
        final Board board = position.board();
        final Player opponent = player.opponent();
        int value = 0;
        for (Square sq : Square.values()) {
            int p;
            SquareState color = board.get(sq);
            if (color == player.color()) {
                p = 1;
            } else if (color == opponent.color()) {
                p = -1;
            } else {
                p = 0;
            }
            value += p * WEIGHTS.get(sq);
        }
        return value;
    }

}
