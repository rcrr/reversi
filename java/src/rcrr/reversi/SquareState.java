/*
 *  SquareState.java
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

import java.util.Collections;
import java.util.Map;
import java.util.HashMap;

/**
 * The {@code SquareState} identifies the state, or as a synonym the "color",
 * of each board square.
 * <p>
 * Each {@code SquareState} has a {@code symbol} assigned to it that is used
 * by the game text (or terminal) user interface.
 */
public enum SquareState {
    /** An empty square. Its {@code symbol} is ".". */
    EMPTY("."),

    /** A black piece. Its {@code symbol} is "@". */
    BLACK("@"),

    /** A white piece. Its {@code symbol} is "O". */
    WHITE("O"),

    /** Marks squares outside the 8x8 board. Its {@code symbol} is "?". */
    OUTER("?");

    /** A static Map to speed up reverse look-up used by
        the {@code valueOfSymbol} method. */
    private static final Map<String, SquareState> SYMBOL_TABLE;

    /** Computes the SYMBOL_TABLE static Map.*/
    static {
        Map<String, SquareState> m = new HashMap<String, SquareState>();
        for (SquareState ss : values()) {
            m.put(ss.symbol(), ss);
        }
        SYMBOL_TABLE = Collections.unmodifiableMap(m);
    }

    /** The color symbol ({@literal e.g.} "@" for BLACK). */
    private final String symbol;

    /**
     * The {@code enum} constructor.
     *
     * @param symbol symbol field
     */
    private SquareState(final String symbol) {
        this.symbol = symbol;
    }

    /**
     * Returns the appropriate {@code SquareState} object given
     * its {@code symbol}. In case of no match returns {@code null}.
     *
     * @param symbol the {@code SquareState}'s {@code symbol}
     * @return       the relative <code>SquareState</code>
     */
    public static SquareState valueOfSymbol(final String symbol) {
        return SYMBOL_TABLE.get(symbol);
    }

    /**
     * Returns the {@code SquareState} printable representation.
     * It returns the {@code symbol} field.
     *
     * @return the {@code SquareState}'s {@code symbol}
     */
    public String symbol() { return symbol; }
}
