/*
 *  Direction.java
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

/**
 * The directions that are available in a regular board's square are
 * eight, Up, Down, Left, Right, and the four diagonal between them.
 * <pre>
 * {@code
 * .   NW    N     NE
 *       \___|____/
 *       |        |
 *     W-| Square |-E
 *       |________|
 *       /   |     \
 *     SW    S     SE
 * }
 * </pre>
 * Each regular {@link Square} has eight neighbor ones,
 * each identified by the proper direction. Boundary squares have fewer neighbors.
 * <p>
 * The {@code Direction enum} is represented by the respective cardinal point literal,
 * for instance the Left is associated with {@code W}.
 */
public enum Direction {
    /**
     * North-West direction.
     */
    NW(-1, -1, "North-West"),
    /**
     * North direction.
     */
    N(-1, 0, "North"),
    /**
     * North-East direction.
     */
    NE(-1, +1, "North-East"),
    /**
     * West direction.
     */
    W(0, -1, "West"),
    /**
     * East direction.
     */
    E(0, +1, "East"),
    /**
     * South-West direction.
     */
    SW(+1, -1, "South-West"),
    /**
     * South direction.
     */
    S(+1, 0, "South"),
    /**
     * South-East direction.
     */
    SE(+1, +1, "South-East");
    
    /** deltaRow field. */
    private final int deltaRow;

    /** deltaColumn field. */
    private final int deltaColumn;

    /** description field. */
    private final String description;
    
    Direction(int deltaRow, int deltaColumn, String description) {
	this.deltaRow = deltaRow;
	this.deltaColumn = deltaColumn;
	this.description = description;
    }

    /**
     * Returns an {@code int} value that quantify the row shift associated with the direction.
     * The return can have three values:
     * <pre>
     * {@code
     * -1 // stands for "go back one row"
     *  0 // stands for "stay on the same row"
     * +1 // stands for "go to the next row"
     * }
     * </pre>
     * The value can then be used as the navigation unit between adiacent columns.
     *
     * @return the delta value to apply as the shift go obtain the target column
     *
     * @see Row
     */
    public int deltaRow() { return deltaRow; }

    /**
     * Returns an {@code int} value that quantify the column shift associated with the direction.
     * The return can have three values:
     * <pre>
     * {@code
     * -1 // stands for "go back one column"
     *  0 // stands for "stay on the same column"
     * +1 // stands for "go to the next column"
     * }
     * </pre>
     * The value can then be used as the navigation unit between adiacent rows.
     *
     * @return the delta value to apply as the shift go obtain the target row
     *
     * @see Column
     */
    public int deltaColumn() { return deltaColumn; }

    /**
     * Returns a {@code String} description for the direction.
     * <p>
     * For instance {@code North} for {@code N}, 
     * or {@code South-West} for {@code SW}. 
     *
     * @return the direction's cardinal point
     */
    public String description() { return description; }

}