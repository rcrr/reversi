/*
 *  Row.java
 *
 *  Copyright (c) 2010, 2012 Roberto Corradini. All rights reserved.
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

import java.util.ArrayList;
import java.util.List;

/**
 * The {@code Row} enum defines a row of the board game.
 */
public enum Row implements File {
    /**
     * First row.
     */
    R1("1"),
    /**
     * Second row.
     */
    R2("2"),
    /**
     * Third row.
     */
    R3("3"),
    /**
     * Fourth row.
     */
    R4("4"),
    /**
     * Fifth row.
     */
    R5("5"),
    /**
     * Sixth row.
     */
    R6("6"),
    /**
     * Seventh row.
     */
    R7("7"),
    /**
     * Eighth row.
     */
    R8("8");

    /** The null instance. */
    public static final Row NULL = null;

    /** The number of rows. */
    private static final int NUMBER_OF = values().length;

    /** The row label. */
    private final String label;

    /**
     * Enum constructor.
     *
     * @param label the tow's label
     */
    private Row(final String label) {
        this.label = label;
    }

    /**
     * Returns a {@code String} that represents the row's label.
     *
     * @return the row's label
     */
    public String label() { return label; }

    /**
     * Returns the row obtained moving in the direction defined by the {@code dir} parameter.
     * Returns {@code null} if the neighbor leads outside the row boundaries.
     *
     * @param dir the direction to look to
     * @return    the neighbor row identified by the dir
     */
    Row neighbor(final Direction dir) {
        Row r;
        int delta = 0;
        switch (dir.axis()) {
        case HORIZONTAL:
            break;
        case VERTICAL:
        case DIAGONAL_LR:
        case DIAGONAL_RL:
            delta = (dir.versus() == Versus.POSITIVE) ? 1 : -1;
            break;
        default: throw new RuntimeException("Switch case not provided. dir.axis()=" + dir.axis());
        }
        int index = ordinal() + delta;
        if (index < 0 || index >= NUMBER_OF) {
            r = Row.NULL;
        } else {
            r = values()[index];
        }
        return r;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Square> squares() {
        return Square.SQUARE_ASSIGNMENT_TO_ROW_TABLE.get(this);
    }

}
