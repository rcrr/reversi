/*
 *  Column.java
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
import java.util.Map;
import java.util.EnumMap;
import java.util.Collections;

/**
 * The {@code Column} enum defines a column of the board game.
 * <p>
 * The column and row enums share the same structure and method implementation.
 * It is handy to have the enum's value as literals, this justify the duplication in the code.
 * A different approach would be to have an abstract common anchestor, than the row and column
 * abstract classes, and then one class for each enum value, way too mutch for removing a so
 * small dode smell.
 */
public enum Column implements File {
    /**
     * First column.
     */
    A("a"),
    /**
     * Second column.
     */
    B("b"),
    /**
     * Third column.
     */
    C("c"),
    /**
     * Fourth column.
     */
    D("d"),
    /**
     * Fifth column.
     */
    E("e"),
    /**
     * Sixth column.
     */
    F("f"),
    /**
     * Seventh column.
     */
    G("g"),
    /**
     * Eighth column.
     */
    H("h");

    /** The null instance. */
    public static final Column NULL = null;

    /** The number of columns. */
    private static final int NUMBER_OF = values().length;

    /** The column label. */
    private final String label;

    /**
     * Enum constructor.
     *
     * @param label the column label
     */
    private Column(final String label) {
        this.label = label;
    }

    /**
     * Returns a {@code String} that represents the column's label.
     *
     * @return the column's label
     */
    public String label() { return label; }

    /**
     * Returns the column obtained moving in the direction defined by the {@code dir} parameter.
     * Returns {@code null} if the neighbor leads outside the row boundaries.
     *
     * @param dir the direction to look to
     * @return    the neighbor column identified by the dir
     */
    Column neighbor(final Direction dir) {
        Column c;
        int delta = 0;
        switch (dir.axis()) {
        case VERTICAL:
            break;
        case HORIZONTAL:
        case DIAGONAL_LR:
            delta = (dir.versus() == Versus.POSITIVE) ? 1 : -1;
            break;
        case DIAGONAL_RL:
            delta = (dir.versus() == Versus.POSITIVE) ? -1 : 1;
            break;
        default: throw new RuntimeException("Switch case not provided. dir.axis()=" + dir.axis());
        }
        int index = ordinal() + delta;
        if (index < 0 || index >= NUMBER_OF) {
            c = Column.NULL;
        } else {
            c = values()[index];
        }
        return c;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Square> squares() {
        return Square.SQUARE_ASSIGNMENT_TO_COLUMN_TABLE.get(this);
    }

}
