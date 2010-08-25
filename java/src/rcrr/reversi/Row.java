/*
 *  Row.java
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

import java.util.List;
import java.util.ArrayList;

/**
 * The board row.
 */
public enum Row {
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
    
    private final String label;
    
    Row(String label) {
	this.label = label;
    }

    /**
     * Returns a {@code String} that represents the row's label.
     *
     * @return the row's label
     */
    public String label() { return label; }

    private static List<Row> rows() {
	List<Row> rows = new ArrayList<Row>();
	for (Row r : Row.values()) {
	    rows.add(r);
	}
	return rows;
    }

    private static List<Row> ROWS = rows();

    private static int SIZE = ROWS.size();

    public static int size() { return SIZE; }

    public static Row getInstance(int index) { return ROWS.get(index); }

    public Row shift(int delta) {
	Row r;
	int index = ROWS.indexOf(this) + delta;
	if (index < 0 || index >= SIZE) r = null;
	else r = ROWS.get(index);
	return r;
    }


}