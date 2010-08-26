/*
 *  Square.java
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
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.EnumMap;
import java.util.Hashtable;

import static rcrr.reversi.Row.*;
import static rcrr.reversi.Column.*;

// to-do list:
// introduce the enum Column and Row
// eliminate position
// eliminate getSquare()
// make the class a "non standard Enum", calculated by a static block.

public enum Square {
    A1(R1, A), B1(R1, B), C1(R1, C), D1(R1, D), E1(R1, E), F1(R1, F), G1(R1, G), H1(R1, H),
    A2(R2, A), B2(R2, B), C2(R2, C), D2(R2, D), E2(R2, E), F2(R2, F), G2(R2, G), H2(R2, H),
    A3(R3, A), B3(R3, B), C3(R3, C), D3(R3, D), E3(R3, E), F3(R3, F), G3(R3, G), H3(R3, H),
    A4(R4, A), B4(R4, B), C4(R4, C), D4(R4, D), E4(R4, E), F4(R4, F), G4(R4, G), H4(R4, H),
    A5(R5, A), B5(R5, B), C5(R5, C), D5(R5, D), E5(R5, E), F5(R5, F), G5(R5, G), H5(R5, H),
    A6(R6, A), B6(R6, B), C6(R6, C), D6(R6, D), E6(R6, E), F6(R6, F), G6(R6, G), H6(R6, H),
    A7(R7, A), B7(R7, B), C7(R7, C), D7(R7, D), E7(R7, E), F7(R7, F), G7(R7, G), H7(R7, H),
    A8(R8, A), B8(R8, B), C8(R8, C), D8(R8, D), E8(R8, E), F8(R8, F), G8(R8, G), H8(R8, H);

    private final Row row;
    private final Column column;

    private Square(Row row, Column column) {
	this.row = row;
	this.column = column;
    }

    private static final Map<Square, String> LABELS = labelTable();

    private static Map<Square, String> labelTable() {
	Map<Square, String> labelMap = new Hashtable<Square, String>();
	for (Square sq : values()) {
	    labelMap.put(sq, sq.column.label() + sq.row.label());
	}
	return Collections.unmodifiableMap(labelMap);
    }

    /** 
     * Returns the Square instance matching the row and column parameters.
     **/
    public static Square instanceOf(Row row, Column column) {
	if (row == null || column == null) return null;
	else return SQUARES.get(Row.size() * row.ordinal() + column.ordinal());
    }

    /** 
     * Returns the Square's Row.
     **/
    public Row row() { return row; }

    /** 
     * Returns the Square's Column.
     **/
    public Column column() { return column; }

    /** 
     * Returns the Square's label.
     **/
    public String label() {
	return LABELS.get(this);
    }

    private static List<Square> SQUARES = squares();

    private static List<Square> squares() {
	List<Square> sl = new ArrayList<Square>(64);
	for (Square sq : values()) sl.add(sq);
	return Collections.unmodifiableList(sl);
    }

    private static int NUMEROSITY = SQUARES.size();

    /** 
     * Returns the number of squares.
     **/
    public static int numerosity() { return NUMEROSITY; }

    /** 
     * Returns the Square instance given its ordinal index.
     **/
    public static Square getInstance(int i) { return SQUARES.get(i); }

    private static final Map<Square, Map<Direction, Square>> NEIGHBOR_TABLE = neighborTable();

    private static Map<Square, Map<Direction, Square>> neighborTable() {
	Map<Square, Map<Direction, Square>> nt = new EnumMap<Square, Map<Direction, Square>>(Square.class);
	for (Square sq : values()) {
	    Map<Direction, Square> snt = new EnumMap<Direction, Square>(Direction.class);
	    for (Direction dir : Direction.values()) {
		Square n = instanceOf(sq.row().shift(dir.deltaRow()), sq.column().shift(dir.deltaColumn()));
		snt.put(dir, n);
	    }
	    nt.put(sq, Collections.unmodifiableMap(snt));
	}
	return Collections.unmodifiableMap(nt);
    }

    /** 
     * Returns a Map that has the Direction has key and the associated neighbor square as value.
     **/
    public Map<Direction, Square> neighbors() {
	return NEIGHBOR_TABLE.get(this);
    }

    public Character getHasegawaLabel() {
	switch (this) {
	case B1:
	case G1:
	case H2:
	case H7:
	case G8:
	case B8:
	case A7:
	case A2:
	    return 'C';
	case C1:
	case F1:
	case H3:
	case H6:
	case C8:
	case F8:
	case A6:
	case A3:
	    return 'A';
	case D1:
	case E1:
	case H4:
	case H5:
	case E8:
	case D8:
	case A5:
	case A4:
	    return 'B';
	case B2:
	case G2:
	case G7:
	case B7:
	    return 'X';
	}
	return null;
    }

    public static final List<Square> corners = Collections.unmodifiableList(Arrays.asList(A1, H1, H8, A8));

    public Boolean isCorner() {
	return corners.contains(this);
    }

    public static Square getSquare(String s) {
	return Square.valueOf(s.toUpperCase());	
    }

}
