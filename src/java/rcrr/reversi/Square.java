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

public enum Square {
    A1("a1", 11), B1("b1", 12), C1("c1", 13), D1("d1", 14), E1("e1", 15), F1("f1", 16), G1("g1", 17), H1("h1", 18),
	A2("a2", 21), B2("b2", 22), C2("c2", 23), D2("d2", 24), E2("e2", 25), F2("f2", 26), G2("g2", 27), H2("h2", 28),
	A3("a3", 31), B3("b3", 32), C3("c3", 33), D3("d3", 34), E3("e3", 35), F3("f3", 36), G3("g3", 37), H3("h3", 38),
	A4("a4", 41), B4("b4", 42), C4("c4", 43), D4("d4", 44), E4("e4", 45), F4("f4", 46), G4("g4", 47), H4("h4", 48),
	A5("a5", 51), B5("b5", 52), C5("c5", 53), D5("d5", 54), E5("e5", 55), F5("f5", 56), G5("g5", 57), H5("h5", 58),
	A6("a6", 61), B6("b6", 62), C6("c6", 63), D6("d6", 64), E6("e6", 65), F6("f6", 66), G6("g6", 67), H6("h6", 68),
	A7("a7", 71), B7("b7", 72), C7("c7", 73), D7("d7", 74), E7("e7", 75), F7("f7", 76), G7("g7", 77), H7("h7", 78),
	A8("a8", 81), B8("b8", 82), C8("c8", 83), D8("d8", 84), E8("e8", 85), F8("f8", 86), G8("g8", 87), H8("h8", 88);

    private String displayName;
    private Integer pos;

    private Square(String displayName, Integer pos) {
	this.displayName = displayName;
	this.pos = pos;
    }

    public String getDisplayName() {
	return displayName;
    }

    public Integer getPos() {
	return pos;
    }

    private static List<Integer> allSquares() {
	List<Integer> as = new ArrayList<Integer>();
	for (Square sq : Square.values()) {
	    as.add(sq.getPos());
	}
	return Collections.unmodifiableList(as);
    }

    public static final List<Integer> ALL_SQUARES = allSquares();

    private static List<Square> oneHundredSquares() {
	List<Square> oneh = new ArrayList<Square>();
	for (int i=0; i<100; i++) {
	    Square square = null;
	    for (Square sq : Square.values()) {
		if (sq.getPos() == i) square = sq;
	    }
	    oneh.add(square);
	}
	return Collections.unmodifiableList(oneh);
    }
    
    public static final List<Square> ONE_HUNDRED_SQUARES = oneHundredSquares();
    
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

    public static Square getSquare(Integer i) {
	return ONE_HUNDRED_SQUARES.get(i);	
    }

    public static Square getSquare(String s) {
	return Square.valueOf(s.toUpperCase());	
    }

    public static Integer strToInt(String s) {
	return getSquare(s).getPos();
    }

    public static String intToString(Integer i) {
	return getSquare(i).getDisplayName();
    }


    public static void main(String[] args) {
	System.out.println(A1);
	System.out.println(A1.name());
	System.out.println(A1.getDisplayName());
	System.out.println(A1.getHasegawaLabel());
	System.out.println(A2.getHasegawaLabel());
	System.out.println(C8.getHasegawaLabel());
	System.out.println("isCorner A1: " + A1.isCorner());
	System.out.println("isCorner B1: " + B1.isCorner());

	System.out.println("strToInt c7: " + strToInt("c7"));
	System.out.println("intToString 73: " + intToString(73));
	
    }

}
