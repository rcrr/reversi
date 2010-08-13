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

import java.util.Map;
import java.util.HashMap;

public enum SquareState {
    EMPTY("An empty square", "."),
    BLACK("A black piece", "@"),
    WHITE("A white piece", "O"),
    OUTER("Marks squares outside the 8x8 board", "?");

    private String description;
    private String name;
    
    private static final Map<String, SquareState> stringToEnum 
    = new HashMap<String, SquareState>();

    static {
	for (SquareState ss : values())
	    stringToEnum.put(ss.toString(), ss);
    }
    
    public static SquareState fromString(String symbol) {
	return stringToEnum.get(symbol);
    }
    
    SquareState(String description, String name) {
	this.description = description;
	this.name = name;
    }

    @Override
    public String toString() { return name; }
}