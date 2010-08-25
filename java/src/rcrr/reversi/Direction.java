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

// to-do
// change the delta values.
// change getDescription into description
// complete the tests .....
// the delta values should be related to the Row.size() value.

/**
 * The directions that are available in a "standard" board square are
 * eight, Up, Down, Left, Right, and the four diagonal between them.
 * Each square has eight neighbor ones, if not on the border of the board,
 * the direction identify them.
 * <code>Direction</code> is identified by the respective cardinal point,
 * for instance the Left is associated with <code>W</code>. Each direction
 * has also an <code>Integer</code> value that is returned by the 
 * <code>delta</code> method.
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
    
    private final Integer deltaRow;
    private final Integer deltaColumn;
    private final String description;
    
    Direction(int deltaRow, int deltaColumn, String description) {
	this.deltaRow = deltaRow;
	this.deltaColumn = deltaColumn;
	this.description = description;
    }

    public int deltaRow() { return deltaRow; }

    public int deltaColumn() { return deltaColumn; }

    /**
     * Returns an <code>Integer</code> that represents the direction's
     * value to be added to the proper square index in order to obtain 
     * its neighbor square index value.
     *
     * @return the index delta value associated with the <code>Direction</code>
     */
    public Integer delta() { return (deltaRow * Row.size()) + deltaColumn; }

    /**
     * Returns a <code>String</code> value that represents the direction's
     * cardinal point.
     * For instance <code>North</code> for <code>N</code>, 
     * or <code>South-West</code> for <code>SW</code>. 
     *
     * @return the direction's cardinal point
     */
    public String description() { return description; }

}