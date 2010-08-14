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
    NW(-11, "North-West"),
    /**
     * North direction.
     */
    N(-10, "North"),
    /**
     * North-East direction.
     */
    NE(-9, "North-East"),
    /**
     * West direction.
     */
    W(-1, "West"),
    /**
     * East direction.
     */
    E(1, "East"),
    /**
     * South-West direction.
     */
    SW(9, "South-West"),
    /**
     * South direction.
     */
    S(10, "South"),
    /**
     * South-East direction.
     */
    SE(11, "South-East");
    
    private final Integer delta;
    private final String description;
    
    Direction(Integer delta, String description) {
	this.delta = delta;
	this.description = description;
    }

    /**
     * Returns an <code>Integer</code> that represents the direction's
     * value to be added to the proper square index in order to obtain 
     * its neighbor square index value. 
     *
     * @return the index delta value associated with the <code>Direction</code>
     */
    public Integer delta() { return delta; }

    /**
     * Returns a <code>String</code> value that represents the direction's
     * cardinal point.
     * For instance <code>North</code> for <code>N</code>, 
     * or <code>South-West</code> for <code>SW</code>. 
     *
     * @return the direction's cardinal point
     */
    public String getDescription() { return description; }

}