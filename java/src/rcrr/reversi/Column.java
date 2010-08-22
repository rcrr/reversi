/*
 *  Column.java
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
 * The board column.
 */
public enum Column {
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
    
    private final String label;
    
    Column(String label) {
	this.label = label;
    }

    /**
     * Returns a {@code String} that represents the column's label.
     *
     * @return the column's label
     */
    public String label() { return label; }

}