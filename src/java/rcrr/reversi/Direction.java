 /*
    Copyright (c) 2010 Roberto Corradini

    This file is part of the reversi program
    http://github.com/rcrr/reversi

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 3, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
    or visit the site <http://www.gnu.org/licenses/>.
*/

package rcrr.reversi;

public enum Direction {
    NE(-11, "North-West"),
	N(-10, "North"),
	NW(-9, "North-East"),
	W(-1, "West"),
	E(1, "East"),
	SW(9, "South-West"),
	S(10, "South"),
	SE(11, "South-East");

	private Integer delta;
	private String name;

	Direction(Integer delta, String name) {
	    this.delta = delta;
	    this.name = name;
	}

	public Integer delta() { return delta; }
	public String toString() { return name; }
}