/*
 *  MutableBoard.java
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
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

final class MutableBoard extends AbstractBoard {

    private final List<SquareState> squares;

    public SquareState get(Integer index) {
	return squares.get(index);
    }
    
    private MutableBoard() {
	this.squares = new ArrayList<SquareState>();
    }

    public MutableBoard(List<SquareState> ssl) {
	// has no check that the List has size 100.
	this.squares = ssl;
    }
    
    List<SquareState> squares() { return this.squares; }

    void set(Integer index, SquareState ss) {
	squares.set(index, ss);
    }

}
