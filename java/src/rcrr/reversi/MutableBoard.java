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
	// has o check that the List has size 100.
	this.squares = ssl;
    }
    
    List<SquareState> squares() { return this.squares; }

    void set(Integer index, SquareState ss) {
	squares.set(index, ss);
    }

    static MutableBoard emptyBoard() {
	MutableBoard mb = new MutableBoard();
	List<SquareState> s = mb.squares();
	for (int i=0; i<100; i++) {
	    if (Square.ALL_SQUARES.contains(i)) {
		s.add(SquareState.EMPTY);
	    } else {
		s.add(SquareState.OUTER);
	    }
	}
	return mb;
    }

    static MutableBoard initialBoard() {
	MutableBoard mb = emptyBoard();
	mb.setInitialDisks();
	return mb;
    }

    static MutableBoard copyBoard(BoardState bs) {
	MutableBoard mb = new MutableBoard();
	List<SquareState> s = mb.squares();	
	for (int i=0; i<100; i++) {
	    s.add(bs.get(i));
	}
	return mb;
    }

    private void setInitialDisks() {
	squares.set(44, SquareState.WHITE);
	squares.set(45, SquareState.BLACK);
	squares.set(54, SquareState.BLACK);
	squares.set(55, SquareState.WHITE);
    }

    /**
     * Updates board to reflect move by player.
     * <p>
     * This method executes the board update during the game development. The board
     * is mutable and this method relates on side effects to change it. It is a sort
     * of set method to change a board according to the game rules.
     *
     * @param  move   an integer that points to the board square where to put the disk
     * @param  player the disk color to put on the board
     */
    void makeMove(Integer move, Player player) {
	set(move, player.color());
	for (Direction dir : Direction.values()) {
	    makeFlips(move, player, dir);
	}
    }

    private void makeFlips(Integer move, Player player, Direction dir) {
	Integer bracketer = wouldFlip(move, player, dir);
	if (bracketer != null) {
	    for (int c = move + dir.delta(); true; c = c + dir.delta()) {
		if (c == bracketer) break;
		set(c, player.color());
	    }
	}
    }


    
}
