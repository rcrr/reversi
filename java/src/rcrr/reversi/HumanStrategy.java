/*
 *  HumanStrategy.java
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

import java.io.BufferedReader;
import java.io.InputStreamReader;

/**
 * An interacting strategy.
 */
public class HumanStrategy implements Strategy {

    private BufferedReader in;
    
    public HumanStrategy() {
	InputStreamReader isr = new InputStreamReader(System.in);
 	in = new BufferedReader(isr);
    }
    
    public Move move(final GameSnapshot gameSnapshot) {
	Square move = null;
	while (move == null) {
	    List<String> moves = new ArrayList<String>();
	    for (Square mv : gameSnapshot.board().legalMoves(gameSnapshot.player())) {
		moves.add(mv.label());
	    }
	    System.out.print(gameSnapshot.player().toString() + " to move " + moves + ": ");
	    String s = null;
	    try {
		s = in.readLine();
	    } catch (Exception e) {
		System.out.println("Error in reading the input command, exiting.");
		System.exit(1);
	    }
	    try {
		move = Square.getInstance(s);
	    } catch (Exception e) {
		System.out.println(s + " is not a move. Retry:");
	    }
	}
	return Move.valueOf(move);
    }

}
