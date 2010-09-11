/*
 *  RandomStrategy.java
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

import java.util.Random;
import java.util.List;
import java.util.Arrays;

public class RandomStrategy implements Strategy {

    private Random r;
    
    public RandomStrategy() {
	r = new Random();
    }

    public Square move(GameState gameState) {
	Player player = gameState.player(); 
	Board board = gameState.board();
	List<Square> moves = board.legalMoves(player);
	int index = r.nextInt(moves.size());
	return moves.get(index);
    }

    public static void main(String[] args) {
	Random ra = new Random();
	List<Integer> list = Arrays.asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
	int index = ra.nextInt(list.size());
	int val = list.get(index);
	System.out.println("index: " + index + ", val: " + val);
    }

}
