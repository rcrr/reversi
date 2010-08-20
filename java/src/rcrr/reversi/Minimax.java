/*
 *  Minimax.java
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

public class Minimax {

    Integer move;
    Integer value;

    public Integer getMove() {return move;}
    public void setMove(Integer move) {this.move = move;}
    public Integer getValue() {return value;}
    public void setValue(Integer value) {this.value = value;}

    public Minimax(Integer move, Integer value) {
	setMove(move);
	setValue(value);
    }

    public Minimax minus() {
	return new Minimax(getMove(), - getValue());
    }
    
    public String toString() {
	return new String("[move=" + move + ", value=" + value + "]");
    }
    
    private static String plyLevel(Integer ply) {
	if (ply < 0) return new String("-");
	StringBuffer sb = new StringBuffer(ply);
	for (int i=0; i<ply; i++) {
	    sb.append('.');
	}
	return sb.toString();
    }

    public static Minimax minimax(Player player, BoardState board, Integer ply, EvalFunction ef) {
	Minimax mm = null;
	Player opponent = player.opponent();
	if (ply == 0) {
	    mm = new Minimax(null, ef.eval(player, board));
	} else {
	    List<Integer> moves = board.legalMoves(player);
	    if (moves.isEmpty()) {
		if (board.anyLegalMove(opponent)) {
		    mm = minimax(opponent, board, ply - 1, ef).minus();
		} else {
		    mm = new Minimax(null, board.finalValue(player));
		}
	    } else {
		mm = new Minimax(null, null);
		for (Integer move : moves) {
		    BoardState board2 = board.copyBoard();
		    board2.makeMove(move, player);
		    int val = minimax(opponent, board2, ply - 1, ef).minus().getValue();
		    if (mm.getValue() == null || val > mm.getValue()) {
			mm.setValue(val);
			mm.setMove(move);
		    }
		}
	    }
	}
	return mm;
    }

    public static Strategy minimaxSearcher(final Integer ply, final EvalFunction ef) {
	return new Strategy() {
	    public Integer move(Player player, BoardState board) {
		Minimax mm = minimax(player, board, ply, ef);
		return mm.getMove();
	    }
	};
    }

    public static Minimax alphabeta(Player player, BoardState board, Integer achievable, Integer cutoff, Integer ply, EvalFunction ef) {
	Minimax ab = null;
	Player opponent = player.opponent();
	if (ply == 0) {
	    ab = new Minimax(null, ef.eval(player, board));
	} else {
	    List<Integer> moves = board.legalMoves(player);
	    if (moves.isEmpty()) {
		if (board.anyLegalMove(opponent)) {
		    ab = alphabeta(opponent, board, - cutoff, - achievable, ply - 1, ef).minus();
		} else {
		    ab = new Minimax(null, board.finalValue(player));
		}
	    } else {
		ab = new Minimax(moves.get(0), achievable);
		outer: for (Integer move : moves) {
		    BoardState board2 = board.makeMove(move, player);
		    int val = alphabeta(opponent, board2, - cutoff, - ab.getValue(), ply - 1, ef).minus().getValue();
		    if (val > ab.getValue()) {
			ab.setValue(val);
			ab.setMove(move);
		    }
		    if (ab.getValue() >= cutoff) break outer;
		}
	    }
	}
	return ab;
    }

    public static Strategy alphabetaSearcher(final Integer ply, final EvalFunction ef) {
	return new Strategy() {
	    public Integer move(Player player, BoardState board) {
		Minimax ab = alphabeta(player, board, Reversi.LOSING_VALUE, Reversi.WINNING_VALUE, ply, ef);
		return ab.getMove();
	    }
	};
    }


}
