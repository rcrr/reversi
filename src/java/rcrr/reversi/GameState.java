/*
 *  GameState.java
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

import java.io.PrintStream;

public class GameState {
    private final Clock clock;
    private final BoardState board;
    private final SquareState player;

    private GameState(BoardState board, SquareState player, Clock clock) {
	this.clock = clock;
	this.board = board;
	this.player = player;
    }

    static GameState valueOf(BoardState board, SquareState player, Clock clock) {
	GameState gs = new GameState(board, player, clock);
	return gs;
    }

    Clock getClock() {
	return clock;
    }

    BoardState getBoard() {
	return board;
    }

    SquareState getPlayer() {
	return player;
    }

    GameState getMove(Strategy strategy, PrintStream ps) throws GameOverException {
	return BoardState.getMove(board, strategy, player, ps, clock);
    }

}
