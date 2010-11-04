/*
 *  Game.java
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

import java.util.Map;
import java.util.EnumMap;

import java.io.PrintStream;

import org.joda.time.Duration;

/**
 * {@code Game} is mutable. 
 * <p>
 * Develop the mutable object.
 * Develop the state machine.
 * Develep the Clock managed by the timer.
 * Develop the end of game due to the clock.
 * <p>
 * Documentation has to be completed.
 * Testing has to be completed.
 */
public class Game {

    private Map<Player, Strategy> strategies;

    private GameSequence sequence;

    private final PrintStream ps;

    // not working yet.
    private Clock aClock;

    private Game(Map<Player, Strategy> strategies, GameSequence sequence, PrintStream ps) {
	this.strategies = strategies;
	this.sequence = sequence;
	this.ps = ps;
    }

    public static Game valueOf(Map<Player, Strategy> strategies, GameSequence sequence, PrintStream ps) {
	return new Game(strategies, sequence, ps);
    }

    public static Game initialGame(Strategy blStrategy, Strategy whStrategy, Duration gameDuration, PrintStream ps) {
	Map<Player, Strategy> transientStrategies = new EnumMap<Player, Strategy>(Player.class);
	transientStrategies.put(Player.BLACK, blStrategy);
	transientStrategies.put(Player.WHITE, whStrategy);
	return valueOf(transientStrategies, GameSequence.initialGameSequence(gameDuration), ps);
    }

    public int play() {
	while(areThereAvailableMoves()) {
	    if (ps != null) ps.print(sequence().last().printGameSnapshot());
	    move();
	    if (ps != null) {
		if (hasOpponentPassed()) {
		    ps.print("\n" + player().opponent() + " has no moves and must pass.\n");
		}
	    }

	}
	if (ps != null) ps.print(sequence().last().printGameSnapshot());
	return countDiscDifference();
    }

    public void move() {

	long t0 = System.currentTimeMillis();
	Square move = strategies.get(player()).move(sequence.last());
	long t1 = System.currentTimeMillis();
	Clock clock = clock().set(player(), new Duration(t0, t1));

	if (validateMove(move)) {
	    if (ps != null) {
		ps.print("\n" + player().name() + " moves to " + move.label() + "\n");
	    }
	    sequence = sequence.add(next(move, clock));
	} else {
	    // clock (snapshot ... ) has to be updated.
	    if (ps != null) ps.print("Illegal move: " + move + "\n");
	    move();
	}
	return;
    }

    public boolean areThereAvailableMoves() {
	return sequence.last().hasAnyPlayerAnyLegalMove();
    }

    public int countDiscDifference() {
	return sequence.last().countDiscDifference();
    }

    public boolean validateMove(Square move) {
	return sequence.last().position().isLegal(move);
    }

    public GameSnapshot next(Square move, Clock clock) {
	Board nextBoard = board().makeMove(move, player());
	Player nextPlayer = nextBoard.nextToPlay(player());
	GameSnapshot gs = GameSnapshot.valueOf(GamePosition.valueOf(nextBoard, nextPlayer), clock);
	return gs;
    }

    public GameSequence sequence() {
	return sequence;
    }

    public Player player() {
	return sequence().last().player();
    }

    public Board board() {
	return sequence().last().board();
    }

    public Clock clock() {
	return sequence().last().clock();
    }

    public boolean hasOpponentPassed() {
	boolean result = false;
	if (sequence().size() > 1) {
	    Player previousPlayer = sequence().get(sequence().size() -2).player();
	    if (player() == previousPlayer) result = true; 
	}
	return result;
    }

}