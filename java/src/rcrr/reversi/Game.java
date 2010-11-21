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
 * 
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

    /** The strategies field. Why is it mutable? Why not create a specific wrapper class? */
    private Map<Player, Strategy> strategies;

    /** The game sequence field. It has to be mutable. Synchronization must be developed. Why not name it gameSnapshotSequence? */
    private GameSequence sequence;

    /** The ps field. It is immutable. Why not develop a specific object that handles the IO? */
    private final PrintStream ps;

    /** The state field. Still not clear how to go on with the game-state-machine. */
    private State state;

    /** 
     * The aClock field. The name is ugly.
     * The game-clock design is coupled with the game-state-machine,
     * and need to work into a different Thread compared with the strategies.
     */
    private Clock aClock;

    /**
     * Private constructor.
     * <p>
     * Parameter {@code strategies} must be not null, and should be checked.
     * Parameter {@code sequence} must be not null.
     * Parameter {@code ps} is allowed to be null, meaning that no output is requested for the game.
     *
     * @param  strategies the strategies field
     * @param  sequence   the game snapshot sequence field
     * @param  ps         the print stream field
     */
    private Game(Map<Player, Strategy> strategies, GameSequence sequence, PrintStream ps) {
	assert (strategies != null) : "Parameter strategies cannot be null.";
	assert (sequence != null) : "Parameter sequence cannot be null.";
	this.strategies = strategies;
	this.sequence = sequence;
	this.ps = ps;
    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameter {@code strategies} must be not null, and should be checked.
     * Parameter {@code sequence} must be not null.
     * Parameter {@code ps} is allowed to be null, meaning that no output is requested for the game.
     *
     * @param  strategies the strategies field
     * @param  sequence   the game snapshot sequence field
     * @param  ps         the print stream field
     * @throws NullPointerException when either strategies or sequence parameter is null
     */
    public static Game valueOf(Map<Player, Strategy> strategies, GameSequence sequence, PrintStream ps) {
	if (strategies == null) throw new NullPointerException("Parameter strategies cannot be null.");
	if (sequence == null) throw new NullPointerException("Parameter sequence cannot be null.");
	return new Game(strategies, sequence, ps);
    }

    /**
     *
     * @return a new initial game
     */
    public static Game initialGame(Strategy blStrategy, Strategy whStrategy, Duration gameDuration, PrintStream ps) {
	Map<Player, Strategy> transientStrategies = new EnumMap<Player, Strategy>(Player.class);
	transientStrategies.put(Player.BLACK, blStrategy);
	transientStrategies.put(Player.WHITE, whStrategy);
	return valueOf(transientStrategies, GameSequence.initialGameSequence(gameDuration), ps);
    }

    public int play() {
	while(areThereAvailableMoves()) {
	    if (ps != null) ps.print(sequence().last().printGameSnapshot());
	    move(MoveRegister.empty());
	    if (ps != null) {
		if (hasOpponentPassed()) {
		    ps.print("\n" + player().opponent() + " has no moves and must pass.\n");
		}
	    }
	}
	if (ps != null) ps.print(sequence().last().printGameSnapshot());
	return countDiscDifference();
    }

    public void move(MoveRegister register) {

	long t0 = System.currentTimeMillis();
	Move move = strategies.get(player()).move(sequence.last());
	long t1 = System.currentTimeMillis();
	Clock clock = clock().set(player(), new Duration(t0, t1));

	register = register.push(MoveRecord.valueOf(move, clock));
	if (validateMove(move.square())) {
	    if (ps != null) {
		ps.print("\n" + player().name() + " moves to " + move.square().label() + "\n");
	    }
	    sequence = sequence.add(next(move.square(), clock, register));
	} else {
	    // clock (snapshot ... ) has to be updated.
	    if (ps != null) ps.print("Illegal move: " + move.square() + "\n");
	    move(register);
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

    public GameSnapshot next(Square move, Clock clock, MoveRegister register) {
	Board nextBoard = board().makeMove(move, player());
	Player nextPlayer = nextBoard.nextToPlay(player());
	GameSnapshot gs = GameSnapshot.valueOf(GamePosition.valueOf(nextBoard, nextPlayer), clock, register);
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

    public static enum State {

	/** The game is created but not started. */
	CREATED,

	/** The game is ongoing, no player has the move assigned. The clock is not running. */
	PLAYING,

	/** The game is ongoing. The move request has been assigned to one player. The clock is running. */
	MOVING,

	/** The game is paused. */
	PAUSED;
    }

}