/*
 *  Game.java
 *
 *  Copyright (c) 2010, 2011 Roberto Corradini. All rights reserved.
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
public final class Game {

    /** Do we really need it? */
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

    /**
     * A static factory that returns an initial game. The {@code black} and {@code white} parameters
     * define the two actors playing the game. The {@code gameDuration} parameter sets the clock time
     * assigned equally to players.
     * The {@code ps} paratemer receives the print stream where the game logging text output is directed.
     * An initial game sequence (see {@link GameSequence#initialGameSequence(Duration)})
     * is generated and assigned to the game.
     * <p>
     * Parameter {@code black} must be not {@code null}.
     * Parameter {@code white} must be not {@code null}.
     * Parameter {@code gameDuration} must be not {@code null}.
     * Parameter {@code ps} is allowed to be {@code null}, meaning that no output is requested for the game.
     *
     * @param  black        the actor playing as black
     * @param  white        the actor playing as black
     * @param  gameDuration the game duration assigned to each player
     * @param  ps           the print stream field
     * @return              a new initial game
     * @throws NullPointerException when either white, or black, or gameDuration parameter is null
     */
    public static Game initialGame(final Actor black,
                                   final Actor white,
                                   final Duration gameDuration,
                                   final PrintStream ps) {
        if (black == null) { throw new NullPointerException("Parameter black cannot be null."); }
        if (white == null) { throw new NullPointerException("Parameter white cannot be null."); }
        if (gameDuration == null) { throw new NullPointerException("Parameter gameDuration cannot be null."); }
        final Map<Player, Actor> actorMap = new EnumMap<Player, Actor>(Player.class);
        actorMap.put(Player.BLACK, black);
        actorMap.put(Player.WHITE, white);
        return newInstance(ActorsPair.valueOf(actorMap), GameSequence.initialGameSequence(gameDuration), ps);
    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameter {@code actors} must be not null.
     * Parameter {@code sequence} must be not null.
     * Parameter {@code ps} is allowed to be null, meaning that no output is requested for the game.
     *
     * @param  actors   the actors field
     * @param  sequence the game snapshot sequence field
     * @param  ps       the print stream field
     * @return          a new game instance
     * @throws NullPointerException when either actors or sequence parameter is null
     */
    public static Game newInstance(final ActorsPair actors, final GameSequence sequence, final PrintStream ps) {
        if (actors == null) { throw new NullPointerException("Parameter actors cannot be null."); }
        if (sequence == null) { throw new NullPointerException("Parameter sequence cannot be null."); }
        return new Game(actors, sequence, ps);
    }

    /** The actors field. */
    private final ActorsPair actors;

    /**
     * The game sequence field. It has to be mutable.
     * Synchronization must be developed.
     * Why not name it gameSnapshotSequence?
     */
    private GameSequence sequence;

    /**
     * The ps field. It is immutable.
     * Why not develop a specific object that handles the IO?
     */
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
     * Class constructor.
     * <p>
     * Parameter {@code actors} must be not {@code null}.
     * Parameter {@code sequence} must be not {@code null}.
     * Parameter {@code ps} is allowed to be {@code null}, meaning that no output is requested for the game.
     *
     * @param  actors   the actors field
     * @param  sequence the game snapshot sequence field
     * @param  ps       the print stream field
     */
    private Game(final ActorsPair actors, final GameSequence sequence, final PrintStream ps) {
        assert (actors != null) : "Parameter actors cannot be null.";
        assert (sequence != null) : "Parameter sequence cannot be null.";
        this.actors = actors;
        this.sequence = sequence;
        this.ps = ps;
    }

    /**
     * Returns true if there are available moves to either player.
     *
     * @return true if there are available moves to either player
     */
    public boolean areThereAvailableMoves() {
        return lastGameSnapshot().hasAnyPlayerAnyLegalMove();
    }

    /**
     * Returns the board of the last game snapshot.
     *
     * @return the last game snapshot board
     */
    public Board board() {
        return lastGameSnapshot().board();
    }

    /**
     * Returns the clock of the last game snapshot.
     *
     * @return the last game snapshot clock
     */
    public Clock clock() {
        return lastGameSnapshot().clock();
    }

    /**
     * Returns the disk difference between the two players.
     *
     * @return the disk difference between the two players
     */
    public int countDiscDifference() {
        return lastGameSnapshot().countDiscDifference();
    }

    /**
     * Returns true when the opponent passed the previous move.
     *
     * @return if the opponent previous move was a pass
     */
    public boolean hasOpponentPassed() {
        boolean result = false;
        if (sequence().size() > 1) {
            Player previousPlayer = sequence().get(sequence().size() - 2).player();
            if (player() == previousPlayer) {
                result = true;
            }
        }
        return result;
    }

    /**
     * Returns the last game snapshot of the game sequenze.
     *
     * @return the last game snapshot of the game sequence
     */
    public GameSnapshot lastGameSnapshot() {
        return sequence.last();
    }

    /**
     * The method execute one move requesting it to the moving player.
     * <p>
     * When the move is legal the method set the game sequence and returns.
     * When the move is not legal the method update the move register and
     * recursevely calls itself.
     * <p>
     * The method alter the game instance recording the new sequence, being
     * created adding to the actual sequence the new game snapshot obtained by
     * executing the move to the actual board and registering the new clock
     * and the move register.
     */
    public void move() {
        move(MoveRegister.empty());
    }

    /**
     * The method implements the {@code move()} method, passind as parameter
     * the move register so far collected.
     * <p>
     * The clock is not handled correctly when the player send an
     * illegal move.
     *
     * @param previousRegister the move register taken from the previous attempt
     *                         or an empty one.
     */
    private void move(final MoveRegister previousRegister) {
        assert (previousRegister != null) : "Parameter previousRegister cannot be null.";

        /** These lines of code should become a new method into MoveRegister class. */
        Clock actualClock;
        if (previousRegister.isEmpty()) {
            actualClock = clock();
        } else {
            actualClock = previousRegister.last().clock();
        }

        long t0 = System.currentTimeMillis();
        Move move = actors.get(player()).strategy().move(sequence.last());
        long t1 = System.currentTimeMillis();
        final Clock updatedClock = actualClock.decrement(player(), new Duration(t0, t1));

        final MoveRegister register = previousRegister.push(MoveRecord.valueOfAtCurrentTime(move, updatedClock));
        if (validateMove(move.square())) {
            if (ps != null) {
                ps.print("\n" + player().name() + " moves to " + move.square().label() + "\n");
            }
            sequence = sequence.add(next(move.square(), updatedClock, register));
        } else {
            if (ps != null) { ps.print("Illegal move: " + move.square().label() + "\n"); }
            move(register);
        }
        return;
    }

    /**
     * The play method executes the game sequence of moves obtained
     * by the two players.
     * <p>
     * It stops when no moves are left.
     * It returns the disk difference between the two players.
     *
     * @return the disk difference between players
     */
    public int play() {
        while (areThereAvailableMoves()) {
            if (ps != null) { ps.print(lastGameSnapshot().printGameSnapshot()); }
            move();
            if (ps != null) {
                if (hasOpponentPassed()) {
                    ps.print("\n" + player().opponent() + " has no moves and must pass.\n");
                }
            }
        }
        if (ps != null) { ps.print(lastGameSnapshot().printGameSnapshot()); }
        return countDiscDifference();
    }

    /**
     * Returns the player of the last game snapshot.
     *
     * @return the last game snapshot player
     */
    public Player player() {
        return lastGameSnapshot().player();
    }

    /**
     * Returns the sequence field.
     *
     * @return the sequence field
     */
    public GameSequence sequence() {
        return sequence;
    }

    /**
     * Returns true if the move is legal.
     * <p>
     * Parameter {@code move} must be not {@code null}.
     *
     * @param move the move to be validated
     * @return     if the move is legal
     * @throws NullPointerException when move parameter is null
     */
    public boolean validateMove(final Square move) {
        if (move == null) { throw new NullPointerException("Parameter move cannot be null."); }
        return lastGameSnapshot().position().isLegal(move);
    }

    /**
     * Returns the next new game snapshot obtained by applying the move
     * to the current game status.
     * <p>
     * The move parameter should be removed. register should contain the info.
     *
     * @param move     the move to apply
     * @param clock    the new clock
     * @param register the move sequence issued by the player
     * @return         a new game snapshot
     */
    private GameSnapshot next(final Square move, final Clock clock, final MoveRegister register) {
        Board nextBoard = board().makeMove(move, player());
        Player nextPlayer = nextBoard.nextToPlay(player());
        GameSnapshot gs = GameSnapshot.valueOf(GamePosition.valueOf(nextBoard, nextPlayer), clock, register);
        return gs;
    }

}
