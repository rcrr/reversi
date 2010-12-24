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
public final class Game {

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
     * Parameter {@code actors} must be not null.
     * Parameter {@code sequence} must be not null.
     * Parameter {@code ps} is allowed to be null, meaning that no output is requested for the game.
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
    public static Game valueOf(final ActorsPair actors, final GameSequence sequence, final PrintStream ps) {
        if (actors == null) { throw new NullPointerException("Parameter actors cannot be null."); }
        if (sequence == null) { throw new NullPointerException("Parameter sequence cannot be null."); }
        return new Game(actors, sequence, ps);
    }

    /**
     * A static factory that returns an initial game.
     *
     * @param  black        the actor playing as black
     * @param  white        the actor playing as black
     * @param  gameDuration the game duration assigned to each player
     * @param  ps           the print stream field
     * @return              a new initial game
     */
    public static Game initialGame(final Actor black,
                                   final Actor white,
                                   final Duration gameDuration,
                                   final PrintStream ps) {
        final Map<Player, Actor> actorMap = new EnumMap<Player, Actor>(Player.class);
        actorMap.put(Player.BLACK, black);
        actorMap.put(Player.WHITE, white);
        return valueOf(ActorsPair.valueOf(actorMap), GameSequence.initialGameSequence(gameDuration), ps);
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
            move(MoveRegister.empty());
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
     * The method execute one move from the moving player.
     * When the move is legal the method set the game sequence and returns.
     * When the move is not legal the method update the move register and
     * recursevely calls itself.
     * <p>
     * The clock is not handled correctly when the player send an
     * illegal move.
     *
     * @param previousRegister the move register taken from the previous attempt
     *                         or an empty one.
     */
    public void move(final MoveRegister previousRegister) {
        if (previousRegister == null) { throw new NullPointerException("Parameter previousRegister cannot be null."); }

        long t0 = System.currentTimeMillis();
        Move move = actors.get(player()).strategy().move(sequence.last());
        long t1 = System.currentTimeMillis();
        final Clock clock = clock().set(player(), new Duration(t0, t1));

        final MoveRegister register = previousRegister.push(MoveRecord.valueOf(move, clock));
        if (validateMove(move.square())) {
            if (ps != null) {
                ps.print("\n" + player().name() + " moves to " + move.square().label() + "\n");
            }
            sequence = sequence.add(next(move.square(), clock, register));
        } else {
            // clock (snapshot ... ) has to be updated.
            if (ps != null) { ps.print("Illegal move: " + move.square() + "\n"); }
            move(register);
        }
        return;
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
     * Returns the disk difference between the two players.
     *
     * @return the disk difference between the two players
     */
    public int countDiscDifference() {
        return lastGameSnapshot().countDiscDifference();
    }

    /**
     * Returns true if the move is legal.
     *
     * @param move the move to be validated
     * @return     if the move is legal
     */
    public boolean validateMove(final Square move) {
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

    /**
     * Returns the sequence field.
     *
     * @return the sequence field
     */
    public GameSequence sequence() {
        return sequence;
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

}
