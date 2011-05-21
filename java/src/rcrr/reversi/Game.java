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
     * This is the maximum number of retries that a strategy is allowed
     * to send to the game before it is stopped.
     * <p>
     * When stopped the move is set to {@code Move.Action.RESIGN}.
     */
    public static final int MAX_ALLOWED_NUMBER_OF_ILLEGAL_MOVES = 7;

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
        return new Game(actors, sequence, (ps == null) ? new NullPrintStream() : ps);
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
     *
     * @return the move
     */
    public Move move() {
        return move(MoveRegister.empty());
    }

    /**
     * Returns the actual clock of the game during the move procedure execution.
     *
     * @param register the current move register during the move execution
     * @return         the actual clock
     */
    private Clock actualClock(final MoveRegister register) {
        assert (register != null) : "Parameter register cannot be null.";
        return (register.isEmpty()) ? clock() : register.last().clock();
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
     * @return                 the move
     */
    private Move move(final MoveRegister previousRegister) {
        assert (previousRegister != null) : "Parameter previousRegister cannot be null.";

        final long t0 = System.currentTimeMillis();
        Move move = actors.get(player()).strategy().move(sequence.last());
        final long t1 = System.currentTimeMillis();
        final Clock updatedClock = actualClock(previousRegister).decrement(player(), new Duration(t0, t1));
        MoveRegister register = previousRegister.push(MoveRecord.valueOfAtCurrentTime(move, updatedClock));
        if (updatedClock.get(player()).isEqual(Duration.ZERO)) {
                move = Move.valueOf(Move.Action.RESIGN);
                register = register.push(MoveRecord.valueOfAtCurrentTime(move, updatedClock));
                ps.print("\n" + player().name() + " RESIGN. Time is ended. " + "\n");
                sequence = sequence.add(next(move, updatedClock, register));
                return move;
        }

        if (validateMove(move)) {
            ps.print("\n" + player().name() + " moves to " + move + "\n");
            sequence = sequence.add(next(move, updatedClock, register));
        } else {
            ps.print("Illegal move: " + move + "\n");
            if (register.size() < MAX_ALLOWED_NUMBER_OF_ILLEGAL_MOVES) {
                move = move(register);
            } else {
                move = Move.valueOf(Move.Action.RESIGN);
                register = register.push(MoveRecord.valueOfAtCurrentTime(move, updatedClock));
                ps.print("\n" + player().name() + " RESIGN. Too many illegal moves. " + "\n");
                sequence = sequence.add(next(move, updatedClock, register));
            }
        }
        return move;
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
            ps.print(lastGameSnapshot().printGameSnapshot());
            move();
            if (hasOpponentPassed()) {
                ps.print("\n" + player().opponent() + " has no moves and must pass.\n");
            }
        }
        ps.print(lastGameSnapshot().printGameSnapshot());
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
     * Returns a formatted string showing a 2d graphical composed view
     * of the game state. It shows the board, the disk count, and the clock.
     *
     * @return a string representation of the game
     */
    public String print() {
        final StringBuilder sbGame = new StringBuilder();
        for (int i = 0; i < sequence().size(); i++) {
            GameSnapshot snapshot = sequence().get(i);
            sbGame.append(snapshot.printGameSnapshot());
            sbGame.append("\n");
        }
        return (sbGame.toString());
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
     * @throws NullPointerException     when move parameter is null
     * @throws IllegalArgumentException when the move action is not handled
     */
    public boolean validateMove(final Move move) {
        if (move == null) { throw new NullPointerException("Parameter move cannot be null."); }
        switch (move.action()) {
        case PUT_DISC:
            return lastGameSnapshot().position().isLegal(move.square());
        case PASS:
            return !lastGameSnapshot().position().hasAnyLegalMove();
        case RESIGN:
            return true;
        default:
            throw new IllegalArgumentException("Unsupported action type. move.action()=" + move.action());
        }
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
    private GameSnapshot next(final Move move, final Clock clock, final MoveRegister register) {
        assert (move != null) : "Parameter move cannot be null.";
        assert (clock != null) : "Parameter clock cannot be null.";
        assert (register != null) : "Parameter clock register be null.";
        final Board nextBoard;
        final Player nextPlayer;
        switch (move.action()) {
        case PUT_DISC:
            nextBoard = board().makeMove(move.square(), player());
            nextPlayer = player().opponent();
            //nextPlayer = nextBoard.nextToPlay(player());
            break;
        case PASS:
            nextBoard = board();
            nextPlayer = player().opponent();
            break;
        case RESIGN:
            nextBoard = Board.fillWithColor(player().opponent());
            nextPlayer = null;
            break;
        default:
            throw new IllegalArgumentException("Unsupported action type. move.action()=" + move.action());
        }
        return GameSnapshot.valueOf(GamePosition.valueOf(nextBoard, nextPlayer), clock, register);
    }

}
