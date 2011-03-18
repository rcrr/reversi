/*
 *  GameSnapshot.java
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

import org.joda.time.Duration;

/**
 * An instance of a game snapshot.
 * <p>
 * A {@code GameSnapshot} object holds the information of the state of the game.
 * It is a value object composed by three fields:
 * <ol>
 *   <li>The game position, returned by the {@code position()} method.</li>
 *   <li>The current clock, returned by the {@code clock()} method.</li>
 *   <li>The move log, returned by the {@code position()} method.</li>
 * </ol>
 * <p>
 * {@code GameSnapshot} is immutable.
 */
public final class GameSnapshot {

    /** ... for printing ... */
    private static final int BOARD_ROWS = 8;

    /**
     * Static factory that returns a new initial game snapshot.
     * <p>
     * The returned game snapshot has the board set with the four central
     * disk, the black player has to move, and each player has the assigned
     * time duration in her own clock.
     * <p>
     * Parameter {@code gameDuration} must be not null.
     *
     * @param gameDuration the time duration assigned to each player
     * @return             a new initial game snapshot as required by international game's rules
     * @throws NullPointerException when {@code gameDuration} is null
     */
    public static GameSnapshot initialGameSnapshot(final Duration gameDuration) {
        if (gameDuration == null) { throw new NullPointerException("Parameter gameDuration cannot be null."); }
        return valueOf(GamePosition.initialGamePosition(), Clock.initialClock(gameDuration), MoveRegister.empty());
    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameters {@code position}, {@code register}, and {@code clock} must be not null.
     *
     * @param  position  the game position
     * @param  clock     the current clock
     * @param  register  the log of moves transmitted by the player
     * @return           a new game snapshot
     * @throws NullPointerException when {@code position}, {@code register}, or {@code clock} parameters are null
     */
    public static GameSnapshot valueOf(final GamePosition position, final Clock clock, final MoveRegister register) {
        if (position == null) { throw new NullPointerException("Parameter position cannot be null."); }
        if (clock == null) { throw new NullPointerException("Parameter clock cannot be null."); }
        if (register == null) { throw new NullPointerException("Parameter register cannot be null."); }
        return new GameSnapshot(position, clock, register);
    }

    /** The game position field. */
    private final GamePosition position;

    /** The clock field. */
    private final Clock clock;

    /** The move register. */
    private final MoveRegister register;

    /**
     * Class constructor.
     * <p>
     * Parameters {@code position} and {@code clock} must be not null.
     *
     * @param position the game position
     * @param clock    the current clock
     * @param register the register of moves transmitted by the player
     */
    private GameSnapshot(final GamePosition position, final Clock clock, final MoveRegister register) {
        assert (position != null) : "Parameter position cannot be null.";
        assert (clock != null) : "Parameter clock cannot be null.";
        assert (register != null) : "Parameter register cannot be null.";
        this.position = position;
        this.clock = clock;
        this.register = register;
    }

    /**
     * Returns the board field as taken from position.
     *
     * @return the game state board
     */
    public Board board() {
        return position().board();
    }

    /**
     * Returns the clock field.
     *
     * @return the game state clock
     */
    public Clock clock() {
        return clock;
    }

    /**
     * Returns the difference of disc count. It counts the
     * black's discs and subtract the white's ones.
     *
     * @return the disc difference
     */
    public int countDiscDifference() {
        return board().countDifference(Player.BLACK);
    }

    /**
     * Returns if the game snapshot admit one or more legal moves.
     *
     * @return {@code true} if the player has at last one legal move
     */
    public boolean hasAnyLegalMove() {
        return position.hasAnyLegalMove();
    }

    /**
     * Returns if either one of the two players has any legal move.
     *
     * @return {@code true} if anyone can play a move.
     */
    public boolean hasAnyPlayerAnyLegalMove() {
        return position.hasAnyPlayerAnyLegalMove();
    }

    /**
     * Returns the player field as taken from position.
     *
     * @return the game player
     */
    public Player player() {
        return position().player();
    }

    /**
     * Returns the position field.
     *
     * @return the game position
     */
    public GamePosition position() {
        return position;
    }

    /**
     * Returns a formatted string showing a 2d graphical composed view
     * of the game state. It shows the board, the disk count, and the clock.
     * <p>
     * The method joins the printBoardWithCount() and the printClock() output,
     * setting the second on the right of the last board's row.
     *
     * @return a string being a 2d representation of the game
     */
    public String printGameSnapshot() {
        StringBuilder sbGameSnapshot = new StringBuilder();
        sbGameSnapshot.append(register().toString());
        String sBoard = board().printBoardWithCount();
        String sClock = clock().printClock();
        String[] lines = sBoard.split("\n");
        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];
            sbGameSnapshot.append(line);
            if (i == BOARD_ROWS) { sbGameSnapshot.append(sClock); }
            sbGameSnapshot.append("\n");
        }
        final Player p = player();
        if (p != null) {
            sbGameSnapshot.append(" Next to play: " + p + ", legal moves: " + board().legalMoves(p) + "\n");
        } else {
            sbGameSnapshot.append(" No player has any legal move. The game is over.");
        }
        return (sbGameSnapshot.toString());
    }

    /**
     * Returns the register field.
     *
     * @return the move register
     */
    public MoveRegister register() {
        return register;
    }

}
