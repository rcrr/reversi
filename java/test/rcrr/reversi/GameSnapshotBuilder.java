/*
 *  GameSnapshotBuilder.java
 *
 *  Copyright (c) 2011 Roberto Corradini. All rights reserved.
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
import org.joda.time.Period;

/**
 * A game snapshot builder is a facility to generate game snapshot instances for testing.
 * <p>
 * {@code GameSnapshotBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class GameSnapshotBuilder {

    private static final Duration ONE_MINUTE_DURATION = Period.minutes(1).toStandardDuration();

    /** The game position field. */
    private GamePosition position;

    /** The clock field. */
    private Clock clock;

    /** The move register. */
    private MoveRegister register;

    /**
     * The class constructor.
     */
    public GameSnapshotBuilder() {
        this.position = GamePosition.initialGamePosition();
        this.clock = Clock.initialClock(ONE_MINUTE_DURATION);
        this.register = MoveRegister.empty();
    }

    /**
     * Returns a new instance of a game snapshot object.
     *
     * @return the game snapshot instance as prepared by the current game snapshot's builder
     */
    public synchronized GameSnapshot build() {
        return GameSnapshot.valueOf(position, clock, register);
    }

    /**
     * The method returns the clock field.
     *
     * @return the clock field
     */
    public synchronized Clock getClock() {
        return this.clock;
    }

    /**
     * The method returns the position field.
     *
     * @return the position field
     */
    public synchronized GamePosition getPosition() {
        return this.position;
    }

    /**
     * The method returns the register field.
     *
     * @return the register field
     */
    public synchronized MoveRegister getRegister() {
        return this.register;
    }

    /**
     * The method sets the clock field.
     *
     * @param clock the update value for the clock field
     */
    private synchronized void setClock(final Clock clock) {
        this.clock = clock;
    }

    /**
     * The method sets the position field.
     *
     * @param position the update value for the position field
     */
    private synchronized void setPosition(final GamePosition position) {
        this.position = position;
    }

    /**
     * The method sets the register field.
     *
     * @param register the update value for the register field
     */
    private synchronized void setRegister(final MoveRegister register) {
        this.register = register;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code clock} field.
     * <p>
     * The {@code clock} parameter cannot be null.
     *
     * @param clock the clock assigned to the game snapshot
     * @return      the {@code this} reference
     */
    public GameSnapshotBuilder withClock(final Clock clock) {
        setClock(clock);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code position} field.
     *
     * @param position the game position assigned to the game snapshot
     * @return         the {@code this} reference
     */
    public GameSnapshotBuilder withPosition(final GamePosition position) {
        setPosition(position);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code register} field.
     *
     * @param register the move register assigned to the game snapshot
     * @return         the {@code this} reference
     */
    public GameSnapshotBuilder withRegister(final MoveRegister register) {
        setRegister(register);
        return this;
    }

}
