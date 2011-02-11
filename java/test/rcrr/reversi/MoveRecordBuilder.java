/*
 *  MoveRecordBuilder.java
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
import org.joda.time.Instant;
import org.joda.time.Period;

/**
 * A move record builder is a facility to generate {@code MoveRecord} instances for testing.
 * <p>
 * {@code MoveRecordBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class MoveRecordBuilder {

    /** A one minute duration. */
    private static final Duration ONE_MINUTE_DURATION = Period.minutes(1).toStandardDuration();

    /** The move field. */
    private Move move;

    /** The clock field. */
    private Clock clock;

    /** The timestamp field. */
    private Instant timestamp;;

    /**
     * The class constructor.
     */
    public MoveRecordBuilder() {
        this.move = Move.A_REGULAR_INSTANCE;
        this.clock = ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS;
        this.timestamp = new Instant(System.currentTimeMillis());
    }

    /**
     * Returns a new instance of a move record object.
     *
     * @return the move record instance as prepared by the current move record's builder
     */
    public synchronized MoveRecord build() {
        return MoveRecord.valueOf(move, clock, timestamp);
    }

    /**
     * The method returns the move field.
     *
     * @return the move field
     */
    public synchronized Move getMove() {
        return this.move;
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
     * The method returns the instant field.
     *
     * @return the instant field
     */
    public synchronized Instant getTimestamp() {
        return this.timestamp;
    }

    /**
     * The method sets the move field.
     *
     * @param move the update value for the move field
     */
    private synchronized void setMove(final Move move) {
        this.move = move;
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
     * The method sets the timestamp field.
     *
     * @param timestamp the update value for the timestamp field
     */
    private synchronized void setTimestamp(final Instant timestamp) {
        this.timestamp = timestamp;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code move} field.
     * <p>
     * The {@code move} parameter cannot be null.
     *
     * @param move the move assigned to the move record
     * @return     the {@code this} reference
     */
    public MoveRecordBuilder withMove(final Move move) {
        if (move == null) {
            throw new NullPointerException("Parameter move cannot be null.");
        }
        setMove(move);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code clock} field.
     * <p>
     * The {@code clock} parameter cannot be null.
     *
     * @param clock the clock assigned to the move record
     * @return     the {@code this} reference
     */
    public MoveRecordBuilder withClock(final Clock clock) {
        if (clock == null) {
            throw new NullPointerException("Parameter clock cannot be null.");
        }
        setClock(clock);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code timestamp} field.
     * <p>
     * The {@code timestamp} parameter cannot be null.
     *
     * @param timestamp the timestamp assigned to the move record
     * @return          the {@code this} reference
     */
    public MoveRecordBuilder withTimestamp(final Instant timestamp) {
        if (timestamp == null) {
            throw new NullPointerException("Parameter timestamp cannot be null.");
        }
        setTimestamp(timestamp);
        return this;
    }

}
