/*
 *  MoveRecord.java
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

import org.joda.time.Instant;
import org.joda.time.Period;
import org.joda.time.Duration;

/**
 * An instance of a move record.
 * <p>
 * A {@code MoveRecord} object holds the information of a proposed move.
 * <p>
 * {@code MoveRecord} is immutable.
 */
public final class MoveRecord {

    /**
     * An instance of this class encapsulates the information needed to instantiate
     * and initialize a move record object. That process is triggered when the {@code build()}
     * method is called.
     * <p>
     * The builder properties and the respectives initializations are:
     * <ul>
     *   <li>{@code move = Move.A_REGULAR_INSTANCE;}</li>
     *   <li>{@code clock = new Clock.Builder().build();}</li>
     *   <li>{@code timestamp = new Instant(System.currentTimeMillis());}</li>
     * </ul>
     * <p>
     * The {@code Builder} class is mutable, and it is thread-safe.
     * The object status is guarded by a lock on {@code this}.
     */
    public static final class Builder {

        /** A one minute duration. */
        private static final Duration ONE_MINUTE_DURATION = Period.minutes(1).toStandardDuration();

        /** The move field. */
        private Move move;

        /** The clock field. */
        private Clock clock;

        /** The timestamp field. */
        private Instant timestamp;;

        /**
         * Construct a new builder.
         */
        public Builder() {
            this.move = Move.A_REGULAR_INSTANCE;
            this.clock = new Clock.Builder().build();
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
        public MoveRecord.Builder withMove(final Move move) {
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
        public MoveRecord.Builder withClock(final Clock clock) {
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
        public MoveRecord.Builder withTimestamp(final Instant timestamp) {
            if (timestamp == null) {
                throw new NullPointerException("Parameter timestamp cannot be null.");
            }
            setTimestamp(timestamp);
            return this;
        }

    }

    /**
     * Static factory that set the timestamp to the current time.
     * <p>
     * Parameter {@code move} cannot be null.
     * Parameter {@code clock} cannnot be null.
     *
     * @param  move  the move send by the player's strategy
     * @param  clock the new clock after the move
     * @return        a new move record
     * @throws NullPointerException when move or clock parameters are null
     */
    public static MoveRecord valueOfAtCurrentTime(final Move move, final Clock clock) {
        if (move == null) { throw new NullPointerException("Parameter move cannot be null."); }
        if (clock == null) { throw new NullPointerException("Parameter clock cannot be null."); }
        return new MoveRecord(move, clock, new Instant(System.currentTimeMillis()));
    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameter {@code move} cannot be null.
     * Parameter {@code clock} cannnot be null.
     *
     * @param  move      the move send by the player's strategy
     * @param  clock     the new clock after the move
     * @param  timestamp the timestamp of the record transaction
     * @return           a new move record
     * @throws NullPointerException when move, clock, or timestamp parameters are null
     */
    public static MoveRecord valueOf(final Move move, final Clock clock, final Instant timestamp) {
        if (move == null) { throw new NullPointerException("Parameter move cannot be null."); }
        if (clock == null) { throw new NullPointerException("Parameter clock cannot be null."); }
        if (timestamp == null) { throw new NullPointerException("Parameter timastamp cannot be null."); }
        return new MoveRecord(move, clock, timestamp);
    }

    /** The move field. */
    private final Move move;

    /** The clock field. */
    private final Clock clock;

    /** The timestamp field. */
    private final Instant timestamp;;

    /**
     * Class constructor.
     * <p>
     * Parameters {@code move}, {@code clock}, and {@code timestamp} must be not null.
     *
     * @param move      the move
     * @param clock     the clock
     * @param timestamp the timestamp
     */
    private MoveRecord(final Move move, final Clock clock, final Instant timestamp) {
        assert (move != null) : "Parameter move cannot be null.";
        assert (clock != null) : "Parameter clock cannot be null.";
        assert (timestamp != null) : "Parameter timestamp cannot be null.";
        this.move = move;
        this.clock = clock;
        this.timestamp = timestamp;
    }

    /**
     * Returns the clock field.
     *
     * @return the clock field
     */
    public Clock clock() { return clock; }

    /**
     * Returns the move field.
     *
     * @return the move field
     */
    public Move move() { return move; }

    /**
     * Returns the record creation instant.
     *
     * @return the object creation instant, as registered by the constructor
     */
    public Instant timestamp() { return timestamp; }

    /**
     * Returns a string representing the {@code MoveRecord} object.
     *
     * @return a string representing the move record
     */
    @Override public String toString() {
        return "[" + move() + "; " + clock() + "; " + timestamp() + "]";
    }

}
