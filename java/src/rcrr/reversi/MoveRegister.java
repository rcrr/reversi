/*
 *  MoveRegister.java
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

import java.util.List;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;

/**
 * An instance of a move register.
 * <p>
 * A {@code MoveRegister} object holds the list of attempted moves by a player.
 * <p>
 * {@code MoveRegister} is immutable.
 */
public final class MoveRegister {

    /**
     * An instance of this class encapsulates the information needed to instantiate
     * and initialize a move register object. That process is triggered when the {@code build()}
     * method is called.
     * <p>
     * The builder has one property, the {@code records} field. It is initialized as follow:
     * <ul>
     *   <li>{@code records = new ArrayList<MoveRecord>()}</li>
     * </ul>
     * <p>
     * The {@code Builder} class is mutable, and it is thread-safe.
     * The object status is guarded by a lock on {@code this}.
     */
    public static final class Builder {

        /** The records field. */
        private List<MoveRecord> records;

        /**
         * Construct a new builder.
         */
        public Builder() {
            this.records = new ArrayList<MoveRecord>();
        }

        /**
         * Returns a new instance of a move record object.
         *
         * @return the move record instance as prepared by the current move record's builder
         */
        public synchronized MoveRegister build() {
            return MoveRegister.valueOf(this.records);
        }

        /**
         * The method returns the records field.
         *
         * @return the records field
         */
        public synchronized List<MoveRecord> getRecords() {
            return this.records;
        }

        /**
         * The method sets the records field.
         *
         * @param records the update value for the records field
         */
        private synchronized void setRecords(final List<MoveRecord> records) {
            this.records = records;
        }

        /**
         * Returns the {@code this} reference after setting the new {@code records} field.
         *
         * @param records the register assigned to the move register
         * @return        the {@code this} reference
         */
        public MoveRegister.Builder withRecords(final List<MoveRecord> records) {
            setRecords(records);
            return this;
        }

        /**
         * Returns the {@code this} reference after setting the new {@code records} field,
         * the {@code records} value is constructed by building the list
         * from the {@code records} array.
         * <p>
         * The {@code records} parameter cannot be null.
         *
         * @param records the array of records assigned to the move records
         * @return        the {@code this} reference
         * @throws NullPointerException when parameter records is null
         */
        public MoveRegister.Builder withRecords(final MoveRecord... records) {
            if (records == null) {
                throw new NullPointerException("Parameter records cannot be null.");
            }
            return withRecords(Arrays.asList(records));
        }

    }

    /** The null move record. */
    private static final MoveRecord NULL_MOVE_RECORD = null;

    /**
     * Returns an empty move register.
     *
     * @return a new empty move register
     */
    public static MoveRegister empty() {
        return new MoveRegister(new ArrayList<MoveRecord>());
    }

    /** The records field. */
    private final List<MoveRecord> records;

    /**
     * Class constructor.
     * <p>
     * Parameter {@code records} must be not null.
     *
     * @param records the register field
     */
    private MoveRegister(final List<MoveRecord> records) {
        assert (records != null) : "Parameter records cannot be null.";
        this.records = Collections.unmodifiableList(records);
    }

    /**
     * Returns the move record element at the specified position in this move register.
     *
     * @param index index of the element to return
     * @return      the specified move record
     * @throws IndexOutOfBoundsException if the index is out of range {@code (index < 0 || index >= size())}
     */
    public MoveRecord get(final int index) {
        if (index < 0 || index >= size()) {
            throw new IndexOutOfBoundsException("Parameter index is out of range."
                                                + "index=" + index
                                                + "; size()=" + size() + ".");
        }
        return records.get(index);
    }

    /**
     * Returns true if the registry is empty.
     *
     * @return true if the register is empty
     */
    public boolean isEmpty() {
        return records.isEmpty();
    }

    /**
     * Returns the last move record registered.
     *
     * @return the last move record registered
     */
    public MoveRecord last() {
        if (isEmpty()) { return null; }
        return records.get(size() - 1);
    }

    /**
     * Returns a new move register having the move record added
     * as the last one.
     * <p>
     * Parameter {@code record} cannot be null.
     *
     * @param record the move record to add as the last one to the register
     * @return       a new move register having the record added
     */
    public MoveRegister push(final MoveRecord record) {
        if (record == null) { throw new NullPointerException("Parameter record cannot be null."); }
        List<MoveRecord> transientRecords = new ArrayList<MoveRecord>(this.records);
        transientRecords.add(record);
        return new MoveRegister(transientRecords);
    }

    /**
     * Returns the register size.
     *
     * @return the register size
     */
    public int size() {
        return this.records.size();
    }

    /**
     * Returns a string representing the {@code MoveRegister} object.
     *
     * @return a string representing the move register
     */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (MoveRecord record : this.records) {
            result = result.append(record.toString()).append("\n");
        }
        if (isEmpty()) { result = result.append("[EMPTY MoveRegister]\n"); }
        return result.toString();
    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameter {@code records} cannot be null.
     *
     * @param records the list of move records
     * @return        a new move register
     * @throws NullPointerException when records parameter is null
     */
    public static MoveRegister valueOf(final List<MoveRecord> records) {
        if (records == null) { throw new NullPointerException("Parameter records cannot be null."); }
        if (records.contains(NULL_MOVE_RECORD)) {
            throw new NullPointerException("Parameter records cannot contain null objects.");
        }
        return new MoveRegister(new ArrayList<MoveRecord>(records));
    }

}
