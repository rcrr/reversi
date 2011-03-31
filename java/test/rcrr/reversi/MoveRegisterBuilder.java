/*
 *  MoveRegisterBuilder.java
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A move register builder is a facility to generate {@code MoveRegister} instances for testing.
 * <p>
 * {@code MoveRegisterBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class MoveRegisterBuilder {

    /** The records field. */
    private List<MoveRecord> records;

    /**
     * The class constructor.
     */
    public MoveRegisterBuilder() {
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
    public MoveRegisterBuilder withRecords(final List<MoveRecord> records) {
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
    public MoveRegisterBuilder withRecords(final MoveRecord... records) {
        if (records == null) {
            throw new NullPointerException("Parameter records cannot be null.");
        }
        return withRecords(Arrays.asList(records));
    }

}
