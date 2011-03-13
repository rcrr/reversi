/*
 *  MoveRegister.java
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

import java.util.List;
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

    /** The register field. */
    private final List<MoveRecord> register;

    /**
     * Class constructor.
     * <p>
     * Parameter {@code register} must be not null.
     *
     * @param register the register field
     */
    private MoveRegister(final List<MoveRecord> register) {
        assert (register != null) : "Parameter register cannot be null.";
        this.register = Collections.unmodifiableList(register);
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
        return register.get(index);
    }

    /**
     * Returns true if the registry is empty.
     *
     * @return true if the register is empty
     */
    public boolean isEmpty() {
        return register.isEmpty();
    }

    /**
     * Returns the last move record registered.
     *
     * @return the last move record registered
     */
    public MoveRecord last() {
        if (isEmpty()) { return null; }
        return register.get(size() - 1);
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
        List<MoveRecord> tmpRegister = new ArrayList<MoveRecord>(register);
        tmpRegister.add(record);
        return new MoveRegister(tmpRegister);
    }

    /**
     * Returns the register size.
     *
     * @return the register size
     */
    public int size() {
        return register.size();
    }

    /**
     * Returns a string representing the {@code MoveRegister} object.
     *
     * @return a string representing the move register
     */
    @Override
    public String toString() {
        StringBuilder result = new StringBuilder();
        for (MoveRecord record : register) {
            result = result.append(record.toString()).append("\n");
        }
        if (isEmpty()) { result = result.append("[EMPTY MoveRegister]\n"); }
        return result.toString();
    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameter {@code register} cannot be null.
     *
     * @param register the list of move records
     * @return         a new move register
     * @throws NullPointerException when register parameter is null
     */
    public static MoveRegister valueOf(final List<MoveRecord> register) {
        if (register == null) { throw new NullPointerException("Parameter register cannot be null."); }
        if (register.contains(NULL_MOVE_RECORD)) {
            throw new NullPointerException("Parameter register cannot contain null objects.");
        }
        return new MoveRegister(register);
    }

}
