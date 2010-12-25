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
     * Returns an empty move register.
     *
     * @return a new empty move register
     */
    public static MoveRegister empty() {
        return new MoveRegister(new ArrayList<MoveRecord>());
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
     * Returns the register size.
     *
     * @return the register size
     */
    public int size() {
        return register.size();
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
     *
     * @param record the move record to add as the last one to the register
     * @return       a new move register having the record added
     */
    public MoveRegister push(final MoveRecord record) {
        List<MoveRecord> tmpRegister = new ArrayList<MoveRecord>(register);
        tmpRegister.add(record);
        return new MoveRegister(tmpRegister);
    }

}
