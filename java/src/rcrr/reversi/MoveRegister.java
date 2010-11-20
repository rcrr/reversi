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
public class MoveRegister {
    
    /** move. */
    private final List<MoveRecord> register;
    
    /**
     * Private constructor.
     * <p>
     * Parameter {@code move} must be not null.
     *
     * @param move the move
     */
    private MoveRegister(List<MoveRecord> register) {
	assert (register != null) : "Parameter register cannot be null.";
	this.register = Collections.unmodifiableList(register);
    }
    
    public static MoveRegister empty() {
	return new MoveRegister(new ArrayList<MoveRecord>());
    }

    public boolean isEmpty() {
	return register.isEmpty();
    }

    public int size() {
	return register.size();
    }
    
    public MoveRecord last() {
	if (isEmpty()) return null;
	return register.get(register.size() - 1);
    }
    
    public MoveRegister push(MoveRecord record) {
	List<MoveRecord> tmpRegister = new ArrayList<MoveRecord>(register);
	tmpRegister.add(record);
	return new MoveRegister(tmpRegister);
    }

}