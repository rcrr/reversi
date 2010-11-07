/*
 *  Move.java
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

import java.util.Map;
import java.util.EnumMap;

/**
 * A move is an action send by a Player/Strategy to the
 * game manager.
 * <p>
 * {@code Move} is immutable.
 */
public final class Move {

    private static final Map<Action, Move> actionInstanceCache = new EnumMap<Action, Move>(Action.class);
    private static final Map<Square, Move> putDiscInstanceCache = new EnumMap<Square, Move>(Square.class);

    /** Initialized by the constructor, cached hashCode. */
    private final int hashCode;

    private final Action action;

    private final Square square;

    private Move(Action action, Square square) {
	assert (!(action == null)) : "Parameter action must be not null";
	assert (!(action == Action.PUT_DISC && square == null)) : "Parameter square cannot be null when action is PUT_DISC";
	assert (!(action != Action.PUT_DISC && square != null)) : "Parameter square must be null when action is not PUT_DISC";

	this.action = action;
	this.square = square;

	int aFirstPrimeNumber = 17;
	int aSecondPrimeNumber = 37;
	int result = aFirstPrimeNumber;
	result = aSecondPrimeNumber * result + action.ordinal();
	if (square != null) result = aSecondPrimeNumber * result + square.ordinal();
	this.hashCode = result;
    }

    static {
	for (Action action : Action.values()) {
	    if (action == Action.PUT_DISC) {
		for (Square square : Square.values()) {
		    putDiscInstanceCache.put(square, new Move(action, square));
		}
	    } else {
		actionInstanceCache.put(action, new Move(action, null));
	    }
	}
    }

    private static Move valueOf(Action action, Square square) {
	if (action == null) throw new NullPointerException("Parameter action cannot be null.");
	if (action == Action.PUT_DISC) {
	    if (square == null) throw new NullPointerException("Parameter square cannot be null when parameter action is PUT_DISC");
	    return putDiscInstanceCache.get(square);
	} else {
	    if (square != null) throw new IllegalArgumentException("Parameter action value invalid for this factory.");
	    return actionInstanceCache.get(action);
	}
	
    }

    public static Move valueOf(Action action) {
	if (action == null) throw new NullPointerException("Parameter action cannot be null.");
	if (action == Action.PUT_DISC) throw new IllegalArgumentException("Parameter action value invalid for this factory.");
	return valueOf(action, null);
    }

    public static Move valueOf(Square square) {
	if (square == null) throw new NullPointerException("Parameter square cannot be null");
	return valueOf(Action.PUT_DISC, square);
    }

    public Action action() { return action; }

    public Square square() { return square; }

    @Override
    public boolean equals(Object object) {
	if (object == this) return true;
	if (!(object instanceof Move)) return false;
	Move move = (Move) object;
	if (action() != move.action()) return false;
	if (square() != move.square()) return false;
	return true;
    }

    /**
     * Returns a hash code for this move.
     *
     * @return a hash code for this move
     */
    @Override
    public int hashCode() {
	return hashCode;
    }

    /**
     * Action is an Enum type that ...
     */
    public static enum Action {
	PUT_DISC(), 
	PASS(),
	RESIGN();
    }

}