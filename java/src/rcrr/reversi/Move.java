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

    /**
     * Action is an Enum type that ...
     */
    public static enum Action {
        PUT_DISC,
        PASS,
        RESIGN;
    }

    private static final Map<Action, Move> ACTION_INSTANCE_CACHE = new EnumMap<Action, Move>(Action.class);
    private static final Map<Square, Move> PUT_DISC_INSTANCE_CACHE = new EnumMap<Square, Move>(Square.class);

    static {
        for (Action action : Action.values()) {
            if (action == Action.PUT_DISC) {
                for (Square square : Square.values()) {
                    PUT_DISC_INSTANCE_CACHE.put(square, new Move(action, square));
                }
            } else {
                ACTION_INSTANCE_CACHE.put(action, new Move(action, null));
            }
        }
    }

    /** The action field. */
    private final Action action;

    /** The square field. */
    private final Square square;

    private static Move valueOf(final Action action, final Square square) {
        if (action == null) { throw new NullPointerException("Parameter action cannot be null."); }
        if (action == Action.PUT_DISC) {
            if (square == null) {
                throw new NullPointerException("Parameter square cannot be null when parameter action is PUT_DISC");
            }
            return PUT_DISC_INSTANCE_CACHE.get(square);
        } else {
            if (square != null) { throw new IllegalArgumentException("Parameter action value invalid for this factory."); }
            return ACTION_INSTANCE_CACHE.get(action);
        }
    }

    public static Move valueOf(final Action action) {
        if (action == null) { throw new NullPointerException("Parameter action cannot be null."); }
        if (action == Action.PUT_DISC) { throw new IllegalArgumentException("Parameter action value invalid for this factory."); }
        return valueOf(action, null);
    }

    public static Move valueOf(final Square square) {
        if (square == null) { throw new NullPointerException("Parameter square cannot be null"); }
        return valueOf(Action.PUT_DISC, square);
    }

    /**
     * Class constructor.
     * <p>
     * {@code action} must be not null.
     * {@code square} must be not null when {@code action} is {@code PUT_DISC}.
     * {@code square} must be null when {@code action} is not {@code PUT_DISC}
     *
     * @param  action the action field
     * @param  square the square field
     */
    private Move(final Action action, final Square square) {
        assert (!(action == null)) : "Parameter action must be not null";
        assert (!(action == Action.PUT_DISC && square == null)) : "Parameter square cannot be null when action is PUT_DISC";
        assert (!(action != Action.PUT_DISC && square != null)) : "Parameter square must be null when action is not PUT_DISC";
        this.action = action;
        this.square = square;
    }

    public Action action() { return action; }

    public Square square() { return square; }

}
