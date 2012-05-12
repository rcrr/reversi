/*
 *  Move.java
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

import java.util.Map;
import java.util.EnumMap;

import rcrr.reversi.board.Board;
import rcrr.reversi.board.Player;
import rcrr.reversi.board.Square;

/**
 * A move is an action send by a Player/Strategy to the
 * game manager.
 * The move has two fields, the action and the square.
 * <p>
 * Available actions are:
 * <ul>
 *   <li>PUT_DISC</li>
 *   <li>PASS</li>
 *   <li>RESIGN</li>
 * </ul>
 * When the action is PUT_DISC the square must be not null.
 * Otherwise PASS and RESIGN actions require a null value assigned
 * to the square field.
 * <p>
 * {@code Move} is immutable.
 */
public final class Move {

    /**
     * Action is an Enum type that classify the move.
     */
    public static enum Action {

        /** The action of putting a disc on the board. */
        PUT_DISC,

        /** The pass action. */
        PASS,

        /** The resign action. */
        RESIGN;

        /** The null action. */
        static final Action NULL = null;

        /** A generic action. */
        static final Action AN_INSTANCE = PASS;

    }

    /** The null move. */
    static final Move NULL = null;

    /** A generic move instance. */
    static final Move AN_INSTANCE = new Move(Action.AN_INSTANCE, Square.NULL);

    /** A regular move instance. */
    static final Move A_REGULAR_INSTANCE = new Move(Action.PUT_DISC, Square.AN_INSTANCE);

    /** Generic move instance cache. */
    private static final Map<Action, Move> ACTION_INSTANCE_CACHE = new EnumMap<Action, Move>(Action.class);

    /** Move instance cache dedicated to the PUT_DISC action. */
    private static final Map<Square, Move> PUT_DISC_INSTANCE_CACHE = new EnumMap<Square, Move>(Square.class);

    /** The static block populates the two map used to hold all the possible move instances. */
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

    /**
     * This static factory returns the identified move instance taken from the pre computed
     * instance caches.
     * <p>
     * The factory checks that the two fields satisfy the class invariants.
     *
     * @param action the action field
     * @param square the square field
     * @return       the identified move instance
     * @throws NullPointerException     if parameter {@code action} is {@code null},
     *                                  or if parameter {@code square} is {@code null}
     *                                  and the {@code action} parameter is {@code PUT_DISC}
     * @throws IllegalArgumentException if parameter {@code action} is not {@code PUT_DISC}
     *                                  and the {@code square} parameter is different
     *                                  from the {@code null} value
     */
    public static Move valueOf(final Action action, final Square square) {
        if (action == null) { throw new NullPointerException("Parameter action cannot be null."); }
        if (action == Action.PUT_DISC) {
            if (square == null) {
                throw new NullPointerException("Parameter square cannot be null when parameter action is PUT_DISC");
            }
            return PUT_DISC_INSTANCE_CACHE.get(square);
        } else {
            if (square != null) {
                throw new IllegalArgumentException("Parameter square invalid for this factory.");
            }
            return ACTION_INSTANCE_CACHE.get(action);
        }
    }

    /**
     * This static factory returns the identified move instance taken from the pre computed
     * instance caches.
     * <p>
     * The factory checks that the action field is not null, and that it is not equal to PUT_DISC.
     *
     * @param action the action field
     * @return       the identified move instance
     * @throws NullPointerException     if parameter {@code action} is {@code null}
     * @throws IllegalArgumentException if parameter {@code action} is {@code PUT_DISC}
     */
    public static Move valueOf(final Action action) {
        if (action == null) {
            throw new NullPointerException("Parameter action cannot be null.");
        }
        if (action == Action.PUT_DISC) {
            throw new IllegalArgumentException("Parameter action value invalid for this factory.");
        }
        return valueOf(action, null);
    }

    /**
     * This static factory returns the identified move instance taken from the pre computed
     * instance caches.
     * <p>
     * The factory checks that the square field is not null. The action field is set to PUT_DISC.
     *
     * @param square the square field
     * @return       the identified move instance
     * @throws NullPointerException if parameter {@code square} is {@code null}
     */
    public static Move valueOf(final Square square) {
        if (square == null) { throw new NullPointerException("Parameter square cannot be null"); }
        return valueOf(Action.PUT_DISC, square);
    }

    /** The action field. */
    private final Action action;

    /** The square field. */
    private final Square square;

    /**
     * Class constructor.
     * <p>
     * {@code action} must be not {@code null}.
     * {@code square} must be not {@code null} when {@code action} is {@code PUT_DISC}.
     * {@code square} must be {@code null} when {@code action} is not {@code PUT_DISC}
     *
     * @param  action the action field
     * @param  square the square field
     */
    private Move(final Action action, final Square square) {
        assert (!(action == null)) : "Parameter action must be not null";
        assert (!(action == Action.PUT_DISC && square == null))
            : "Parameter square cannot be null when action is PUT_DISC";
        assert (!(action != Action.PUT_DISC && square != null))
            : "Parameter square must be null when action is not PUT_DISC";
        this.action = action;
        this.square = square;
    }

    /**
     * Returns the action field.
     *
     * @return the action field
     */
    public Action action() { return action; }

    /**
     * Returns the square field.
     *
     * @return the square field
     */
    public Square square() { return square; }

    /**
     * Returns a string representing the {@code Move} object.
     *
     * @return a string representing the move
     */
    @Override public String toString() {
        return "[" + action() + "; " + square() + "]";
    }

}
