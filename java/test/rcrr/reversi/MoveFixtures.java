/*
 *  MoveFixtures.java
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

/**
 * The class host a number of predefined move.
 * <p>
 * The {@code Move} class defines immutable objects thus {@code MoveFixtures}
 * implements move instances as public static shared objects. Tests can
 * freely share the instances without any modification issue.
 */
public final class MoveFixtures {

    /** The null move. */
    public static final Move NULL = null;

    /** A generic move instance. */
    public static final Move AN_INSTANCE = Move.valueOf(Move.Action.AN_INSTANCE, Square.NULL);

    /** A regular move instance. */
    public static final Move A_REGULAR_INSTANCE = Move.valueOf(Move.Action.PUT_DISC, Square.AN_INSTANCE);

    /**
     * A regular move instance that put disc on square A1.
     * <p>
     * The move instance is defined as follow:
     * <p>
     * The action getter method returns {@link Move.Action#PUT_DISC}
     * <p>
     * The square getter method returns {@link Square#A1}
     **/
    public static final Move A1 = Move.valueOf(Move.Action.PUT_DISC, Square.A1);

    /** Class constructor. */
    private MoveFixtures() { }

}
