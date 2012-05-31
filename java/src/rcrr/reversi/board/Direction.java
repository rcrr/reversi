/*
 *  Direction.java
 *
 *  Copyright (c) 2010, 2012 Roberto Corradini. All rights reserved.
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

package rcrr.reversi.board;

/**
 * The directions that are available in a regular board's square are
 * eight, Up, Down, Left, Right, and the four diagonal between them.
 * <pre>
 * {@code
 * .   NW    N     NE
 *       \___|____/
 *       |        |
 *     W-| Square |-E
 *       |________|
 *       /   |     \
 *     SW    S     SE
 * }
 * </pre>
 * Each regular {@link Square} has eight neighbor ones,
 * each identified by the proper direction. Boundary squares have fewer neighbors.
 * <p>
 * The {@code Direction enum} is represented by the respective cardinal point literal,
 * for instance the Left is associated with {@code W}.
 */
public enum Direction {
    /**
     * North-West direction.
     */
    NW(Axis.DIAGONAL_LR, Versus.NEGATIVE),
    /**
     * North direction.
     */
    N(Axis.VERTICAL, Versus.NEGATIVE),
    /**
     * North-East direction.
     */
    NE(Axis.DIAGONAL_RL, Versus.NEGATIVE),
    /**
     * West direction.
     */
    W(Axis.HORIZONTAL, Versus.NEGATIVE),
    /**
     * East direction.
     */
    E(Axis.HORIZONTAL, Versus.POSITIVE),
    /**
     * South-West direction.
     */
    SW(Axis.DIAGONAL_RL, Versus.POSITIVE),
    /**
     * South direction.
     */
    S(Axis.VERTICAL, Versus.POSITIVE),
    /**
     * South-East direction.
     */
    SE(Axis.DIAGONAL_LR, Versus.POSITIVE);

    /** axis field. */
    private final Axis axis;

    /** versus field. */
    private final Versus versus;

    /**
     * Enum constructor.
     *
     * @param axis        the axis that the direction belongs to
     * @param versus      the versus that identifies the direction
     */
    private Direction(final Axis axis, final Versus versus) {
        this.axis = axis;
        this.versus = versus;
    }

    /**
     * Returns the axis associated with the direction.
     *
     * @return the axis of the direction
     *
     * @see Axis
     */
    public Axis axis() { return axis; }

    /**
     * Returns the versus defined by the direction on the axis associated with it.
     *
     * @return the versus of the direction
     *
     * @see Versus
     */
    public Versus versus() { return versus; }

}
