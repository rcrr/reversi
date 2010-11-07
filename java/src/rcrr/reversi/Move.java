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

/**
 * A move is an action send by a Player/Strategy to the
 * game manager.
 * <p>
 * {@code Move} is immutable.
 */
public final class Move {

    private final MoveAction action;

    private final Square square;

    private Move(MoveAction action, Square square) {
	this.action = action;
	this.square = square;
    }

    public static Move valueOf(MoveAction action) {
	if (action == null) throw new NullPointerException("Parameter action cannot be null.");
	if (action == MoveAction.PUT_DISK) throw new IllegalArgumentException("Parameter action value invalid for this factory.");
	return new Move(action, null);
    }

    public static Move valueOf(Square square) {
	if (square == null) throw new NullPointerException("Parameter square cannot be null.");
	return new Move(MoveAction.PUT_DISK, square);
    }

    public MoveAction action() { return action; }

    public Square square() { return square; }

    /**
     * MoveAction is an Enum type that ...
     */
    public static enum MoveAction {
	PUT_DISK(), 
	PASS(),
	RESIGN();
    }

}