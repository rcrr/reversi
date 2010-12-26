/*
 *  Strategy.java
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
 * A strategy is an object capable to provide a move
 * given a game snapshot.
 */
public interface Strategy {

    /**
     * Returns the game move for the strategy.
     * <p>
     * When the {@code gameSnapshot} doesn't offer a legal move to the player
     * the strategy has to return a {@code null} value.
     * When otherwise a legal move is available the {@code null} return
     * value is not allowed.
     *
     * @param gameSnapshot the game state
     * @return             the move to play
     */
    Move move(GameSnapshot gameSnapshot);
}
