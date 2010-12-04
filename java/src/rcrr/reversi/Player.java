/*
 *  Player.java
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
 * The {@code Player} is one of the two competitors in the game.
 * She is either one among the Black and the White opponents.
 */
public enum Player {
    /** The Black player. */
    BLACK("The Black player", SquareState.BLACK),

    /** The White player. */
    WHITE("The White player", SquareState.WHITE);
    
    /** The {@code Player}'s description field. */
    private final String description;

    /** The {@code Player}'s color field. */
    private final SquareState color;
    
    /**
     * Enum constructor.
     *
     * @param  description the {@code Player}'s description
     * @param  color       the {@code Player}'s {@code SquareState} also named "color"
     */
    private Player(String description, SquareState color) {
	this.description = description;
	this.color = color;
    }
    
    /**
     * Returns the {@code Player}'s description.
     *
     * @return the {@code Player}'s description
     */
    public String description() { return description; }
    
    /**
     * Returns the {@code SquareState} value representing the {@code Player}'s color.
     *
     * @return the {@code Player}'s color
     */
    public SquareState color() { return color; }
    
    /**
     * Returns a {@code String} value that is the {@code Player}'s color symbol.
     *
     * @return the {@code Player}'s color symbol
     */
    public String symbol() { return color().symbol(); }
    
    /**
     * Returns the {@code Player} opponent. The Black for the White,
     * while the White for the Black.
     *
     * @return the opponent {@code Player}
     */
    public Player opponent() {
	return (this == BLACK) ? WHITE : BLACK;
    }

}