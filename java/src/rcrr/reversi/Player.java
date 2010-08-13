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
 * A <code>Player</code> is either one among the Black and the White opponents.
 */
public enum Player {
    /**
     * The Black player.
     */
    BLACK("The Black player", "Black", SquareState.BLACK),
    /**
     * The White player.
     */
    WHITE("The White player", "White", SquareState.WHITE);
    
    private String description;
    private String name;
    private SquareState color;
    
    /**
     * Class constructor.
     *
     * @param  description the <code>Player</code> description
     * @param  name        the <code>Player</code> name
     * @param  color       the <code>SquareState</code> or "color" relative to the <code>Player</code>
     */
    Player(String description, String name, SquareState color) {
	this.description = description;
	this.name = name;
	this.color = color;
    }
    
    /**
     * Returns a <code>String</code> value that is the player's description.
     *
     * @return        the <code>Player</code> description
     */
    public String getDescription() { return description; }
    
    /**
     * Returns a <code>String</code> value that is the player's name.
     *
     * @return        the <code>Player</code> name
     */
    public String getName() { return name; }
    
    /**
     * Returns a <code>SquareState</code> value that is the player's color.
     *
     * @return        the <code>Player</code> color
     */
    public SquareState color() { return color; }
    
    /**
     * Returns a <code>String</code> value that is the player's color symbol.
     *
     * @return        the <code>Player</code> symbol
     */
    public String symbol() { return color().toString(); }
    
    /**
     * Returns the <code>Player</code> opponent. The Black for the White,
     * while the White for the Black.
     *
     * @return        the <code>Player</code> opponent
     */
    public Player opponent() {
	return (this == BLACK) ? WHITE : BLACK;
    }

}