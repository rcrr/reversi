/*
 *  SearchNode.java
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
 * An object representing a node part of the search tree.
 * <p>
 * {@code SearchNode} is immutable.
 * <p>
 * Has to be fully investigated if:
 * - register also the (board, player) tuple.
 * - organize nodes into a real tree (taken from a library or here developed).
 * - organize the value field as a "stack" of values obtained deepening the search.
 */
public final class SearchNode {
    
    /** The move field. */
    private final Square move;
    
    /** The value field. */
    private final int value;
    
    /** The achievable (alpha or lower bound) field. */
    private final int achievable;
    
    /** The cutoff (beta or upper bound) field. */
    private final int cutoff;
    
    /** The ply (depth of the search) field*/
    private final int ply;
    
    /** The player field. */
    private final Player player;
    
    /** The board field. */
    private final Board board;
    
    /** Class constructor. */
    SearchNode(final Square move, final int value) {
	this.move = move;
	this.value = value;

	/** To be fixed ... */
	this.achievable = 0;
	this.cutoff = 0;
	this.ply = 0;
	this.player = null;
	this.board = null;
    }
    
    /** Getter method for move field. */
    public Square move() { return move; }
    
    /** Getter method for value field. */
    public int value() { return value; }
    
    /** Getter method for achievable field. */
    public int achievable() { return achievable; }
    
    /** Getter method for cutoff field. */
    public int cutoff() { return cutoff; }
	
    /** Getter method for ply field. */
    public int ply() { return ply; }
    
    /** Getter method for player field. */
    public Player player() { return player; }
    
    /** Getter method for board field. */
    public Board board() { return board; }
    
    /** Returns a new node having the value sign negated. */
    public SearchNode negated() { return new SearchNode(move, - value); }
    
    /**
     * Returns a String representing the {@code Node} object.
     * <p>
     * The format is: {@code [move=b4, value=567]}
     * 
     * @return a string showing the minimax's node move and value fields
     */
    @Override
    public String toString() {
	return "[move=" + move + ", value=" + value + "]";
    }
    
} 
