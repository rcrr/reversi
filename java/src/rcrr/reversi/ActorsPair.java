/*
 *  ActorsPair.java
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
import java.util.Collections;

/**
 * A pair of actors is a mandatory field of a game object.
 * <p>
 * {@code ActorsPair} is immutable.
 * <p>
 * @see Actor
 */
public class ActorsPair {

    /** The actors field. */
    private final Map<Player, Actor> actors;

    /**
     * Class constructor.
     * <p>
     * Parameter {@code actors} must be not null.
     * It must contain two entry, null keys are not allowed.
     * The two actors could be the same, but must be not null.
     *
     * @param actors the two actors assigned to black and white players
     */
    private ActorsPair(final Map<Player, Actor> actors) {
	assert (actors != null) : "Parameter actors cannot be null.";
	assert (actors.size() == Player.values().length) : "Parameter actors size is not consistent." + 
	    " actors.size()=" + actors.size() +
	    " expected value: " + Player.values().length;
	EnumMap<Player, Actor> actorEnumMap = (actors instanceof EnumMap) ? 
	    (EnumMap<Player, Actor>) actors : new EnumMap<Player, Actor>(actors);
	this.actors = Collections.unmodifiableMap(actorEnumMap);
    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameter {@code actors} must be not null.
     * It must contain two entry, null keys are not allowed.
     * The two actors could be the same, but must be not null.
     *
     * @param actors the two actors assigned to black and white players
     * @return        a new actor
     * @throws NullPointerException when parameter actors is null,
     *                              when an actors map maps one or more keys to null,
     *                              and when one key is null
     * @throws IllegalArgumentException if the map size is different from
     *                                  the Player enum values length
     */
    public static ActorsPair valueOf(final Map<Player, Actor> actors) {
	if (actors == null) throw new NullPointerException("Parameter actors cannot be null.");
	if (actors.size() != Player.values().length)
	    throw new IllegalArgumentException("Parameter actors size is not consistent." + 
					       " actors.size()=" + actors.size() +
					       " expected value: " + Player.values().length);
	if (actors.containsKey(null))
	    throw new NullPointerException("Parameter actors cannot have null keys. actors=" + actors);
	if (actors.containsValue(null))
	    throw new NullPointerException("Parameter actors cannot have a null value. actors=" + actors);
	return new ActorsPair(actors);
    }

    /**
     * Returns the actor associated with the given color.
     *
     * @return the actor playing the given color
     */
    public Actor get(Player color) {
	return actors.get(color);
    }

}