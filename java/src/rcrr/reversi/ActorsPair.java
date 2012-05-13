/*
 *  ActorsPair.java
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

import java.util.EnumMap;
import java.util.Collections;
import java.util.Map;

import rcrr.reversi.board.Player;

/**
 * A pair of actors is a mandatory field of a game object.
 * <p>
 * {@code ActorsPair} is immutable.
 * <p>
 * @see Actor
 */
public final class ActorsPair {

    /**
     * An instance of this class encapsulates the information needed to instantiate
     * and initialize an actors' pair object. That process is triggered when the {@code build()}
     * method is called.
     * <p>
     * The builder unique property is the {@code actors} map
     * and its initialization is:
     * <pre>
     * {@code
     * actors.put(Player.BLACK, new Actor.Builder().build());
     * actors.put(Player.WHITE, new Actor.Builder().build());
     * }
     * </pre>
     * The {@code Builder} class is mutable, and it is thread-safe.
     * The object status is guarded by a lock on {@code this}.
     */
    public static final class Builder {

        /** The actors field. */
        private Map<Player, Actor> actors;

        /**
         * Construct a new builder.
         */
        public Builder() {
            this.actors = new EnumMap<Player, Actor>(Player.class);
            this.actors.put(Player.BLACK, new Actor.Builder().build());
            this.actors.put(Player.WHITE, new Actor.Builder().build());
        }

        /**
         * Returns a new instance of an actors pair object.
         *
         * @return the actors pair instance as prepared by the current builder
         */
        public synchronized ActorsPair build() {
            return ActorsPair.valueOf(actors);
        }

        /**
         * The method returns the actors field.
         *
         * @return the actors field
         */
        public synchronized Map<Player, Actor> getActors() {
            return this.actors;
        }

        /**
         * The method sets the actors field.
         *
         * @param player the player selected for the update
         * @param actor  the actor assigned to the player key
         */
        private synchronized void setActor(final Player player, final Actor actor) {
            this.actors.put(player, actor);
        }

        /**
         * The method sets the actors field.
         *
         * @param actors the update value for the actors field
         */
        private synchronized void setActors(final Map<Player, Actor> actors) {
            this.actors = actors;
        }

        /**
         * Returns the {@code this} reference after setting the new {@code actor} value
         * to the {@code player} key into the {@code actors} map.
         *
         * @param player the player selected for the update
         * @param actor  the actor assigned to the player key
         * @return         the {@code this} reference
         */
        public ActorsPair.Builder withActor(final Player player, final Actor actor) {
            setActor(player, actor);
            return this;
        }

    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameter {@code actors} must be not null.
     * It must contain two entry, null keys are not allowed.
     * The two actors could be the same, but must be not null.
     *
     * @param actors the two actors assigned to black and white players
     * @return        a new actors' pair
     * @throws NullPointerException when parameter actors is null,
     *                              when an actors map maps one or more keys to null,
     *                              and when one key is null
     * @throws IllegalArgumentException if the map size is different from
     *                                  the Player enum values length
     */
    public static ActorsPair valueOf(final Map<Player, Actor> actors) {
        if (actors == null) { throw new NullPointerException("Parameter actors cannot be null."); }
        if (actors.size() != Player.values().length) {
            throw new IllegalArgumentException("Parameter actors size is not consistent."
                                               + " actors.size()=" + actors.size()
                                               + " expected value: " + Player.values().length);
        }
        if (actors.containsKey(null)) {
            throw new NullPointerException("Parameter actors cannot have null keys. actors=" + actors);
        }
        if (actors.containsValue(null)) {
            throw new NullPointerException("Parameter actors cannot have a null value. actors=" + actors);
        }
        return new ActorsPair(actors);
    }

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
        assert (actors.size() == Player.values().length) : "Parameter actors size is not consistent."
            + " actors.size()=" + actors.size()
            + " expected value: " + Player.values().length;
        this.actors = Collections.unmodifiableMap(new EnumMap<Player, Actor>(actors));
    }

    /**
     * Returns the actor associated with the given color.
     * <p>
     * Parameter {@code color} cannot be null.
     *
     * @param color the color for which query the actor
     * @return      the actor playing the given color
     */
    public Actor get(final Player color) {
        if (color == null) { throw new NullPointerException("Parameter color cannot be null."); }
        return actors.get(color);
    }

    /**
     * Returns a formatted string, showing the actors pair.
     *
     * @return a string showing the actors pair
     */
    public String print() {
        return "[Black's actor=" + get(Player.BLACK).print() + ", white's actor=" + get(Player.WHITE).print() + "]";
    }

}
