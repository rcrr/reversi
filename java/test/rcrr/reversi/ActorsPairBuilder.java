/*
 *  ActorsPairBuilder.java
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import java.util.Map;
import java.util.EnumMap;
import java.util.Collections;

/**
 * An actors pair builder is a facility to generate {@code ActorsPair} instances for testing.
 * <p>
 * {@code ActorsPairBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class ActorsPairBuilder {

    /** A generic strategy field used by AN_ACTOR actor fixture. */
    private static final Strategy A_STRATEGY = new RandomStrategy();

    /** The field used by AN_ACTOR actor fixture. */
    private static final String AN_ACTOR_NAME = "An Actor";

    /** A generic actor fixture. */
    private static final Actor AN_ACTOR = Actor.valueOf(AN_ACTOR_NAME, A_STRATEGY);

    /** The actors field. */
    private Map<Player, Actor> actors;

    /**
     * The class constructor.
     */
    public ActorsPairBuilder() {
        this.actors = new EnumMap<Player, Actor>(Player.class);
        this.actors.put(Player.BLACK, AN_ACTOR);
        this.actors.put(Player.WHITE, AN_ACTOR);
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
     * @param actors the update value for the actors field
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
    public ActorsPairBuilder withActor(final Player player, final Actor actor) {
        setActor(player, actor);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code actors} field.
     *
     * @param actors the map of actors assigned to the players
     * @return       the {@code this} reference
     */
    public ActorsPairBuilder withActors(final Map<Player, Actor> actors) {
        setActors(actors);
        return this;
    }

}
