/*
 *  ActorsPairTest.java
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

import java.util.Map;
import java.util.EnumMap;
import java.util.HashMap;

import rcrr.reversi.board.Player;

import org.junit.Test;
import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for the {@code ActorsPair} class.
 *
 * @see ActorsPair
 */
public class ActorsPairTest {

    /** A generic actors' map. */
    private static final Map<Player, Actor> AN_ACTOR_MAP;

    /** A null actors' map.  */
    private static final Map<Player, Actor> NULL_ACTOR_MAP = null;

    /** The Robot actors' map. */
    private static final Map<Player, Actor> ROBOT_ACTOR_MAP;

    /** The ROBOTS pair of actors. */
    private static final ActorsPair ROBOTS;

    /** Actor C3PO. */
    private static final Actor C3PO = Actor.valueOf("C-3PO", new RandomStrategy());

    /** Actor R2D2. */
    private static final Actor R2D2 = Actor.valueOf("R2-D2", new RandomStrategy());

    /** An incomplete actors'map. */
    private static final Map<Player, Actor> INCOMPLETE_ACTOR_MAP;

    /** An actors' map having a null key. */
    private static final Map<Player, Actor> NULL_KEY_ACTOR_MAP;

    /** An actors' map having a null value. */
    private static final Map<Player, Actor> NULL_VALUE_ACTOR_MAP;

    /** A generic actor. */
    private static final Actor AN_ACTOR = Actor.valueOf("Jack Random AI", new RandomStrategy());

    /** A null actor. */
    private static final Actor NULL_ACTOR = null;

    /** A null player. */
    private static final Player NULL_PLAYER = null;

    /** Static initialization block. */
    static {
        AN_ACTOR_MAP = new EnumMap<Player, Actor>(Player.class);
        AN_ACTOR_MAP.put(Player.BLACK, AN_ACTOR);
        AN_ACTOR_MAP.put(Player.WHITE, AN_ACTOR);

        ROBOT_ACTOR_MAP = new EnumMap<Player, Actor>(Player.class);
        ROBOT_ACTOR_MAP.put(Player.BLACK, C3PO);
        ROBOT_ACTOR_MAP.put(Player.WHITE, R2D2);
        ROBOTS = ActorsPair.valueOf(ROBOT_ACTOR_MAP);

        INCOMPLETE_ACTOR_MAP = new EnumMap<Player, Actor>(Player.class);
        INCOMPLETE_ACTOR_MAP.put(Player.BLACK, AN_ACTOR);

        NULL_KEY_ACTOR_MAP = new HashMap<Player, Actor>();
        NULL_KEY_ACTOR_MAP.put(Player.BLACK, AN_ACTOR);
        NULL_KEY_ACTOR_MAP.put(NULL_PLAYER, AN_ACTOR);

        NULL_VALUE_ACTOR_MAP = new HashMap<Player, Actor>();
        NULL_VALUE_ACTOR_MAP.put(Player.BLACK, AN_ACTOR);
        NULL_VALUE_ACTOR_MAP.put(Player.WHITE, NULL_ACTOR);

    }

    /** Class constructor. */
    public ActorsPairTest() { }

    /**
     * Tests the get method when parameter {@code color} is null.
     *
     * @see ActorsPair#get(Player)
     */
    @Test(expected = NullPointerException.class)
    public final void testGet_boundaryConditions_checkNullParameter() {
        Actor actor = ROBOTS.get(NULL_PLAYER);
    }

    /**
     * Tests the get method.
     *
     * @see ActorsPair#get(Player)
     */
    @Test
    public final void testGet() {
        assertThat("ActorsPair's black player for ROBOTS is C3PO.",
                   ROBOTS.get(Player.BLACK), is(C3PO));
        assertThat("ActorsPair's white player for ROBOTS is R2D2.",
                   ROBOTS.get(Player.WHITE), is(R2D2));
    }

    /**
     * Tests the valueOf factory when parameter {@code actors} is null.
     *
     * @see ActorsPair#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_checkNullMap() {
        ActorsPair.valueOf(NULL_ACTOR_MAP);
    }

    /**
     * Tests the valueOf factory when parameter {@code actors} has
     * a size different from two.
     *
     * @see ActorsPair#valueOf(Map)
     */
    @Test(expected = IllegalArgumentException.class)
    public final void testValueOf_boundaryConditions_checkMapSize() {
        ActorsPair.valueOf(INCOMPLETE_ACTOR_MAP);
    }

    /**
     * Tests the valueOf factory when parameter {@code actors} has
     * a null key.
     *
     * @see ActorsPair#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_checkMapNullKey() {
        ActorsPair.valueOf(NULL_KEY_ACTOR_MAP);
    }

    /**
     * Tests the valueOf factory when parameter {@code actors} has
     * a null value.
     *
     * @see ActorsPair#valueOf(Map)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_checkMapNullValue() {
        ActorsPair.valueOf(NULL_VALUE_ACTOR_MAP);
    }

    /**
     * Tests the {@code valueOf(Map<Player, Actor>)} factory.
     *
     * @see ActorsPair#valueOf(Map)
     */
    @Test
    public final void testValueOf() {
        assertThat("ActorsPair.valueOf(AN_ACTOR_MAP) must be an instance of Actors class.",
                   ActorsPair.valueOf(AN_ACTOR_MAP), instanceOf(ActorsPair.class));
    }

    /**
     * Test the {@code valueOf(Map<Player, Actor>)} factory.
     * <p>
     * The factory receives the actors parameter, and any further change to it
     * must not be reflected to the returned move register instance.
     *
     * @see ActorsPair#valueOf(Map)
     */
    @Test
    public final void testValueOf_actorsMustBeUnchangeable() {

        final Map<Player, Actor> changeable = new EnumMap<Player, Actor>(Player.class);
        changeable.put(Player.BLACK, AN_ACTOR);
        changeable.put(Player.WHITE, C3PO);
        final ActorsPair instance = ActorsPair.valueOf(changeable);
        changeable.put(Player.WHITE, R2D2);

        assertThat("The actors pair instance must be not affected by a"
                   + " change in the actors parameter.",
                   instance.get(Player.WHITE), is(C3PO));
    }

}
