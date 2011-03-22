/*
 *  ActorTest.java
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

import org.junit.Test;
import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for the {@code Actor} class.
 *
 * @see Actor
 */
public class ActorTest {

    /** A generic strategy field used by AN_ACTOR actor fixture. */
    private static final Strategy A_STRATEGY = new RandomStrategy();

    /** The field used by AN_ACTOR actor fixture. */
    private static final String AN_ACTOR_NAME = "An Actor";

    /** A generic actor fixture. */
    private static final Actor AN_ACTOR = Actor.valueOf(AN_ACTOR_NAME, A_STRATEGY);


    /** Random strategy field used by RANDOM AI actor. */
    private static final Strategy RANDOM_STRATEGY = new RandomStrategy();

    /** Random AI name field used by RANDOM AI actor. */
    private static final String RANDOM_AI = "Random AI";

    /** Random AI actor fixture. */
    private static final Actor RANDOM_AI_ACTOR = Actor.valueOf(RANDOM_AI, RANDOM_STRATEGY);


    /** The null strategy fixture. */
    private static final Strategy NULL_STRATEGY = null;

    /** The null string fixture. */
    private static final String NULL_ACTOR_NAME = null;

    /** Class constructor. */
    public ActorTest() { }

    /**
     * Tests the name getter method.
     *
     * @see Actor#name()
     */
    @Test
    public final void testName() {
        assertThat("Actor's name for RANDOM_AI_ACTOR is RANDOM_AI.",
                   RANDOM_AI_ACTOR.name(), is(RANDOM_AI));
    }

    /**
     * Tests the strategy getter method.
     *
     * @see Actor#strategy()
     */
    @Test
    public final void testStrategy() {
        assertThat("Actor's strategy for RANDOM_AI_ACTOR is RANDOM_STRATEGY.",
                   RANDOM_AI_ACTOR.strategy(), is(RANDOM_STRATEGY));
    }

    /**
     * Tests the valueOf factory when parameter {@code name} is null.
     *
     * @see Actor#valueOf(String, Strategy)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_c1() {
        Actor.valueOf(NULL_ACTOR_NAME, A_STRATEGY);
    }

    /**
     * Tests the valueOf factory when parameter {@code strategy} is null.
     *
     * @see Actor#valueOf(String, Strategy)
     */
    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_c2() {
        Actor.valueOf(AN_ACTOR_NAME, NULL_STRATEGY);
    }

    /**
     * Tests the valueOf factory.
     *
     * @see Actor#valueOf(String, Strategy)
     */
    @Test
    public final void testValueOf() {
        assertThat("Actor.valueOf(AN_ACTOR_NAME, A_STRATEGY) must be an instance of Actor class.",
                   Actor.valueOf(AN_ACTOR_NAME, A_STRATEGY), instanceOf(Actor.class));
    }

}
