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

import org.junit.*;
import static org.junit.Assert.*;

import static org.hamcrest.CoreMatchers.*;
import static org.junit.matchers.JUnitMatchers.*;

public class ActorTest {

    private static final Strategy A_STRATEGY = new RandomStrategy();
    private static final String ACTOR_NAME = "Random AI";
    private static final Actor AN_ACTOR = Actor.valueOf(ACTOR_NAME, A_STRATEGY);

    private static final Strategy RANDOM_STRATEGY = new RandomStrategy();
    private static final String RANDOM_AI = "Random AI";
    private static final Actor RANDOM_AI_ACTOR = Actor.valueOf(RANDOM_AI, RANDOM_STRATEGY);

    private static final Strategy NULL_STRATEGY = null;
    private static final String NULL_ACTOR_NAME = null;

    @Test
    public final void testName() {
        assertThat("Actor's name for RANDOM_AI_ACTOR is RANDOM_AI.",
                   RANDOM_AI_ACTOR.name(), is(RANDOM_AI));
    }

    @Test
    public final void testStrategy() {
        assertThat("Actor's strategy for RANDOM_AI_ACTOR is RANDOM_STRATEGY.",
                   RANDOM_AI_ACTOR.strategy(), is(RANDOM_STRATEGY));
    }

    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_c1() {
        Actor.valueOf(NULL_ACTOR_NAME, A_STRATEGY);
    }

    @Test(expected = NullPointerException.class)
    public final void testValueOf_boundaryConditions_c2() {
        Actor.valueOf(ACTOR_NAME, NULL_STRATEGY);
    }

    @Test
    public final void testValueOf() {
        assertThat("Actor.valueOf(ACTOR_NAME, A_STRATEGY) must be an instance of Actor class.",
                   Actor.valueOf(ACTOR_NAME, A_STRATEGY), instanceOf(Actor.class));
    }

}
