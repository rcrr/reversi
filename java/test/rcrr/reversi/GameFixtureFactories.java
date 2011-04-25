/*
 *  GameFixtureFactories.java
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

/**
 * The class host a number of predefined game instance factories.
 * <p>
 * The {@code Game} class defines mutable objects thus {@code GameFixtureFactories}
 * implements static methods that return predefined game instances.
 * <p>
 * Each test has to request and consume a new game object.
 */
public final class GameFixtureFactories {

    /**
     * The null instance.
     */
    public static final Game NULL = null;

    /**
     * Returns a generic instance.
     *
     * @return a generic game instance
     */
    public static Game anInstance() {
        return  new GameBuilder()
            .build();
    }

    /**
     * Returns a game instance that is characterized by having a
     * {@code GameSequenceFixtures.THREE_SNAPSHOTS} sequence.
     *
     * @return a threeSnapshots game instance
     *
     * @see GameSequenceFixtures#THREE_SNAPSHOTS
     */
    public static Game threeSnapshots() {
        return  new GameBuilder()
            .withSequence(GameSequenceFixtures.THREE_SNAPSHOTS)
            .build();
    }

    /** Class constructor. */
    private GameFixtureFactories() { }

}
