/*
 *  GameBuilder.java
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

import java.io.PrintStream;

/**
 * A game builder is a facility to generate game instances for testing.
 * <p>
 * {@code GameBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class GameBuilder {

    /** The actors field. */
    private ActorsPair actors;

    /** The sequence field. */
    private GameSequence sequence;

    /** The print stream field. */
    private PrintStream ps;

    /**
     * The class constructor.
     */
    public GameBuilder() {
        this.actors = new ActorsPairBuilder().build();
        this.sequence = new GameSequenceBuilder().build();
        this.ps = null;
    }

    /**
     * Returns a new instance of a game object.
     *
     * @return the game instance as prepared by the current game's builder
     */
    public synchronized Game build() {
        return Game.newInstance(actors, sequence, ps);
    }

    /**
     * The setter method for the sequence field.
     *
     * @param sequence the update for the sequence field
     */
    private synchronized void setSequence(final GameSequence sequence) {
        this.sequence = sequence;
    }

    /**
     * The setter method for the actors field.
     *
     * @param actors the update for the actors field
     */
    private synchronized void setActors(final ActorsPair actors) {
        this.actors = actors;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code actors}
     * field value.
     *
     * @param actors the actors field
     * @return       the {@code this} reference
     */
    public GameBuilder withActors(final ActorsPair actors) {
        setActors(actors);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code sequence}
     * field value.
     *
     * @param sequence the game sequence
     * @return         the {@code this} reference
     */
    public GameBuilder withSequence(final GameSequence sequence) {
        setSequence(sequence);
        return this;
    }

}
