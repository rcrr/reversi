/*
 *  ActorBuilder.java
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
 * An actor builder is a facility to generate actor instances for testing.
 * <p>
 * {@code ActorBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class ActorBuilder {

    /** The name field. */
    private String name;

    /** The strategy field. */
    private Strategy strategy;

    /**
     * The class constructor.
     */
    public ActorBuilder() {
        this.name = new String("An actor");
        this.strategy = new RandomStrategy();
    }

    /**
     * Returns a new instance of an actor object.
     *
     * @return the actor instance as prepared by the current actor's builder
     */
    public synchronized Actor build() {
        return Actor.valueOf(name, strategy);
    }

    /**
     * The method returns the name field.
     *
     * @return the name field
     */
    public synchronized String getName() {
        return this.name;
    }

    /**
     * The method returns the strategy field.
     *
     * @return the strategy field
     */
    public synchronized Strategy getStrategy() {
        return this.strategy;
    }

    /**
     * The method sets the name field.
     *
     * @param name the update value for the name field
     */
    private synchronized void setName(final String name) {
        this.name = name;
    }

    /**
     * The method sets the strategy field.
     *
     * @param strategy the update value for the strategy field
     */
    private synchronized void setStrategy(final Strategy strategy) {
        this.strategy = strategy;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code name} field.
     *
     * @param name the name assigned to the actor
     * @return     the {@code this} reference
     */
    public ActorBuilder withName(final String name) {
        setName(name);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code strategy} field.
     *
     * @param strategy the strategy assigned to the actor
     * @return         the {@code this} reference
     */
    public ActorBuilder withStrategy(final Strategy strategy) {
        setStrategy(strategy);
        return this;
    }

}
