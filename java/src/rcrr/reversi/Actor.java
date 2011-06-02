/*
 *  Actor.java
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

/**
 * An actor is an identity playing the game.
 * <p>
 * {@code Actor} is immutable.
 * <p>
 * The {@code strategy} field is final and cannot be changed, but
 * be careful that the particular strategy object assigned to the actor
 * could be mutable.
 * The class constructor doesn't make a copy of the {@code strategy} object
 * passed as an argument to the builder or the static factory methods, so it is
 * the responsibility of the client to assure that the provided strategy is
 * either immutable or it is a fresh object kept confined to the constructed actor.
 */
public final class Actor {

    /**
     * An instance of this class encapsulates the information needed to instantiate
     * and initialize an actor object. That process is triggered when the {@code build()}
     * method is called.
     * <p>
     * The builder properties and the respectives initializations are:
     * <ul>
     *   <li>{@code name = new String("An actor")}</li>
     *   <li>{@code strategy = new RandomStrategy()}</li>
     * </ul>
     * <p>
     * The {@code Builder} class is mutable, and it is thread-safe.
     * The object status is guarded by a lock on {@code this}.
     */
    public static final class Builder {

        /** The name field. */
        private String name;

        /** The strategy field. */
        private Strategy strategy;

        /**
         * Construct a new builder.
         */
        public Builder() {
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
        public Actor.Builder withName(final String name) {
            setName(name);
            return this;
        }

        /**
         * Returns the {@code this} reference after setting the new {@code strategy} field.
         *
         * @param strategy the strategy assigned to the actor
         * @return         the {@code this} reference
         */
        public Actor.Builder withStrategy(final Strategy strategy) {
            setStrategy(strategy);
            return this;
        }

    }

    /**
     * Base static factory for the class.
     * <p>
     * Parameter {@code name} must be not null.
     * Parameter {@code strategy} must be not null.
     *
     * @param name     the actor's name
     * @param strategy the actor's strategy
     * @return         a new actor
     * @throws NullPointerException when either name or strategy parameter is null
     */
    public static Actor valueOf(final String name, final Strategy strategy) {
        if (name == null) { throw new NullPointerException("Parameter name cannot be null."); }
        if (strategy == null) { throw new NullPointerException("Parameter strategy cannot be null."); }
        return new Actor(name, strategy);
    }

    /** The name field. */
    private final String name;

    /** The strategy field. */
    private final Strategy strategy;

    /**
     * Class constructor.
     * <p>
     * Parameter {@code name} must be not null.
     * Parameter {@code strategy} must be not null.
     *
     * @param name     the actor's name
     * @param strategy the actor's strategy
     */
    private Actor(final String name, final Strategy strategy) {
        assert (name != null) : "Parameter name cannot be null.";
        assert (strategy != null) : "Parameter strategy cannot be null.";
        this.name = name;
        this.strategy = strategy;
    }

    /**
     * Returns the name field.
     *
     * @return the actor's name
     */
    public String name() {
        return name;
    }

    /**
     * Returns the strategy field.
     *
     * @return the actor's strategy
     */
    public Strategy strategy() {
        return strategy;
    }

    /**
     * Returns a formatted string, showing the actor's name.
     *
     * @return a string showing the actor's name
     */
    public String print() {
        return "[" + name + "]";
    }

}
