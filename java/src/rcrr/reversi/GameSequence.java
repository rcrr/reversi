/*
 *  GameSequence.java
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

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import org.joda.time.Duration;


/**
 * An game sequence instance collects an ordered list of game snapshots.
 * <p>
 * Null parameters and out of bound indexes are not checked!
 * <p>
 * {@code GameSequence} is immutable.
 */
public final class GameSequence {

   /**
     * A static factory that returns an initial game sequence.
     *
     * @param gameDuration the initial game time assigned to players
     * @return             a new game sequence initialised with a initial game snapshot
     */
    public static GameSequence initialGameSequence(final Duration gameDuration) {
        return valueOf(Arrays.asList(GameSnapshot.initialGameSnapshot(gameDuration)));
    }

   /**
     * The base static factory.
     * <p>
     * Parameter {@code sequence} cannot be null, and cannot contain null values.
     *
     * @param sequence the sequence of game snapshot
     * @return         the game built with the given sequence
     * @throws NullPointerException if parameter {@code sequence} is null
     * @throws NullPointerException if parameter {@code sequence} contains null values
     */
    public static GameSequence valueOf(final List<GameSnapshot> sequence) {
        if (sequence == null) {
            throw new NullPointerException("Parameter sequence cannot be null.");
        }
        if (sequence.contains(null)) {
            throw new NullPointerException("Parameter sequence cannot contain null values.");
        }
        return new GameSequence(sequence);
    }

    /** The game snapshot sequence field. */
    private final List<GameSnapshot> sequence;

    /**
     * Class constructor.
     *
     * @param sequence the sequence of game snapshot
     */
    private GameSequence(final List<GameSnapshot> sequence) {
        this.sequence = Collections.unmodifiableList(sequence);
    }

   /**
     * Returns a new game having the new state added to the current sequence.
     *
     * @param gameSnapshot the new snapshot to add to the sequence
     * @return             a new game modified by adding the game snapshot parameter
     */
    public GameSequence add(final GameSnapshot gameSnapshot) {
        List<GameSnapshot> newSequence = new ArrayList<GameSnapshot>(sequence);
        newSequence.add(gameSnapshot);
        return valueOf(newSequence);
    }

   /**
     * Returns the game snapshot identified by {@code index}.
     *
     * @param index the game snapshot index in the sequence
     * @return      the game snapshot identified by the index parameter
     */
    public GameSnapshot get(final int index) {
        return sequence.get(index);
    }

   /**
     * Returns true if the game snapshot sequence is empty.
     *
     * @return true if the sequence is empty
     */
    public boolean isEmpty() {
        return sequence.isEmpty();
    }

   /**
     * Returns the last game snapshot.
     *
     * @return the last game snapshot
     */
    public GameSnapshot last() {
        return sequence.get(size() - 1);
    }

    /**
     * Returns the number of game snapshot recordered.
     * <p>
     * When the game starts from the initial game snapshot the size is
     * equal to the number of moves already played plus one.
     *
     * @return the size of the sequence of the game snapshot recordered
     */
    public int size() {
        return sequence.size();
    }

}
