/*
 *  GameSequence.java
 *
 *  Copyright (c) 2010 Roberto Corradini. All rights reserved.
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
 * An instance of a game sequence of game states.
 * <p>
 * {@code GameSequence} is immutable.
 */
public final class GameSequence {

   /**
     * The base static factory.
     *
     * @param sequence the sequence of game snapshot
     * @return         the game built with the given sequence
     */
    public static GameSequence valueOf(final List<GameSnapshot> sequence) {
        return new GameSequence(sequence);
    }

   /**
     * A static factory that returns an initial game sequence.
     *
     * @param gameDuration the initial game time assigned to players
     * @return             a new game sequence initialised with a initial game snapshot
     */
    public static GameSequence initialGameSequence(final Duration gameDuration) {
        return valueOf(Arrays.asList(GameSnapshot.initialGameSnapshot(gameDuration)));
    }

    /** The game state sequence field. */
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
     * Returns the number of game states recordered.
     * <p>
     * When the game starts from the initial position the size is
     * equal to the number of moves already played plus one.
     *
     * @return the size of the sequence of the game's states recordered
     */
    public int size() {
        return sequence.size();
    }

   /**
     * Returns the game state identified by {@code index}.
     *
     * @param index the game snapshot index in the sequence
     * @return      the game snapshot identified by the index parameter
     */
    public GameSnapshot get(final int index) {
        return sequence.get(index);
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
     * Returns true if the game state sequence is empty.
     *
     * @return if the game sequence is empty
     */
    public boolean isEmpty() {
        return sequence.isEmpty();
    }

}
