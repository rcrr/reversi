/*
 *  GameSequenceBuilder.java
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

import java.util.Arrays;
import java.util.List;
import java.util.ArrayList;

/**
 * A game sequence builder is a facility to generate game sequence instances for testing.
 * <p>
 * {@code GameSequenceBuilder} is mutable, and it is thread-safe.
 * The object status is guarded by a lock on {@code this}.
 */
public final class GameSequenceBuilder {

    /** The squares field. */
    private List<GameSnapshot> snapshots;

    /**
     * The class constructor.
     */
    public GameSequenceBuilder() {
        this.snapshots = new ArrayList<GameSnapshot>();
    }

    /**
     * Returns a new instance of a game sequence object.
     *
     * @return the game sequence instance as prepared by the current game sequence's builder
     */
    public synchronized GameSequence build() {
        return GameSequence.valueOf(snapshots);
    }

    /**
     * The method returns the square's state to the given board's square.
     *
     * @param index index of the game snapshot to return from the sequence
     * @return       the selected game snapshot
     */
    private synchronized GameSnapshot get(final int index) {
        return this.snapshots.get(index);
    }

    /**
     * The method add a game snapshot to the end of the sequence.
     *
     * @param snapshot the game snapshot to be appended to the sequence
     */
    private synchronized void add(final GameSnapshot snapshot) {
        this.snapshots.add(snapshot);
    }

    /**
     * The method insert a game snapshot at the specified position.
     *
     * @param index    the index at which insert the game snapshot
     * @param snapshot the game snapshot to be appended to the sequence
     */
    private synchronized void add(final int index, final GameSnapshot snapshot) {
        this.snapshots.add(index, snapshot);
    }

    /**
     * Replaces the game snapshot at the specified position in this sequence
     * with the specified one.
     *
     * @param index the index at which set the game snapshot
     * @param snapshot the game snapshot to be set in the sequence
     */
    private synchronized void set(final int index, final GameSnapshot snapshot) {
        this.snapshots.set(index, snapshot);
    }

    /**
     * Removes the game snapshot at the specified position in this sequence.
     *
     * @param index the index at which remove the game snapshot
     */
    private synchronized void set(final int index) {
        this.snapshots.remove(index);
    }

    /**
     * The setter method for the squares field.
     *
     * @param snapshots the update for the snapshots field
     */
    private synchronized void setSnapshots(final List<GameSnapshot> snapshots) {
        this.snapshots = snapshots;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code squareState}
     * value to the {@code square} entry.
     *
     * @param square      the board's square
     * @param squareState the board's square assigned state
     * @return            the {@code this} reference
     */
    public GameSequenceBuilder withSnapshot(final GameSnapshot snapshot) {
        add(snapshot);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code snapshots}
     * field value.
     *
     * @param snapshots the game snapshot sequence
     * @return          the {@code this} reference
     */
    public GameSequenceBuilder withSnapshots(final List<GameSnapshot> snapshots) {
        setSnapshots(snapshots);
        return this;
    }

    /**
     * Returns the {@code this} reference after setting the new {@code snapshots}
     * field value.
     *
     * @param snapshots the game snapshot sequence
     * @return          the {@code this} reference
     */
    public GameSequenceBuilder withSnapshots(final GameSnapshot... snapshots) {
        setSnapshots(Arrays.asList(snapshots));
        return this;
    }
}
