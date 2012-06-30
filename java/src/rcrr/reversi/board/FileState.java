/*
 *  FileState.java
 *
 *  Copyright (c) 2012 Roberto Corradini. All rights reserved.
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

package rcrr.reversi.board;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A file configuration or state.
 *
 * 
 */
public final class FileState {

    /**
     * An array of the directions that can be taken moving along the axis.
     */
    private static final int[] DIRECTIONS = {-1, +1};

    /**
     * The minimum order of a file.
     */
    private static final int MIN_ORDER = 3;

    /**
     * The maximum order of a file.
     */
    private static final int MAX_ORDER = FileUtils.FILE_MAX_LENGTH;

    /**
     * For each order, the maximum index reachable.
     */
    private static final int[] MAX_INDEX = new int[MAX_ORDER + 1];

    /**
     * Each position in the array is computed as the power base three of the position itself (1, 3, 9, 27, ...).
     */
    private static final int[] POSITION_COEFFICIENTS = new int[MAX_ORDER + 1];

    /**
     * Two dimension array that contains all the possible FileState instances.
     * The first dimension is driven by the order. Orders range between 0 and MAX_ORDER + 1.
     * The second dimension is the index of the file state. It ranges between 0 and MAX_INDEX[order].
     */
    private static final FileState[][] STATES = new FileState[MAX_ORDER + 1][];

    static {

        /**
         * Computes the POSITION_COEFFICIENTS array.
         */
        for (int i = 0; i < MAX_ORDER + 1; i++) {
            POSITION_COEFFICIENTS[i] = BigInteger.valueOf(3).pow(i).intValue();
        }

        /**
         * Computes the MAX_IDEX and STATES arrays.
         */
        for (int i = MIN_ORDER; i < MAX_ORDER + 1; i++) {
            MAX_INDEX[i] = POSITION_COEFFICIENTS[i] - 1;
            STATES[i] = new FileState[MAX_INDEX[i] + 1];
            for (int j = 0; j <= MAX_INDEX[i]; j++) {
                STATES[i][j] = new FileState(i, j);
            }
        }

    }

    /**
     * Static factory method for the class.
     * <p>
     * The {@code index} parameter is a base three encoding of the square sequence.
     * <p>
     * All the values are precomputed during class initialization.
     * Two call with equal parameters returns the same object.
     *
     * @param order the number of squares of the file
     * @param index the index corresponding to a file configuration
     * @return      the file state identified by the two parameters
     * @throw IndexOutOfBoundsException when parameters are out of their boundaries
     */
    public static FileState valueOf(final int order, final int index) {
        if (order < MIN_ORDER || order > MAX_ORDER) {
            throw new IndexOutOfBoundsException("Parameter order is invalid. order = " + order);
        }
        if (index < 0 || index > MAX_INDEX[order]) {
            throw new IndexOutOfBoundsException("Parameter index is invalid. index = " + index);
        }
        return STATES[order][index];
    }

    /**
     * Verifies the consistency of the given {@code configuration} parameter.
     *
     * @param configuration the file state
     * @throw NullPointerException     when {@code configuration} parameter is null or contains null values
     * @throw IllegalArgumentException when {@code configuration} parameter is null or contains OUTER values
     */
    private static void checkConfiguration(final SquareState[] configuration) {
        if (configuration == null) { throw new NullPointerException("PArameter configuration cannot be null."); }
        for (final SquareState state : configuration) {
            if (state == null) { throw new NullPointerException("Parameter configuration has null values."); }
            if (state == SquareState.OUTER) {
                throw new IllegalArgumentException("Parameter configuration cannot contain OUTER values.");
            }
        }
    }

    /**
     * Returns the index of the given {@code configuration}.
     *
     * @param configuration the file state
     * @return the index of the configuration
     */
    private static int computeIndex(final SquareState[] configuration) {
        checkConfiguration(configuration);
        int index = 0;
        for (int i = 0; i < configuration.length; i++) {
            index += configuration[i].ordinal() * POSITION_COEFFICIENTS[i];
        }
        return index;
    }

    /**
     * Returns a new configuration array flipping blacks and whites.
     *
     * @param configuration the file configuration to be flipped
     * @return              a new file configuration
     */
    private static SquareState[] flipConfiguration(final SquareState[] configuration) {
        checkConfiguration(configuration);
        final SquareState[] newConfiguration = configuration.clone();
        for (int square = 0; square < configuration.length; square++) {
            switch (configuration[square]) {
            case BLACK: newConfiguration[square] = SquareState.WHITE; break;
            case WHITE: newConfiguration[square] = SquareState.BLACK; break;
            default: break;
            }
        }
        return newConfiguration;
    }

    /**
     * Returns a string describing the {@code configuration} parameter.
     *
     * @param configuration the configuration to be printed
     * @return              a string describing the configuration
     */
    private static String printConfiguration(final SquareState[] configuration) {
        checkConfiguration(configuration);
        final String separator = " ";
        final StringBuffer sb = new StringBuffer(separator);
        for (int i = 0; i < configuration.length; i++) {
            sb.append(configuration[i].symbol() + separator);
        }
        return sb.toString();
    }

    /** The configuration field. */
    private final SquareState[] configuration;

    /** The legalMoves field. */
    private final Map<Integer, Integer> legalMoves;

    /** The order field. */
    private final int order;

    /** The index field. */
    private final int index;

    /** The flipped field. It is the index of the file state obtained flipping BLACKs with WHITEs and vice-versa. */
    private final int flipped;

    /**
     * Class constructor.
     *
     * @param order the number of squares of the file
     * @param index the index corresponding to the file configuration
     */
    private FileState(final int order, final int index) {
        this.order = order;
        this.index = index;
        this.configuration = new SquareState[order];

        /**
         * Computes the configuration field.
         */
        int remainder = index;
        for (int i = order - 1; i >= 0; i--) {
            final int config = remainder / POSITION_COEFFICIENTS[i];
            final SquareState squareState;
            switch (config) {
            case 0: squareState = SquareState.EMPTY; break;
            case 1: squareState = SquareState.BLACK; break;
            case 2: squareState = SquareState.WHITE; break;
            default: throw new RuntimeException("Config value must be in between 0 and 2. config=" + config);
            }
            configuration[i] = squareState;
            remainder = remainder % POSITION_COEFFICIENTS[i];
        }

        /**
         * Computes the legalMoves field.
         */
        this.legalMoves = Collections.unmodifiableMap(computeLegalMoves());

        /**
         * Computes the flipped field.
         */
        this.flipped = computeIndex(flipConfiguration(configuration));

    }

    /**
     * Returns the file state having the black flipped to white and vice-versa.
     *
     * @return the flipped file state
     */
    public FileState flip() {
        return valueOf(this.order, this.flipped);
    }

    /**
     * Returns the index field.
     *
     * @return the index field
     */
    public int index() {
        return this.index;
    }

    /**
     * Returns the legalMoves field.
     *
     * @return the legalMoves field
     */
    public Map<Integer, Integer> legalMoves() {
        return this.legalMoves;
    }

    /**
     * Returns the order field.
     *
     * @return the order field
     */
    public int order() {
        return this.order;
    }

    /**
     * Returns a {@code String} representing the {@code FileState} object.
     *
     * @return a {@code String} representing the file state
     */
    @Override public String toString() {
        return String.format("[(order=%d, index=%d) [%s]]", order, index, printConfiguration(configuration));
    }

    /**
     * Returns the legal moves map for the file state. It is used by the constructor to populate the specific field.
     * The map has an integer as key that represent the move, and anothr integer as value that represent the index of the configuration
     * reachable executing the move.
     *
     * @return a map of legal moves and the corresponding index
     */
    private Map<Integer, Integer> computeLegalMoves() {
        final Map<Integer, Integer> legalMoves = new HashMap<Integer, Integer>();
        for (int move = 0; move < configuration.length; move++) {
            if (isLegal(move)) {
                legalMoves.put(move, computeIndex(makeMove(move)));
            }
        }
        return legalMoves;
    }

    /**
     * Returns the position in the file corresponding to the braketing piece.
     * Returns -1 in case there is no bracketing piece.
     *
     * @param square the potential bracketing square
     * @param dir    the direction to follow
     * @return       the position of the bracketing piece
     */
    private int findBracketingPiece(final int square, final int dir) {
        final SquareState squareColor = configuration[square];
        switch (squareColor) {
        case EMPTY: return -1;
        case BLACK: return square;
        case WHITE:
            final int next = square + dir;
            if (next < 0 || next >= configuration.length) { return -1; }
            return findBracketingPiece(next, dir);
        default: throw new RuntimeException("Unexpected square color. Got " + squareColor);
        }        
    }

    /**
     * Returns true if the {@code move} is legal.
     *
     * @param move the move to be checked for validity
     * @return     true when the move is legal
     */
    private boolean isLegal(final int move) {
        if (move < 0 || move >= configuration.length) {
            throw new IllegalArgumentException("Parameter move is out of range.");
        }
        if (configuration[move] != SquareState.EMPTY) { return false; }
        for (final int dir : DIRECTIONS) {
            if (wouldFlip(move, dir) != -1) { return true; }
        }
        return false;
    }

    /**
     * Returns the configuration obtained applying the {@code move} given as parameter.
     * The move must be legal, this prerequisite is not enforced.
     *
     * @param move the move to be executed
     * @return     the new configuration obtained by moving
     */
    private SquareState[] makeMove(final int move) {
        final SquareState[] newConfiguration = configuration.clone();
        newConfiguration[move] = SquareState.BLACK;
        for (final int dir : DIRECTIONS) {
            final int bracketer = wouldFlip(move, dir);
            if (bracketer != -1) {
                for (int square = move + dir; true; square = square + dir) {
                    if (square == bracketer) { break; }
                    newConfiguration[square] = SquareState.BLACK;
                }
            }
        }
        return newConfiguration;
    }

    /**
     * Returns the position that flips or -1 if the move/direction doesn't flip.
     *
     * @param move the potential move to be checked for validity
     * @param dir  the direction to be investigated
     * @return     the position that flips or -1
     */
    private int wouldFlip(final int move, final int dir) {
        final int neighbor = move + dir;
        if (neighbor < 0 || neighbor >= configuration.length) { return -1; }
        final int next = neighbor + dir;
        if (next < 0 || next >= configuration.length) { return -1; }
        final SquareState neighborColor = configuration[neighbor];
        switch (neighborColor) {
        case EMPTY: return -1;
        case BLACK: return -1;
        case WHITE: return findBracketingPiece(next, dir);
        default: throw new RuntimeException("Unexpected square color. Got " + neighborColor);
        }
    }

}
