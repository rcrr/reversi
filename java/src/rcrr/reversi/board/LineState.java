/*
 *  LineState.java
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A line configuration or state.
 * 
 */
public final class LineState {

    /**
     * A table that hosts for each file square the transfer matrix on the other files.
     * Should be part of the File class ....
     * The Map has an entry for each file. The entry value is a list corresponding to the squares of the file.
     * The List contains for each square an inner map that has an entry for each file crossing the square.
     * The inner map entry has the "target" file as key and an integer value as value.
     * The integer is computed as the integer 3 elevated to the ordinal power ...
     */
    private static final Map<File, List<Map<File, Integer>>> FILE_TRANSFER_MATRIX = initializeComputeTheFileTransferMatrix();

    private static final Map<File, List<Map<File, Integer>>> initializeComputeTheFileTransferMatrix() {
        final Map<File, List<Map<File, Integer>>> fileTransferMatrix = new HashMap<File, List<Map<File, Integer>>>();
        for (final File file: FileUtils.files()) {
            final List<Map<File, Integer>> squares = new ArrayList<Map<File, Integer>>();
            for (final Square square: Line.getInstance(file).squares()) {
                final Map<File, Integer> transferMap = new HashMap<File, Integer>();
                for (final Line affectedLine : Line.linesForSquare(square)) {
                    final File affectedFile = affectedLine.file();
                    if (affectedFile != null) {
                        final int squarePosition = Line.getInstance(affectedFile).squares().indexOf(square);
                        final int transferCoefficient = Line.squarePositionInLineBase3Coefficient(squarePosition);
                        transferMap.put(affectedFile, transferCoefficient);
                    }
                }
                squares.add(transferMap);
            }
            fileTransferMatrix.put(file, squares);
        }
        return fileTransferMatrix;
    }

    public static int fileTransferMatrix(final File changed, final int square, final File affected) {
        if (changed == null) { throw new NullPointerException("Parameter changed cannot be null."); }
        if (affected == null) { throw new NullPointerException("Parameter affected cannot be null."); }
        final List<Map<File, Integer>> squares = FILE_TRANSFER_MATRIX.get(changed);
        final Map<File, Integer>  transferMatrix = squares.get(square);
        return transferMatrix.get(affected);
    }

    /**
     * Too many controls that will be removed when debugging and tests will be ready.
     */
    public static int[] fileIndexDeltasByMove(final File file, final int index, final int move) {
        if (file == null) { throw new NullPointerException("Parameter file cannot be null."); }
        final int order = Line.getInstance(file).squares().size();
        final int boundary = indexBoundary(order);
        if (index < 0 || index > boundary) { throw new IndexOutOfBoundsException("Parameter index is out of range."); }
        final LineState fileState = LineState.valueOf(order, index);
        final LineIndex fileIndex = LineIndex.valueOf(Line.getInstance(file), index);
        if(!fileIndex.legalMoves().keySet().contains(move)) { throw new IllegalArgumentException("Parameter move is not valid. move=" + move); }
        return LineIndexMove.valueOf(fileIndex, move).getDeltas();
    }

    /**
     * An array of the directions that can be taken moving along the axis.
     */
    private static final int[] DIRECTIONS = {-1, +1};

    /**
     * For each order, the maximum index reachable.
     */
    private static final int[] MAX_INDEX = new int[Line.MAX_ORDER + 1];

    /**
     * Two dimension array that contains all the possible LineState instances.
     * The first dimension is driven by the order. Orders range between 0 and Line.MAX_ORDER + 1.
     * The second dimension is the index of the file state. It ranges between 0 and MAX_INDEX[order].
     */
    private static final LineState[][] STATES = new LineState[Line.MAX_ORDER + 1][];

    static {

        /**
         * Computes the MAX_IDEX and STATES arrays.
         */
        for (int i = Line.MIN_ORDER; i < Line.MAX_ORDER + 1; i++) {
            MAX_INDEX[i] = Line.squarePositionInLineBase3Coefficient(i) - 1;
            STATES[i] = new LineState[MAX_INDEX[i] + 1];
            for (int j = 0; j <= MAX_INDEX[i]; j++) {
                STATES[i][j] = new LineState(i, j);
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
     * @throws IndexOutOfBoundsException when parameters are out of their boundaries
     */
    public static LineState valueOf(final int order, final int index) {
        if (order < Line.MIN_ORDER || order > Line.MAX_ORDER) {
            throw new IndexOutOfBoundsException("Parameter order is invalid. order = " + order);
        }
        if (index < 0 || index > MAX_INDEX[order]) {
            throw new IndexOutOfBoundsException("Parameter index is invalid. index = " + index);
        }
        return STATES[order][index];
    }

    public static int indexBoundary(final int order) {
        if (order < Line.MIN_ORDER || order > Line.MAX_ORDER) {
            throw new IndexOutOfBoundsException("Parameter order is invalid. order = " + order);
        }
        return MAX_INDEX[order];
    }

    /**
     * Verifies the consistency of the given {@code configuration} parameter.
     *
     * @param configuration the file state
     * @throws NullPointerException     when {@code configuration} parameter is null or contains null values
     * @throws IllegalArgumentException when {@code configuration} parameter is null or contains OUTER values
     */
    private static void checkConfiguration(final List<SquareState> configuration) {
        if (configuration == null) { throw new NullPointerException("Parameter configuration cannot be null."); }
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
    private static int computeIndex(final List<SquareState> configuration) {
        checkConfiguration(configuration);
        int index = 0;
        for (int i = 0; i < configuration.size(); i++) {
            index += configuration.get(i).ordinal() * Line.squarePositionInLineBase3Coefficient(i);
        }
        return index;
    }

    /**
     * Returns a new configuration array flipping blacks and whites.
     *
     * @param configuration the file configuration to be flipped
     * @return              a new file configuration
     */
    private static List<SquareState> flipConfiguration(final List<SquareState> configuration) {
        checkConfiguration(configuration);
        final List<SquareState> newConfiguration = new ArrayList<SquareState>(configuration);
        for (int square = 0; square < configuration.size(); square++) {
            switch (configuration.get(square)) {
            case BLACK: newConfiguration.set(square, SquareState.WHITE); break;
            case WHITE: newConfiguration.set(square, SquareState.BLACK); break;
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
    private static String printConfiguration(final List<SquareState> configuration) {
        checkConfiguration(configuration);
        final String separator = " ";
        final StringBuffer sb = new StringBuffer(separator);
        for (int i = 0; i < configuration.size(); i++) {
            sb.append(configuration.get(i).symbol() + separator);
        }
        return sb.toString();
    }

    /** The configuration field. */
    private final List<SquareState> configuration;

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
    private LineState(final int order, final int index) {
        this.order = order;
        this.index = index;
        List<SquareState> transientConfiguration = new ArrayList<SquareState>(order);

        /** Prepares the empy cells in the configuration list. */
        for (int i = 0; i < order; i++) {
            transientConfiguration.add(null);
        }

        /**
         * Computes the configuration field.
         */
        int remainder = index;
        for (int i = order - 1; i >= 0; i--) {
            final int config = remainder / Line.squarePositionInLineBase3Coefficient(i);
            final SquareState squareState;
            switch (config) {
            case 0: squareState = SquareState.EMPTY; break;
            case 1: squareState = SquareState.BLACK; break;
            case 2: squareState = SquareState.WHITE; break;
            default: throw new RuntimeException("Config value must be in between 0 and 2. config=" + config);
            }
            transientConfiguration.set(i, squareState);
            remainder = remainder % Line.squarePositionInLineBase3Coefficient(i);
        }
        this.configuration = Collections.unmodifiableList(transientConfiguration);

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
     * Returns the configuration field.
     *
     * @return the configuration field
     */
    public List<SquareState> configuration() {
        return this.configuration;
    }

    /**
     * Returns the file state having the black flipped to white and vice-versa.
     *
     * @return the flipped file state
     */
    public LineState flip() {
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
     * Returns a {@code String} representing the {@code LineState} object.
     *
     * @return a {@code String} representing the line state
     */
    @Override public String toString() {
        return String.format("[(order=%d, index=%d) [%s]]", order, index, printConfiguration(configuration));
    }

    /**
     * Returns the legal moves map for the line state. It is used by the constructor to populate the specific field.
     * The map has an integer as key that represent the move, and anothr integer as value that represent the index of the configuration
     * reachable executing the move.
     *
     * @return a map of legal moves and the corresponding index
     */
    private Map<Integer, Integer> computeLegalMoves() {
        final Map<Integer, Integer> legalMoves = new HashMap<Integer, Integer>();
        for (int move = 0; move < configuration.size(); move++) {
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
        final SquareState squareColor = configuration.get(square);
        switch (squareColor) {
        case EMPTY: return -1;
        case BLACK: return square;
        case WHITE:
            final int next = square + dir;
            if (next < 0 || next >= configuration.size()) { return -1; }
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
        if (move < 0 || move >= configuration.size()) {
            throw new IllegalArgumentException("Parameter move is out of range.");
        }
        if (configuration.get(move) != SquareState.EMPTY) { return false; }
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
    private List<SquareState> makeMove(final int move) {
        final List<SquareState> newConfiguration = new ArrayList<SquareState>(configuration);
        newConfiguration.set(move, SquareState.BLACK);
        for (final int dir : DIRECTIONS) {
            final int bracketer = wouldFlip(move, dir);
            if (bracketer != -1) {
                for (int square = move + dir; true; square = square + dir) {
                    if (square == bracketer) { break; }
                    newConfiguration.set(square, SquareState.BLACK);
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
        if (neighbor < 0 || neighbor >= configuration.size()) { return -1; }
        final int next = neighbor + dir;
        if (next < 0 || next >= configuration.size()) { return -1; }
        final SquareState neighborColor = configuration.get(neighbor);
        switch (neighborColor) {
        case EMPTY: return -1;
        case BLACK: return -1;
        case WHITE: return findBracketingPiece(next, dir);
        default: throw new RuntimeException("Unexpected square color. Got " + neighborColor);
        }
    }

}
