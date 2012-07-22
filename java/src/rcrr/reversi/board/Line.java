/*
 *  Line.java
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

import java.util.Set;
import java.util.EnumSet;
import java.util.Map;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Arrays;
import java.util.Collections;
import java.util.ArrayList;
import java.util.List;

import static rcrr.reversi.board.Square.*;
import static rcrr.reversi.board.Axis.*;

/**
 * The {@code Line} enum defines a line of the board game.
 */
public enum Line {
    R1(   squaresAsList(A1, B1, C1, D1, E1, F1, G1, H1), HORIZONTAL),
    R2(   squaresAsList(A2, B2, C2, D2, E2, F2, G2, H2), HORIZONTAL),
    R3(   squaresAsList(A3, B3, C3, D3, E3, F3, G3, H3), HORIZONTAL),
    R4(   squaresAsList(A4, B4, C4, D4, E4, F4, G4, H4), HORIZONTAL),
    R5(   squaresAsList(A5, B5, C5, D5, E5, F5, G5, H5), HORIZONTAL),
    R6(   squaresAsList(A6, B6, C6, D6, E6, F6, G6, H6), HORIZONTAL),
    R7(   squaresAsList(A7, B7, C7, D7, E7, F7, G7, H7), HORIZONTAL),
    R8(   squaresAsList(A8, B8, C8, D8, E8, F8, G8, H8), HORIZONTAL),
    A(    squaresAsList(A1, A2, A3, A4, A5, A6, A7, A8), VERTICAL),
    B(    squaresAsList(B1, B2, B3, B4, B5, B6, B7, B8), VERTICAL),
    C(    squaresAsList(C1, C2, C3, C4, C5, C6, C7, C8), VERTICAL),
    D(    squaresAsList(D1, D2, D3, D4, D5, D6, D7, D8), VERTICAL),
    E(    squaresAsList(E1, E2, E3, E4, E5, E6, E7, E8), VERTICAL),
    F(    squaresAsList(F1, F2, F3, F4, F5, F6, F7, F8), VERTICAL),
    G(    squaresAsList(G1, G2, G3, G4, G5, G6, G7, G8), VERTICAL),
    H(    squaresAsList(H1, H2, H3, H4, H5, H6, H7, H8), VERTICAL),
    A6_C8(squaresAsList(A6, B7, C8),                     DIAGONAL_LR),
    A5_D8(squaresAsList(A5, B6, C7, D8),                 DIAGONAL_LR),
    A4_E8(squaresAsList(A4, B5, C6, D7, E8),             DIAGONAL_LR),
    A3_F8(squaresAsList(A3, B4, C5, D6, E7, F8),         DIAGONAL_LR),
    A2_G8(squaresAsList(A2, B3, C4, D5, E6, F7, G8),     DIAGONAL_LR),
    A1_H8(squaresAsList(A1, B2, C3, D4, E5, F6, G7, H8), DIAGONAL_LR),
    B1_H7(squaresAsList(B1, C2, D3, E4, F5, G6, H7),     DIAGONAL_LR),
    C1_H6(squaresAsList(C1, D2, E3, F4, G5, H6),         DIAGONAL_LR),
    D1_H5(squaresAsList(D1, E2, F3, G4, H5),             DIAGONAL_LR),
    E1_H4(squaresAsList(E1, F2, G3, H4),                 DIAGONAL_LR),
    F1_H3(squaresAsList(F1, G2, H3),                     DIAGONAL_LR),
    C1_A3(squaresAsList(C1, B2, A3),                     DIAGONAL_RL),
    D1_A4(squaresAsList(D1, C2, B3, A4),                 DIAGONAL_RL),
    E1_A5(squaresAsList(E1, D2, C3, B4, A5),             DIAGONAL_RL),
    F1_A6(squaresAsList(F1, E2, D3, C4, B5, A6),         DIAGONAL_RL),
    G1_A7(squaresAsList(G1, F2, E3, D4, C5, B6, A7),     DIAGONAL_RL),
    H1_A8(squaresAsList(H1, G2, F3, E4, D5, C6, B7, A8), DIAGONAL_RL),
    H2_B8(squaresAsList(H2, G3, F4, E5, D6, C7, B8),     DIAGONAL_RL),
    H3_C8(squaresAsList(H3, G4, F5, E6, D7, C8),         DIAGONAL_RL),
    H4_D8(squaresAsList(H4, G5, F6, E7, D8),             DIAGONAL_RL),
    H5_E8(squaresAsList(H5, G6, F7, E8),                 DIAGONAL_RL),
    H6_F8(squaresAsList(H6, G7, F8),                     DIAGONAL_RL);

    /** The null instance. */
    public static final Line NULL = null;

    /** The number of lines. */
    public static final int NUMBER_OF = values().length;

    /** A generic line instance. */
    public static final Line AN_INSTANCE = H2_B8;

    /**
     * The minimum order of a line.
     */
    public static final int MIN_ORDER = 3;

    /**
     * The maximum order of a line.
     */
    public static final int MAX_ORDER = 8;

    private static final int[] SQUARE_POSITION_IN_LINE_BASE_3_COEFFICIENT = new int[MAX_ORDER + 1];

    private static final Map<File, Line> LINE_TO_FILE_MAP = new HashMap<File, Line>();

    private static final Map<Square, List<Line>> LINES_FOR_SQUARE;

    /**
     * Each position is computed as the power base three of the position itself (1, 3, 9, 27, ...).
     */
    public static final int squarePositionInLineBase3Coefficient(final int index) {
        return SQUARE_POSITION_IN_LINE_BASE_3_COEFFICIENT[index];
    }

    public static final Line getInstance(final File file) {
        if (file == null) { throw new NullPointerException("Parameter file cannot be null."); }
        return LINE_TO_FILE_MAP.get(file);
    }

    public static final List<Line> linesForSquare(final Square square) {
        return LINES_FOR_SQUARE.get(square);
    }

    /** The list of the four corners. */
    private static final List<Square> squaresAsList(Square... squares) {
        return Collections.unmodifiableList(Arrays.asList(squares));
    }

    static {

        /**
         * Computes the SQUARE_POSITION_IN_LINE_BASE_3_COEFFICIENT array.
         */
        for (int index = 0; index < MAX_ORDER + 1; index++) {
            SQUARE_POSITION_IN_LINE_BASE_3_COEFFICIENT[index] = BigInteger.valueOf(3).pow(index).intValue();
        }

        /** Finalli has to be removed. */
        for (int i = 0; i < NUMBER_OF; i++) {
            LINE_TO_FILE_MAP.put(FileUtils.files().get(i), values()[i]);
        }

        /** Prepares the LINES_FOR_SQUARE map. */
        final Map<Square, List<Line>> transientLinesForSquare = new EnumMap<Square, List<Line>>(Square.class);
        for (final Square square : Square.values()) {
            transientLinesForSquare.put(square, new ArrayList<Line>());
        }
        for (final Line line : values()) {
            for (final Square square : line.squares()) {
                final List<Line> lines = transientLinesForSquare.get(square);
                lines.add(line);
            }
        }
        for (final Square square : Square.values()) {
            transientLinesForSquare.put(square, Collections.unmodifiableList(transientLinesForSquare.get(square)));
        }
        LINES_FOR_SQUARE = Collections.unmodifiableMap(transientLinesForSquare);

        /** Prepares the LINE_CROSSING map. */


    }

    /** The axis field. */
    private final Axis axis;

    /** The squares field. */
    private final List<Square> squares;

    /** The squareSet field. */
    private final EnumSet<Square> squareSet;

    /**
     * Enum constructor.
     *
     * @param squares the list of squares
     */
    private Line(final List<Square> squares, final Axis axis) {

        this.squares = squares;

        this.squareSet = EnumSet.noneOf(Square.class);
        this.squareSet.addAll(squares);

        this.axis = axis;
    }

    public File file() {
        return FileUtils.files().get(ordinal());
    }

    public Axis axis() {
        return this.axis;
    }

    public int order() {
        return this.squares.size();
    }

    public List<Square> squares() {
        return this.squares;
    }

    public EnumSet<Square> squareSet() {
        return this.squareSet.clone();
    }

    public Square cross(final Line other) {
        if (other == null) { throw new NullPointerException("Parameter other must be not null."); }
        if (this.equals(other)) { throw new IllegalArgumentException("Parameter other must be different from this. other=" + other); }
        final Square intersection;
        final EnumSet<Square> crossSet = other.squareSet();
        crossSet.retainAll(this.squareSet);
        final int crossSetSize = crossSet.size();
        if (crossSetSize == 0) {
            intersection = Square.NULL;
        } else if (crossSetSize == 1) {
            intersection = (Square) crossSet.toArray()[0];
        } else {
            throw new RuntimeException("Line crossing can have zero or one element. crossSet=" + crossSet);
        }
        return intersection;
    }

}
