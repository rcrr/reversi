/*
 *  Square.java
 *
 *  Copyright (c) 2010, 2011, 2012 Roberto Corradini. All rights reserved.
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
import java.util.List;
import java.util.Arrays;
import java.util.Collections;
import java.util.Map;
import java.util.EnumMap;
import java.util.HashMap;

import static rcrr.reversi.board.Row.*;
import static rcrr.reversi.board.Column.*;
import static rcrr.reversi.board.DiagonalLR.*;
import static rcrr.reversi.board.DiagonalRL.*;

/**
 * Square is an Enum type that realize the base unit of the game board.
 * Squares are represented by two characters, a letter and a numeric digit.
 * <p>
 * For instance, let's take a square: D4.
 * This symbol identifies the square at the cross of column d and row 4.
 * <p>
 * Here is represented the collection of the 64 Square as them are
 * organized in the game board.
 * <pre>
 * {@code
 * .    a    b    c    d    e    f    g    h
 *   =========================================
 * 1 = A1 = B1 = C1 = D1 = E1 = F1 = G1 = H1 =
 *   =========================================
 * 2 = A2 = B2 = C2 = D2 = E2 = F2 = G2 = H2 =
 *   =========================================
 * 3 = A3 = B3 = C3 = D3 = E3 = F3 = G3 = H3 =
 *   =========================================
 * 4 = A4 = B4 = C4 = D4 = E4 = F4 = G4 = H4 =
 *   =========================================
 * 5 = A5 = B5 = C5 = D5 = E5 = F5 = G5 = H5 =
 *   =========================================
 * 6 = A6 = B6 = C6 = D6 = E6 = F6 = G6 = H6 =
 *   =========================================
 * 7 = A7 = B7 = C7 = D7 = E7 = F7 = G7 = H7 =
 *   =========================================
 * 8 = A8 = B8 = C8 = D8 = E8 = F8 = G8 = H8 =
 *   =========================================
 * }
 * </pre>
 * Has to be noticed that the sequence of the squares is organized by rows. It means that
 * the square ordered list is as follow:
 * <p>
 * {@code (A1, B1, C1, D1, E1, F1, G1, H1, A2, ... H8)}.
 */
public enum Square {

    /** Square a1. */
    A1(R1, A, A1_H8,           DiagonalRL.NULL),

    /** Square b1. */
    B1(R1, B, B1_H7,           DiagonalRL.NULL),

    /** Square c1. */
    C1(R1, C, C1_H6,           C1_A3),

    /** Square d1. */
    D1(R1, D, D1_H5,           D1_A4),

    /** Square e1. */
    E1(R1, E, E1_H4,           E1_A5),

    /** Square f1. */
    F1(R1, F, DiagonalLR.NULL, F1_A6),

    /** Square g1. */
    G1(R1, G, DiagonalLR.NULL, G1_A7),

    /** Square h1. */
    H1(R1, H, DiagonalLR.NULL, H1_A8),

    /** Square a2. */
    A2(R2, A, A2_G8,           DiagonalRL.NULL),

    /** Square b2. */
    B2(R2, B, A1_H8,           C1_A3),

    /** Square c2. */
    C2(R2, C, B1_H7,           D1_A4),

    /** Square d2. */
    D2(R2, D, C1_H6,           E1_A5),

    /** Square e2. */
    E2(R2, E, D1_H5,           F1_A6),

    /** Square f2. */
    F2(R2, F, E1_H4,           G1_A7),

    /** Square g2. */
    G2(R2, G, F1_H3,           H1_A8),

    /** Square h2. */
    H2(R2, H, DiagonalLR.NULL, H2_B8),

    /** Square a3. */
    A3(R3, A, A3_F8,           C1_A3),

    /** Square b3. */
    B3(R3, B, A2_G8,           D1_A4),

    /** Square c3. */
    C3(R3, C, A1_H8,           E1_A5),

    /** Square d3. */
    D3(R3, D, B1_H7,           F1_A6),

    /** Square e3. */
    E3(R3, E, C1_H6,           G1_A7),

    /** Square f3. */
    F3(R3, F, D1_H5,           H1_A8),

    /** Square g3. */
    G3(R3, G, E1_H4,           H2_B8),

    /** Square h3. */
    H3(R3, H, F1_H3,           H3_C8),

    /** Square a4. */
    A4(R4, A, A4_E8,           D1_A4),

    /** Square b4. */
    B4(R4, B, A3_F8,           E1_A5),

    /** Square c4. */
    C4(R4, C, A2_G8,           F1_A6),

    /** Square d4. */
    D4(R4, D, A1_H8,           G1_A7),

    /** Square e4. */
    E4(R4, E, B1_H7,           H1_A8),

    /** Square f4. */
    F4(R4, F, C1_H6,           H2_B8),

    /** Square g4. */
    G4(R4, G, D1_H5,           H3_C8),

    /** Square h4. */
    H4(R4, H, E1_H4,           H4_D8),

    /** Square a5. */
    A5(R5, A, A5_D8,           E1_A5),

    /** Square b5. */
    B5(R5, B, A4_E8,           F1_A6),

    /** Square c5. */
    C5(R5, C, A3_F8,           G1_A7),

    /** Square d5. */
    D5(R5, D, A2_G8,           H1_A8),

    /** Square e5. */
    E5(R5, E, A1_H8,           H2_B8),

    /** Square f5. */
    F5(R5, F, B1_H7,           H3_C8),

    /** Square g5. */
    G5(R5, G, C1_H6,           H4_D8),

    /** Square h5. */
    H5(R5, H, D1_H5,           H5_E8),

    /** Square a6. */
    A6(R6, A, A6_C8,           F1_A6),

    /** Square b6. */
    B6(R6, B, A5_D8,           G1_A7),

    /** Square c6. */
    C6(R6, C, A4_E8,           H1_A8),

    /** Square d6. */
    D6(R6, D, A3_F8,           H2_B8),

    /** Square e6. */
    E6(R6, E, A2_G8,           H3_C8),

    /** Square f6. */
    F6(R6, F, A1_H8,           H4_D8),

    /** Square g6. */
    G6(R6, G, B1_H7,           H5_E8),

    /** Square h6. */
    H6(R6, H, C1_H6,           H6_F8),

    /** Square a7. */
    A7(R7, A, DiagonalLR.NULL, G1_A7),

    /** Square b7. */
    B7(R7, B, A6_C8,           H1_A8),

    /** Square c7. */
    C7(R7, C, A5_D8,           H2_B8),

    /** Square d7. */
    D7(R7, D, A4_E8,           H3_C8),

    /** Square e7. */
    E7(R7, E, A3_F8,           H4_D8),

    /** Square f7. */
    F7(R7, F, A2_G8,           H5_E8),

    /** Square g7. */
    G7(R7, G, A1_H8,           H6_F8),

    /** Square h7. */
    H7(R7, H, B1_H7,           DiagonalRL.NULL),

    /** Square a8. */
    A8(R8, A, DiagonalLR.NULL, H1_A8),

    /** Square b8. */
    B8(R8, B, DiagonalLR.NULL, H2_B8),

    /** Square c8. */
    C8(R8, C, A6_C8,           H3_C8),

    /** Square d8. */
    D8(R8, D, A5_D8,           H4_D8),

    /** Square e8. */
    E8(R8, E, A4_E8,           H5_E8),

    /** Square f8. */
    F8(R8, F, A3_F8,           H6_F8),

    /** Square g8. */
    G8(R8, G, A2_G8,           DiagonalRL.NULL),

    /** Square h8. */
    H8(R8, H, A1_H8,           DiagonalRL.NULL);

    /** The null square. */
    public static final Square NULL = null;

    /** A generic square instance. */
    public static final Square AN_INSTANCE = B3;

    /** The number of squares. */
    public static final int NUMBER_OF = values().length;

    /** The list of the four corners. */
    private static final List<Square> CORNERS = Collections.unmodifiableList(Arrays.asList(A1, H1, H8, A8));

    /** The list of the four x-squares. */
    private static final List<Square> X_SQUARES = Collections.unmodifiableList(Arrays.asList(B2, G2, G7, B7));

    /** The map that links a corner with its own x-square. It is computed and initialized by the static block. */
    private static final Map<Square, Square> CORNER_TO_X_SQUARE_MAP;

    /** The map that links an x-square with its own corner. It is computed and initialized by the static block. */
    private static final Map<Square, Square> X_SQUARE_TO_CORNER_MAP;

    /** The inverse labels map. It is computed and initialized by the static block. */
    private static final Map<String, Square> INVERSE_LABELS;

    /** The labels map. It is computed and initialized by the static block. */
    private static final Map<Square, String> LABELS;

    /** The neighbor table. */
    private static final Map<Square, Map<Direction, Square>> NEIGHBOR_TABLE = neighborTable();

    /** The capable to flip direction table. */
    private static final Map<Square, List<Direction>> CAPABLE_TO_FLIP_DIRECTION_TABLE = capableToFlipDirectionTable();

    /** The list of files crossing each square. */
    private static final Map<Square, List<File>> FILES_FOR_SQUARE;

    /**
     * Contains the ordinal position of squares in files.
     * It has a length of the number os squares multiplyied by the number of axes,
     * so in practice 64 x 4 = 256.
     * The access is organized by the formula: Axis.NUMBER_OF * square.ordinal() + axis.ordinal()
     * It is computed in the static block.
     */
    private static final int[] SQUARE_ORDINAL_POSITION_IN_FILE = new int[Square.NUMBER_OF * Axis.NUMBER_OF];

    /** The square assignment to column table. */
    static final Map<Column, List<Square>> SQUARE_ASSIGNMENT_TO_COLUMN_TABLE = squareAssignmentToColumnTable();

    /** The square assignment to row table. */
    static final Map<Row, List<Square>> SQUARE_ASSIGNMENT_TO_ROW_TABLE = squareAssignmentToRowTable();

    /** The square assignment to diagonal lr table. */
    static final Map<DiagonalLR, List<Square>> SQUARE_ASSIGNMENT_TO_DIAGONAL_LR_TABLE = squareAssignmentToDiagonalLRTable();

    /** The square assignment to diagonal rl table. */
    static final Map<DiagonalRL, List<Square>> SQUARE_ASSIGNMENT_TO_DIAGONAL_RL_TABLE = squareAssignmentToDiagonalRLTable();

    /**
     * Static initialization block:
     * . - sets and initializes {@code LABELS} map
     * . - sets and initializes {@code INVERSE_LABELS} map
     * . - sets and initializes {@code CORNER_TO_X_SQUARE_MAP} map
     * . - sets and initializes {@code X_SQUARE_TO_CORNER_MAP} map
     * . - sets and initializes {@code FILES_FOR_SQUARE} map
     */
    static {
        /** Computes the LABELS and the INVERSE_LABELS maps. */
        final Map<Square, String> labelMap = new EnumMap<Square, String>(Square.class);
        final Map<String, Square> inverseLabelMap = new HashMap<String, Square>();
        for (Square sq : values()) {
            String label = sq.column.label() + sq.row.label();
            labelMap.put(sq, label);
            inverseLabelMap.put(label, sq);
        }
        LABELS = Collections.unmodifiableMap(labelMap);
        INVERSE_LABELS = Collections.unmodifiableMap(inverseLabelMap);

        /** Computes the CORNER_TO_X_SQUARE_MAP and the X_SQUARE_TO_CORNER_MAP maps*/
        final Map<Square, Square> cornerToXSquareMap = new EnumMap<Square, Square>(Square.class);
        final Map<Square, Square> xSquareToCornerMap = new EnumMap<Square, Square>(Square.class);
        for (int idx = 0; idx < CORNERS.size(); idx++) {
            cornerToXSquareMap.put(CORNERS.get(idx), X_SQUARES.get(idx));
            xSquareToCornerMap.put(X_SQUARES.get(idx), CORNERS.get(idx));
        }
        CORNER_TO_X_SQUARE_MAP = Collections.unmodifiableMap(cornerToXSquareMap);
        X_SQUARE_TO_CORNER_MAP = Collections.unmodifiableMap(xSquareToCornerMap);

        /** Computes the FILES_FOR_SQUARE map. */
        Map<Square, List<File>> filesForSquare = new EnumMap<Square, List<File>>(Square.class);
        for (final Square sq : Square.values()) {
            final List<File> filesCrossingTheSquare = new ArrayList<File>();
            filesCrossingTheSquare.add(sq.row());
            filesCrossingTheSquare.add(sq.column());
            filesCrossingTheSquare.add(sq.diagonalLR());
            filesCrossingTheSquare.add(sq.diagonalRL());
            filesForSquare.put(sq, Collections.unmodifiableList(filesCrossingTheSquare));
        }
        FILES_FOR_SQUARE = Collections.unmodifiableMap(filesForSquare);

        /** Computes the SQUARE_ORDINAL_POSITION_IN_FILE array. */
        int[] squareOrdinalPositionInFileCounter = new int[FileUtils.NUMBER_OF_FILES]; // it is initialized to zero.
        for (final Square sq : values()) {
            for (final Axis axis : Axis.values()) {
                // square and axis have to identify a file. A function in FileUtils has to be prepared.
                SQUARE_ORDINAL_POSITION_IN_FILE[Axis.NUMBER_OF * sq.ordinal() + axis.ordinal()]
                    = squareOrdinalPositionInFileCounter[0]; // MUST BE COMPLETED !!!!
                squareOrdinalPositionInFileCounter[0] += 1; // MUST BE COMPLETED
            }
        }
    }

    /**
     * Returns the list of the four corner squares.
     *
     * @return the four corners
     */
    public static List<Square> corners() { return CORNERS; }

    /**
     * Returns the square matching the specified label.
     * <p>
     * Parameter {@code label} cannot be {@code null}.
     * <p>
     * Throws an exception in case the label is not associated with a square.
     *
     * @param label the square's label
     * @return      the identified square
     * @throws NullPointerException     if the label parameter is null
     * @throws IllegalArgumentException if the label parameter is not valid
     */
    public static Square getInstance(final String label) {
        if (label == null) {
            throw new NullPointerException("Parameter label cannot be null.");
        }
        final Square sq = INVERSE_LABELS.get(label);
        if (sq == null) {
            throw new IllegalArgumentException("The specified label: <"
                                               + label + ">, does not match any square's label.");
        } else {
            return sq;
        }
    }

    /**
     * Returns the square instance matching the row and column parameters.
     * <p>
     * Returns {@code null} if either the row or the column parameters are {@code null}.
     *
     * @param row    square's row
     * @param column square's column
     * @return       the square pointed by row and column,
     *               or {@code null} in case row or column are themself {@code null}
     **/
    public static Square getInstance(final Row row, final Column column) {
        if (row == Row.NULL || column == Column.NULL) {
            return Square.NULL;
        } else {
            return Square.values()[Row.values().length * row.ordinal() + column.ordinal()];
        }
    }

    /**
     * Computes the neighborTable.
     *
     * @return the neighbor table
     */
    private static Map<Square, Map<Direction, Square>> neighborTable() {
        final Map<Square, Map<Direction, Square>> neighborTable
            = new EnumMap<Square, Map<Direction, Square>>(Square.class);
        for (Square sq : values()) {
            final Map<Direction, Square> snt = new EnumMap<Direction, Square>(Direction.class);
            for (final Direction dir : Direction.values()) {
                final Square neighbor = getInstance(sq.row().neighbor(dir),
                                                     sq.column().neighbor(dir));
                snt.put(dir, neighbor);
            }
            neighborTable.put(sq, Collections.unmodifiableMap(snt));
        }
        return Collections.unmodifiableMap(neighborTable);
    }

    /**
     * Computes the squareAssignmentTable for columns.
     * <p>
     * There are four methods that compute the square assignment to file table for the respective axes.
     * It is at best not elegant.
     * <p>
     * Probably the duplication can be avoided applying some "java generics magick".
     *
     * @return the square assignment table
     */
    private static final Map<Column, List<Square>> squareAssignmentToColumnTable() {
        final Map<Column, List<Square>> squareAssignmentTable
            = new EnumMap<Column, List<Square>>(Column.class);
        for (final Column c : Column.values()) {
            final List<Square> squares = new ArrayList<Square>();
            for (final Square sq : Square.values()) {
                if (sq.column() == c) {
                    squares.add(sq);
                }
            }
            squareAssignmentTable.put(c, Collections.unmodifiableList(squares));
        }
        return Collections.unmodifiableMap(squareAssignmentTable);
    }

    /**
     * Computes the squareAssignmentTable for rows.
     * <p>
     * See the comment on the first squareAssignmentTable methods: squareAssignmentToColumnTable().
     *
     * @return the square assignment table
     */
    private static final Map<Row, List<Square>> squareAssignmentToRowTable() {
        final Map<Row, List<Square>> squareAssignmentTable
            = new EnumMap<Row, List<Square>>(Row.class);
        for (final Row r : Row.values()) {
            final List<Square> squares = new ArrayList<Square>();
            for (final Square sq : Square.values()) {
                if (sq.row() == r) {
                    squares.add(sq);
                }
            }
            squareAssignmentTable.put(r, Collections.unmodifiableList(squares));
        }
        return Collections.unmodifiableMap(squareAssignmentTable);
    }

    /**
     * Computes the squareAssignmentTable for diagonals of type lr.
     * <p>
     * See the comment on the first squareAssignmentTable methods: squareAssignmentToColumnTable().
     *
     * @return the square assignment table
     */
    private static final Map<DiagonalLR, List<Square>> squareAssignmentToDiagonalLRTable() {
        final Map<DiagonalLR, List<Square>> squareAssignmentTable
            = new EnumMap<DiagonalLR, List<Square>>(DiagonalLR.class);
        for (final DiagonalLR d : DiagonalLR.values()) {
            final List<Square> squares = new ArrayList<Square>();
            for (final Square sq : Square.values()) {
                if (sq.diagonalLR() == d) {
                    squares.add(sq);
                }
            }
            squareAssignmentTable.put(d, Collections.unmodifiableList(squares));
        }
        return Collections.unmodifiableMap(squareAssignmentTable);
    }

    /**
     * Computes the squareAssignmentTable for diagonals of type lr.
     * <p>
     * See the comment on the first squareAssignmentTable methods: squareAssignmentToColumnTable().
     *
     * @return the square assignment table
     */
    private static final Map<DiagonalRL, List<Square>> squareAssignmentToDiagonalRLTable() {
        final Map<DiagonalRL, List<Square>> squareAssignmentTable
            = new EnumMap<DiagonalRL, List<Square>>(DiagonalRL.class);
        for (final DiagonalRL d : DiagonalRL.values()) {
            final List<Square> squares = new ArrayList<Square>();
            for (final Square sq : Square.values()) {
                if (sq.diagonalRL() == d) {
                    squares.add(sq);
                }
            }
            squareAssignmentTable.put(d, Collections.unmodifiableList(squares));
        }
        return Collections.unmodifiableMap(squareAssignmentTable);
    }

    /**
     * Computes the capableToFlipDirectionTable.
     *
     * @return the capable to flip direction table
     */
    private static Map<Square, List<Direction>> capableToFlipDirectionTable() {
        final Map<Square, List<Direction>> ct = new EnumMap<Square, List<Direction>>(Square.class);
        for (final Square sq : values()) {
            final List<Direction> capableToFlipdirections = new ArrayList<Direction>();
            for (final Direction dir : Direction.values()) {
                final Square neighbor = sq.neighbors().get(dir);
                if (neighbor != null && neighbor.neighbors().get(dir) != null) { capableToFlipdirections.add(dir); }
            }
            ct.put(sq, Collections.unmodifiableList(capableToFlipdirections));
        }
        return Collections.unmodifiableMap(ct);
    }

    /** The row field. */
    private final Row row;

    /** The column field. */
    private final Column column;

    /** The diagonalLR field. */
    private final DiagonalLR diagonalLR;

    /** The diagonalRL field. */
    private final DiagonalRL diagonalRL;

    /**
     * Enum constructor.
     *
     * @param row    square's row
     * @param column square's column
     */
    private Square(final Row row, final Column column, final DiagonalLR diagonalLR, final DiagonalRL diagonalRL) {
        this.row = row;
        this.column = column;
        this.diagonalLR = diagonalLR;
        this.diagonalRL = diagonalRL;
    }

    /**
     * Returns a map that has the directions as keys and the associated neighbor square as values.
     *
     * @return the square's neighbor map
     **/
    public List<Direction> capableToFlipDirections() {
        return CAPABLE_TO_FLIP_DIRECTION_TABLE.get(this);
    }

    /**
     * Returns the square's column.
     *
     * @return the square's column
     **/
    public Column column() { return column; }

    /**
     * Returns the relative corner when the square is an x-square, otherwise null.
     *
     * @return the associated corner, or null when the square is not an x-square
     */
    public Square cornerFor() {
        return X_SQUARE_TO_CORNER_MAP.get(this);
    }

    /**
     * Returns the square's diagonal from left-up to rigth-down.
     *
     * @return the square's diagonal lr
     **/
    public DiagonalLR diagonalLR() { return this.diagonalLR; }

    /**
     * Returns the square's diagonal from rigth-up to left-down.
     *
     * @return the square's diagonal rl
     **/
    public DiagonalRL diagonalRL() { return this.diagonalRL; }

    /**
     * Returns the list of files crossing the square.
     *
     * @return the four files crossing the square
     */
    public List<File> files() {
        return FILES_FOR_SQUARE.get(this);
    }

    /**
     * Returns the Hasegawa's naming for the edge squares. Returns
     * null if the square is not one of the identified squares.
     *
     * @return the Hasegawa square name, or null if the value is not defined
     **/
    public char getHasegawaLabel() {
        switch (this) {
        case B1:
        case G1:
        case H2:
        case H7:
        case G8:
        case B8:
        case A7:
        case A2:
            return 'C';
        case C1:
        case F1:
        case H3:
        case H6:
        case C8:
        case F8:
        case A6:
        case A3:
            return 'A';
        case D1:
        case E1:
        case H4:
        case H5:
        case E8:
        case D8:
        case A5:
        case A4:
            return 'B';
        case B2:
        case G2:
        case G7:
        case B7:
            return 'X';
        default:
            return ' ';
        }
    }

    public int ordinalPositionInFile(final Axis axis) {
        return SQUARE_ORDINAL_POSITION_IN_FILE[Axis.NUMBER_OF * this.ordinal() + axis.ordinal()];
    }

    /**
     * Returns true if the square is a corner, otherwise false.
     *
     * @return true or false if the square is either a corner or not
     */
    public boolean isCorner() {
        return CORNERS.contains(this);
    }

    /**
     * Returns true if the square is an X square, otherwise false.
     *
     * @return true or false if the square is either an X square or not
     */
    public boolean isXSquare() {
        return X_SQUARES.contains(this);
    }

    /**
     * Returns the square's label.
     *
     * @return the square's label
     **/
    public String label() {
        return LABELS.get(this);
    }

    /**
     * Returns a map that has the directions as keys and the associated neighbor square as values.
     *
     * @return the square's neighbor map
     **/
    public Map<Direction, Square> neighbors() {
        return NEIGHBOR_TABLE.get(this);
    }

    /**
     * Returns the square's row.
     *
     * @return the square's row
     **/
    public Row row() { return row; }

    /**
     * Returns the relative x-square when the square is a corner, otherwise null.
     *
     * @return the associated x-square, or null when the square is not a corner
     */
    public Square xSquareFor() {
        return CORNER_TO_X_SQUARE_MAP.get(this);
    }

}
