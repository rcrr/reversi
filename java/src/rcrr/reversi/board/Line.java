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

    private static final Map<File, Line> LINE_TO_FILE_MAP = new HashMap<File, Line>();

    private static final Map<Square, List<Line>> LINES_FOR_SQUARE;

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

    }

    /** The axis field. */
    private final Axis axis;

    /** The squares field. */
    private final List<Square> squares;

    /**
     * Enum constructor.
     *
     * @param squares the list of squares
     */
    private Line(final List<Square> squares, final Axis axis) {
        this.squares = squares;
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

    static enum SquareTransition {

        NO_TRANSITION(0),
        EMPTY_TO_BLACK(0), // This is a transition that happens only to the square where the black's player moves. It is handled by a special factor ....
        WHITE_TO_BLACK(-1);

        private final int delta;

        private SquareTransition(final int delta) {
            this.delta = delta;
        }

        public final int delta() { return this.delta; }

    }


    static class FileIndex {

        private static final Map<File, List<FileIndex>> FILE_INDEX_MAP;

        public static FileIndex valueOf(final File file, final int index) {
            return FILE_INDEX_MAP.get(file).get(index);
        }

        public static Map<File, List<FileIndex>> fileIndexMap() {
            return FILE_INDEX_MAP;
        }

        static {

            /**
             * Computes FILE_INDEX_MAP map.
             */
            final Map<File, List<FileIndex>> transientFileIndexMap = new HashMap<File, List<FileIndex>>();
            for (final File file : FileUtils.files()) {
                final List<FileIndex> transientFileIndexList = new ArrayList<FileIndex>();
                for (int index = 0; index <= FileState.indexBoundary(Line.getInstance(file).squares().size()); index++) {
                    final FileIndex fileIndex = new FileIndex(file, index);
                    transientFileIndexList.add(fileIndex);
                }
                transientFileIndexMap.put(file, Collections.unmodifiableList(transientFileIndexList));
            }
            FILE_INDEX_MAP = Collections.unmodifiableMap(transientFileIndexMap);

        }

        private final File file;
        private final int index;
        FileIndex(final File file, final int index) {
            this.file = file;
            this.index = index;
        }

        public FileState fileState() {
            return FileState.valueOf(Line.getInstance(file).order(), index);
        }

        public List<SquareState> configuration() {
            return fileState().configuration();
        }

        public Map <Integer, FileIndex> legalMoves() {
            final HashMap<Integer, FileIndex> legalMoves = new HashMap<Integer, FileIndex>();
            for (final Map.Entry<Integer, Integer> entry : fileState().legalMoves().entrySet()) {
                legalMoves.put(entry.getKey(), valueOf(file, entry.getValue()));
            }
            return legalMoves;
        }

        public File file() {
            return file;
        }

        public int index() {
            return index;
        }

        public FileIndex flip() {
            return valueOf(file, fileState().flip().index());
        }

        /**
         * Returns a {@code String} representing the {@code FileIndex} object.
         *
         * @return a {@code String} representing the file index
         */
        @Override public String toString() {
            return String.format("[file=%s, index=%d]", file(), index());
        }

    }


    static class FileIndexMove {

        private static final Map<FileIndex, Map<Integer, FileIndexMove>> FILE_INDEX_MOVE_MAP;

        public static FileIndexMove valueOf(final FileIndex fileIndex, final int move) {
            return FILE_INDEX_MOVE_MAP.get(fileIndex).get(move);
        }

        static {

            /**
             * Computes FILE_INDEX_MOVE_MAP map.
             */
            final Map<FileIndex, Map<Integer, FileIndexMove>> transientFileIndexMoveMap = new HashMap<FileIndex, Map<Integer, FileIndexMove>>();
            for (final Map.Entry<File, List<FileIndex>> entry : FileIndex.fileIndexMap().entrySet()) {
                for (final FileIndex fileIndex : entry.getValue()) {
                    final Map<Integer, FileIndexMove> transientInnerMap = new HashMap<Integer, FileIndexMove>();
                    for (final Map.Entry<Integer, FileIndex> move : fileIndex.legalMoves().entrySet()) {
                        transientInnerMap.put(move.getKey(), new FileIndexMove(fileIndex, move.getKey()));
                    }
                    transientFileIndexMoveMap.put(fileIndex, Collections.unmodifiableMap(transientInnerMap));
                }
            }
            FILE_INDEX_MOVE_MAP = Collections.unmodifiableMap(transientFileIndexMoveMap);

        }

        private final FileIndex fileIndex;
        private final int move;
        FileIndexMove(final FileIndex fileIndex, final int move) {
            this.fileIndex = fileIndex;
            this.move = move;
        }

        /**
         * Transitions are:
         * Empty to Black and White to Black .... all the other are not possible because we are evaluating only moves played by the black.
         */
        public List<SquareTransition> fileTransitions() {
            final List<SquareTransition> transitions = new ArrayList<SquareTransition>();
            final List<SquareState> from = fileIndex.configuration();
            final List<SquareState> to = fileIndex.legalMoves().get(move).configuration();
            for (int i = 0; i < from.size(); i++) {
                SquareTransition st;
                final SquareState fss = from.get(i);
                final SquareState tss = to.get(i);
                if (fss == tss) {
                    st = SquareTransition.NO_TRANSITION;
                } else if (fss == SquareState.EMPTY && tss == SquareState.BLACK) {
                    st = SquareTransition.EMPTY_TO_BLACK;
                } else if (fss == SquareState.WHITE && tss == SquareState.BLACK) {
                    st = SquareTransition.WHITE_TO_BLACK;
                } else {
                    throw new RuntimeException("Square transition not allowed. from=" + fss + ", to=" + tss);
                }
                transitions.add(st);
            }
            return Collections.unmodifiableList(transitions);
        }

        public int[] getDeltas() {
            final int[] deltas = new int[FileUtils.NUMBER_OF_FILES];
            final File file = fileIndex.file();
            int squareOrdinal = 0;
            for (final SquareTransition st : fileTransitions()) {
                final Square sq = Line.getInstance(file).squares().get(squareOrdinal);
                for (final Line affectedLine : Line.linesForSquare(sq)) {
                    final File affectedFile = affectedLine.file();
                    if (affectedFile != null) {
                        int delta = st.delta() * FileState.fileTransferMatrix(fileIndex.file(), squareOrdinal, affectedFile);
                        deltas[FileUtils.files().indexOf(affectedFile)] += delta;
                    }
                }
                squareOrdinal++;
            }
            return deltas;
        }

        /**
         * Returns a {@code String} representing the {@code FileIndexMove} object.
         *
         * @return a {@code String} representing the file index move
         */
        @Override public String toString() {
            return String.format("[move=%d, fileIndex=%s]", move, fileIndex);
        }

    }


}
