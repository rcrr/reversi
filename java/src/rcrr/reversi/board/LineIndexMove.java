/*
 *  LineIndexMove.java
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

class LineIndexMove {

    private static final Map<LineIndex, Map<Integer, LineIndexMove>> LINE_INDEX_MOVE_MAP;

    public static LineIndexMove valueOf(final LineIndex fileIndex, final int move) {
        return LINE_INDEX_MOVE_MAP.get(fileIndex).get(move);
    }

    static {

        /**
         * Computes LINE_INDEX_MOVE_MAP map.
         */
        final Map<LineIndex, Map<Integer, LineIndexMove>> transientLineIndexMoveMap = new HashMap<LineIndex, Map<Integer, LineIndexMove>>();
        for (final Map.Entry<File, List<LineIndex>> entry : LineIndex.fileIndexMap().entrySet()) {
            for (final LineIndex fileIndex : entry.getValue()) {
                final Map<Integer, LineIndexMove> transientInnerMap = new HashMap<Integer, LineIndexMove>();
                for (final Map.Entry<Integer, LineIndex> move : fileIndex.legalMoves().entrySet()) {
                    transientInnerMap.put(move.getKey(), new LineIndexMove(fileIndex, move.getKey()));
                }
                transientLineIndexMoveMap.put(fileIndex, Collections.unmodifiableMap(transientInnerMap));
            }
        }
        LINE_INDEX_MOVE_MAP = Collections.unmodifiableMap(transientLineIndexMoveMap);

    }

    private final LineIndex fileIndex;
    private final int move;
    LineIndexMove(final LineIndex fileIndex, final int move) {
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
                    int delta = st.delta() * LineState.fileTransferMatrix(fileIndex.file(), squareOrdinal, affectedFile);
                    deltas[FileUtils.files().indexOf(affectedFile)] += delta;
                }
            }
            squareOrdinal++;
        }
        return deltas;
    }

    /**
     * Returns a {@code String} representing the {@code LineIndexMove} object.
     *
     * @return a {@code String} representing the file index move
     */
    @Override public String toString() {
        return String.format("[move=%d, fileIndex=%s]", move, fileIndex);
    }

}
