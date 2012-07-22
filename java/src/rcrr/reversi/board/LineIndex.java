/*
 *  LineIndex.java
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

class LineIndex {

    private static final Map<Line, List<LineIndex>> LINE_INDEX_MAP;

    public static LineIndex valueOf(final Line line, final int index) {
        return LINE_INDEX_MAP.get(line).get(index);
    }

    public static List<LineIndex> lineIndexes(final Line line) {
        return LINE_INDEX_MAP.get(line);
    }

    static {

        /**
         * Computes LINE_INDEX_MAP map.
         */
        final Map<Line, List<LineIndex>> transientLineIndexMap = new HashMap<Line, List<LineIndex>>();
        for (final Line line : Line.values()) {
            final List<LineIndex> transientLineIndexList = new ArrayList<LineIndex>();
            for (int index = 0; index <= LineState.indexBoundary(line.order()); index++) {
                final LineIndex lineIndex = new LineIndex(line, index);
                transientLineIndexList.add(lineIndex);
            }
            transientLineIndexMap.put(line, Collections.unmodifiableList(transientLineIndexList));
        }
        LINE_INDEX_MAP = Collections.unmodifiableMap(transientLineIndexMap);

    }

    private final Line line;
    private final int index;
    LineIndex(final Line line, final int index) {
        this.line = line;
        this.index = index;
    }

    public LineState lineState() {
        return LineState.valueOf(line.order(), index);
    }

    public List<SquareState> configuration() {
        return lineState().configuration();
    }

    public Map <Square, LineIndex> legalMoves() {
        final Map<Square, LineIndex> legalMoves = new EnumMap<Square, LineIndex>(Square.class);
        for (final Map.Entry<Integer, Integer> move : lineState().legalMoves().entrySet()) {
            final int moveOrdinalPositionInLine = move.getKey();
            final int newIndex = move.getValue();
            final Square moveSquare = line().squares().get(moveOrdinalPositionInLine);
            legalMoves.put(moveSquare, valueOf(line(), newIndex));
        }
        return Collections.unmodifiableMap(legalMoves);
    }

    public File file() {
        return line.file();
    }

    public Line line() {
        return this.line;
    }

    public int index() {
        return index;
    }

    public LineIndex flip() {
        return valueOf(line(), lineState().flip().index());
    }

    /**
     * Returns a {@code String} representing the {@code LineIndex} object.
     *
     * @return a {@code String} representing the file index
     */
    @Override public String toString() {
        return String.format("[line=%s, index=%d]", line(), index());
    }

}
