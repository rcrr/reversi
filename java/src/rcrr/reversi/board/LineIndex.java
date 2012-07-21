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

    private static final Map<File, List<LineIndex>> FILE_INDEX_MAP;

    public static LineIndex valueOf(final File file, final int index) {
        return FILE_INDEX_MAP.get(file).get(index);
    }

    public static Map<File, List<LineIndex>> fileIndexMap() {
        return FILE_INDEX_MAP;
    }

    static {

        /**
         * Computes FILE_INDEX_MAP map.
         */
        final Map<File, List<LineIndex>> transientLineIndexMap = new HashMap<File, List<LineIndex>>();
        for (final File file : FileUtils.files()) {
            final List<LineIndex> transientLineIndexList = new ArrayList<LineIndex>();
            for (int index = 0; index <= LineState.indexBoundary(Line.getInstance(file).squares().size()); index++) {
                final LineIndex fileIndex = new LineIndex(file, index);
                transientLineIndexList.add(fileIndex);
            }
            transientLineIndexMap.put(file, Collections.unmodifiableList(transientLineIndexList));
        }
        FILE_INDEX_MAP = Collections.unmodifiableMap(transientLineIndexMap);

    }

    private final File file;
    private final int index;
    LineIndex(final File file, final int index) {
        this.file = file;
        this.index = index;
    }

    public LineState fileState() {
        return LineState.valueOf(Line.getInstance(file).order(), index);
    }

    public List<SquareState> configuration() {
        return fileState().configuration();
    }

    public Map <Integer, LineIndex> legalMoves() {
        final HashMap<Integer, LineIndex> legalMoves = new HashMap<Integer, LineIndex>();
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

    public LineIndex flip() {
        return valueOf(file, fileState().flip().index());
    }

    /**
     * Returns a {@code String} representing the {@code LineIndex} object.
     *
     * @return a {@code String} representing the file index
     */
    @Override public String toString() {
        return String.format("[file=%s, index=%d]", file(), index());
    }

}
