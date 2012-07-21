/*
 *  FileIndex.java
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

class FileIndex {

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
