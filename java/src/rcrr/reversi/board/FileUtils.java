/*
 *  FileUtils.java
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

import java.lang.reflect.Method;

import java.util.EnumMap;
import java.util.Map;
import java.util.Collections;
import java.util.List;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

/**
 * Static methods related to the {@code File} interface.
 */
final class FileUtils {

    private static final Set<Class> FILE_TYPES
        = Collections.unmodifiableSet(new HashSet<Class>() {{
                    add(Row.class);
                    add(Column.class);
                    // Diagonals have to be added here.
                }});

    private static final List<File> FILES;

    private static final Map<Square, List<File>> FILES_FOR_SQUARE;

    static {
        List<File> files = new ArrayList<File>();
        for (final Class c : FILE_TYPES) {
            try {
                for (final File file : (File[]) c.getEnumConstants()) {
                    files.add(file);
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        FILES = Collections.unmodifiableList(files);


        Map<Square, List<File>> filesForSquare = new EnumMap<Square, List<File>>(Square.class);
        for (final Square sq : Square.values()) {
            final List<File> filesCrossingTheSquare = new ArrayList<File>();
            filesCrossingTheSquare.add(Row.R1);
            filesCrossingTheSquare.add(Column.A);
            filesCrossingTheSquare.add(DiagonalLR.A1_H8);
            // ... has to compute the file list
            filesForSquare.put(sq, Collections.unmodifiableList(filesCrossingTheSquare));
        }
        FILES_FOR_SQUARE = Collections.unmodifiableMap(filesForSquare);
    }

    public static final List<File> files(final Square square) {
        return FILES_FOR_SQUARE.get(square);
    }

    public static final Set<Class> fileTypes() {
        return FILE_TYPES;
    }

    /** Class constructor. */
    private FileUtils() { }

}
