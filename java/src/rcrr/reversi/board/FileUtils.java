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

import java.math.BigInteger;

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

    private static final List<File> FILES;

    static final int NUMBER_OF_FILES;

    static final int FILE_MAX_LENGTH = 8;

    static final int[] FILE_INDEX_COEFFICIENT = new int[FILE_MAX_LENGTH];

    static final int[] FILE_INDEX_BY_SQUARE_AND_AXIS = new int[Square.NUMBER_OF * Axis.NUMBER_OF];

    static {

        /**
         * Computes the FILE list.
         */
        List<File> files = new ArrayList<File>();
        for (final Axis axis : Axis.values()) {
            final Class c = axis.relatedEnum();
            try {
                for (final File file : (File[]) c.getEnumConstants()) {
                    files.add(file);
                }
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }
        FILES = Collections.unmodifiableList(files);

        /**
         * Compute the NUMBER_OF_FILES integer;
         */
        NUMBER_OF_FILES = FILES.size();

        /**
         * Computes the FILE_INDEX_COEFFICIENT array.
         */
        for (int i = 0; i < FILE_MAX_LENGTH; i++) {
            FILE_INDEX_COEFFICIENT[i] = BigInteger.valueOf(3).pow(i).intValue();
        }

        for (final Square sq : Square.values()) {
            for (final Axis axis : Axis.values()) {
                // ma non è meglio fare un metodo in square che dato l'axis mi ritorna il file?
            }
        }
        //FILE_INDEX_BY_SQUARE_AND_AXIS

    }

    public static final List<File> files() {
        return FILES;
    }

    public static final File valueOf(final Square square, final Axis axis) {
        final int index = Axis.NUMBER_OF * square.ordinal() + axis.ordinal();
        return FILES.get(FILE_INDEX_BY_SQUARE_AND_AXIS[index]);
    }

    /** Class constructor. */
    private FileUtils() { }

}
