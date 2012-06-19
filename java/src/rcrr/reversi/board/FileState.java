/*
 *  FileState.java
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;

/**
 * A file configuration or state.
 */
public final class FileState {

    private static final int MAX_ORDER = FileUtils.FILE_MAX_LENGTH;

    private static final int[] MAX_INDEX = new int[MAX_ORDER];

    private static final FileState[][] STATES = new FileState[MAX_ORDER][];

    static {

        for (int i = 0; i < MAX_ORDER; i++) {
            MAX_INDEX[i] = BigInteger.valueOf(3).pow(i).intValue();
        }
        ; // computes STATES
    }

    /**
     * Verify if shifting the arrays by one save the subtraction by one.
     */
    public FileState valueOf(final int order, final int index) {
        if (order < 1 || order > MAX_ORDER) { throw new IndexOutOfBoundsException("Parameter order is invalid. order = " + order); }
        if (index < 1 || index > MAX_INDEX[order]) { throw new IndexOutOfBoundsException("Parameter index is invalid. index = " + index); }
        return STATES[order - 1][index - 1];
    }

    private List<FileState> computeConfigurations(final int fileLength) {
        return new ArrayList<FileState>();
    }

    private final List<SquareState> configuration;
    private final int order;
    private final int index;

    private FileState(final int order, final int index) {
        this.order = order;
        this.index = index;
        this.configuration = new ArrayList<SquareState>();
    }

}