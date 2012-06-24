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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A file configuration or state.
 *
 * 
 */
public final class FileState {

    private static final int[] DIRECTIONS = { -1, +1};

    /**
     * The maximum order of a file.
     */
    private static final int MAX_ORDER = FileUtils.FILE_MAX_LENGTH;
    //private static final int MAX_ORDER = 4;

    /**
     * For each order, the maximum index reachable.
     */
    private static final int[] MAX_INDEX = new int[MAX_ORDER + 1];

    private static final int[] POSITION_COEFFICIENTS = new int[MAX_ORDER + 1];

    /**
     * Two dimension array that contains all the possible FileState instances.
     * The first dimension is driven by the order. Orders range between 0 and MAX_ORDER + 1.
     * The second dimension is the index of the file state.
     */
    private static final FileState[][] STATES = new FileState[MAX_ORDER + 1][];

    static {

        for (int i = 0; i < MAX_ORDER + 1; i++) {
            POSITION_COEFFICIENTS[i] = BigInteger.valueOf(3).pow(i).intValue();
            //System.out.println("i, POSITION_COEFFICIENTS[i] = " + i + ", " + POSITION_COEFFICIENTS[i]);
        }

        /**
         *
         */
        MAX_INDEX[0] = 0;
        STATES[0] = null;
        for (int i = 1; i < MAX_ORDER + 1; i++) {
            MAX_INDEX[i] = POSITION_COEFFICIENTS[i] - 1;
            //System.out.println("i, MAX_INDEX[i] = " + i + ", " + MAX_INDEX[i]);
            STATES[i] = new FileState[MAX_INDEX[i] + 1];
            for (int j = 0; j <= MAX_INDEX[i]; j++) {
                STATES[i][j] = new FileState(i, j);
            }
        }

        //System.out.println("Static block completed.");

    }

    /**
     * Verify if shifting the arrays by one save the subtraction by one.
     */
    public static FileState valueOf(final int order, final int index) {
        if (order < 1 || order > MAX_ORDER) { throw new IndexOutOfBoundsException("Parameter order is invalid. order = " + order); }
        if (index < 0 || index > MAX_INDEX[order]) { throw new IndexOutOfBoundsException("Parameter index is invalid. index = " + index); }
        return STATES[order][index];
    }

    private List<FileState> computeConfigurations(final int fileLength) {
        return new ArrayList<FileState>();
    }

    private final SquareState[] configuration;
    private final Map<Integer, Integer> legalMoves;
    private final int order;
    private final int index;

    private FileState(final int order, final int index) {
        this.order = order;
        this.index = index;
        this.configuration = new SquareState[order];

        /**
         * Computes the configuration field.
         */
        int remainder = index;
        for (int i = order - 1; i >= 0; i--) {
            int config = remainder / POSITION_COEFFICIENTS[i];
            if (config == 0) { configuration[i] = SquareState.EMPTY; }
            if (config == 1) { configuration[i] = SquareState.BLACK; }
            if (config == 2) { configuration[i] = SquareState.WHITE; }
            //System.out.println("--> i= " + i + ", remainder=" + remainder + ", config=" + config);
            remainder = remainder % POSITION_COEFFICIENTS[i];
        }

        legalMoves = computeLegalMoves();

        //System.out.println("order=" + order + ", index=" + index + ", configuration=" + printConfiguration(configuration) + ", legalMoves=" + legalMoves);

    }

    private Map<Integer, Integer> computeLegalMoves() {
        final Map<Integer, Integer> legalMoves = new HashMap<Integer, Integer>();
        for (int move = 0; move < configuration.length; move++) {
            if (isLegal(move)) {
                legalMoves.put(move, computeIndex(makeMove(move)));
            }
        }
        return legalMoves;
    }

    private static int computeIndex(final SquareState[] configuration) {
        int index = 0;
        for (int i = 0; i < configuration.length; i++) {
            index += configuration[i].ordinal() * POSITION_COEFFICIENTS[i];
        }
        return index;
    }

    private SquareState[] makeMove(final int move) {
        SquareState[] newConfiguration = configuration.clone();
        newConfiguration[move] = SquareState.BLACK;
        for (final int dir : DIRECTIONS) {
            int bracketer = wouldFlip(move, dir);
            if (bracketer != -1) {
                for (int square = move + dir; true; square = square + dir) {
                    if (square == bracketer) { break; }
                    newConfiguration[square] = SquareState.BLACK;
                }
            }
        }
        return newConfiguration;
    }

    private boolean isLegal(final int move) {
        if (configuration[move] != SquareState.EMPTY) { return false; }
        for (int dir : DIRECTIONS) {
            if (wouldFlip(move, dir) != -1) { return true; }
        }
        return false;
    }

    private int wouldFlip(final int move, final int dir) {
        final int neighbor = move + dir;
        if (neighbor < 0 || neighbor >= configuration.length) { return -1; }
        final int next = neighbor + dir;
        if (next < 0 || next >= configuration.length) { return -1; }
        final SquareState neighborColor = configuration[neighbor];
        switch (neighborColor) {
        case EMPTY: return -1;
        case BLACK: return -1;
        case WHITE: return findBracketingPiece(next, dir);
        default: throw new RuntimeException("Unexpected square color. Got " + neighborColor);
        }
    }

    private int findBracketingPiece(final int square, final int dir) {
        final SquareState squareColor = configuration[square];
        switch (squareColor) {
        case EMPTY: return -1;
        case BLACK: return square;
        case WHITE:
            final int next = square + dir;
            if (next < 0 || next >= configuration.length) { return -1; }
            return findBracketingPiece(next, dir);
        default: throw new RuntimeException("Unexpected square color. Got " + squareColor);
        }        
    }

    private static String printConfiguration(final SquareState[] configuration) {
        final StringBuffer sb = new StringBuffer(".");
        for (int i = 0; i < configuration.length; i++) {
            sb.append(configuration[i] + ".");
        }
        return sb.toString();
    }

}