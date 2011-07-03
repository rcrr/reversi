/*
 *  Iago.java
 *
 *  Copyright (c) 2011 Roberto Corradini. All rights reserved.
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

package rcrr.reversi;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

/**
 * Iago is an advanced strategy, that implements the features described in the PAIP book 18.12.
 * See for reference the paper:
 * <i>"Paul S. Rosenbloom. A World-Championship-Level Othello Program. Artif. Intell., 1982: 279~320"</i>
 * <p>
 * <i>"Kay-Fu Lee, S. Mahajan. The development of a world class Othello program. Artif. Intell., 1990: 21~36"</i>
 * <p>
 * The strategy mixes the concepts of mobility and edge stability ...
 */
public class Iago implements EvalFunction {

    public static final class Mobility {
        private final int current;
        private final int potential;
        public Mobility(final int current, final int potential) {
            this.current = current;
            this.potential = potential;
        }
        public int current() { return this.current; }
        public int potential() { return this.potential; }
    }

    public enum SquareValue {
        PLAYER,
        OPPONENT,
        EMPTY;
    }

    public static final int SQUARE_VALUE_LENGTH = SquareValue.values().length;

    public static final List<Square> TOP_EDGE
        = Collections.unmodifiableList(Arrays.asList(Square.B2, Square.A1, Square.B1, Square.C1, Square.D1,
                                                     Square.E1, Square.F1, Square.G1, Square.H1, Square.G2));

    public static final List<Square> BOTTOM_EDGE
        = Collections.unmodifiableList(Arrays.asList(Square.B7, Square.A8, Square.B8, Square.C8, Square.D8,
                                                     Square.E8, Square.F8, Square.G8, Square.H8, Square.G7));

    public static final List<Square> LEFT_EDGE
        = Collections.unmodifiableList(Arrays.asList(Square.B2, Square.A1, Square.A2, Square.A3, Square.A4,
                                                     Square.A5, Square.A6, Square.A7, Square.A8, Square.B7));

    public static final List<Square> RIGHT_EDGE
        = Collections.unmodifiableList(Arrays.asList(Square.G2, Square.H1, Square.H2, Square.H3, Square.H4,
                                                     Square.H5, Square.H6, Square.H7, Square.H8, Square.G7));

    public static final int EDGE_SIZE = TOP_EDGE.size();

    public static final List<List<Square>> EDGE_AND_X_LISTS;

    /** The table size have to be 59,049. */
    private static final int EDGE_TABLE_SIZE = new Double(Math.pow(SQUARE_VALUE_LENGTH,
                                                                   EDGE_SIZE)).intValue();


    private static final List<Integer> EDGE_TABLE;

    static {
        /** Computes EDGE_AND_X_LISTS. */
        List<List<Square>> tempEdgeAndXLists = new ArrayList<List<Square>>();
        tempEdgeAndXLists.add(TOP_EDGE);
        tempEdgeAndXLists.add(BOTTOM_EDGE);
        tempEdgeAndXLists.add(LEFT_EDGE);
        tempEdgeAndXLists.add(RIGHT_EDGE);
        EDGE_AND_X_LISTS = Collections.unmodifiableList(tempEdgeAndXLists);

        /** Computes EDGE_TABLE. */
        List<Integer> tempEdgeTable = new ArrayList<Integer>(EDGE_TABLE_SIZE);
        // EDGE_TABLE must be calculated here ....
        EDGE_TABLE = Collections.unmodifiableList(tempEdgeTable);
    }

    public static int edgeIndex(final Player player, final Board board, final List<Square> edge) {
        int index = 0;
        for (Square square : edge) {
            SquareState state = board.get(square);
            int incr;
            if (state == SquareState.EMPTY) {
                incr = 0;
            } else if (state == player.color()) {
                incr = 1;
            } else {
                incr = 2;
            }
            index = (index * SQUARE_VALUE_LENGTH) + incr;
        }
        return index;
    }

    /**
     * Class constructor.
     */
    public Iago() { }

    /**
     *
     * @param position the game position to evaluate
     * @return the position value
     * @throws NullPointerException if parameter {@code position} is null
     */
    public final int eval(final GamePosition position) {
        int result = 0;
        return result;
    }

    /**
     * Current mobility is the number of legal moves.
     * Potential mobility is the number of blank squares adjacent to an opponent that are not legal moves.
     * Returns current and potential mobility for the player.
     * <p>
     * See PAIP 18.12 pages 637-638.
     *
     * @param position the game position to evaluate
     * @return the position mobility evaluation
     * @throws NullPointerException if parameter {@code position} is null
     */
    public final Mobility mobility(final GamePosition position) {
        if (position == null) { throw new NullPointerException("Parameter position cannot be null."); }

        int current = 0;
        int potential = 0;

        Board board = position.board();
        Player player = position.player();
        Player opp = player.opponent();

        for (Square square : Square.values()) {
            if (board.get(square) == SquareState.EMPTY) {
                if (board.isLegal(square, player)) {
                    current++;
                } else if (isPotentialMove(board, square, opp)) {
                    potential ++;
                }
            }
        }
        return new Mobility(current, potential);
    }

    private boolean isPotentialMove(final Board board,
                                    final Square square,
                                    final Player opp) {
        for (Square neighbor : square.neighbors().values()) {
            if (board.get(neighbor) == opp.color()) {
                return true;
            }
        }
        return false;
    }

}
