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

    public static final class PossibilityValue {
	private final double possibility;
	private final int value;
	public PossibilityValue(final double possibility, final int value) {
	    this.possibility = possibility;
	    this.value = value;
	}
	public double possibility() { return this.possibility; }
	public int value() { return this.value; }
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


    public static final Integer[][] STATIC_EDGE_TABLE = { { null,    0, -2000 },
							  {  700, null,  null },
							  { 1200,  200,   -25 },
							  { 1000,  200,    75 },
							  { 1000,  200,    50 },
							  { 1000,  200,    50 },
							  { 1000,  200,    75 },
							  { 1200,  200,   -25 },
							  {  700, null,  null },
							  { null,    0, -2000 } };

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
        EDGE_TABLE = Collections.unmodifiableList(initEdgeTable());
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

    private static final int ITERATIONS_FOR_IMPROVING_EDGE_TABLE = 5;

    public static final List<Integer> initEdgeTable() {
        final List<Integer> edgeTable = new ArrayList<Integer>(EDGE_TABLE_SIZE);
        for (int idx = 0; idx < EDGE_TABLE_SIZE; idx++) {
            edgeTable.add(0);
        }
        /** Initialize the static values. */
        for (int nPieces = 0; nPieces < 11; nPieces++) {
            mapEdgeNPieces(new Fn0() {
                    public void funcall(final Board board, final int index) {
                        edgeTable.set(index, staticEdgeStability(Player.BLACK, board));
                    }
                },
                Player.BLACK,
                Board.initialBoard(),
                nPieces,
                TOP_EDGE,
                0);
        }
        /** Now iterate five times trying to improve: */
        for (int i = 0; i < ITERATIONS_FOR_IMPROVING_EDGE_TABLE; i++) {
            /** Do the indexes with more pieces first. From 9 to 1. */
            for (int nPieces = 9; nPieces > 0; nPieces--) {
                mapEdgeNPieces(new Fn0() {
                        public void funcall(final Board board, final int index) {
                            edgeTable.set(index, possibleEdgeMovesValue(Player.BLACK,
                                                                            board,
                                                                            index));
                        }
                    },
                    Player.BLACK,
                    Board.initialBoard(),
                    nPieces,
                    TOP_EDGE,
                    0);
            }
        }
        return edgeTable;
    }
 
    // MUST BE COMPLETED!
    /**
     * Compute this edge's static stability
     */
    public static final int staticEdgeStability(final Player player, final Board board) {
        int sum = 0;
	int i = 0;
	for (Square sq : TOP_EDGE) {
	    int addendum;
	    if (board.get(sq) == SquareState.EMPTY) {
		addendum = 0;
	    } else if (board.get(sq) == player.color()) {
		addendum = STATIC_EDGE_TABLE[i][pieceStability(board, sq)];
	    } else {
		addendum = - STATIC_EDGE_TABLE[i][pieceStability(board, sq)];
	    }
	    sum += addendum;
	    i++;
	}
        return sum;
    }

    public static final int pieceStability(final Board board, final Square sq) {
	final int stable = 0;
	final int semiStable = 1;
	final int unstable = 2;

	int stability = 0;
	if (sq.isCorner()) {
	    stability = stable;
	} else if (sq.isXSquare()) {
	    stability = (board.get(sq.cornerFor()) == SquareState.EMPTY) ? unstable : semiStable;
	} else {
	    stability = 1; // MUST BE CALCULATED
	}
	return stability;
    }

    // MUST BE COMPLETED!
    /**
     * Consider all possible edge moves.
     * Combine their values into a single number.
     */
    public static final int possibleEdgeMovesValue(final Player player, final Board board, final int index) {
        int result = 0;
	combineEdgeMoves();
        return result;
    }

    // MUST BE COMPLETED!
    public static final PossibilityValue possibleEdgeMove(final Player player, final Board board, final Square sq) {
	return new PossibilityValue(edgeMoveProbability(),
				    // edgeTable
				    0 // a dummy value that must be replaced
				    );
    }

    // MUST BE COMPLETED!
    public static final int combineEdgeMoves() {
        int result = 0;
        return result;
    }

    public static final double edgeMoveProbability() {
	double result = 0.0;
	return result;
    }

    private static interface Fn0 {
        public void funcall(final Board board, final int index);
    }

    /**
     * Call method fn on all configurations for an edge having n pieces.
     * The function 
     */
    // MUST BE COMPLETED!
    public static final void mapEdgeNPieces(final Fn0 fn,
                                            final Player player,
                                            Board board,
                                            final int n,
                                            final List<Square> squares,
                                            final int index) {
        /** Index counts 1 for player; 2 for opponent. */
        if (squares.size() < n) { return; }
        else if (squares.size() == 0) { fn.funcall(board, index); }
        else {
            int index3 = 3 * index;
            Square sq = squares.get(0);
            mapEdgeNPieces(fn, player, board, n, squares.subList(1, squares.size()), index3);
            if (n > 0 && board.get(sq) == SquareState.EMPTY) {
                board = new Board.Builder(board).withSquare(sq, player.color()).build();
                mapEdgeNPieces(fn, player, board, n -1, squares.subList(1, squares.size()), index3 + 1);
                board = new Board.Builder(board).withSquare(sq, player.opponent().color()).build();
                mapEdgeNPieces(fn, player, board, n -1, squares.subList(1, squares.size()), index3 + 2);
                board = new Board.Builder(board).withSquare(sq, SquareState.EMPTY).build();
            }
        }
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
     * @return         the position mobility evaluation
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

    /**
     * Total edge evaluation for the game position.
     * <p>
     * See PAIP 18.12 page 639.
     *
     * @param position the game position to evaluate
     * @return         total edge evaluation for the game position
     * @throws NullPointerException if parameter {@code position} is null
     */
    public final int edgeStability(final GamePosition position) {
        int evaluation = 0;
        for (List<Square> edge : EDGE_AND_X_LISTS) {
            evaluation += EDGE_TABLE.get(edgeIndex(position.player(), position.board(), edge));
        }
        return evaluation;
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
