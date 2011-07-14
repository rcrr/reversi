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
import java.util.Comparator;

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

    public static final class ProbabilityValue {
	private final double probability;
	private final int value;
	public ProbabilityValue(final double probability, final int value) {
	    this.probability = probability;
	    this.value = value;
	}
	public double probability() { return this.probability; }
	public int value() { return this.value; }

	@Override public String toString() {
	    return "(" + probability() + " , " + value() + ")";
    }

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
        //EDGE_TABLE = Collections.unmodifiableList(initEdgeTable());
        EDGE_TABLE = null;
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

    /**
     * Computes the piece stability of a disc belonging to an edge.
     * The stability can assume three values: 0 for stable, 1 for semi stable,
     * and 2 for unstable.
     * <p>
     * Corners are always stable. X squares could be unstable when the related corner is
     * free, or semi-stable when the corner is taken.
     * A, B, and C squares (according to Hasegawa's naming) could have all three stability
     * values subject to the configuration.
     * The computation is based on identifying the p1 and p2 square states, where p1 is
     * the first square state reached moving on the right that is different from the evaluated
     * square, and p2 is the respective state on the left. States p1 and p2 can assume the values
     * opponent, empty, and outer.
     * A disc is stable when cannot be flipped, it is unstable when can be flipped by a legal move,
     * it is semi-stable when could be not promptly flipped but is not stable.
     * <p>
     * The computation is not meaningful for empty squares. In such a case the return value is {@code null}.
     *
     * @param board the edge configuration
     * @param sq    the square for which compute stability
     * @return      the piece stability index
     */
    public static final Integer pieceStability(final Board board, final Square sq) {
	final int stable = 0;
	final int semiStable = 1;
	final int unstable = 2;

	SquareState player = board.get(sq);
	if (player == SquareState.EMPTY) { return null; }

	int stability;
	if (sq.isCorner()) {
	    stability = stable;
	} else if (sq.isXSquare()) {
	    stability = (board.get(sq.cornerFor()) == SquareState.EMPTY) ? unstable : semiStable;
	} else {
	    /** The assignement to opp is consistent with a literal translation of the PAIP CL version of this function. */
	    SquareState opp = (player == SquareState.BLACK) ? SquareState.WHITE : SquareState.BLACK;
	    SquareState p1 = SquareState.OUTER;
	    for (int i = TOP_EDGE.indexOf(sq); i < 9; i++) {
		SquareState s = board.get(TOP_EDGE.get(i));
		if (s != player) { p1 = s; break; }
	    }
	    SquareState p2 = SquareState.OUTER;
	    for (int i = TOP_EDGE.indexOf(sq); i > 0; i--) {
		SquareState s = board.get(TOP_EDGE.get(i));
		if (s != player) { p2 = s; break; }
	    }
	    if (((p1 == SquareState.EMPTY) && (p2 == opp))
		|| ((p2 == SquareState.EMPTY) && (p1 == opp))) {
		/** Unstable pieces can be captured immediately by playing in the empty square. */
		stability = unstable;
	    } else if ((p1 == opp) && (p2 == opp) && (edgeHasEmptySquares(board))) {
		/** Semi-stable pieces might be captured. */
		stability = semiStable;
	    } else if ((p1 == SquareState.EMPTY) && (p2 == SquareState.EMPTY)) {
		stability = semiStable;
	    } else {
		/** Stable pieces can never be captured. */
		stability = stable;
	    }
	}
	return stability;
    }

    private static final boolean edgeHasEmptySquares(final Board topEdge) {
	for (Square sq : TOP_EDGE.subList(1, 9)) {
	    if (topEdge.get(sq) == SquareState.EMPTY) {
		return true;
	    }
	}
	return false;
    }

    // MUST BE COMPLETED!
    /**
     * Consider all possible edge moves.
     * Combine their values into a single number.
     * <p>
     * The function searches through all possible moves to determine an edge value that is more accurate
     * than a static evaluation. It loops through every empty square on the edge, calling possibleEdgeMove
     * to retur a ProbabilityValue object. Since it is also possible for a player not to make any move
     * at all on an edge, the pair (1.0 current-value) is also included.
     */
    public static final int possibleEdgeMovesValue(final Player player,
						   final Board board,
						   final int index) {
	List<ProbabilityValue> possibilities = new ArrayList<ProbabilityValue>();
        return combineEdgeMoves(possibilities, player);
    }

    // MUST BE COMPLETED!
    public static final ProbabilityValue possibleEdgeMove(final Player player, final Board board, final Square sq) {
	return new ProbabilityValue(edgeMoveProbability(player, board, sq),
				    // edgeTable
				    0 // a dummy value that must be replaced
				    );
    }

    /**
     * Combine the best moves.
     * <p>
     * The possible moves are combined with combineEdgeMoves(possibilities, player),
     * which sorts the moves best-first. We then go down the moves, increasing the total
     * value by the value of each move times the probability of the move. Since there
     * will always be a least one move (pass) with probability 1.0, this is guaranteed
     * to converge.
     */
    public static final int combineEdgeMoves(final List<ProbabilityValue> possibilities,
					     final Player player) {
	double prob = 1.0;
	double val = 0.0;
	List<ProbabilityValue> sortedPossibilities
	    = sortPossibilities(possibilities,
				((player == Player.BLACK) ? LESS_THAN : GREATER_THAN));
	for (ProbabilityValue pair : sortedPossibilities) {
	    if (prob >= 0.0) {
		val += prob * pair.probability() * pair.value();
		prob -= prob * pair.probability();
	    }
	}
        return Math.round((float)val);
    }

    public static final List<ProbabilityValue> sortPossibilities(final List<ProbabilityValue> possibilities,
								 final Comparator<ProbabilityValue> comparator) {
	List<ProbabilityValue> result = new ArrayList<ProbabilityValue>(possibilities);
	Collections.sort(result, comparator);
	return result;
    }

    public static final Comparator<ProbabilityValue> GREATER_THAN = new Comparator<ProbabilityValue>() {
	public int compare(final ProbabilityValue pv0,
			   final ProbabilityValue pv1) {
	    final int v0 = pv0.value();
	    final int v1 = pv1.value();
	    if (v0 < v1) {
		return -1;
	    } else if (v0 == v1) {
		return 0;
	    } else {
		return +1;
	    }
	}
    };

    public static final Comparator<ProbabilityValue> LESS_THAN = new Comparator<ProbabilityValue>() {
	public int compare(final ProbabilityValue pv0,
			   final ProbabilityValue pv1) {
	    final int v0 = pv0.value();
	    final int v1 = pv1.value();
	    if (v0 > v1) {
		return -1;
	    } else if (v0 == v1) {
		return 0;
	    } else {
		return +1;
	    }
	}
    };

    /**
     * What's the probability that player can move to this square?
     */
    public static final double edgeMoveProbability(final Player player,
						   final Board board,
						   final Square square) {
	if (square.isXSquare()) {
	    return 0.5;
	} else if (board.isLegal(square, player)) {
	    return 1.0;
	} else if (square.isCorner()) {
	    Square xSquare = square.xSquareFor();
	    if (board.get(xSquare) == SquareState.EMPTY) {
		return 0.1;
	    } else if (board.get(xSquare) == player.color()) {
		return 0.001;
	    } else {
		return 0.9;
	    }
	} else {
	    double chancesCoefficient = (board.isLegal(square, player.opponent())) ? 2. : 1.;
	    return EDGE_STATIC_PROBABILITY[countEdgeNeighbors(player, board, square)]
		[countEdgeNeighbors(player.opponent(), board, square)] / chancesCoefficient;
	}
    }

    public static final Double[][] EDGE_STATIC_PROBABILITY = { { .10,  .40,  .70 },
							       { .05,  .30, null },
							       { .01, null, null } };

    /**
     * Count the neighbors of this square occupied by player.
     * The function can return 0, 1, or 2.
     */
    public static final int countEdgeNeighbors(final Player player,
					       final Board board,
					       final Square square) {
	int result = 0;
	if (board.get(square.neighbors().get(Direction.W)) == player.color()) { result++; }
	if (board.get(square.neighbors().get(Direction.E)) == player.color()) { result++; }
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
