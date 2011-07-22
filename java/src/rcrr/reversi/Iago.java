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
import java.util.Map;
import java.util.EnumMap;

import java.text.DecimalFormat;

import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;


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

    public enum SquareValue {
        PLAYER,
        OPPONENT,
        EMPTY;
    }

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
	private static final DecimalFormat FOUR_DIGIT_DECIMAL_FORMAT = new DecimalFormat("0.0000");
	private final double probability;
	private final int value;
	public ProbabilityValue(final double probability, final int value) {
	    this.probability = probability;
	    this.value = value;
	}
	public double probability() { return this.probability; }
	public int value() { return this.value; }
	@Override public String toString() {
	    return "(" + FOUR_DIGIT_DECIMAL_FORMAT.format(probability()) + " " + value() + ")";
	}
    }

    public static final class EdgeTable {

	public static final List<Integer> computeStatic() {
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
	    return edgeTable;	
	}

	public static final List<Integer> refine(final List<Integer> edgeTable) {
	    /** Do the indexes with more pieces first. From 9 to 1. */
	    for (int nPieces = 9; nPieces > 0; nPieces--) {
		mapEdgeNPieces(new Fn0() {
			public void funcall(final Board board, final int index) {
			    int tableValue = possibleEdgeMovesValue(edgeTable,
								    Player.BLACK,
								    board,
								    index);
			    edgeTable.set(index, tableValue);
			}
		    },
		    Player.BLACK,
		    Board.initialBoard(),
		    nPieces,
		    TOP_EDGE,
		    0);
	    }
	    return edgeTable;
	}

	private final List<Integer> table;

	public EdgeTable() {
	    this.table = new ArrayList<Integer>(EDGE_TABLE_SIZE);
	    for (int idx = 0; idx < EDGE_TABLE_SIZE; idx++) {
		this.table.add(0);
	    }
	}

	public EdgeTable(final List<Integer> table) {
	    this.table = new ArrayList<Integer>(table);
	}

	public final int set(final int index, final int value) {
	    return table.set(index, Integer.valueOf(value));
	}

	public final int get (final int index) {
	    return table.get(index);
	}

	public static final List<Integer> init() {
	    List<Integer> edgeTable = EdgeTable.computeStatic();
	    for (int i = 0; i < ITERATIONS_FOR_IMPROVING_EDGE_TABLE; i++) {
		edgeTable = EdgeTable.refine(edgeTable);
	    }
	    return edgeTable;
	}

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

    private static final int ITERATIONS_FOR_IMPROVING_EDGE_TABLE = 5;

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
    public static final int EDGE_TABLE_SIZE = new Double(Math.pow(SQUARE_VALUE_LENGTH,
                                                                   EDGE_SIZE)).intValue();

    private static final List<Integer> EDGE_TABLE;

    public static final Integer[][] STATIC_EDGE_TABLE = { { null,    0, -2000 },   /** X square. */
							  {  700, null,  null },   /** Corner.   */
							  { 1200,  200,   -25 },   /** C square. */
							  { 1000,  200,    75 },   /** A square. */
							  { 1000,  200,    50 },   /** B square. */
							  { 1000,  200,    50 },   /** B square. */
							  { 1000,  200,    75 },   /** A square. */
							  { 1200,  200,   -25 },   /** C square. */
							  {  700, null,  null },   /** Corner.   */
							  { null,    0, -2000 } }; /** X square. */

    public static final Double[][] EDGE_STATIC_PROBABILITY = { { .10,  .40,  .70 },
							       { .05,  .30, null },
							       { .01, null, null } };

    static {

        /** Computes EDGE_AND_X_LISTS. */
        List<List<Square>> tempEdgeAndXLists = new ArrayList<List<Square>>();
        tempEdgeAndXLists.add(TOP_EDGE);
        tempEdgeAndXLists.add(BOTTOM_EDGE);
        tempEdgeAndXLists.add(LEFT_EDGE);
        tempEdgeAndXLists.add(RIGHT_EDGE);
        EDGE_AND_X_LISTS = Collections.unmodifiableList(tempEdgeAndXLists);

        /** Computes EDGE_TABLE. */
        EDGE_TABLE = Collections.unmodifiableList(EdgeTable.init());

    }

    public static final String[] readInputStreamAsStringArray(final String resource) {
	InputStream in = new Iago().getClass().getClassLoader().getResourceAsStream(resource);
	if (in == null) {
	    throw new RuntimeException("Resource \"" + resource + "\" cannot be found.");
	}
	ByteArrayOutputStream buf;
	BufferedInputStream bis = new BufferedInputStream(in);
	try {
	    buf = new ByteArrayOutputStream();
	    int result = bis.read();
	    while(result != -1) {
		byte b = (byte)result;
		buf.write(b);
		result = bis.read();
	    }
	} catch (IOException ioe) {
	    throw new RuntimeException(ioe);
	}
	return buf.toString().split("\\n");
    }

    public static final List<Integer> loadEdgeTable(final String resource) {
	StringBuilder log = new StringBuilder();
	log.append("LOG: Reading the resource: " + resource + "\n");
	String[] lines = readInputStreamAsStringArray(resource);
	log.append("LOG: The resource has been read." +  "\n");
	int tableLength;
	int numberOfLines = lines.length;
	if (numberOfLines > 2) {
	    try {
		tableLength = Integer.valueOf(lines[1].trim());
	    } catch (NumberFormatException nfe) {
		log.append("ERROR: Unable to read the number of rows." + "\n");
		throw new RuntimeException(log.toString(), nfe);
	    }
	} else {
	    log.append("ERROR: The file format is wrong." + "\n");
	    throw new RuntimeException(log.toString());
	}
	log.append("LOG: File header: " + lines[0] + "\n");
	log.append("LOG: tableLength: " + tableLength + "\n");
	if (numberOfLines != tableLength + 2) {
	    log.append("ERROR: The file length is not consistent. numberOfLines=" + numberOfLines + "\n");
	    throw new RuntimeException(log.toString());
	}
	if (tableLength != Iago.EDGE_TABLE_SIZE) {
	    log.append("ERROR: The declared table length is not consistent with EDGE_TABLE_SIZE." + "\n");
	    throw new RuntimeException(log.toString());
	}
	List<Integer> edgeTable = new ArrayList<Integer>();
	log.append("LOG: Reading the edge table values ..." + "\n");
	for (int i = 2; i < numberOfLines; i++) {
	    int value;
	    try {
		value = Integer.valueOf(lines[i].trim());
	    } catch (NumberFormatException nfe) {
		log.append("ERROR: Unable to parse line " + i + ".\n");
		throw new RuntimeException(log.toString(), nfe);
	    }
	    edgeTable.add(value);
	}
	log.append("LOG: File reading completed, edge table constructed.");
	return edgeTable;
    }

    public static final void writeEdgeTable(final String fileOut,
					   final List<Integer> edgeTable) {
	try {
	    PrintWriter out = new PrintWriter(new FileWriter(fileOut));
	    out.println("# Written by Iago.writeEdgeTable method.");
	    out.println(EDGE_TABLE_SIZE);
	    for (int i = 0; i < EDGE_TABLE_SIZE; i++) {
		out.println(edgeTable.get(i));
	    }
	    out.close();
	} catch (IOException ioe) {
	    throw new RuntimeException(ioe);
	}
    }

    // Fully tested.
    /**
     * Computes the edge index used to execute a lookup into th edge table.
     * Given a player, a board, and the selected edge, the method returns the index to be
     * used to query the edge table.
     *
     * @param player the player for whom compute the index
     * @param board  the board configuration
     * @param edge   one among the four edge
     * @return       the index value associated to the given configuration and the chosen edge
     */
    public static int edgeIndex(final Player player, final Board board, final List<Square> edge) {
	assert (player != null) : "Parameter player cannot be null.";
	assert (board != null) : "Parameter board cannot be null.";
	assert (edge != null) : "Parameter edge cannot be null.";
	assert (edge.size() == 10) : "Parameter edge must have ten entries.";
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
 
    // Fully tested.
    /**
     * Compute this edge's static stability. The evaluation sums each piece's value
     * according to the STATIC_EDGE_TABLE.
     *
     * @param player the player for this configuration
     * @param board  the board for this configuration
     * @return       the static edge stability value
     */ 
    public static final int staticEdgeStability(final Player player, final Board board) {
	assert (player != null) : "Parameter player cannot be null.";
	assert (board != null) : "Parameter board cannot be null.";
        int sum = 0;
	int i = 0;
	for (Square sq : TOP_EDGE) {
	    int addendum;
	    if (board.get(sq) == SquareState.EMPTY) {
		addendum = 0;
	    } else {
		Integer staticEdgeTable = STATIC_EDGE_TABLE[i][pieceStability(board, sq)];
		assert (staticEdgeTable != null) : "Parameter staticEdgeTable cannot be null.";
		addendum = (board.get(sq) == player.color()) ? + staticEdgeTable : - staticEdgeTable;
	    }
	    sum += addendum;
	    i++;
	}
        return sum;
    }

    // Fully tested.
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
	assert(board != null) : "Parameter board cannot be null.";
	assert(sq != null) : "Parameter sq cannot be null.";

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

    // Tested.
    /**
     * Considers all possible edge moves and combines their values into a single number.
     * <p>
     * The function searches through all possible moves to determine an edge value that is more accurate
     * than a static evaluation. It loops through every empty square on the edge, calling possibleEdgeMove
     * to retur a ProbabilityValue object. Since it is also possible for a player not to make any move
     * at all on an edge, the pair (1.0 current-value) is also included.
     *
     * @param edgeTable the edge table reference that is under calculation.
     * @param player    the palyer for whom run the calculation
     * @param board     the edge configuration
     * @param index     
     * @return          an edge value that is more accurate than a static evaluation
     */
    public static final int possibleEdgeMovesValue(final List<Integer> edgeTable,
						   final Player player,
						   final Board board,
						   final int index) {
	assert(edgeTable != null) : "Parameter edgeTable cannot be null.";
	assert(player != null) : "Parameter player cannot be null.";
	assert(board != null) : "Parameter board cannot be null.";
	if (edgeIndex(player, board, TOP_EDGE) != index) {
	    // one test run with the two values not alligned! Why?
	    // the "regular run" has always the two values alligned. Could be removed from the paramenter list? I guess so!
	    /*
	    System.out.println("board=\n" + board.printBoard());
	    System.out.println("player=" + player);
	    System.out.println("edgeIndex(player, board, TOP_EDGE)=" + edgeIndex(player, board, TOP_EDGE) + ", index=" + index);
	    */ ;
	}
	List<ProbabilityValue> possibilities = new ArrayList<ProbabilityValue>();
	possibilities.add(new ProbabilityValue(1.0, edgeTable.get(index)));
	for (Square sq : TOP_EDGE) {
	    if (board.get(sq) == SquareState.EMPTY) {
		possibilities.add(possibleEdgeMove(edgeTable, player, board, sq));
	    }
	}
        return combineEdgeMoves(possibilities, player);
    }

    /**
     * Return a probability value pair for a possible edge move.
     */
    public static final ProbabilityValue possibleEdgeMove(final List<Integer> edgeTable,
							  final Player player,
							  final Board board,
							  final Square sq) {
	Board newBoard = makeMoveWithoutLegalCheck(board, sq, player);
	return new ProbabilityValue(edgeMoveProbability(player, board, sq),
				    - edgeTable.get(edgeIndex(player.opponent(), newBoard, TOP_EDGE)));
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
	for (ProbabilityValue pair : sortPossibilities(possibilities,
						       (player == Player.BLACK) ? LESS_THAN : GREATER_THAN)) {
	    if (prob >= 0.0) {
		val += prob * pair.probability() * pair.value();
		prob -= prob * pair.probability();
	    }
	}
        return Math.round((float)val);
    }

    public static final List<ProbabilityValue> sortPossibilities(final List<ProbabilityValue> possibilities,
								 final Comparator<ProbabilityValue> comparator) {
	assert (comparator != null) : "Parameter comparator cannot be null.";
	List<ProbabilityValue> result = new ArrayList<ProbabilityValue>(possibilities);
	Collections.sort(result, comparator);
	return result;
    }

    /**
     * What's the probability that player can move to this square?
     */
    public static final double edgeMoveProbability(final Player player,
						   final Board board,
						   final Square square) {
        assert (player != null) : "Argument player must be not null";
        assert (board != null) : "Argument board must be not null";
        assert (square != null) : "Argument square must be not null";
	double result;
	if (square.isXSquare()) {
	    result = 0.5;
	} else if (board.isLegal(square, player)) {
	    result = 1.0;
	} else if (square.isCorner()) {
	    Square xSquare = square.xSquareFor();
	    if (board.get(xSquare) == SquareState.EMPTY) {
		result = 0.1;
	    } else if (board.get(xSquare) == player.color()) {
		result = 0.001;
	    } else {
		result = 0.9;
	    }
	} else {
	    double chancesCoefficient = (board.isLegal(square, player.opponent())) ? 2. : 1.;
	    Double edgeStaticProbability = EDGE_STATIC_PROBABILITY[countEdgeNeighbors(player, board, square)]
		[countEdgeNeighbors(player.opponent(), board, square)];
	    assert (edgeStaticProbability != null) : "Variable edgeStaticProbability cannot be null.";
	    result =  edgeStaticProbability / chancesCoefficient;
	}
	return result;
    }

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

    /**
     * Call method fn on all configurations for an edge having n pieces.
     * The function 
     */
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

    private static interface Fn0 {
        public void funcall(final Board board, final int index);
    }

    private static final Board makeMoveWithoutLegalCheck(final Board board,
							 final Square square,
							 final Player player) {
	if (board.isLegal(square, player)) {
	    return board.makeMove(square, player);
	} else if (board.get(square) == SquareState.EMPTY) {
	    Map<Square, SquareState> squares = new EnumMap<Square, SquareState>(Square.class);
	    for (Square sq : Square.values()) {
		squares.put(sq, board.get(sq));
	    }
	    squares.put(square, player.color());
	    return Board.valueOf(squares);
	} else {
	    return board;
	}
    }

    private static final boolean edgeHasEmptySquares(final Board topEdge) {
	for (Square sq : TOP_EDGE.subList(1, 9)) {
	    if (topEdge.get(sq) == SquareState.EMPTY) {
		return true;
	    }
	}
	return false;
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
