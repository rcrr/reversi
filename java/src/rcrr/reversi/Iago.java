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

/**
 * Iago is an advanced evaluation function, that implements the features described in the <i>"PAIP"</i> book,
 * paragraph 18.12.
 * <p>
 * Iago evaluation function mixes the concepts of mobility and edge stability. Mobility is a measure of the ability
 * to make moves; moves are not weighted by their goodness, simply the more moves a player has, the higher is the
 * value returned by the mobility evaluation. Mobility is computed as the sum of two factors, current mobility, and
 * potential mobility. The first is the current number of legal moves available to the player, the second is
 * computed by counting the empty squares that stands on the neighbor of the opponent's pieces.
 *
 * Edge stability is computed by adding the contributes given by each one of the four edges. An edge is a portion
 * of the board composed by the eight proper edge squares plus the two corresponding x-squares.
 * The ten squares, having each three possible states, gave a state space of 59,049 positions. Each of them
 * is pre-computed and stored into an indexed table. The resulting run-time evaluation is then blazing fast.
 * The table is computed by a quite sophisticate procedure, clearly explained in the above cited "PAIP" paragraph.
 *
 * Current mobility, potential mobility, and edge stability are then linearly added with coefficients that change
 * based on the number of empty disc on the board (a very effective measure of the game stage). The final result
 * is then the return value of the eval function.
 * <p>
 * The Java implementation here proposed is equivalent to the original Common Lisp version
 * described in the original work. The only very small difference is found in the final edge table values,
 * where some entries differ by one unit. The reason of this deviation has to be ascribed to the way
 * floating points values are rounded to integer values, where the Common Lisp version uses ROUND that rounds to
 * the nearest integer. If the argument is exactly halfway between two integers, it rounds to the nearest even integer.
 * While the Java version applies the {@code Math.round(float a)} function, where result is
 * rounded to an integer by adding 1/2, taking the floor of the result, and casting the result to type {@code int}.
 * <p>
 * See for reference Chapter 18, <i>"Search and the Game of Othello"</i>, and more in detail
 * paragraph 18.12, <i>"Championship Programs: Iago and Bill"</i>:
 * <ul>
 *   <li><i>"Peter Norvig. Paradigms of Artificial Intelligence Programming: Case Studies in Common Lisp.
 *           Morgan Kaufmann, 1992: 637~646"</i></li>
 * </ul>
 * <p>
 * See also for reference the two papers:
 * <ul>
 *   <li><i>"Paul S. Rosenbloom. A World-Championship-Level Othello Program.
             Artif. Intell., 1982: 279~320"</i></li>
 *   <li><i>"Kay-Fu Lee, S. Mahajan. The development of a world class Othello program.
             Artif. Intell., 1990: 21~36"</i></li>
 * </ul>
 * <p>
 */
public class Iago implements EvalFunction {

    /**
     * Each square can have three values. They are player, opponent, and empty.
     */
    enum SquareValue {

        /** Identifies a square occupied by the player. */
        PLAYER,

        /** Identifies a square occupied by the player's opponent. */
        OPPONENT,

        /** Identifies an empty square. */
        EMPTY;

        /** The number of states that a square can take. */
        public static final int LENGTH = values().length;
    }

    /**
     * It is the set of the squares belonging to a board's edge.
     */
    enum Edge {

        /** The upper edge of the board. */
        TOP(Square.B2, Square.A1, Square.B1, Square.C1, Square.D1,
            Square.E1, Square.F1, Square.G1, Square.H1, Square.G2),

        /** The right edge. */
        RIGHT(Square.G2, Square.H1, Square.H2, Square.H3, Square.H4,
              Square.H5, Square.H6, Square.H7, Square.H8, Square.G7),

        /** The lower edge. */
        BOTTOM(Square.B7, Square.A8, Square.B8, Square.C8, Square.D8,
               Square.E8, Square.F8, Square.G8, Square.H8, Square.G7),

        /** The left edge. */
        LEFT(Square.B2, Square.A1, Square.A2, Square.A3, Square.A4,
             Square.A5, Square.A6, Square.A7, Square.A8, Square.B7);

        /** The number of squares part of an edge. */
        static final int SQUARES_COUNT = 10;

        /** The number of edges. */
        static final int LENGTH = values().length;

        /** The squares field. */
        private final List<Square> squares;

        /**
         * Returns the squared of the edge.
         *
         * @return the list of the squares belonging to the edge
         */
        final List<Square> squares() { return this.squares; }

        /**
         * Enum constructor.
         *
         * @param squares squares field
         */
        Edge(final Square... squares) {
            assert (squares.length == SQUARES_COUNT) : "The number of squares assigned to the edge is wrong.";
            this.squares = Collections.unmodifiableList(Arrays.asList(squares));
        }
    }

    /**
     * The class defines value objects that hold the current and potential mobility fields.
     * <p>
     * {@code Iago.Mobility} is immutable.
     */
    static final class Mobility {

        /** The current field. */
        private final int current;

        /** The potential field. */
        private final int potential;

        /**
         * Class constructor.
         *
         * @param current   current mobility field
         * @param potential potential mobility field
         */
        public Mobility(final int current, final int potential) {
            this.current = current;
            this.potential = potential;
        }

        /**
         * Returns the current field.
         *
         * @return the current field
         */
        public int current() { return this.current; }

        /**
         * Returns the potential field.
         *
         * @return the potential field
         */
        public int potential() { return this.potential; }

        /**
         * Returns a {@code String} representing the {@code Iago.Mobility} object.
         *
         * @return a {@code String} representing the mobility
         */
        @Override public String toString() {
            return "(C=" + current() + ", P=" + potential() + ")";
        }
    }

    /**
     * The class defines value objects that hold the value and the probability.
     * <p>
     * {@code Iago.ProbabilityValue} is immutable.
     */
    static final class ProbabilityValue {

        /**
         * Compares two {@code ProbabilityValue} objects sorting them by value, taking first the greater.
         */
        public static final Comparator<ProbabilityValue> GT = new Comparator<ProbabilityValue>() {

            /**
             * Required by the {@code Comparator<T>} interface.
             * Compares its two arguments for order.
             * Returns a negative integer, zero, or a positive integer as the first argument is less than,
             * equal to, or greater than the second.
             *
             * @param pv0 the first probability value to be compared
             * @param pv1 the second probability value to be compared
             * @return    minus one, zero, or one as the first argument is less than, equal to,
             *            or greater than the second.
             * @throws NullPointerException if parameter {@code pv0}, or parameter {@code pv1} are null
             */
            public int compare(final ProbabilityValue pv0,
                               final ProbabilityValue pv1) {
                if (pv0 == null || pv1 == null) {
                    throw new NullPointerException("Parameters pv0 and pv1 cannot be null.");
                }
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

        /**
         * Compares two {@code ProbabilityValue} objects sorting them by value, taking first the smaller.
         */
        public static final Comparator<ProbabilityValue> LT = new Comparator<ProbabilityValue>() {
            public int compare(final ProbabilityValue pv0,
                               final ProbabilityValue pv1) {
                return GT.compare(pv1, pv0);
            }
        };

        /** Format used by the toString method. */
        private static final DecimalFormat FOUR_DIGIT_DECIMAL_FORMAT = new DecimalFormat("0.0000");

        /**
         * Returns a newly constructed sorted list applying the {@code comparator} parameter
         * to the {@code possibilities} parameter.
         * <p>
         * Parameter {@code possibilities} cannot be {@code null}.
         * Parameter {@code comparator} cannot be {@code null}.
         *
         * @param possibilities a list to be sorted
         * @param comparator    the comparator used for sorting
         * @return a new sorted list
         * @throws NullPointerException if parameter {@code possibilities} is null
         * @throws NullPointerException if parameter {@code comparator} is null
         */
        public static List<ProbabilityValue> sortPossibilities(final List<ProbabilityValue> possibilities,
                                                               final Comparator<ProbabilityValue> comparator) {
            if (possibilities == null) { throw new NullPointerException("Parameter possibilities cannot be null."); }
            if (comparator == null) { throw new NullPointerException("Parameter comparator cannot be null."); }
            final List<ProbabilityValue> results = new ArrayList<ProbabilityValue>(possibilities);
            Collections.sort(results, comparator);
            return results;
        }

        /** The probability field. */
        private final double probability;

        /** The value field. */
        private final int value;

        /**
         * Class constructor.
         *
         * @param probability the probability field
         * @param value       the value field
         */
        public ProbabilityValue(final double probability, final int value) {
            this.probability = probability;
            this.value = value;
        }

        /**
         * Returns the probability field.
         *
         * @return the probability field
         */
        public double probability() { return this.probability; }

        /**
         * Returns the value field.
         *
         * @return the value field
         */
        public int value() { return this.value; }

        /**
         * Returns a {@code String} representing the {@code ProbabilityValue} object.
         *
         * @return a {@code String} representing the probability-value object
         */
        @Override public String toString() {
            return "(" + FOUR_DIGIT_DECIMAL_FORMAT.format(probability()) + " " + value() + ")";
        }
    }

    /**
     * The class defines the edge table used by the Iago evaluation function.
     * The table stores in an indexed structure the precomputed edge stability value.
     * <p>
     * {@code Iago.EdgeTable} is immutable.
     */
    static final class EdgeTable {

        /**
         * The {@code Fn0} interface defines a function object used to be passed as a parameter to
         * higher order functions.
         */
        private static interface Fn0 {

            /**
             * Function call interface method.
             *
             * @param board the board to be elaborated
             * @param index the table index entry
             */
            void funcall(final Board board, final int index);
        }

        /** The size of the edge table has to be equal to 59,049. */
        private static final int SIZE = new Double(Math.pow(SquareValue.LENGTH,
                                                            Edge.SQUARES_COUNT)).intValue();

        /**
         * The edge table is refined by an iterative process.
         * The number of iterations run in order to converge is proposed by PAIP to be equal to 5.
         * Further analysis is required to verify the real convergence of the process.
         **/
        private static final int ITERATIONS_FOR_IMPROVING = 5;

        /**
         * A table of weights assigned to the squares of an edge. Weights are given for three cases:
         * stable (first column), semi-stable (second), and unstable (third one).
         */
        private static final Integer[][] STATIC_EDGE_TABLE = {{null,    0, -2000},   /** X square. */
                                                              {700 , null,  null},   /** Corner.   */
                                                              {1200,  200,   -25},   /** C square. */
                                                              {1000,  200,    75},   /** A square. */
                                                              {1000,  200,    50},   /** B square. */
                                                              {1000,  200,    50},   /** B square. */
                                                              {1000,  200,    75},   /** A square. */
                                                              {1200,  200,   -25},   /** C square. */
                                                              {700 , null,  null},   /** Corner.   */
                                                              {null,    0, -2000}};  /** X square. */

        /**
         * Static probability that a move can be legal.
         */
        private static final Double[][] EDGE_STATIC_PROBABILITY = {{.10,  .40,  .70},
                                                                   {.05,  .30, null},
                                                                   {.01, null, null}};

        /** Probability equal to 100%. */
        private static final double P_1 = 1.0;

        /** Probability equal to 90%. */
        private static final double P_09 = 0.9;

        /** Probability equal to 50%. */
        private static final double P_05 = 0.5;

        /** Probability equal to 10%. */
        private static final double P_01 = 0.1;

        /** Probability equal to 0.1%. */
        private static final double P_0001 = 0.001;

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
        private static int index(final Player player, final Board board, final Edge edge) {
            assert (player != null) : "Parameter player cannot be null.";
            assert (board != null) : "Parameter board cannot be null.";
            assert (edge != null) : "Parameter edge cannot be null.";
            int index = 0;
            for (final Square square : edge.squares()) {
                final SquareState state = board.get(square);
                int incr;
                if (state == SquareState.EMPTY) {
                    incr = 0;
                } else if (state == player.color()) {
                    incr = 1;
                } else {
                    incr = 2;
                }
                index = (index * SquareValue.LENGTH) + incr;
            }
            return index;
        }

        /**
         * The static factory create a new edge table starting from scratch,
         * values are calculated by means of a static table of values.
         *
         * @return a new edge table generated by a static computation
         */
        private static EdgeTable computeStatic() {
            final List<Integer> values = new ArrayList<Integer>(SIZE);
            for (int idx = 0; idx < SIZE; idx++) {
                values.add(0);
            }
            final EdgeTable table = new EdgeTable(values);
            /** Initialize the static values. */
            for (int nPieces = 0; nPieces <= Edge.SQUARES_COUNT; nPieces++) {
                mapEdgeNPieces(new Fn0() {
                        public void funcall(final Board board, final int index) {
                            table.set(index, staticEdgeStability(Player.BLACK, board));
                        }
                    },
                    Board.emptyBoard(),
                    nPieces,
                    Edge.TOP.squares(),
                    0);
            }
            return table;
        }

        /**
         * The refine method execute one run of the iterative process for improvement.
         * The edge table is modified.
         */
        private void refine() {
            /** Do the indexes with more pieces first. From 9 to 1. */
            for (int nPieces = Edge.SQUARES_COUNT - 1; nPieces > 0; nPieces--) {
                mapEdgeNPieces(new Fn0() {
                        public void funcall(final Board board, final int index) {
                            set(index, possibleEdgeMovesValue(Player.BLACK,
                                                              board));
                        }
                    },
                    Board.emptyBoard(),
                    nPieces,
                    Edge.TOP.squares(),
                    0);
            }
        }

        /**
         * Compute this edge's static stability. The evaluation sums each piece's value
         * according to the STATIC_EDGE_TABLE.
         *
         * @param player the player for this configuration
         * @param board  the board for this configuration
         * @return       the static edge stability value
         */
        private static int staticEdgeStability(final Player player, final Board board) {
            assert (player != null) : "Parameter player cannot be null.";
            assert (board != null) : "Parameter board cannot be null.";
            int sum = 0;
            int i = 0;
            for (Square sq : Edge.TOP.squares()) {
                int addendum;
                if (board.get(sq) == SquareState.EMPTY) {
                addendum = 0;
                } else {
                    Integer staticEdgeTable = STATIC_EDGE_TABLE[i][pieceStability(board, sq)];
                    assert (staticEdgeTable != null) : "Parameter staticEdgeTable cannot be null.";
                    addendum = (board.get(sq) == player.color()) ? +staticEdgeTable : -staticEdgeTable;
                }
                sum += addendum;
                i++;
            }
            return sum;
        }

        /**
         * Calls the given fn method on all configurations for an edge having n pieces. It relays
         * on side effects delegated to the fn function.
         * <p>
         * The function iterates through all edge positions with a total of {@code n} not empty squares,
         * applying a function {@code fn} to each such position. It also keeps a running count of the
         * edge index as it goes.
         * <p>
         * The {@code fn} parameter is an object that implements the {@code Fn0} interface, that define
         * the funcall methods as follow:
         * <p>
         * {@code public void funcall(final Board board, final int index)}
         * <p>
         * The {@code mapEdgeNPieces} function has three cases:
         * - If the number of squares remaining is less than {@code n}, then it will be impossible to
         *   place {@code n} pieces on those squares, so we give up.
         * - If there are no more squares then {@code n} must also be zero, so this is a valid position,
         *   and the function {@code fn} is called.
         * - Otherwise we first try leaving the current square EMPTY, then try filling it with player's piece,
         *   and then with the opponent's piece, in each case calling {@code mapEdgeNPieces} recursively.
         *
         * @param fn      the function object to be called
         * @param board   the edge state
         * @param n       number of pieces that populate the edge
         * @param squares the list of squares belonging to the edge that have to be elaborated
         * @param index   the edge configuration index
         */
        private static void mapEdgeNPieces(final Fn0 fn,
                                          final Board board,
                                          final int n,
                                          final List<Square> squares,
                                          final int index) {
            final int squaresSize = squares.size();
            /** Index counts 1 for player; 2 for opponent. */
            if (squaresSize < n) {
                return;
            } else if (squaresSize == 0) {
                fn.funcall(board, index);
            } else {
                final List<Square> squaresRest = squares.subList(1, squaresSize);
                final int index3 = 3 * index;
                final Square sq = squares.get(0);
                mapEdgeNPieces(fn, board, n, squaresRest, index3);
                if (n > 0 && board.get(sq) == SquareState.EMPTY) {
                    Board.Builder bb = new Board.Builder(board);
                    mapEdgeNPieces(fn,
                                   bb.withSquare(sq, SquareState.BLACK).build(),
                                   n - 1,
                                   squaresRest,
                                   index3 + 1);
                    mapEdgeNPieces(fn,
                                   bb.withSquare(sq, SquareState.WHITE).build(),
                                   n - 1,
                                   squaresRest,
                                   index3 + 2);
                }
            }
        }

        /**
         * Returns a new edge table loaded from the {@code resource} parameter.
         *
         * @param resource a string that identifies a file
         * @return         a new edge table
         */
        private static EdgeTable load(final String resource) {
            StringBuilder log = new StringBuilder();
            log.append("LOG: Reading the resource: " + resource + "\n");
            String[] lines = Utils.readInputStreamAsStringArray(resource);
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
            if (tableLength != SIZE) {
                log.append("ERROR: The declared table length is not consistent with SIZE." + "\n");
                throw new RuntimeException(log.toString());
            }
            final List<Integer> values = new ArrayList<Integer>();
            log.append("LOG: Reading the edge table values ..." + "\n");
            for (int i = 2; i < numberOfLines; i++) {
                int value;
                try {
                    value = Integer.valueOf(lines[i].trim());
                } catch (NumberFormatException nfe) {
                    log.append("ERROR: Unable to parse line " + i + ".\n");
                    throw new RuntimeException(log.toString(), nfe);
                }
                values.add(value);
            }
            log.append("LOG: File reading completed, edge table constructed.");
            return new EdgeTable(values);
        }

        /**
         * Writes the edge table to the target file identified by the {@code fileOut} parameter.
         *
         * @param fileOut the file where to write the table in
         */
        private void write(final String fileOut) {
            try {
                PrintWriter out = new PrintWriter(new FileWriter(fileOut));
                out.println("# Written by Iago.EdgeTable.write method.");
                out.println(SIZE);
                for (int i = 0; i < SIZE; i++) {
                    out.println(get(i));
                }
                out.close();
            } catch (IOException ioe) {
                throw new RuntimeException(ioe);
            }
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
        private static Integer pieceStability(final Board board, final Square sq) {
            assert (board != null) : "Parameter board cannot be null.";
            assert (sq != null) : "Parameter sq cannot be null.";

            final int stable = 0;
            final int semiStable = 1;
            final int unstable = 2;

            final SquareState player = board.get(sq);
            if (player == SquareState.EMPTY) { return null; }

            int stability;
            if (sq.isCorner()) {
                stability = stable;
            } else if (sq.isXSquare()) {
                stability = (board.get(sq.cornerFor()) == SquareState.EMPTY) ? unstable : semiStable;
            } else {
                /** The assignment to opp is consistent with a literal translation
                    of the PAIP CL version of this function. */
                final SquareState opp = (player == SquareState.BLACK) ? SquareState.WHITE : SquareState.BLACK;
                SquareState p1 = SquareState.OUTER;
                for (int i = Edge.TOP.squares().indexOf(sq); i < Edge.SQUARES_COUNT - 1; i++) {
                    SquareState s = board.get(Edge.TOP.squares().get(i));
                    if (s != player) { p1 = s; break; }
                }
                SquareState p2 = SquareState.OUTER;
                for (int i = Edge.TOP.squares().indexOf(sq); i > 0; i--) {
                    SquareState s = board.get(Edge.TOP.squares().get(i));
                    if (s != player) { p2 = s; break; }
                }
                if (((p1 == SquareState.EMPTY) && (p2 == opp))
                    || ((p2 == SquareState.EMPTY) && (p1 == opp))) {
                    /** Unstable pieces can be captured immediately by playing in the empty square. */
                    stability = unstable;
                } else if ((p1 == opp) && (p2 == opp) && (topEdgeHasEmptySquares(board))) {
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
         * to return a ProbabilityValue object. Since it is also possible for a player not to make any move
         * at all on an edge, the pair (1.0 current-value) is also included.
         *
         * @param player the player for whom run the calculation
         * @param board  the edge configuration
         * @return       an edge value that is more accurate than a static evaluation
         */
        private int possibleEdgeMovesValue(final Player player,
                                           final Board board) {
            assert (player != null) : "Parameter player cannot be null.";
            assert (board != null) : "Parameter board cannot be null.";
            final List<ProbabilityValue> possibilities = new ArrayList<ProbabilityValue>();
            possibilities.add(new ProbabilityValue(1.0, get(index(player, board, Edge.TOP))));
            for (final Square sq : Edge.TOP.squares()) {
                if (board.get(sq) == SquareState.EMPTY) {
                    possibilities.add(possibleEdgeMove(player, board, sq));
                }
            }
            return combineEdgeMoves(possibilities, player);
        }

        /**
         * Return a probability value pair for a move that is possible on the edge.
         * <p>
         * Parameter player cannot be null.
         * Parameter board cannot be null.
         * Parameter sq cannot be null.
         *
         * @param player the player that has to make the move
         * @param board  the board configuration
         * @param sq     the square where to move on
         * @return       a new probability value pair
         */
        ProbabilityValue possibleEdgeMove(final Player player,
                                          final Board board,
                                          final Square sq) {
            assert (player != null) : "Parameter player cannot be null.";
            assert (board != null) : "Parameter board cannot be null.";
            assert (sq != null) : "Parameter sq cannot be null.";
            return new ProbabilityValue(edgeMoveProbability(player, board, sq),
                                        -get(index(player.opponent(),
                                                   makeMoveWithoutLegalCheck(board,
                                                                             sq,
                                                                             player),
                                                   Edge.TOP)));
        }

        /**
         * Combine the best moves.
         * <p>
         * The possible moves are combined with combineEdgeMoves(possibilities, player),
         * which sorts the moves best-first. We then go down the moves, increasing the total
         * value by the value of each move times the probability of the move. Since there
         * will always be a least one move (pass) with probability 1.0, this is guaranteed
         * to converge.
         *
         * @param possibilities the list of probability-value pairs
         * @param player        the player that has the move
         * @return              the combined value of the given possibilities
         */
        private static int combineEdgeMoves(final List<ProbabilityValue> possibilities,
                                            final Player player) {
            assert (possibilities != null) : "Parameter possibilities must be not null.";
            assert (player != null) : "Parameter player must be not null.";
            double prob = 1.0;
            double val = 0.0;
            for (final ProbabilityValue pair
                     : ProbabilityValue.sortPossibilities(possibilities,
                                                          (player == Player.BLACK)
                                                          ? ProbabilityValue.LT
                                                          : ProbabilityValue.GT)) {
                if (prob >= 0.0) {
                    val += prob * pair.probability() * pair.value();
                    prob -= prob * pair.probability();
                }
            }
            return Math.round((float) val);
        }

        /**
         * What's the probability that player can move to this square?
         * <p>
         * Parameter player cannot be null.
         * Parameter board cannot be null.
         * Parameter square cannot be null.
         * <p>
         * The method is side effect free.
         *
         * @param player the player that moves
         * @param board  the board position
         * @param square the square where to move
         * @return       the probability of the move
         */
        private static double edgeMoveProbability(final Player player,
                                                  final Board board,
                                                  final Square square) {
            assert (player != null) : "Parameter player must be not null.";
            assert (board != null) : "Parameter board must be not null.";
            assert (square != null) : "Parameter square must be not null.";
            double result;
            if (square.isXSquare()) {
                result = P_05;
            } else if (board.isLegal(square, player)) {
                result = P_1;
            } else if (square.isCorner()) {
                final Square xSquare = square.xSquareFor();
                if (board.get(xSquare) == SquareState.EMPTY) {
                    result = P_01;
                } else if (board.get(xSquare) == player.color()) {
                    result = P_0001;
                } else {
                    result = P_09;
                }
            } else {
                final double chancesCoefficient = (board.isLegal(square, player.opponent())) ? 2. : 1.;
                final Double edgeStaticProbability = EDGE_STATIC_PROBABILITY[countEdgeNeighbors(player, board, square)]
                    [countEdgeNeighbors(player.opponent(), board, square)];
                assert (edgeStaticProbability != null) : "Variable edgeStaticProbability cannot be null.";
                result =  edgeStaticProbability / chancesCoefficient;
            }
            return result;
        }

        /**
         * Count the neighbors of this square occupied by player.
         * The function can return 0, 1, or 2.
         * It works specifically when the square is part of the top edge only.
         * <p>
         * Parameter player cannot be null.
         * Parameter board cannot be null.
         * Parameter square cannot be null.
         * <p>
         * The method is side effect free.
         *
         * @param player the player to use for the inquiry
         * @param board  the board position
         * @param square the square to check
         * @return       the number of neighbors occupied by the player
         */
        private static int countEdgeNeighbors(final Player player,
                                              final Board board,
                                              final Square square) {
            assert (player != null) : "Parameter player must be not null.";
            assert (board != null) : "Parameter board must be not null.";
            assert (square != null) : "Parameter square must be not null.";
            int result = 0;
            final SquareState color = player.color();
            final Map<Direction, Square> neighbors = square.neighbors();
            if (board.get(neighbors.get(Direction.W)) == color) { result++; }
            if (board.get(neighbors.get(Direction.E)) == color) { result++; }
            return result;
        }

        /**
         * Returns a new board applying a change to the given {@code board} parameter.
         * There are three options:
         * - The {@code player} move to {@code square} is legal:
         *   the move is played as per the game rules.
         * - The move is not legal and the square is EMPTY:
         *   the square is turned to the color of the {@code player}.
         * - The {@code square} is not EMPTY:
         *   the returned board is equal to the {@code board} parameter.
         * <p>
         * Parameter board cannot be null.
         * Parameter square cannot be null.
         * Parameter player cannot be null.
         * <p>
         * The method is side effect free.
         *
         * @param board  the board configuration
         * @param square the square where to move on
         * @param player the player that has to make the move
         * @return       a new updated board
         */
        private static Board makeMoveWithoutLegalCheck(final Board board,
                                                       final Square square,
                                                       final Player player) {
            assert (board != null) : "Parameter board must be not null.";
            assert (square != null) : "Parameter square must be not null.";
            assert (player != null) : "Parameter player must be not null.";
            if (board.isLegal(square, player)) {
                return board.makeMove(square, player);
            } else if (board.get(square) == SquareState.EMPTY) {
                final Map<Square, SquareState> squares = new EnumMap<Square, SquareState>(Square.class);
                for (final Square sq : Square.values()) {
                    squares.put(sq, board.get(sq));
                }
                squares.put(square, player.color());
                return Board.valueOf(squares);
            } else {
                return board;
            }
        }

        /**
         * Returns true when at last one square being part of the eight top ones is empty.
         *
         * @param board the edge configuration to consider
         * @return      true if one or more squares, parts of the top edge, are empty
         */
        private static boolean topEdgeHasEmptySquares(final Board board) {
            for (Square sq : Edge.TOP.squares().subList(1, Edge.SQUARES_COUNT - 1)) {
                if (board.get(sq) == SquareState.EMPTY) {
                    return true;
                }
            }
            return false;
        }

        /** The values field. */
        private final List<Integer> values;

        /** Class constructor. */
        private EdgeTable() {
            this.values = new ArrayList<Integer>(SIZE);
            for (int idx = 0; idx < SIZE; idx++) {
                this.values.add(0);
            }
        }

        /**
         * Class constructor.
         * <p>
         * Parameter {@code values} cannot be null.
         * Parameter {@code values} cannot have null entries.
         * Parameter {@code values} must have a proper lenght.
         *
         * @param values a list having the edge table values
         * @throws NullPointerException if parameter {@code values} is null
         * @throws NullPointerException if parameter {@code values} has null entries
         * @throws IllegalArgumentException if parameter {@code values} has the wrong size
         */
        EdgeTable(final List<Integer> values) {
            if (values == null) { throw new NullPointerException("Parameter values cannot be null."); }
            final ArrayList<Integer> transientValues = new ArrayList<Integer>(values);
            if (transientValues.size() != SIZE) {
                throw new IllegalArgumentException("Parameter values must have a proper size.");
            }
            for (Integer value : transientValues) {
                if (value == null) {
                    throw new NullPointerException("Parameter values cannnot have null entries.");
                }
            }
            this.values = transientValues;
        }

        /**
         * Replaces the value at the specified position in the table with the value specified
         * by the {@code value} parameter.
         * <p>
         * Parameter {@code index} must be in the range 0, 59,049 - 1.
         *
         * @param index the position that identify the element to update
         * @param value the value to be assigned
         * @return      the value just assigned
         */
        private int set(final int index, final int value) {
            assert (index >= 0 && index < SIZE) : "Parameter index must be in the range 0, SIZE - 1.";
            return values.set(index, Integer.valueOf(value));
        }

        /**
         * Get the value stored by the edge table at the address specified by {@code index}.
         * <p>
         * Parameter {@code index} must be in the range 0, 59,049 - 1.
         *
         * @param index the position that identify the element to retrieve
         * @return      the requested value
         */
        private int get(final int index) {
            assert (index >= 0 && index < SIZE) : "Parameter index must be in the range 0, SIZE - 1.";
            return values.get(index);
        }

        /**
         * Returns the values field.
         *
         * @return the values field
         */
        private List<Integer> values() {
            return this.values;
        }

        /**
         * Create a new edge table as a copy of the current object.
         *
         * @return a new copied edge table
         */
        private EdgeTable copy() {
            return new EdgeTable(new ArrayList<Integer>(this.values()));
        }

        /**
         * Create and compute a new edge table.
         *
         * @return the new initialized edge table
         */
        private static EdgeTable init() {
            final EdgeTable table = computeStatic();
            for (int i = 0; i < ITERATIONS_FOR_IMPROVING; i++) {
                table.refine();
            }
            return table;
        }

    }

    /** The edge list. It is computed into the static block. */
    private static final List<Edge> EDGE_AND_X_LISTS;

    /** An edge table instance. It is computed into the static block. */
    private static final EdgeTable TABLE;

    /** Eval function tuning parameter. */
    private static final long EVAL_MAGIC_61 = 61;

    /** Eval function tuning parameter. */
    private static final long EVAL_MAGIC_312000 = 312000;

    /** Eval function tuning parameter. */
    private static final long EVAL_MAGIC_6240 = 6240;

    /** Eval function tuning parameter. */
    private static final long EVAL_MAGIC_25 = 25;

    /** Eval function tuning parameter. */
    private static final long EVAL_MAGIC_20000 = 20000;

    /** Eval function tuning parameter. */
    private static final long EVAL_MAGIC_1000 = 1000;

    /** Eval function tuning parameter. */
    private static final long EVAL_MAGIC_2000 = 2000;

    /** Eval function tuning parameter. */
    private static final long EVAL_MAGIC_50000 = 50000;

    /** Eval function tuning parameter. */
    private static final long EVAL_MAGIC_75000 = 75000;

    /** Eval function tuning parameter. */
    private static final long EVAL_MAGIC_32000 = 32000;

    static {

        /** Computes EDGE_AND_X_LISTS. */
        List<Edge> tempEdgeAndXLists = new ArrayList<Edge>();
        tempEdgeAndXLists.add(Edge.TOP);
        tempEdgeAndXLists.add(Edge.BOTTOM);
        tempEdgeAndXLists.add(Edge.LEFT);
        tempEdgeAndXLists.add(Edge.RIGHT);
        EDGE_AND_X_LISTS = Collections.unmodifiableList(tempEdgeAndXLists);

        /** Computes EDGE_TABLE. */
        TABLE = EdgeTable.init();

    }

    /** The table field. */
    private final EdgeTable table;

    /**
     * Standard class constructor. It loads the default edge table, calculated
     * by the class static block during class initialization.
     */
    public Iago() {
        this(TABLE);
    }

    /**
     * Class constructor.
     *
     * @param table assigned to the table field
     */
    private Iago(final EdgeTable table) {
        this.table = table;
    }

    /**
     * @return the edge table field.
     */
    private EdgeTable table() {
        return this.table;
    }

    /**
     * Combine edge stability, current mobility and potential mobility to arrive
     * at an evaluation.
     * <p>
     * Parameter {@code position} cannot be null.
     *
     * @param position the game position to evaluate
     * @return the position value
     * @throws NullPointerException if parameter {@code position} is null
     */
    public final int eval(final GamePosition position) {
        if (position == null) { throw new NullPointerException("Parameter position cannot be null."); }
        /** The three factors are multiplied by coefficients that vary by move number. */
        final long moveNumber = EVAL_MAGIC_61 - position.board().countPieces(SquareState.EMPTY);
        final long cEdg = EVAL_MAGIC_312000 + (EVAL_MAGIC_6240 * moveNumber);
        final long cCur = (moveNumber < EVAL_MAGIC_25)
            ? EVAL_MAGIC_50000 + (EVAL_MAGIC_2000 * moveNumber)
            : EVAL_MAGIC_75000 + (EVAL_MAGIC_1000 * moveNumber);
        final long cPot = EVAL_MAGIC_20000;
        final Mobility pMob = mobility(position);
        final Mobility oMob = mobility(GamePosition.valueOf(position.board(), position.player().opponent()));
        final long eStab = edgeStability(position);
        /** Combine the three factors into one value. */
        long value = (cEdg * eStab) / EVAL_MAGIC_32000
            + (cCur * (pMob.current() - oMob.current())) / (pMob.current() + oMob.current() + 2)
            + (cPot * (pMob.potential() - oMob.potential())) / (pMob.potential() + oMob.potential() + 2);
        return (int) value;
    }

    /**
     * Current mobility is the number of legal moves.
     * Potential mobility is the number of blank squares adjacent to an opponent that are not legal moves.
     * Returns current and potential mobility for the player.
     * <p>
     * See PAIP 18.12 pages 637-638.
     * <p>
     * Parameter {@code position} cannot be null.
     *
     * @param position the game position to evaluate
     * @return         the position mobility evaluation
     */
    private Mobility mobility(final GamePosition position) {
        assert (position != null) : "Parameter position cannot be null.";

        int current = 0;
        int potential = 0;

        final Board board = position.board();
        final Player player = position.player();
        final Player opp = player.opponent();

        for (final Square square : Square.values()) {
            if (board.get(square) == SquareState.EMPTY) {
                if (board.isLegal(square, player)) {
                    current++;
                    potential++;
                } else if (isPotentialMove(board, square, opp)) {
                    potential++;
                }
            }
        }
        return new Mobility(current, potential);
    }

    /**
     * Total edge evaluation for the game position.
     * <p>
     * See PAIP 18.12 page 639.
     * <p>
     * Parameter {@code position} cannot be null.
     *
     * @param position the game position to evaluate
     * @return         total edge evaluation for the game position
     */
    private int edgeStability(final GamePosition position) {
        assert (position != null) : "Parameter position cannot be null.";
        int evaluation = 0;
        for (final Edge edge : EDGE_AND_X_LISTS) {
            evaluation += table().get(EdgeTable.index(position.player(), position.board(), edge));
        }
        return evaluation;
    }

    /**
     * Evaluates if the square is a potential move.
     * <p>
     * Parameter {@code board} cannot be null.
     * Parameter {@code square} cannot be null.
     * Parameter {@code opponent} cannot be null.
     *
     * @param board    the board to assess
     * @param square   the square where to move
     * @param opponent the opponent player
     * @return       true if the square is a potential move
     */
    private boolean isPotentialMove(final Board board,
                                    final Square square,
                                    final Player opponent) {
        assert (board != null) : "Parameter board cannot be null.";
        assert (square != null) : "Parameter square cannot be null.";
        assert (opponent != null) : "Parameter board opponent be null.";
        for (Square neighbor : square.neighbors().values()) {
            if (board.get(neighbor) == opponent.color()) {
                return true;
            }
        }
        return false;
    }

}
