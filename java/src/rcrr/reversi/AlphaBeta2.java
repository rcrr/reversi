/*
 *  AlphaBeta2.java
 *
 *  Copyright (c) 2010, 2011 Roberto Corradini. All rights reserved.
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

import rcrr.reversi.Square;

import java.util.Comparator;
import java.util.Collections;
import java.util.Map;
import java.util.HashMap;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.ArrayList;
import java.util.List;
import java.util.Arrays;
import java.util.Set;
import java.util.HashSet;

/**
 * The {@code AlphaBeta2} class implements {@code DecisionRule} and provides
 * an implementation of the search method applying the alpha-beta pruning
 * algorithm.
 * Compared with the AlphaBeta, this class provides some features that govern
 * the search ordering of the moves.
 * <p>
 * Results (see the AlphaBeta2Analytics.java under the test codebase) show that the move ordering
 * in accordance with the evaluation function is a key practice.
 * The number of board constructed and the evaluation performed drop dramatically if the ordering
 * is performed. The time spent in the searching can be furtherly reduced if the board calculated
 * and the evaluation done during the ordering are saved and then reused by the minimax process.
 * This practice is not applyied here. To abilitate it a proper data structure has to be prepared.
 * <p>
 * The alpha-beta family of algorithms is described by the wikipedia
 * page: <a href="http://en.wikipedia.org/wiki/Alpha-beta_pruning">alpha-beta pruning</a>.
 */
public final class AlphaBeta2 extends AbstractDecisionRule {

    public static enum Variant {
	/** Minimax. */
	MINIMAX,

	/** Moves are not sorted. */
	UNORDERED_ALPHABETA,

	/** Moves are sorted by the static weighetd squares function. */
	STATIC_ORDERED_ALPHABETA,

	/** Moves are sorted using the evaluation function. */
	EF_ORDERED_ALPHABETA,

	/** Iterative deepening. */
	MULTI_LEVEL_ORDERING
    }

    private int efInvokeCount;
    private int boardConstructionCount;
    private int[] cutoffCount;
    private long searchTimeInNanosecond;
    private final Map<String, Object> statistics;

    private final Variant variant;

    /**
     * Class static factory.
     *
     * @return a new alphabeta instance
     */
    public static AlphaBeta2 getInstance(final Variant variant,
					 final Map<String, Object> statistics) {
        return new AlphaBeta2(variant, statistics);
    }

    /** Class constructor. */
    private AlphaBeta2(final Variant variant,
		       final Map<String, Object> statistics) {
	this.variant = variant;
	this.statistics = statistics;
    };

    /**
     * Implemented by means of the alpha-beta algorithm.
     *
     * @param position the reached game position
     * @param ply      the search depth
     * @param ef       the evaluation function
     * @return         a new search node
     */
    public SearchNode search(final GamePosition position,
                             final int ply,
                             final EvalFunction ef) {

        this.efInvokeCount = 0;
	this.boardConstructionCount = 0;
	this.cutoffCount = new int[ply];

	long systemTimeBeforeSearch = System.nanoTime();

        SearchNode result;
	switch(variant) {
        case MINIMAX:
	    result = searchImpl1(position.player(), position.board(), ply, ef);
	    break;
        case UNORDERED_ALPHABETA:
	    result = searchImpl2(position.player(), position.board(), LOSING_VALUE, WINNING_VALUE, ply, ef);
	    break;
        case STATIC_ORDERED_ALPHABETA:
	    result = searchImpl3(position.player(), position.board(), LOSING_VALUE, WINNING_VALUE, ply, ef);
	    break;
        case EF_ORDERED_ALPHABETA:
	    result = searchImpl4(position.player(), position.board(), LOSING_VALUE, WINNING_VALUE, ply, ef, true);
	    break;
	case MULTI_LEVEL_ORDERING:
	    result = searchIterativeDeep(position, LOSING_VALUE, WINNING_VALUE, ply, ef, 0, true);
	    break;
	default: throw new RuntimeException("Unreachable condition found. variant=" + variant);
	}

	searchTimeInNanosecond = System.nanoTime() - systemTimeBeforeSearch;

	if (this.statistics != null) {
	    statistics.put("result", result);
	    statistics.put("cutoffCount", cutoffCount);
	    statistics.put("variant", variant);
	    statistics.put("efInvokeCount", efInvokeCount);
	    statistics.put("boardConstructionCount", boardConstructionCount);
	    statistics.put("searchTimeInMilliseconds", searchTimeInNanosecond / 1000000);
	}

        return result;
    }

    /**
     * The minimax function.
     *
     * @param player     the player that has the move
     * @param board      the reached board
     * @param ply        the search depth reached
     * @param ef         the evaluation function
     * @return           a node in the search tree
     */
    public SearchNode searchImpl1(final Player player,
				  final Board board,
				  final int ply,
				  final EvalFunction ef) {
        SearchNode node;
        final Player opponent = player.opponent();
        if (ply == 0) {
            node = SearchNode.valueOf(null, ef.eval(GamePosition.valueOf(board, player)));
            efInvokeCount++;
        } else {
            List<Square> moves = board.legalMoves(player);
            if (moves.isEmpty()) {
                if (board.hasAnyLegalMove(opponent)) {
                    node = searchImpl1(opponent, board, ply - 1, ef).negated();
                } else {
                    node = SearchNode.valueOf(null, finalValue(board, player));
                }
            } else {
                node = SearchNode.valueOf(null, Integer.MIN_VALUE);
                for (Square move : moves) {
                    int value = searchImpl1(opponent, board.makeMove(move, player),
                                       ply - 1, ef).negated().value();
		    boardConstructionCount++;
                    if (value > node.value()) {
                        node = SearchNode.valueOf(move, value);
                    }
                }
            }
        }
        return node;
    }

    /**
     * Implemented by means of the alpha-beta algorithm.
     *
     * @param player     the player having the move
     * @param board      the board
     * @param achievable the upper bound
     * @param cutoff     the lower bound
     * @param ply        the search depth
     * @param ef         the evaluation function
     * @return a new search node
     */
    private SearchNode searchImpl2(final Player player,
				   final Board board,
				   final int achievable,
				   final int cutoff,
				   final int ply,
				   final EvalFunction ef) {
        SearchNode node;
        final Player opponent = player.opponent();
        if (ply == 0) {
            node = SearchNode.valueOf(null, ef.eval(GamePosition.valueOf(board, player)));
            efInvokeCount++;
        } else {
            List<Square> moves = board.legalMoves(player);
            if (moves.isEmpty()) {
                if (board.hasAnyLegalMove(opponent)) {
                    node = searchImpl2(opponent, board, -cutoff, -achievable, ply - 1, ef).negated();
                } else {
                    node = SearchNode.valueOf(null, finalValue(board, player));
                }
            } else {
                node = SearchNode.valueOf(moves.get(0), achievable);
                outer: for (Square move : moves) {
                    Board board2 = board.makeMove(move, player);
		    boardConstructionCount++;
                    int val = searchImpl2(opponent, board2, -cutoff, -node.value(), ply - 1, ef).negated().value();
                    if (val > node.value()) {
                        node = SearchNode.valueOf(move, val);
                    }
                    if (node.value() >= cutoff) {  cutoffCount[ply]++; break outer; }
                }
            }
        }
        return node;
    }

    /**
     * Implemented by means of the alpha-beta algorithm applying a static ordering
     * of the moves.
     *
     * @param player     the player having the move
     * @param board      the board
     * @param achievable the upper bound
     * @param cutoff     the lower bound
     * @param ply        the search depth
     * @param ef         the evaluation function
     * @return a new search node
     */
    private SearchNode searchImpl3(final Player player,
				   final Board board,
				   final int achievable,
				   final int cutoff,
				   final int ply,
				   final EvalFunction ef) {
        SearchNode node;
        final Player opponent = player.opponent();
        if (ply == 0) {
            node = SearchNode.valueOf(null, ef.eval(GamePosition.valueOf(board, player)));
            efInvokeCount++;
        } else {
	    List<Square> moves = staticSortedLegalMoves(player, board);
            if (moves.isEmpty()) {
                if (board.hasAnyLegalMove(opponent)) {
                    node = searchImpl3(opponent, board, -cutoff, -achievable, ply - 1, ef).negated();
                } else {
                    node = SearchNode.valueOf(null, finalValue(board, player));
                }
            } else {
                node = SearchNode.valueOf(moves.get(0), achievable);
                outer: for (Square move : moves) {
                    Board board2 = board.makeMove(move, player);
		    boardConstructionCount++;
                    int val = searchImpl3(opponent, board2, -cutoff, -node.value(), ply - 1, ef).negated().value();
                    if (val > node.value()) {
                        node = SearchNode.valueOf(move, val);
                    }
                    if (node.value() >= cutoff) {  cutoffCount[ply]++; break outer; }
                }
            }
        }
        return node;
    }

    /**
     * Implemented by means of the alpha-beta algorithm applying a dynamic ordering
     * of the moves.
     *
     * @param player     the player having the move
     * @param board      the board
     * @param achievable the upper bound
     * @param cutoff     the lower bound
     * @param ply        the search depth
     * @param ef         the evaluation function
     * @return a new search node
     */
    private SearchNode searchImpl4(final Player player,
				   final Board board,
				   final int achievable,
				   final int cutoff,
				   final int ply,
				   final EvalFunction ef,
				   final boolean log) {
        SearchNode node;
        final Player opponent = player.opponent();
        if (ply == 0) {
            node = SearchNode.valueOf(null, ef.eval(GamePosition.valueOf(board, player)));
            efInvokeCount++;
        } else {
	    List<Square> moves = new ArrayList<Square>(dynamicSortedLegalMoves(player, board, ef, false).keySet());
	    if (log) {
		List<Square> moves1 = staticSortedLegalMoves(player, board);
		List<Square> moves2 = board.legalMoves(player);
		System.out.println("dynamic - moves=" + moves);
		System.out.println("static  - moves=" + moves1);
		System.out.println("basic   - moves=" + moves2);
	    }
            if (moves.isEmpty()) {
                if (board.hasAnyLegalMove(opponent)) {
                    node = searchImpl4(opponent, board, -cutoff, -achievable, ply - 1, ef, false).negated();
                } else {
                    node = SearchNode.valueOf(null, finalValue(board, player));
                }
            } else {
                node = SearchNode.valueOf(moves.get(0), achievable);
                outer: for (Square move : moves) {
                    Board board2 = board.makeMove(move, player);
		    boardConstructionCount++;
                    int val = searchImpl4(opponent, board2, -cutoff, -node.value(), ply - 1, ef, false).negated().value();
                    if (val > node.value()) {
                        node = SearchNode.valueOf(move, val);
                    }
                    if (node.value() >= cutoff) {  cutoffCount[ply]++; break outer; }
                }
            }
        }
        return node;
    }

    /**
     * Implemented by means of the alpha-beta algorithm applying a dynamic ordering
     * of the moves.
     *
     * @param position   the game position
     * @param achievable the upper bound
     * @param cutoff     the lower bound
     * @param ply        the search depth
     * @param ef         the evaluation function
     * @param log        activate the function logging
     * @return a new search node
     */
    private SearchNode searchIterativeDeep(final GamePosition position,
					   final int achievable,
					   final int cutoff,
					   final int ply,
					   final EvalFunction ef,
					   final int efNodeValue,
					   final boolean log) {
        SearchNode node;
        final Board board = position.board();
        final Player player = position.player();
        final Player opponent = player.opponent();
        if (ply == 0) {
            node = SearchNode.valueOf(null, efNodeValue);
        } else {
	    SortedMap<Square, MoveData> moves = dynamicSortedLegalMoves(player, board, ef, log);
            if (moves.isEmpty()) {
                if (board.hasAnyLegalMove(opponent)) {
                    node = searchIterativeDeep(GamePosition.valueOf(board, opponent), -cutoff, -achievable, ply - 1, ef, -efNodeValue, false).negated();
                } else {
                    node = SearchNode.valueOf(null, finalValue(board, player));
                }
            } else {
                node = SearchNode.valueOf(moves.firstKey(), achievable);
                outer: for (Square move : moves.keySet()) {
                    int val = searchIterativeDeep(moves.get(move).position(),
						  -cutoff, -node.value(),
						  ply - 1,
						  ef,
						  - moves.get(move).value(),
						  false)
			.negated().value();
                    if (val > node.value()) {
                        node = SearchNode.valueOf(move, val);
                    }
                    if (node.value() >= cutoff) { cutoffCount[ply]++; break outer; }
                }
            }
        }
        return node;
    }

    private static final Map<Square, Integer> SQUARE_WEIGHTS = WeightedSquares.weights();

    private static final Comparator<Square> SQUARE_WEIGHT_COMPARATOR = new Comparator<Square>() {
	public int compare(final Square sq0,
			   final Square sq1) {
	    final int v0 = SQUARE_WEIGHTS.get(sq0);
	    final int v1 = SQUARE_WEIGHTS.get(sq1);
	    if (v0 > v1) {
		return -1;
	    } else if (v0 == v1) {
		return 0;
	    } else {
		return +1;
	    }
	}
    };

    private static List<Square> staticSortedLegalMoves(final Player player, final Board board) {
	final List<Square> moves = board.legalMoves(player);
	Collections.sort(moves, SQUARE_WEIGHT_COMPARATOR);
	return moves;
    }

    private SortedMap<Square, MoveData> dynamicSortedLegalMoves(final Player player, final Board board, final EvalFunction ef, final boolean log) {
	final List<Square> moves = board.legalMoves(player);
	final Map<Square, MoveData> values = new HashMap<Square, MoveData>();
	for (Square move : moves) {
	    boardConstructionCount++;
            efInvokeCount++;
	    final GamePosition position = GamePosition.valueOf(board.makeMove(move, player), player.opponent());
	    final MoveData data = new MoveData(position, - ef.eval(position));
	    values.put(move, data);
	}
	final SortedMap<Square, MoveData> results = new TreeMap<Square, MoveData>(new ValueComparator<Square>(values));
	results.putAll(values);
	return results;
    }

    private static final class MoveData {
	private final int value;
	private final GamePosition position;
	MoveData(final GamePosition position, final int value) {
	    this.value = value;
	    this.position = position;
	}
	final GamePosition position() { return this.position; }
	final int value() { return this.value; }
    }

    private static final class ValueComparator<Square> implements Comparator<Square> {

	private final Map<Square, MoveData> values;

	ValueComparator(final Map<Square, MoveData> values) {
	    this.values = values;
	}

	@SuppressWarnings(value = "unchecked")
	public int compare(final Object object1,
			   final Object object2) {
	    if (object1 == object2) { return 0; }
	    rcrr.reversi.Square sq0 = (rcrr.reversi.Square) object1;
	    rcrr.reversi.Square sq1 = (rcrr.reversi.Square) object2;
	    final int v0 = values.get(sq0).value();
	    final int v1 = values.get(sq1).value();
	    if (v0 == v1) {
		final int ordinalSq0 = sq0.ordinal();
		final int ordinalSq1 = sq1.ordinal();
		return (ordinalSq0 > ordinalSq1) ? -1: +1;
	    }
	    if (v0 > v1) {
		return -1;
	    } else {
		return +1;
	    }
	}

	public boolean equals(final Object objectA, final Object objectB) {
	    if (objectA == objectB) { return true; }
	    return false;
	}


    }





}
