/*
 *  IndexedBoard.java
 *
 *  Copyright (c) 2010, 2011, 2012 Roberto Corradini. All rights reserved.
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

import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumMap;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

/**
 * A board concrete implementation.
 * <p>
 * A {@code IndexedBoard} object holds the information of the state of each board's square.
 * The board state is kept into a map holding the state of the sixtyfour squares.
 * <p>
 * {@code IndexedBoard} is immutable.
 * <p>
 * What to do:
 * move indexes from an array to a List<FileIndex> or better a Map<File, FileIndex>
 * implements countPieces by means of a precomputed value hosted in the FileIndex object
 * a method that converts indexes to squares has to be arranged. It will be used to support the get method.
 * turn compute indexes to return the List or the Map. Remove the debug version. It has to become static ....
 *
 * @see Square
 */
public final class IndexedBoard extends AbstractBoard {

    /**
     * Base static factory for the class.
     * <p>
     * {@code squares} must be not null, and must have an entry for every board square.
     * Given that the map cannot have duplicate keys, its size must be equal to the number
     * of class instances defined by the {@code Square} enum.
     *
     * @param  squares the map of squares
     * @return         a new board having as state the given square map
     * @throws NullPointerException     if parameter {@code squares} is null
     * @throws IllegalArgumentException if the {@code squares} is not complete
     */
    static Board valueOf(final Map<Square, SquareState> squares) {
        BoardUtils.checkForConsistencyTheSquareMap(squares);
        return new IndexedBoard(squares);
    }

    /**
     * Lazily initialized, cached legalMoves.
     * In case of a multi-threadd use must be applied a ReadWriteLock on this field.
     */
    private final transient Map<Player, SortedSet<Square>> legalMovesForPlayer
        = new EnumMap<Player, SortedSet<Square>>(Player.class);

    /**
     * Verify that the transient modifier is really neaded.
     */
    private final transient int[] indexes;

    /** The squares field. */
    private final transient EnumMap<Square, SquareState> squares;

    /**
     * Class constructor.
     * <p>
     * {@code squares} must be not null, and must have a size equal to
     * the number of squares, as defined by the {@code Square} enum.
     *
     * @param  squares the squares field
     */
    private IndexedBoard(final Map<Square, SquareState> squares) {
        assert (squares != null) : "Parameter squares cannot be null.";
        assert (squares.size() == Square.values().length) : "Parameter squares size is not consistent."
            + " squares.size()=" + squares.size()
            + " expected value: " + Square.values().length;
        assert (!squares.containsKey(null)) : "Parameter squares cannot contains null keys.";
        assert (!squares.containsValue(null)) : "Parameter squares cannot contains null values.";
        this.squares = new EnumMap<Square, SquareState>(squares);
        this.indexes = computeIndexes(squares);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public SquareState get(final Square square) {
        return (square == null) ? SquareState.OUTER : squares.get(square);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isLegal(final Square move, final Player player) {
        if (move == null) {
            throw new NullPointerException("Parameter move must be not null.");
        }
        if (player == null) {
            throw new NullPointerException("Parameter player must be not null.");
        }
        if (get(move) != SquareState.EMPTY) { return false; }
        for (Direction dir : move.capableToFlipDirections()) {
            if (wouldFlip(move, player, dir) != null) { return true; }
        }
        return false;
    }

    /**
     * Returns a list holding the legal moves that the {@code player} can
     * do at the board position. When no moves are available to the player
     * the method returns an empty list.
     *
     * This version overrides the method defined by the {@code AbstractBoard} class.
     * It memoizes the value calculated.
     *
     * @param player the player
     * @return       the moves available to the player
     * @throws NullPointerException if parameter {@code player} is null
     */
    @Override
    public List<Square> legalMoves(final Player player) {
        if (player == null) { throw new NullPointerException("Parameter player must be not null."); }
        SortedSet<Square> cached = this.legalMovesForPlayer.get(player);
        if (cached != null) { return new ArrayList<Square>(cached); }

        final List<File> files = FileUtils.files();
        final List<FileState.FileIndex> fileIndexList = new ArrayList<FileState.FileIndex>(files.size());
        final SortedSet<Square> legalMovesAsSet = new TreeSet<Square>();
        for (int i = 0; i < files.size(); i++) {
            final FileState.FileIndex fi = FileState.FileIndex.valueOf(files.get(i), getIndex(player, i));
            fileIndexList.add(fi);
            for (final Map.Entry<Integer, FileState.FileIndex> entry : fi.legalMoves().entrySet()) {
                final Square move = Line.getInstance(entry.getValue().file()).squares().get(entry.getKey());
                legalMovesAsSet.add(move);
            }
        }

        cached = Collections.unmodifiableSortedSet(legalMovesAsSet);
        this.legalMovesForPlayer.put(player, cached);
        return new ArrayList<Square>(cached);
    }

    /**
     * Should be moved in FileIndex .....
     */
    private int getIndex(final Player player, final int file) {
        return (player == Player.BLACK) ? indexes[file]: FileState.FileIndex.valueOf(FileUtils.files().get(file), indexes[file]).flip().index();
    }

    /**
     * To be DISCARDED!
     */
    private List<Square> legalMoves_(final Player player) {
        final List<Square> legalMoves = new ArrayList<Square>();
        for (final Square move : Square.values()) {
            if (isLegal(move, player)) { legalMoves.add(move); }
        }
        return Collections.unmodifiableList(legalMoves);
    }

    /**
     * Returns a new updated board to reflect move by player. This static
     * factory executes a game move to the board and returns a new one,
     * reflecting the move. The original board is not modified.
     * <p>
     * A null value for player is not allowed, a {@code NullPointerException}
     * is thrown in such a case.
     * <p>
     * A null value for move is allowed, and moreover is the only valid value
     * acceptable by the method, when the player has not any legal move.
     * Otherwise a null move is forbidden, and a {@code NullPointerException}
     * is risen.
     * <p>
     * The method does check if the move is legal. It throws an
     * {@code IllegalArgumentException} in case it is not.
     *
     * @param  move   the board square where to put the disk
     * @param  player the disk color to put on the board
     * @return        a new {@code Board} reflecting the move made
     * @throws NullPointerException     if parameter {@code move}
     *                                  or {@code player} is null
     * @throws IllegalArgumentException if the {@code move}
     *                                  by {@code player} is illegal
     */
    @Override
    public Board makeMove(final Square move, final Player player) {
        if (player == null) {
            throw new NullPointerException("Parameter player must be not null.");
        }
        if (move == null) {
            if (hasAnyLegalMove(player)) {
                throw new NullPointerException("Parameter move must be not null when a legal one is available.");
            } else {
                return this;
            }
        }
        if (!isLegal(move, player)) {
            throw new IllegalArgumentException("The move<"
                                               + move + "> by player<"
                                               + player + "> is illegal.");
        }
        final Map<Square, SquareState> sm = squares.clone();
        sm.put(move, player.color());
        for (final Direction dir : move.capableToFlipDirections()) {
            Square bracketer = wouldFlip(move, player, dir);
            if (bracketer != null) {
                for (Square c = move.neighbors().get(dir); true; c = c.neighbors().get(dir)) {
                    if (c == bracketer) { break; }
                    sm.put(c, player.color());
                }
            }
        }

        /**
         * Black is ok. White is not. Indexes must be accessed using getIndex().
         * Finally the indexes must be flipped back.
         *
         */
        final int[] newIndexes = new int[indexes.length];

        final List<FileState.FileIndexMove> moveAddendums = new ArrayList<FileState.FileIndexMove>();
        for (final Line line : Line.linesForSquare(move)) {
            final File file = line.file();
            final FileState.FileIndex fi = FileState.FileIndex.valueOf(file, getIndex(player, FileUtils.files().indexOf(file)));
            for (final Map.Entry<Integer, FileState.FileIndex> entry : fi.legalMoves().entrySet()) {
                final int moveOrdinalPosition = entry.getKey();
                final FileState.FileIndexMove fim = FileState.FileIndexMove.valueOf(fi, moveOrdinalPosition);
                final Square sq = Line.getInstance(entry.getValue().file()).squares().get(entry.getKey());
                if (sq == move) { moveAddendums.add(fim); }
            }
        }

        final int[] addedDiscDeltas = new int[indexes.length];
        for (final Line line : Line.linesForSquare(move)) {
            final int ordinal = line.squares().indexOf(move);
            addedDiscDeltas[line.ordinal()] = BigInteger.valueOf(3).pow(ordinal).intValue();
        }

        int i = 0;
        for (final File file : FileUtils.files()) {
            newIndexes[i] = getIndex(player, i) + addedDiscDeltas[i];
            for (int j = 0; j < moveAddendums.size(); j++) {
                newIndexes[i] += moveAddendums.get(j).getDeltas()[i];
            }
            i++;
        }

        /** Flip indexes if player is White */
        if (player == Player.WHITE) {
            int k = 0;
            for (final Line line : Line.values()) {
                newIndexes[k] = FileState.FileIndex.valueOf(line.file(), newIndexes[k]).flip().index();
                k++;
            }
        }
        /** */

        if (!Arrays.equals(computeIndexes(sm), newIndexes)) {
            for (int k = 0; k < FileUtils.files().size(); k++) {
                System.out.println("k, file, newIndexes, expected: " + k + ", " + FileUtils.files().get(k) + ", " + newIndexes[k] + ", " + computeIndexes(sm)[k]);
            }
            throw new RuntimeException("Indexes are wrong .... !!! ...");
        }

        return valueOf(sm);
    }

    /**
     * The {@code writeReplace()} method for the serialization proxy pattern.
     * <p>
     * The method return a newly created instance of the class {@code AbstractBoard.SerializationProxy}.
     * This instance is then serialized instead of the actual board object.
     * <p>
     * See the book: <i>"Bloch, Joshua. Effective Java Second Edition. Addison-Wesley, 2008"</i>.
     *
     * @return a new seialization proxy for the board object
     */
    @Override
    Object writeReplace() {
        return super.writeReplace();
    }

    /**
     * Returns the bracketing square or null if it is missing.
     * The method does not check that the move is legal and that the square parameter
     * is one step from move in the given direction.
     * <p>
     * The method should be private. It is not private to enable unit testing.
     * <p>
     * Parameters square, player, and dir must be not null. Java assert statements check
     * against this case to occur.
     *
     * @param square the square obtained moving by one from the move in the given direction
     * @param player the player
     * @param dir    the direction
     * @return       the bracketing square, or null if it is not found
     */
    private Square findBracketingPiece(final Square square, final Player player, final Direction dir) {
        assert (square != null) : "Argument square must be not null";
        assert (player != null) : "Argument player must be not null";
        assert (dir != null) : "Argument dir must be not null";
        if (get(square) == player.color()) {
            return square;
        } else if (get(square) == player.opponent().color()) {
            final Square next = square.neighbors().get(dir);
            if (next != null) { return findBracketingPiece(next, player, dir); }
        }
        return null;
    }

    /**
     * Returns the indexes field.
     *
     * @return the array representing the indexes of the board
     */
    public int[] indexes() {
        return this.indexes;
    }

    /**
     * Returns the bracketing square or null if it is not found.
     * The method does not check that the move is legal.
     * <p>
     * The method should be private. It is not private to enable unit testing.
     * <p>
     * Parameters move, player, and dir must be not null. Java assert statements check
     * against this case to occur.
     *
     * @param move   the square where to move
     * @param player the player
     * @param dir    the direction
     * @return       the bracketing square, or null if it is not found
     */
    private Square wouldFlip(final Square move, final Player player, final Direction dir) {
        assert (move != null) : "Argument square must be not null";
        assert (player != null) : "Argument player must be not null";
        assert (dir != null) : "Argument dir must be not null";
        final Square neighbor = move.neighbors().get(dir);
        Square bracketing = null;
        if (get(neighbor) == player.opponent().color()) {
            final Square next = neighbor.neighbors().get(dir);
            if (next != null) { bracketing = findBracketingPiece(next, player, dir); }
        }
        return bracketing;
    }

    public static final int[] computeIndexes(final Map<Square, SquareState> squares) {
        final int[] transientIndexes = new int[FileUtils.NUMBER_OF_FILES];
        /**
         * The call to indexOf(file) has to be avoided registering the values in an appropriate static array once for all.
         * The switch has to go away. Option one into a call to SquareState.intValue() or in a board "trasformation".
         *
         * squarePosition is the index position of the square in the file.
         */
        for (final Square sq : Square.values()) {
            for (final Line line : Line.linesForSquare(sq)) {
                final File file = line.file();
                if (file != null) {
                    final int color = squares.get(sq).ordinal();
                    final int squarePosition = Line.getInstance(file).squares().indexOf(sq);
                    final int index = FileUtils.files().indexOf(file);
                    transientIndexes[index] += color * FileUtils.FILE_INDEX_COEFFICIENT[squarePosition];
                }
            }
        }
        return transientIndexes;
    }

}
