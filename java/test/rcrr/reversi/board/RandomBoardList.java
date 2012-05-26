/*
 *  RandomBoardList.java
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

import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

import org.junit.Test;

import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Performance Suite for {@code Board} class.
 */
public class RandomBoardList {

    /** The random field. */
    private final Random random;

    /** The boardFactory field. */
    private final BoardFactory boardFactory;

    /** The boards field. */
    private final List<Board> boards;

    /** Class constructor. */
    public RandomBoardList(final BoardFactory boardFactory,
                           final int numberOfBoards,
                           final int minNumberOfDiscs,
                           final int maxNumberOfDiscs) {
        this.random = new Random();
        this.boardFactory = boardFactory;
        this.boards = prepareRandomListOfBoards(numberOfBoards,
                                                minNumberOfDiscs,
                                                maxNumberOfDiscs);        
    }

    public static List<Square> randomSquares(final int numberOfSquares) {
        final Random r = new Random();
        final List<Square> squares = new ArrayList<Square>(numberOfSquares);
        while (squares.size() < numberOfSquares) {
            squares.add(Square.values()[r.nextInt(64)]);
        }
        return Collections.unmodifiableList(squares);
    }

    public static List<Player> randomPlayers(final int numberOfPlayers) {
        final Random r = new Random();
        final List<Player> players = new ArrayList<Player>(numberOfPlayers);
        while (players.size() < numberOfPlayers) {
            players.add(Player.values()[r.nextInt(2)]);
        }
        return Collections.unmodifiableList(players);
    }

    public List<Board> boards() {
        return Collections.unmodifiableList(boards);
    }

    private List<Board> prepareRandomListOfBoards(final int size,
                                                  final int minNumberOfDiscs,
                                                  final int maxNumberOfDiscs) {
        if (size < 0) {
            throw new IllegalArgumentException("Parameter size must be greather than zero.");
        }
        if (minNumberOfDiscs < 4) {
            throw new IllegalArgumentException("Parameter minNumberOfDiscs must be >= 4.");
        }
        if (maxNumberOfDiscs < minNumberOfDiscs || maxNumberOfDiscs > 64) {
            throw new IllegalArgumentException("Parameter maxNumberOfDiscs is wrong.");
        }
        final List<Board> transientResult = new ArrayList<Board>(size);
        for (int i = 0; i < size; i++) {
            int numberOfDiscs = minNumberOfDiscs + random.nextInt(maxNumberOfDiscs - minNumberOfDiscs + 1);
            transientResult.add(randomBoard(numberOfDiscs));
        }
        return Collections.unmodifiableList(transientResult);
    }

    private Board randomBoard(final int numberOfDiscs) {
        Board board = boardFactory.initialBoard();
        Player player = Player.BLACK;
        compute_board:
        for (int i = 4; i < numberOfDiscs;) {
            List<Square> moves = board.legalMoves(player);
            int movesSize = moves.size();
            if (movesSize != 0) {
                i++;
                Square move = moves.get(random.nextInt(moves.size()));
                board = board.makeMove(move, player);
            }
            if (board.hasAnyPlayerAnyLegalMove()) {
                player = player.opponent();
            } else {
                break compute_board;
            }
        }
        return board;
    }

}
