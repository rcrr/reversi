/*
 *  BoardPerf.java
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
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;

import org.junit.Test;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Performance Suite for {@code Board} class.
 */
public class BoardPerf {

    private static final double CONVERT_NANOSEC_TO_SEC = 1 / 1000000000.;

    /** Class constructor. */
    public BoardPerf() { }

    /**
     * Test performances for isLegal() method.
     */
    @Test
    public final void testPerf_isLegal_10000_boards() {

        /** The number of iteration of the test. */
        final int size = 10000;

        final List<BoardFactory> boardFactories = new ArrayList<BoardFactory>();
        boardFactories.add(new EnumMapBoardFactory());
        boardFactories.add(new BitBoardFactory());

        for (BoardFactory bf : boardFactories) {

            final RandomBoardList rbl = new RandomBoardList(bf,
                                                            size, //numberOfBoards
                                                            20,   //minNumberOfDiscs
                                                            40);  //maxNumberOfDiscs
            final List<Board> boards = rbl.boards();
            final List<Square> squares = RandomBoardList.randomSquares(size);
            final List<Player> players = RandomBoardList.randomPlayers(size);

            final long start = System.nanoTime();

            for (int i = 0; i < size; i++) {
                boards.get(i).isLegal(squares.get(i), players.get(i));
            }

            final long stop = System.nanoTime();
            final double duration = (stop - start) * CONVERT_NANOSEC_TO_SEC;
            System.out.printf("Method %16s: BoardFactory %50s, n. of runs %6d, duration[sec]=%8f\n",
                              "isLegal()", bf.getClass().toString(), size, duration);
        }

        assertTrue("Always ok.", true);
    }

    /**
     * Test performances for legalMovesl() method.
     */
    @Test
    public final void testPerf_legalMoves_10000_boards() {

        /** The number of iteration of the test. */
        final int size = 10000;

        final List<BoardFactory> boardFactories = new ArrayList<BoardFactory>();
        boardFactories.add(new EnumMapBoardFactory());
        boardFactories.add(new BitBoardFactory());

        for (BoardFactory bf : boardFactories) {

            final RandomBoardList rbl = new RandomBoardList(bf,
                                                            size, //numberOfBoards
                                                            20,   //minNumberOfDiscs
                                                            40);  //maxNumberOfDiscs
            final List<Board> boards = rbl.boards();

            final long start = System.nanoTime();

            for (int i = 0; i < size; i++) {
                boards.get(i).legalMoves(Player.BLACK);
            }

            final long stop = System.nanoTime();
            final double duration = (stop - start) * CONVERT_NANOSEC_TO_SEC;
            System.out.printf("Method %16s: BoardFactory %50s, n. of runs %6d, duration[sec]=%8f\n",
                              "legalMoves()", bf.getClass().toString(), size, duration);
        }

        assertTrue("Always ok.", true);
    }

    /**
     * Test performances for makeMove() method.
     */
    @Test
    public final void testPerf_makeMove_10000_boards() {

        /** The number of iteration of the test. */
        final int size = 10000;

        final List<BoardFactory> boardFactories = new ArrayList<BoardFactory>();
        boardFactories.add(new EnumMapBoardFactory());
        boardFactories.add(new BitBoardFactory());

        for (BoardFactory bf : boardFactories) {

            final RandomBoardList rbl = new RandomBoardList(bf,
                                                            size, //numberOfBoards
                                                            20,   //minNumberOfDiscs
                                                            40);  //maxNumberOfDiscs
            final List<Board> boards = rbl.boards();

            final Random r = new Random();
            final List<Square> moves = new ArrayList<Square>();
            for (int i = 0; i < size; i++) {
                final List<Square> legalMoves = boards.get(i).legalMoves(Player.BLACK);
                if (legalMoves.size() > 0) {
                    moves.add(legalMoves.get(r.nextInt(legalMoves.size())));
                } else {
                    moves.add(Square.NULL);
                }
            }

            final long start = System.nanoTime();

            for (int i = 0; i < size; i++) {
                final Square move = moves.get(i);
                if (move != Square.NULL) {
                    boards.get(i).makeMove(move, Player.BLACK);
                }
            }

            final long stop = System.nanoTime();
            final double duration = (stop - start) * CONVERT_NANOSEC_TO_SEC;
            System.out.printf("Method %16s: BoardFactory %50s, n. of runs %6d, duration[sec]=%8f\n",
                              "makeMove()", bf.getClass().toString(), size, duration);
        }

        assertTrue("Always ok.", true);
    }

}
