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

import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Performance Suite for {@code Board} class.
 */
public class BoardPerf {

    /** Class constructor. */
    public BoardPerf() { }

    /**
     * Test performances for isLegal() method.
     */
    @Test
    public final void testPerf_isLegal_10000_boards() {
        final int size = 10000;

        final List<BoardFactory> boardFactories = new ArrayList<BoardFactory>();
        boardFactories.add(new EnumMapBoardFactory());
        boardFactories.add(new BitBoardFactory());

        for (BoardFactory bf : boardFactories) {

            RandomBoardList rbl = new RandomBoardList(bf,
                                                      size, //numberOfBoards
                                                      20, //minNumberOfDiscs
                                                      40); //maxNumberOfDiscs
            List<Board> boards = rbl.boards();
            List<Square> squares = RandomBoardList.randomSquares(size);

            long start = System.currentTimeMillis();

            for (int i = 0; i < size; i++) {
                boards.get(i).isLegal(squares.get(i), Player.BLACK);
            }

            long stop = System.currentTimeMillis();
            double duration = (stop - start) / 1000.;
            System.out.println("isLegal(): bf=" + bf + ", size=" + size + ", duration=" + duration);
        }

        assertThat("Always ok.", true, is(true));
    }

    /**
     * Test performances for legalMovesl() method.
     */
    @Test
    public final void testPerf_legalMoves_10000_boards() {
        final int size = 10000;

        final List<BoardFactory> boardFactories = new ArrayList<BoardFactory>();
        boardFactories.add(new EnumMapBoardFactory());
        boardFactories.add(new BitBoardFactory());

        for (BoardFactory bf : boardFactories) {

            RandomBoardList rbl = new RandomBoardList(bf,
                                                      size, //numberOfBoards
                                                      20, //minNumberOfDiscs
                                                      40); //maxNumberOfDiscs
            List<Board> boards = rbl.boards();

            long start = System.currentTimeMillis();

            for (int i = 0; i < size; i++) {
                boards.get(i).legalMoves(Player.BLACK);
            }

            long stop = System.currentTimeMillis();
            double duration = (stop - start) / 1000.;
            System.out.println("legalMoves(): bf=" + bf + ", size=" + size + ", duration=" + duration);
        }

        assertThat("Always ok.", true, is(true));
    }

    /**
     * Test performances for makeMove() method.
     */
    @Test
    public final void testPerf_makeMove_10000_boards() {
        final int size = 10000;

        final List<BoardFactory> boardFactories = new ArrayList<BoardFactory>();
        boardFactories.add(new EnumMapBoardFactory());
        boardFactories.add(new BitBoardFactory());

        for (BoardFactory bf : boardFactories) {

            RandomBoardList rbl = new RandomBoardList(bf,
                                                      size, //numberOfBoards
                                                      20, //minNumberOfDiscs
                                                      40); //maxNumberOfDiscs
            List<Board> boards = rbl.boards();

            Random r = new Random();
            List<Square> moves = new ArrayList<Square>();
            for (int i = 0; i < size; i++) {
                List<Square> legalMoves = boards.get(i).legalMoves(Player.BLACK);
                if (legalMoves.size() > 0) {
                    moves.add(legalMoves.get(r.nextInt(legalMoves.size())));
                } else {
                    moves.add(Square.NULL);
                }
            }

            long start = System.currentTimeMillis();

            for (int i = 0; i < size; i++) {
                Square move = moves.get(i);
                if (move != Square.NULL) {
                    boards.get(i).makeMove(move, Player.BLACK);
                }
            }

            long stop = System.currentTimeMillis();
            double duration = (stop - start) / 1000.;
            System.out.println("makeMove(): bf=" + bf + ", size=" + size + ", duration=" + duration);
        }

        assertThat("Always ok.", true, is(true));
    }

}
