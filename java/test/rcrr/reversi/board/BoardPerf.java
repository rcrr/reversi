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

import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectInputStream;
import java.io.FileInputStream;

import org.junit.Test;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Performance Suite for {@code Board} class.
 */
public class BoardPerf {

    private static final int NUMBER_OF_BOARDS = 100000;
    private static final int MIN_NUMBER_OF_DISCS = 10;
    private static final int MAX_NUMBER_OF_DISCS = 60;

    private static final BoardFactory DEFAULT_BOARD_FACTORY = new EnumMapBoardFactory();
    private static final List<BoardFactory> BOARD_FACTORIES = initBoardFactories();

    private static final List<Board> BOARDS = initBoards();
    private static final List<Square> SQUARES = RandomBoardList.randomSquares(NUMBER_OF_BOARDS);
    private static final List<Player> PLAYERS = RandomBoardList.randomPlayers(NUMBER_OF_BOARDS);

    private static List<Board> initBoards() {
        final String fileName = getFileName();
        if (new java.io.File(fileName).exists()) {
            return loadBoardList(DEFAULT_BOARD_FACTORY, fileName);
        } else {
            return saveBoardList(new RandomBoardList(DEFAULT_BOARD_FACTORY,
                                                     NUMBER_OF_BOARDS,
                                                     MIN_NUMBER_OF_DISCS,
                                                     MAX_NUMBER_OF_DISCS)
                                 .boards(),
                                 fileName);
        }
    }

    private static String getFileName() {
        return "AbstractBoard-serialized-board-list-"
            + NUMBER_OF_BOARDS + "-" + MIN_NUMBER_OF_DISCS +  "-" + MAX_NUMBER_OF_DISCS + ".dat";
    }

    @SuppressWarnings("unchecked")
    private static List<Board> loadBoardList(final BoardFactory transientBoardFactory, final String fileName) {
        List<Board> boards;
        BoardFactory currentBoardFactory = BoardFactoryHolder.getInstance().boardFactory();
        try {
            BoardFactoryHolder.getInstance().setBoardFactory(transientBoardFactory);
            try {
                final ObjectInputStream ois = new ObjectInputStream(new FileInputStream(fileName));
                boards = (List<Board>) ois.readObject();
                ois.close();
            }
            catch (Exception e) {
                throw new RuntimeException(e);
            }
        } finally {
            BoardFactoryHolder.getInstance().setBoardFactory(currentBoardFactory);
        }
        return boards;
    }

    private static List<Board> saveBoardList(final List<Board> boards, final String fileName) {
        try {
            final ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(fileName));
            oos.writeObject(boards);
            oos.close();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        return boards;
    }

    private static List<BoardFactory> initBoardFactories() {
        final List<BoardFactory> boardFactories = new ArrayList<BoardFactory>();
        boardFactories.add(new EnumMapBoardFactory());
        boardFactories.add(new BitBoardFactory.Type0());
        boardFactories.add(new BitBoardFactory.Type1());
        boardFactories.add(new BitBoardFactory.Type2());
        boardFactories.add(new IndexedBoardFactory());
        return boardFactories;
    }

    private static final double CONVERT_NANOSEC_TO_SEC = 1 / 1000000000.;

    /** Class constructor. */
    public BoardPerf() { }

    /**
     * Test performances for isLegal() method.
     */
    @Test
    public final void testPerf_isLegal() {

        for (BoardFactory bf : BOARD_FACTORIES) {

            final List<Board> boards = loadBoardList(bf, getFileName());
            final List<Square> squares = SQUARES;
            final List<Player> players = PLAYERS;

            final long start = System.nanoTime();

            for (int i = 0; i < NUMBER_OF_BOARDS; i++) {
                boards.get(i).isLegal(squares.get(i), players.get(i));
            }

            final long stop = System.nanoTime();
            final double duration = (stop - start) * CONVERT_NANOSEC_TO_SEC;
            System.out.printf("Method %16s: BoardFactory %50s, n. of runs %6d, duration[sec]=%8f\n",
                              "isLegal()", bf.getClass().toString(), NUMBER_OF_BOARDS, duration);
        }

        assertTrue("Always ok.", true);
    }

    /**
     * Test performances for legalMovesl() method.
     */
    @Test
    public final void testPerf_legalMoves() {

        for (BoardFactory bf : BOARD_FACTORIES) {

            final List<Board> boards = loadBoardList(bf, getFileName());

            final long start = System.nanoTime();

            for (int i = 0; i < NUMBER_OF_BOARDS; i++) {
                boards.get(i).legalMoves(Player.BLACK);
            }

            final long stop = System.nanoTime();
            final double duration = (stop - start) * CONVERT_NANOSEC_TO_SEC;
            System.out.printf("Method %16s: BoardFactory %50s, n. of runs %6d, duration[sec]=%8f\n",
                              "legalMoves()", bf.getClass().toString(), NUMBER_OF_BOARDS, duration);
        }

        assertTrue("Always ok.", true);
    }

    /**
     * Test performances for makeMove() method.
     */
    @Test
    public final void testPerf_makeMove() {

        for (BoardFactory bf : BOARD_FACTORIES) {

            final List<Board> boards = loadBoardList(bf, getFileName());

            /*
            Class bc = boards.get(0).getClass();
            System.out.println("bc="+bc);
            */

            final Random r = new Random();
            final List<Square> moves = new ArrayList<Square>();
            for (int i = 0; i < NUMBER_OF_BOARDS; i++) {
                final List<Square> legalMoves = boards.get(i).legalMoves(Player.BLACK);
                if (legalMoves.size() > 0) {
                    moves.add(legalMoves.get(r.nextInt(legalMoves.size())));
                } else {
                    moves.add(Square.NULL);
                }
            }

            final long start = System.nanoTime();

            for (int i = 0; i < NUMBER_OF_BOARDS; i++) {
                final Square move = moves.get(i);
                if (move != Square.NULL) {
                    boards.get(i).makeMove(move, Player.BLACK);
                }
            }

            final long stop = System.nanoTime();
            final double duration = (stop - start) * CONVERT_NANOSEC_TO_SEC;
            System.out.printf("Method %16s: BoardFactory %50s, n. of runs %6d, duration[sec]=%8f\n",
                              "makeMove()", bf.getClass().toString(), NUMBER_OF_BOARDS, duration);
        }

        assertTrue("Always ok.", true);
    }

}
