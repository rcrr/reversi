/*
 *  BitBoard1Test.java
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

import java.util.Map;
import java.util.HashMap;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for {@code BitBoard1} class.
 */
public class BitBoard1Test {

    public static final Board CASE_A = new BoardBuilder()
        .withSquaresLiteral(0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 2, 1, 2, 0, 0,
                            0, 0, 0, 1, 1, 1, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0)
        .build();

    @Test
    public final void testMakeMove() {

        final Square move = Square.D6;
        final Player player = Player.WHITE;

        System.out.println("CASE_A class: " + CASE_A.getClass().getName());

        System.out.println(CASE_A.printBoard());

        System.out.println("move=" + move + ", player=" + player);

        final Board result = CASE_A.makeMove(move, player);

        System.out.println("---> result ::");
        System.out.println(result.printBoard());

        assertTrue(true);

    }

    /*
    @Test
    public final void testIsLegal() {

        final Map<Square, SquareState> squares = new HashMap<Square, SquareState>();
        for (final Square sq : Square.values()) {
            squares.put(sq, BoardFixtures.BLACK_HAS_TO_PASS.get(sq));
        }
        final Board board = BitBoard.valueOf(squares);
        final BitBoard bitboard = (BitBoard) board;

        assertThat("bitboard.isLegal(Square.H7, Player.WHITE) is true.",
                   bitboard.isLegal(Square.H7, Player.WHITE),
                   is(true));

        assertThat("bitboard.isLegal(Square.A4, Player.WHITE) is true.",
                   bitboard.isLegal(Square.A4, Player.WHITE),
                   is(true));

        assertThat("bitboard.isLegal(Square.A1, Player.WHITE) is true.",
                   bitboard.isLegal(Square.A1, Player.WHITE),
                   is(false));

        assertThat("bitboard.isLegal(Square.A1, Player.BLACK) is true.",
                   bitboard.isLegal(Square.A1, Player.BLACK),
                   is(false));

    }
    */

}
