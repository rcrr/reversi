/*
 *  BoardFixtures.java
 *
 *  Copyright (c) 2010 Roberto Corradini. All rights reserved.
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

import java.util.Arrays;
import java.util.List;

/**
 * The class host a number of predefined boards.
 * <p>
 * The {@code Board} class defines immutable objects thus {@code BoardFixtures}
 * implements board instances as public static shared objects. Tests can
 * freely share the instances without any modification issue.
 */
public class BoardFixtures {

    /** The black player has to pass. */
    public static Board BLACK_HAS_TO_PASS = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(2, 1, 0, 1, 0, 2, 0, 0,
                                        1, 1, 1, 1, 1, 1, 1, 2,
                                        0, 1, 2, 2, 1, 1, 2, 2,
                                        0, 1, 2, 1, 2, 2, 2, 2,
                                        0, 1, 2, 1, 2, 2, 2, 2,
                                        0, 1, 2, 1, 1, 2, 1, 2,
                                        0, 1, 2, 1, 1, 1, 1, 0,
                                        2, 2, 2, 2, 2, 2, 1, 2))
        .build();

    /**
     * The board after nine moves of a generic game. This board position takes
     * in the tests the name EARLY_GAME_B_9_MOVES.
     * The white player has to move, and has two legal chances.
     *  - C3 that leads to the EARLY_GAME_BC3_10_MOVES board
     *  - C6 that leads to the EARLY_GAME_BC6_10_MOVES board
     */
    public static Board EARLY_GAME_B_9_MOVES = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 1, 1, 1, 0, 0,
                                        0, 0, 0, 0, 1, 0, 0, 0,
                                        0, 0, 0, 1, 1, 2, 2, 0,
                                        0, 0, 0, 1, 1, 0, 0, 0,
                                        0, 0, 0, 1, 1, 0, 0, 0,
                                        0, 0, 0, 0, 1, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * The board after twelve moves of a generic game. This board position takes
     * in the tests the name EARLY_GAME_C_12_MOVES.
     */
    public static Board EARLY_GAME_C_12_MOVES = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        2, 1, 1, 1, 0, 0, 2, 0,
                                        0, 2, 0, 2, 1, 2, 0, 0,
                                        0, 2, 2, 1, 2, 0, 0, 0,
                                        0, 0, 1, 1, 0, 2, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /** The EARLY_GAME_BC3_10_MOVES board. */
    public static Board EARLY_GAME_BC3_10_MOVES = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 1, 1, 1, 0, 0,
                                        0, 0, 0, 0, 1, 0, 0, 0,
                                        0, 0, 2, 2, 2, 2, 2, 0,
                                        0, 0, 0, 1, 1, 0, 0, 0,
                                        0, 0, 0, 1, 1, 0, 0, 0,
                                        0, 0, 0, 0, 1, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /** The EARLY_GAME_BC6_10_MOVES board. */
    public static Board EARLY_GAME_BC6_10_MOVES = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 1, 1, 1, 0, 0,
                                        0, 0, 0, 0, 1, 0, 0, 0,
                                        0, 0, 0, 1, 1, 2, 2, 0,
                                        0, 0, 0, 1, 2, 0, 0, 0,
                                        0, 0, 0, 2, 1, 0, 0, 0,
                                        0, 0, 2, 0, 1, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /** The empty board. */
    public static Board EMPTY = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /** 
     * The list used for generating the two board
     * used for the Equality test.
     */
    private static List<Integer> EQL_LIST = 
        Arrays.asList(2, 1, 0, 1, 0, 2, 0, 0,
                      1, 1, 1, 1, 1, 1, 1, 2,
                      0, 1, 2, 2, 1, 1, 2, 2,
                      0, 1, 2, 1, 2, 2, 2, 2,
                      0, 1, 2, 1, 2, 2, 2, 2,
                      0, 1, 2, 1, 1, 2, 1, 2,
                      0, 1, 2, 1, 1, 1, 1, 0,
                      2, 2, 2, 2, 2, 2, 1, 2);

    /** The first board used for Equality test. */
    public static Board EQL_TEST_A = new BoardBuilder()
        .withBoardLiteral(EQL_LIST)
        .build();

    /** The second board used for Equality test. */
    public static Board EQL_TEST_B = new BoardBuilder()
        .withBoardLiteral(EQL_LIST)
        .build();

    /**
     * A final position board. All sixtyfour squares are occupied by a player.
     * The black counts 37 discs, while the white 27.
     */
    public static Board FINAL_B37_W27 = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(2, 2, 2, 2, 2, 1, 1, 1,
                                        2, 2, 2, 1, 1, 1, 1, 1,
                                        2, 2, 2, 1, 1, 1, 2, 1,
                                        2, 2, 1, 2, 1, 1, 2, 1,
                                        1, 1, 2, 1, 2, 1, 2, 1,
                                        1, 2, 1, 2, 1, 2, 1, 1,
                                        1, 1, 1, 1, 1, 1, 2, 1,
                                        1, 1, 1, 1, 2, 2, 2, 2))
        .build();

    /** The board after the black's first move in D3. */
    public static Board FIRST_MOVE_D3 = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 1, 0, 0, 0, 0,
                                        0, 0, 0, 1, 1, 0, 0, 0,
                                        0, 0, 0, 1, 2, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /** The initial board. */
    public static Board INITIAL = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 2, 1, 0, 0, 0,
                                        0, 0, 0, 1, 2, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

}
