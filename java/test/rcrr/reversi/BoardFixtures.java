/*
 *  BoardFixtures.java
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

import java.util.Arrays;
import java.util.List;

/**
 * The class host a number of predefined boards.
 * <p>
 * The {@code Board} class defines immutable objects thus {@code BoardFixtures}
 * implements board instances as public static shared objects. Tests can
 * freely share the instances without any modification issue.
 * <p>
 * Boards are represented in the source code of {@code BoardFixtures} by means of
 * a literal representation organized as an array of integers. The conversion
 * from the literal definition to a {@code Board} object is carried out applying
 * the facilities given by {@code BoardBuilder}.
 * 
 * The square colors are translated as follow:
 * <pre>
 * <table border="0" cellpadding="6">
 *   <caption>SquareState conversion table</caption>
 *   <tr>
 *     <th>SquareState</th>
 *     <th>Integer Literal</th>
 *     <th>Character Representation</th>
 *   </tr>
 *   <tr>
 *     <td>EMPTY</td>
 *     <td>0</td>
 *     <td>.</td>
 *   </tr>
 *   <tr>
 *     <td>BLACK</td>
 *     <td>1</td>
 *     <td>@</td>
 *   </tr>
 *   <tr>
 *     <td>WHITE</td>
 *     <td>2</td>
 *     <td>O</td>
 *   </tr>
 *   <tr>
 *     <td>OUTER</td>
 *     <td>3</td>
 *     <td>?</td>
 *   </tr>
 * </table>
 * </pre>
 *
 * @see SquareState
 */
public final class BoardFixtures {

    /** The black player has to pass.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 O @ . @ . O . .
     * 2 @ @ @ @ @ @ @ O
     * 3 . @ O O @ @ O O
     * 4 . @ O @ O O O O
     * 5 . @ O @ O O O O
     * 6 . @ O @ @ O @ O
     * 7 . @ O @ @ @ @ .
     * 8 O O O O O O @ O
     * }
     * </pre>
     */
    public static final Board BLACK_HAS_TO_PASS = new BoardBuilder()
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
     * The board after nine moves of a generic game.
     * <p>
     * This board position takes the name {@code EARLY_GAME_B_9_MOVES}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . @ @ @ . .
     * 2 . . . . @ . . .
     * 3 . . . @ @ O O .
     * 4 . . . @ @ . . .
     * 5 . . . @ @ . . .
     * 6 . . . . @ . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     * The white player has to move, and has two legal chances:
     * <ul>
     *   <li>{@code C3} that leads to the {@code EARLY_GAME_BC3_10_MOVES} board;</li>
     *   <li>{@code C6} that leads to the {@code EARLY_GAME_BC6_10_MOVES} board.</li>
     * </ul>
     *
     * @see BoardFixtures#EARLY_GAME_BC3_10_MOVES
     * @see BoardFixtures#EARLY_GAME_BC6_10_MOVES
     */
    public static final Board EARLY_GAME_B_9_MOVES = new BoardBuilder()
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
     * The board after ten moves of a generic game.
     * <p>
     * The board is obtained taking as baseline the
     * {@code EARLY_GAME_B_9_MOVES} board and executing the move
     * <i>"white moves to c3"</i>.
     * <p>
     * This board position takes the name {@code EARLY_GAME_BC3_10_MOVES}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . @ @ @ . .
     * 2 . . . . @ . . .
     * 3 . . O O O O O .
     * 4 . . . @ @ . . .
     * 5 . . . @ @ . . .
     * 6 . . . . @ . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     *
     * @see BoardFixtures#EARLY_GAME_B_9_MOVES
     */
    public static final Board EARLY_GAME_BC3_10_MOVES = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 1, 1, 1, 0, 0,
                                        0, 0, 0, 0, 1, 0, 0, 0,
                                        0, 0, 2, 2, 2, 2, 2, 0,
                                        0, 0, 0, 1, 1, 0, 0, 0,
                                        0, 0, 0, 1, 1, 0, 0, 0,
                                        0, 0, 0, 0, 1, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * The board after ten moves of a generic game.
     * <p>
     * The board is obtained taking as baseline the
     * {@code EARLY_GAME_B_9_MOVES} board and executing the move
     * <i>"white moves to c6"</i>.
     * <p>
     * This board position takes the name {@code EARLY_GAME_BC6_10_MOVES}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . @ @ @ . .
     * 2 . . . . @ . . .
     * 3 . . . @ @ O O .
     * 4 . . . @ O . . .
     * 5 . . . O @ . . .
     * 6 . . O . @ . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     *
     * @see BoardFixtures#EARLY_GAME_B_9_MOVES
     */
    public static final Board EARLY_GAME_BC6_10_MOVES = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 1, 1, 1, 0, 0,
                                        0, 0, 0, 0, 1, 0, 0, 0,
                                        0, 0, 0, 1, 1, 2, 2, 0,
                                        0, 0, 0, 1, 2, 0, 0, 0,
                                        0, 0, 0, 2, 1, 0, 0, 0,
                                        0, 0, 2, 0, 1, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * The board after twelve moves of a generic game.
     * <p>
     * This board position takes the name {@code EARLY_GAME_C_12_MOVES}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . . . . . .
     * 2 . . . . . . . .
     * 3 O @ @ @ . . O .
     * 4 . O . O @ O . .
     * 5 . O O @ O . . .
     * 6 . . @ @ . O . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     */
    public static final Board EARLY_GAME_C_12_MOVES = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        2, 1, 1, 1, 0, 0, 2, 0,
                                        0, 2, 0, 2, 1, 2, 0, 0,
                                        0, 2, 2, 1, 2, 0, 0, 0,
                                        0, 0, 1, 1, 0, 2, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * The empty board.
     * <p>
     * This board position takes the name {@code EMPTY}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . . . . . .
     * 2 . . . . . . . .
     * 3 . . . . . . . .
     * 4 . . . . . . . .
     * 5 . . . . . . . .
     * 6 . . . . . . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     */
    public static final Board EMPTY = new BoardBuilder()
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
     * <ul>
     *   <li>{@code EQL_TEST_A}</li>
     *   <li>{@code EQL_TEST_B}</li>
     * </ul>
     */
    private static final List<Integer> EQL_LIST =
        Arrays.asList(2, 1, 0, 1, 0, 2, 0, 0,
                      1, 1, 1, 1, 1, 1, 1, 2,
                      0, 1, 2, 2, 1, 1, 2, 2,
                      0, 1, 2, 1, 2, 2, 2, 2,
                      0, 1, 2, 1, 2, 2, 2, 2,
                      0, 1, 2, 1, 1, 2, 1, 2,
                      0, 1, 2, 1, 1, 1, 1, 0,
                      2, 2, 2, 2, 2, 2, 1, 2);

    /**
     * The first board used for Equality test.
     * Boards {@code EQL_TEST_A} and {@code EQL_TEST_B} are equal
     * but are not the same.
     * <p>
     * This board position takes the name {@code EQL_TEST_A}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 O @ . @ . O . .
     * 2 @ @ @ @ @ @ @ O
     * 3 . @ O O @ @ O O
     * 4 . @ O @ O O O O
     * 5 . @ O @ O O O O
     * 6 . @ O @ @ O @ O
     * 7 . @ O @ @ @ @ .
     * 8 O O O O O O @ O
     * }
     * </pre>
     *
     * @see BoardFixtures#EQL_TEST_B
     */
    public static final Board EQL_TEST_A = new BoardBuilder()
        .withBoardLiteral(EQL_LIST)
        .build();

    /**
     * The second board used for Equality test.
     * Boards {@code EQL_TEST_A} and {@code EQL_TEST_B} are equals
     * but are not the same.
     * <p>
     * This board position takes the name {@code EQL_TEST_B}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 O @ . @ . O . .
     * 2 @ @ @ @ @ @ @ O
     * 3 . @ O O @ @ O O
     * 4 . @ O @ O O O O
     * 5 . @ O @ O O O O
     * 6 . @ O @ @ O @ O
     * 7 . @ O @ @ @ @ .
     * 8 O O O O O O @ O
     * }
     * </pre>
     *
     * @see BoardFixtures#EQL_TEST_A
     */
    public static final Board EQL_TEST_B = new BoardBuilder()
        .withBoardLiteral(EQL_LIST)
        .build();

    /**
     * A final position board. All sixtyfour squares are occupied by a player.
     * The black counts 37 discs, while the white 27.
     * <p>
     * This board position takes the name {@code EQL_TEST_B}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 O O O O O @ @ @
     * 2 O O O @ @ @ @ @
     * 3 O O O @ @ @ O @
     * 4 O O @ O @ @ O @
     * 5 @ @ O @ O @ O @
     * 6 @ O @ O @ O @ @
     * 7 @ @ @ @ @ @ O @
     * 8 @ @ @ @ O O O O
     * }
     * </pre>
     */
    public static final Board FINAL_B37_W27 = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(2, 2, 2, 2, 2, 1, 1, 1,
                                        2, 2, 2, 1, 1, 1, 1, 1,
                                        2, 2, 2, 1, 1, 1, 2, 1,
                                        2, 2, 1, 2, 1, 1, 2, 1,
                                        1, 1, 2, 1, 2, 1, 2, 1,
                                        1, 2, 1, 2, 1, 2, 1, 1,
                                        1, 1, 1, 1, 1, 1, 2, 1,
                                        1, 1, 1, 1, 2, 2, 2, 2))
        .build();

    /**
     * The board after the black's first move in D3.
     * <p>
     * This board position takes the name {@code FIRST_MOVE_D3}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . . . . . .
     * 2 . . . . . . . .
     * 3 . . . @ . . . .
     * 4 . . . @ @ . . .
     * 5 . . . @ O . . .
     * 6 . . . . . . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     */
    public static final Board FIRST_MOVE_D3 = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 1, 0, 0, 0, 0,
                                        0, 0, 0, 1, 1, 0, 0, 0,
                                        0, 0, 0, 1, 2, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * The initial board.
     * <p>
     * This board position takes the name {@code INITIAL}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . . . . . .
     * 2 . . . . . . . .
     * 3 . . . . . . . .
     * 4 . . . O @ . . .
     * 5 . . . @ O . . .
     * 6 . . . . . . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     */
    public static final Board INITIAL = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 2, 1, 0, 0, 0,
                                        0, 0, 0, 1, 2, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * Fixture boards for testing makeMove method.
     * Boards come in pairs:
     * - MAKE_MOVE_TEST_CASE_X_BEFORE is a board configuration
     * - MAKE_MOVE_TEST_CASE_X_AFTER is the expected configuration after a defined move
     */

    /**
     * Test case "<i>MAKE MOVE A</i>": before the move.
     * <p>
     * This board position takes the name {@code MAKE_MOVE_TEST_CASE_A_BEFORE}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . . . . . .
     * 2 . O O O O O . .
     * 3 . O @ @ @ O . .
     * 4 . O @ . @ O . .
     * 5 . O @ @ @ O . .
     * 6 . O O O O O . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     *
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_A_AFTER
     */
    public static final Board MAKE_MOVE_TEST_CASE_A_BEFORE = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 2, 2, 2, 2, 2, 0, 0,
                                        0, 2, 1, 1, 1, 2, 0, 0,
                                        0, 2, 1, 0, 1, 2, 0, 0,
                                        0, 2, 1, 1, 1, 2, 0, 0,
                                        0, 2, 2, 2, 2, 2, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * Test case "<i>MAKE MOVE A</i>": after the d4 move.
     * <p>
     * This board position takes the name {@code MAKE_MOVE_TEST_CASE_A_AFTER}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . . . . . . . .
     * 2 . O O O O O . .
     * 3 . O O O O O . .
     * 4 . O O O O O . .
     * 5 . O O O O O . .
     * 6 . O O O O O . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     *
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_A_BEFORE
     */
    public static final Board MAKE_MOVE_TEST_CASE_A_AFTER = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 2, 2, 2, 2, 2, 0, 0,
                                        0, 2, 2, 2, 2, 2, 0, 0,
                                        0, 2, 2, 2, 2, 2, 0, 0,
                                        0, 2, 2, 2, 2, 2, 0, 0,
                                        0, 2, 2, 2, 2, 2, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * Test case "<i>MAKE MOVE B</i>": before the move.
     * <p>
     * This board position takes the name {@code MAKE_MOVE_TEST_CASE_B_BEFORE}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 O O O O O O O .
     * 2 O @ @ @ @ @ O .
     * 3 O @ @ @ @ @ O .
     * 4 O @ @ . @ @ O .
     * 5 O @ @ @ @ @ O .
     * 6 O @ @ @ @ @ O .
     * 7 O O O O O O O .
     * 8 . . . . . . . .
     * }
     * </pre>
     *
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_B_AFTER
     */
    public static final Board MAKE_MOVE_TEST_CASE_B_BEFORE = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(2, 2, 2, 2, 2, 2, 2, 0,
                                        2, 1, 1, 1, 1, 1, 2, 0,
                                        2, 1, 1, 1, 1, 1, 2, 0,
                                        2, 1, 1, 0, 1, 1, 2, 0,
                                        2, 1, 1, 1, 1, 1, 2, 0,
                                        2, 1, 1, 1, 1, 1, 2, 0,
                                        2, 2, 2, 2, 2, 2, 2, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * Test case "<i>MAKE MOVE B</i>": after the d4 move.
     * <p>
     * This board position takes the name {@code MAKE_MOVE_TEST_CASE_B_AFTER}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 O O O O O O O .
     * 2 O O @ O @ O O .
     * 3 O @ O O O @ O .
     * 4 O O O O O O O .
     * 5 O @ O O O @ O .
     * 6 O O @ O @ O O .
     * 7 O O O O O O O .
     * 8 . . . . . . . .
     * }
     * </pre>
     *
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_B_BEFORE
     */
    public static final Board MAKE_MOVE_TEST_CASE_B_AFTER = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(2, 2, 2, 2, 2, 2, 2, 0,
                                        2, 2, 1, 2, 1, 2, 2, 0,
                                        2, 1, 2, 2, 2, 1, 2, 0,
                                        2, 2, 2, 2, 2, 2, 2, 0,
                                        2, 1, 2, 2, 2, 1, 2, 0,
                                        2, 2, 1, 2, 1, 2, 2, 0,
                                        2, 2, 2, 2, 2, 2, 2, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * Test case "<i>MAKE MOVE C</i>": before the move.
     * <p>
     * This board position takes the name {@code MAKE_MOVE_TEST_CASE_C_BEFORE}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 @ @ @ @ @ @ @ O
     * 2 @ @ @ @ @ @ @ O
     * 3 @ @ @ @ @ @ @ O
     * 4 @ @ @ . @ @ @ O
     * 5 @ @ @ @ @ @ @ O
     * 6 @ @ @ @ @ @ @ O
     * 7 @ @ @ @ @ @ @ O
     * 8 O O O O O O O O
     * }
     * </pre>
     *
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_C_AFTER
     */
    public static final Board MAKE_MOVE_TEST_CASE_C_BEFORE = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(1, 1, 1, 1, 1, 1, 1, 2,
                                        1, 1, 1, 1, 1, 1, 1, 2,
                                        1, 1, 1, 1, 1, 1, 1, 2,
                                        1, 1, 1, 0, 1, 1, 1, 2,
                                        1, 1, 1, 1, 1, 1, 1, 2,
                                        1, 1, 1, 1, 1, 1, 1, 2,
                                        1, 1, 1, 1, 1, 1, 1, 2,
                                        2, 2, 2, 2, 2, 2, 2, 2))
        .build();

    /**
     * Test case "<i>MAKE MOVE C</i>": after the d4 move.
     * <p>
     * This board position takes the name {@code MAKE_MOVE_TEST_CASE_C_AFTER}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 @ @ @ @ @ @ @ O
     * 2 @ @ @ @ @ @ @ O
     * 3 @ @ @ @ @ @ @ O
     * 4 @ @ @ O O O O O
     * 5 @ @ @ O O @ @ O
     * 6 @ @ @ O @ O @ O
     * 7 @ @ @ O @ @ O O
     * 8 O O O O O O O O
     * }
     * </pre>
     *
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_C_BEFORE
     */
    public static final Board MAKE_MOVE_TEST_CASE_C_AFTER = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(1, 1, 1, 1, 1, 1, 1, 2,
                                        1, 1, 1, 1, 1, 1, 1, 2,
                                        1, 1, 1, 1, 1, 1, 1, 2,
                                        1, 1, 1, 2, 2, 2, 2, 2,
                                        1, 1, 1, 2, 2, 1, 1, 2,
                                        1, 1, 1, 2, 1, 2, 1, 2,
                                        1, 1, 1, 2, 1, 1, 2, 2,
                                        2, 2, 2, 2, 2, 2, 2, 2))
        .build();

    /**
     * Test case "<i>MAKE MOVE D</i>": before the move.
     * <p>
     * This board position takes the name {@code MAKE_MOVE_TEST_CASE_D_BEFORE}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . @ . . O . . .
     * 2 . O . O . . . .
     * 3 @ @ @ . . . . .
     * 4 @ . @ . O . . .
     * 5 @ @ @ . . . . .
     * 6 . O . @ . . . .
     * 7 . @ . . O . . .
     * 8 . O . . . . . .
     * }
     * </pre>
     *
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_D_AFTER
     */
    public static final Board MAKE_MOVE_TEST_CASE_D_BEFORE = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 1, 0, 0, 2, 0, 0, 0,
                                        0, 2, 0, 2, 0, 0, 0, 0,
                                        1, 1, 1, 0, 0, 0, 0, 0,
                                        1, 0, 1, 0, 2, 0, 0, 0,
                                        1, 1, 1, 0, 0, 0, 0, 0,
                                        0, 2, 0, 1, 0, 0, 0, 0,
                                        0, 1, 0, 0, 2, 0, 0, 0,
                                        0, 2, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * Test case "<i>MAKE MOVE D</i>": after the b4 move.
     * <p>
     * This board position takes the name {@code MAKE_MOVE_TEST_CASE_D_AFTER}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 . @ . . O . . .
     * 2 . O . O . . . .
     * 3 @ O O . . . . .
     * 4 @ O @ . O . . .
     * 5 @ O O . . . . .
     * 6 . O . O . . . .
     * 7 . @ . . O . . .
     * 8 . O . . . . . .
     * }
     * </pre>
     *
     * @see BoardFixtures#MAKE_MOVE_TEST_CASE_D_BEFORE
     */
    public static final Board MAKE_MOVE_TEST_CASE_D_AFTER = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(0, 1, 0, 0, 2, 0, 0, 0,
                                        0, 2, 0, 2, 0, 0, 0, 0,
                                        1, 2, 2, 0, 0, 0, 0, 0,
                                        1, 2, 1, 0, 2, 0, 0, 0,
                                        1, 2, 2, 0, 0, 0, 0, 0,
                                        0, 2, 0, 2, 0, 0, 0, 0,
                                        0, 1, 0, 0, 2, 0, 0, 0,
                                        0, 2, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * A board for testing the minimax algorithm.
     * <p>
     * This board position takes the name {@code MINIMAX_TEST_CASE_A}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 O . O . O . O .
     * 2 @ . @ . @ . @ .
     * 3 . . @ . @ . @ .
     * 4 . . . . @ . . .
     * 5 . . . . . . . .
     * 6 . . . . . . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     */
    public static final Board MINIMAX_TEST_CASE_A = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(2, 0, 2, 0, 2, 0, 2, 0,
                                        1, 0, 1, 0, 1, 0, 1, 0,
                                        0, 0, 1, 0, 1, 0, 1, 0,
                                        0, 0, 0, 0, 1, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /**
     * A board for testing the minimax algorithm.
     * <p>
     * This board position takes the name {@code MINIMAX_TEST_CASE_B}.
     * <pre>
     * {@code
     * . a b c d e f g h
     * 1 O @ O . . . . .
     * 2 @ . @ . . . . .
     * 3 . . @ . . . . .
     * 4 . . . . . . . .
     * 5 . . . . . . . .
     * 6 . . . . . . . .
     * 7 . . . . . . . .
     * 8 . . . . . . . .
     * }
     * </pre>
     */
    public static final Board MINIMAX_TEST_CASE_B = new BoardBuilder()
        .withBoardLiteral(Arrays.asList(2, 1, 2, 0, 0, 0, 0, 0,
                                        1, 0, 1, 0, 0, 0, 0, 0,
                                        0, 0, 1, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0,
                                        0, 0, 0, 0, 0, 0, 0, 0))
        .build();

    /** Class constructor. */
    private BoardFixtures() { }

}
