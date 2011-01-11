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
