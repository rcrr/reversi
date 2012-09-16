/*
 *  BitBoardTest.java
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
 * Test Suite for {@code BitBoard} class.
 */
public class BitBoardTest extends AbstractBoardTest {

    /** The applicationWideBoardFactory field. */
    private BoardFactory applicationWideBoardFactory = null;

    /** Class constructor. */
    public BitBoardTest() { }

    @Before
    public void setBoardFactory() {
        this.applicationWideBoardFactory = BoardFactoryHolder.getInstance().boardFactory();
        BoardFactoryHolder.getInstance().setBoardFactory(new BitBoardFactory());
    }

    @After
    public void unsetBoardFactory() {
        BoardFactoryHolder.getInstance().setBoardFactory(this.applicationWideBoardFactory);
        this.applicationWideBoardFactory = null;
    }

    @Test
    public final void testValueOf_withBitboard() {

        final long core  = 0x007E7E7E7E7E7E00L;
        final long edges = 0xFF818181818181FFL;

        final long[] bitboard = {core, edges}; 

        //System.out.println(BitBoard.valueOf(bitboard).printBoard());

        assertTrue(true);

    }

    @Test
    public final void testMakeMove_withBitboard() {

        final long core  = 0x007E7E7E7E7E7E00L;
        final long edges = 0xFF818181818181FFL;

        final long[] bitboard = {core, edges}; 

        //System.out.println(BitBoard.valueOf(bitboard).printBoard());

        assertTrue(true);

    }

}
