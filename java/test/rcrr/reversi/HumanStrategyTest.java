/*
 *  HumanStrategyTest.java
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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;

import rcrr.reversi.board.Square;

import org.junit.Test;

import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

/**
 * Test Suite for {@code HumanStrategy} class.
 */
public class HumanStrategyTest {

    /** Class constructor. */
    public HumanStrategyTest() { }

    /**
     * Tests the {@code move(GameSnapshot)} method when the input move is not legal.
     *
     * @see HumanStrategy#move(GameSnapshot)
     */
    @Test
    public final void testMove_whenTheMoveIsIllegal() {
        assertThat("The expected move is Move.valueOf(Square.A1)).",
                   new HumanStrategy(new ByteArrayInputStream("a1".getBytes()),
                                     new ByteArrayOutputStream())
                   .move(GameSnapshotFixtures.INITIAL),
                   is(Move.valueOf(Square.A1)));
    }

    /**
     * Tests the {@code move(GameSnapshot)} method when the input move is legal.
     *
     * @see HumanStrategy#move(GameSnapshot)
     */
    @Test
    public final void testMove_whenTheMoveIsLegal() {
        assertThat("The expected move is Move.valueOf(Square.D3)).",
                   new HumanStrategy(new ByteArrayInputStream("d3".getBytes()),
                                     new ByteArrayOutputStream())
                   .move(GameSnapshotFixtures.INITIAL),
                   is(Move.valueOf(Square.D3)));
    }

    /**
     * Tests the {@code move(GameSnapshot)} method when the input move is not a move.
     *
     * @see HumanStrategy#move(GameSnapshot)
     */
    @Test
    public final void testMove_whenTheMoveIsNotASquareLabel() {
        assertThat("The expected move is Move.valueOf(Square.D3)).",
                   new HumanStrategy(new ByteArrayInputStream("ax\nd3".getBytes()),
                                     new ByteArrayOutputStream())
                   .move(GameSnapshotFixtures.INITIAL),
                   is(Move.valueOf(Square.D3)));
    }

}
