/*
 *  BoardFactoryHolderTest.java
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

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for {@code BoardFactoryHolder} class.
 */
public class BoardFactoryHolderTest {

    /** Class constructor. */
    public BoardFactoryHolderTest() { }

    /**
     * Tests the {@code getInstance()} method.
     *
     * @see BoardFactoryHolder#getInstance()
     */
    @Test
    public final void testGetInstance() {
        assertThat("The method getInstance() has to return the proper class.",
                   BoardFactoryHolder.getInstance(),
                   instanceOf(BoardFactoryHolder.class));

        assertEquals("Calling twice the method it has to return the same reference.",
                     BoardFactoryHolder.getInstance(),
                     BoardFactoryHolder.getInstance());
    }

    /**
     * Tests the {@code setBoardFactory(BoardFactory)} method.
     *
     * @see BoardFactoryHolder#setBoardFactory(BoardFactory)
     */
    @Test
    public final void testSetBoardFactory_hasToBeDeveloped() {
        assertThat("The test has to be developed.",
                   true,
                   is(true));
    }

    /**
     * Tests the {@code boardFactory()} method.
     *
     * @see BoardFactoryHolder#boardFactory()
     */
    @Test
    public final void testBoardFactory_hasToBeDeveloped() {
        assertThat("The test has to be developed.",
                   true,
                   is(true));
    }

}
