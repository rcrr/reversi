/*
 *  InterfaceCheckTest.java
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

package rcrr.reversi.util;

import org.junit.Test;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.instanceOf;

import java.io.Serializable;

/**
 * Test Suite for the {@code InterfaceCheck} class.
 *
 * @see InterfaceCheck
 */
public class InterfaceCheckTest {

    /** Class constructor. */
    public InterfaceCheckTest() { }

    /**
     * Tests the {@code doesImplements()} method.
     *
     * @see InterfaceCheck#doesImplement(Class, Class)
     */
    @Test
    public final void testDoesImplement_0() {
        assertThat("An instance of type Object doesn't implement the Serializable interface.",
                   InterfaceCheck.doesImplement(Serializable.class, new Object().getClass()), is(false));
    }

    @Test
    public final void testDoesImplement_1() {
        assertThat("An instance of type StrinReader does implement the Readable interface.",
                   InterfaceCheck.doesImplement(Readable.class, new java.io.StringReader("").getClass()), is(true));
    }

    @Test
    public final void testDoesImplement_2() {
        assertThat("An instance of type ArrayList does implement the Iterable interface.",
                   InterfaceCheck.doesImplement(Iterable.class, java.util.ArrayList.class), is(true));
    }

}
