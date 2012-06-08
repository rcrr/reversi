/*
 *  FileUtilsTest.java
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
import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import static org.junit.Assert.assertThat;

import static org.hamcrest.CoreMatchers.is;

import static org.junit.matchers.JUnitMatchers.hasItems;


/**
 * Test Suite for {@code FileUtils} class.
 */
public class FileUtilsTest {

    /** Class constructor. */
    public FileUtilsTest() { }

    /**
     * Test the {@code files(Square)} method.
     * <p>
     * The method receives the {@code square} parameter, it cannot contains null values.
     *
     * @see FileUtils#files(Square)
     */
    @Test
    public final void testFiles() {

        assertThat("Square A1 is crossed by column A, row, R1, and diagonal left-up to right-down A1_H8.",
                   FileUtils.files(Square.A1),
                   hasItems((File) Column.A,
                            (File) Row.R1,
                            (File) DiagonalLR.A1_H8));

    }


}
