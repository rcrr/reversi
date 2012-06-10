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
     * Test the {@code files()} method.
     *
     * @see FileUtils#files()
     */
    @Test
    public final void testFiles() {

        assertThat("The files() method has to return the thirtyeight files of a board.",
                   FileUtils.files(),
                   hasItems((File) Row.R1,
                            (File) Row.R2,
                            (File) Row.R3,
                            (File) Row.R4,
                            (File) Row.R5,
                            (File) Row.R6,
                            (File) Row.R7,
                            (File) Row.R8,
                            (File) Column.A,
                            (File) Column.B,
                            (File) Column.C,
                            (File) Column.D,
                            (File) Column.E,
                            (File) Column.F,
                            (File) Column.G,
                            (File) Column.H,
                            (File) DiagonalLR.A6_C8,
                            (File) DiagonalLR.A5_D8,
                            (File) DiagonalLR.A4_E8,
                            (File) DiagonalLR.A3_F8,
                            (File) DiagonalLR.A2_G8,
                            (File) DiagonalLR.A1_H8,
                            (File) DiagonalLR.B1_H7,
                            (File) DiagonalLR.C1_H6,
                            (File) DiagonalLR.D1_H5,
                            (File) DiagonalLR.E1_H4,
                            (File) DiagonalLR.F1_H3,
                            (File) DiagonalRL.C1_A3,
                            (File) DiagonalRL.D1_A4,
                            (File) DiagonalRL.E1_A5,
                            (File) DiagonalRL.F1_A6,
                            (File) DiagonalRL.G1_A7,
                            (File) DiagonalRL.H1_A8,
                            (File) DiagonalRL.H2_B8,
                            (File) DiagonalRL.H3_C8,
                            (File) DiagonalRL.H4_D8,
                            (File) DiagonalRL.H5_E8,
                            (File) DiagonalRL.H6_F8));

    }


}
