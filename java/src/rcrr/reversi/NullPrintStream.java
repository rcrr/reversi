/*
 *  NullPrintStream.java
 *
 *  Copyright (c) 2011 Roberto Corradini. All rights reserved.
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

import java.io.OutputStream;
import java.io.PrintStream;

/**
 * A null object implementation for the {@code PrintStream} class.
 */
public final class NullPrintStream extends PrintStream {

    /**
     * Private inner null object implementation for the {@code OutputStream} class.
     */
    private static class NullOutputStream extends OutputStream {

        /**
         * The do nothing write implementation.
         *
         * @param b a byte to be written
         */
        @Override
        public void write(final int b) { /** Do nothing. */ }
    }

    /** Class constructor. */
    public NullPrintStream() {
            super(new NullOutputStream());
    }

}
