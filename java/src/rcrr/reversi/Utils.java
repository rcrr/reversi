/*
 *  Utils.java
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

import java.io.IOException;
import java.io.InputStream;
import java.io.ByteArrayOutputStream;
import java.io.BufferedInputStream;

/**
 * The class is a container for static utility methods.
 */
public final class Utils {

    /**
     * Open the {@code resource} parameter as an input stream, assign each
     * line read fron the resource to an entry of the returned array.
     * <p>
     * Parameter {@code resource} cannot be null.
     *
     * @param resource the java resource to open as an input stream
     * @return         an array of strings
     * @throws NullPointerException if parameter {@code resource} is null
     * @throws RuntimeException     if the resource cannot be found or opened
     */
    public static String[] readInputStreamAsStringArray(final String resource) {
        if (resource == null) { throw new NullPointerException("Parameter resource cannot be null."); }
        final InputStream in = new Iago().getClass().getClassLoader().getResourceAsStream(resource);
        if (in == null) {
            throw new RuntimeException("Resource \"" + resource + "\" cannot be found.");
        }
        final BufferedInputStream bis = new BufferedInputStream(in);
        final ByteArrayOutputStream buf;
        try {
            buf = new ByteArrayOutputStream();
            int result = bis.read();
            while (result != -1) {
                byte b = (byte) result;
                buf.write(b);
                result = bis.read();
            }
        } catch (IOException ioe) {
            throw new RuntimeException(ioe);
        }
        return buf.toString().split("\\n");
    }

    /**
     * Class constructor.
     */
    private Utils() { }

}
