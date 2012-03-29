/*
 *  BoardFactory.java
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

package rcrr.reversi;

import java.util.EnumMap;
import java.util.List;
import java.util.Map;

public final class BoardFactory {

    /**
     * Name of the default board class.
     */
    private static final String DEFAULT_BOARD_CLASS = "rcrr.reversi.EnumMapBoard";

    /**
     * Singleton pattern.
     */
    private static final BoardFactory instance = new BoardFactory();

    /**
     * Private constructor prevents instantiation from other classes.
     */
    private BoardFactory() {
        String sBoardClass = System.getProperty("rcrr.reversi.board.class");
        if (sBoardClass == null) { sBoardClass = DEFAULT_BOARD_CLASS; }
        try {
            Class boardClass = Class.forName(sBoardClass);
        } catch (ClassNotFoundException cnfe) {
            System.out.println("ClassNotFoundException=" + cnfe);
            System.out.println("Loading default board class. DEFAULT_BOARD_CLASS=" + DEFAULT_BOARD_CLASS);
            try {
                Class boardClass = Class.forName(DEFAULT_BOARD_CLASS);
            } catch (ClassNotFoundException cnfe1) {
                System.out.println("ClassNotFoundException=" + cnfe1);
                System.out.println("Default Board Class must be loadable. Critical Error!");
                throw new RuntimeException(cnfe1);
            }
        }
    }

    /**
     * Returns the singleton instange.
     *
     * @return the singleton instance
     */
    public static BoardFactory getInstance() {
        return instance;
    }

}
