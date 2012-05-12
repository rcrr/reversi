/*
 *  BoardFactoryHolder.java
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
import java.util.List;
import java.util.ArrayList;
import java.util.Map;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import rcrr.reversi.util.InterfaceCheck;

/**
 * A board factory holder is a singleton that holds a reference to an instance of a board's factory.
 * <p>
 * {@code BoardFactoryHolder} is mutable, and thread safe.
 * <p>
 * The access to the {@code boardFactory} field is protected by a read write lock.
 * <p>
 * @see BoardFactory
 */
public final class BoardFactoryHolder {

    /**
     * Name of the default board class.
     */
    private static final String DEFAULT_BOARD_FACTORY_CLASS = "rcrr.reversi.board.BitBoardFactory";
    //private static final String DEFAULT_BOARD_FACTORY_CLASS = "rcrr.reversi.board.EnumMapBoardFactory";

    /**
     * Name of the default board class.
     */
    private static final String BOARD_FACTORY_CLASS_KEY = "rcrr.reversi.boardfactory";

    /**
     * Singleton pattern.
     */
    private static final BoardFactoryHolder instance = new BoardFactoryHolder();

    /**
     * Returns the singleton instange.
     *
     * @return the singleton instance
     */
    public static BoardFactoryHolder getInstance() {
        return instance;
    }

    /**
     * boardFactory field.
     */
    private BoardFactory boardFactory;

    /**
     * readWriteLock field.
     */
    private final ReentrantReadWriteLock readWriteLock = new ReentrantReadWriteLock();

    /**
     * The read lock field.
     */
    private final Lock read  = readWriteLock.readLock();

    /**
     * The write lock field.
     */
    private final Lock write = readWriteLock.writeLock();

    /**
     * Private constructor prevents instantiation from other classes.
     */
    private BoardFactoryHolder() {
        BoardFactory transientBoardFactory = null;
        Class boardFactoryClass = null;
        String sBoardFactoryClass = System.getProperty(BOARD_FACTORY_CLASS_KEY);
        if (sBoardFactoryClass == null) { sBoardFactoryClass = DEFAULT_BOARD_FACTORY_CLASS; }
        try {
            boardFactoryClass = Class.forName(sBoardFactoryClass);
        } catch (ClassNotFoundException cnfe) {
            System.out.println("ClassNotFoundException=" + cnfe);
            System.out.println("Loading default board factory class. DEFAULT_BOARD_FACTORY_CLASS = " + DEFAULT_BOARD_FACTORY_CLASS);
            try {
                boardFactoryClass = Class.forName(DEFAULT_BOARD_FACTORY_CLASS);
            } catch (ClassNotFoundException cnfe1) {
                System.out.println("ClassNotFoundException=" + cnfe1);
                System.out.println("Default Board Factory Class must be loadable. Critical Error!");
                throw new RuntimeException(cnfe1);
            }
        }
        System.out.println("Board FactoryClass = " + boardFactoryClass.getName());
        if (!InterfaceCheck.doesImplement(BoardFactory.class, boardFactoryClass)) {
            throw new RuntimeException(boardFactoryClass.getName() + " does not implement the Board interface. Critical Error!");
        }
        try {
            transientBoardFactory = (BoardFactory) boardFactoryClass.newInstance();
        } catch (InstantiationException ie) {
            throw new RuntimeException(ie);
        } catch (IllegalAccessException iae) {
            throw new RuntimeException(iae);
        }
        setBoardFactory(transientBoardFactory);
    }

    public final BoardFactory boardFactory() {
        read.lock();
        try{
            return this.boardFactory;
        } finally {
            read.unlock();
        }
    }

    public final void setBoardFactory(final BoardFactory boardFactory) {
        write.lock();
        try {
            this.boardFactory = boardFactory;
        } finally {
            write.unlock();
        }
    }

}
