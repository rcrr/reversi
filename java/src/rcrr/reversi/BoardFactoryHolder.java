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

package rcrr.reversi;

import java.util.EnumMap;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;

import java.lang.reflect.Type;
import java.lang.reflect.WildcardType;
import java.lang.reflect.ParameterizedType;

public final class BoardFactoryHolder {

    /**
     * Name of the default board class.
     */
    private static final String DEFAULT_BOARD_FACTORY_CLASS = "rcrr.reversi.EnumMapBoardFactory";

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
     * Private constructor prevents instantiation from other classes.
     */
    private BoardFactoryHolder() {
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
        if (!isBoardFactory(boardFactoryClass)) {
            throw new RuntimeException(boardFactoryClass.getName() + " does not implement the Board interface. Critical Error!");
        }
        try {
            boardFactory = (BoardFactory) boardFactoryClass.newInstance();
        } catch (InstantiationException ie) {
            throw new RuntimeException(ie);
        } catch (IllegalAccessException iae) {
            throw new RuntimeException(iae);
        }
    }

    public final BoardFactory boardFactory() {
        return this.boardFactory;
    }

    // The three methods has to be put in a dedicated class.
    // The code is an Utility that has to work nt just for Board
    // Some refctoring is needed.
    /**
     * Returns true if parameter {@code type} is implementing the {@code BoardFactory} interface, false  otherwise.
     *
     * @param type the type to check.
     * @return {@code true} if {@code type} is implementing {@code BoardFactory}, {@code false} otherwise.
     */
    private static boolean isBoardFactory(final Type type) {
        if (type instanceof Class && isBoardFactoryClass((Class) type)) {
            return true;
        }
        if (type instanceof ParameterizedType) {
            return isBoardFactory(((ParameterizedType) type).getRawType());
        }
        if (type instanceof WildcardType) {
            final Type[] upperBounds = ((WildcardType) type).getUpperBounds();
            return upperBounds.length != 0 && isBoardFactory(upperBounds[0]);
        }
        return false;
    }

    /**
     * Checks whether the specified class parameter is an instance of a collection class.
     *
     * @param clazz {@code Class} to check.
     * @return {@code true} is {@code clazz} is instance of a collection class, {@code false} otherwise.
     */
    private static boolean isBoardFactoryClass(Class<?> clazz) {
        List<Class<?>> classes = new ArrayList<Class<?>>();
        computeClassHierarchy(clazz, classes);
        return classes.contains(BoardFactory.class);
    }

    /**
     * Get all superclasses and interfaces recursively.
     *
     * @param clazz The class to start the search with.
     * @param classes List of classes to which to add all found super classes and interfaces.
     */
    private static void computeClassHierarchy(Class<?> clazz, List<Class<?>> classes) {
        for (Class current = clazz; current != null; current = current.getSuperclass()) {
            if (classes.contains(current)) {
                return;
            }
            classes.add(current);
            for (Class currentInterface : current.getInterfaces()) {
                computeClassHierarchy(currentInterface, classes);
            }
        }
    }

}
