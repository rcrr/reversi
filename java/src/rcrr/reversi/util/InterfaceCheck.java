/*
 *  InterfaceCheck.java
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

public final class InterfaceCheck {

    /**
     * Returns true if parameter {@code objectClass} is implementing the {@code parentInterface} interface, false otherwise.
     *
     * @param parentInterface the interface that is verified being an anchestor.
     * @param objectClass      the class to check.
     * @return {@code true} if {@code objectClass} is implementing {@code parentInterface}, {@code false} otherwise.
     */
    public static boolean doesImplement(final Class<?> parentInterface, final Class objectClass) {
        if (parentInterface == objectClass) { return true; }
        final Class<?>[] interfaces = objectClass.getInterfaces();
        for (final Class<?> anInterface : interfaces) {
            return doesImplement(parentInterface, anInterface);
        }
        final Class<?> superClass = objectClass.getSuperclass();
        if (superClass != null) { return doesImplement(parentInterface, superClass); }
        return false;
    }

}
