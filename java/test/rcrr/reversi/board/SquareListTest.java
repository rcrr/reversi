/*
 *  SquareListTest.java
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

import java.util.List;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.ListIterator;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.CoreMatchers.instanceOf;

/**
 * Test Suite for {@code SquareList} class.
 */
public class SquareListTest {

    @Test(expected = UnsupportedOperationException.class)
    public final void testAdd_1() {
        new SquareList(0L).add(Square.A1);
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testAdd_2() {
        new SquareList(0L).add(0, Square.A1);
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testAddAll_1() {
        final List<Square> coll = new ArrayList<Square>();
        coll.add(Square.D3);
        new SquareList(0L).addAll(coll);
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testAddAll_2() {
        final List<Square> coll = new ArrayList<Square>();
        coll.add(Square.D3);
        new SquareList(0L).addAll(1, coll);
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testClear() {
        new SquareList(0L).clear();
    }

    @Test
    public final void testContains() {
        assertThat("new SquareList(1L).contains(Square.A1) is true.",
                   new SquareList(1L).contains(Square.A1),
                   is(true));
        assertThat("new SquareList(2L).contains(Square.A1) is false.",
                   new SquareList(2L).contains(Square.A1),
                   is(false));
    }

    @Test
    public final void testContainsAll() {
        final List<Square> coll = new ArrayList<Square>();
        coll.add(Square.A1);
        coll.add(Square.B1);
        assertThat("new SquareList(3L).containsAll(coll) is true.",
                   new SquareList(3L).containsAll(coll),
                   is(true));
        assertThat("new SquareList(2L).containsAll(call) is false.",
                   new SquareList(2L).containsAll(coll),
                   is(false));
    }

    @Test
    public final void testGet() {
        assertThat("new SquareList(3L).get(0) is true.",
                   new SquareList(3L).get(0),
                   is(Square.A1));
        assertThat("new SquareList(3L).get(1) is true.",
                   new SquareList(3L).get(1),
                   is(Square.B1));
    }

    @Test(expected = ClassCastException.class)
    public final void testIndexOf_whenParameterOIsNotASquare() {
        new SquareList(0L).indexOf(new Object());
    }

    @Test(expected = NullPointerException.class)
    public final void testIndexOf_whenParameterOIsNull() {
        new SquareList(0L).indexOf(null);
    }

    @Test
    public final void testIndexOf() {
        assertThat("new SquareList(3L).indexOf(Square.A1) is 0.",
                   new SquareList(3L).indexOf(Square.A1),
                   is(0));
        assertThat("new SquareList(2L).indexOf(Square.B1) is 0.",
                   new SquareList(2L).indexOf(Square.B1),
                   is(0));
        assertThat("new SquareList(3L).indexOf(Square.B1) is 1.",
                   new SquareList(3L).indexOf(Square.B1),
                   is(1));
    }

    @Test
    public final void testIsEmpty() {
        assertThat("new SquareList(3L).isEmpty() is false.",
                   new SquareList(3L).isEmpty(),
                   is(false));
        assertThat("new SquareList(0L).isEmpty() is true.",
                   new SquareList(0L).isEmpty(),
                   is(true));
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testIterator_whenCallingRemove() {
        final Iterator<Square> iter = new SquareList(3L).iterator();
        iter.remove();
    }

    @Test
    public final void testIterator() {
        final Iterator<Square> iter = new SquareList(3L).iterator();
        assertThat("iter.hasNext() is true.",
                   iter.hasNext(),
                   is(true));
        assertThat("iter.next() is Square.A1.",
                   iter.next(),
                   is(Square.A1));
        assertThat("iter.hasNext() is true.",
                   iter.hasNext(),
                   is(true));
        assertThat("iter.next() is Square.B1.",
                   iter.next(),
                   is(Square.B1));
        assertThat("iter.hasNext() is false.",
                   iter.hasNext(),
                   is(false));
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testListIterator_whenCallingSet() {
        final ListIterator<Square> li = new SquareList(3L).listIterator();
        li.set(Square.D3);
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testListIterator_whenCallingAdd() {
        final ListIterator<Square> li = new SquareList(3L).listIterator();
        li.add(Square.D3);
    }

    @Test
    public final void testListIterator_0() {
        final ListIterator<Square> li = new SquareList(3L).listIterator();
        assertThat("iter.hasPrevious() is false.",
                   li.hasPrevious(),
                   is(false));
        assertThat("iter.next() is Square.A1.",
                   li.next(),
                   is(Square.A1));
        assertThat("iter.hasPrevious() is true.",
                   li.hasPrevious(),
                   is(true));
        assertThat("iter.next() is Square.B1.",
                   li.next(),
                   is(Square.B1));
        assertThat("iter.nextIndex() is 2.",
                   li.nextIndex(),
                   is(2));
        assertThat("iter.previousIndex() is 1.",
                   li.previousIndex(),
                   is(1));
        assertThat("iter.previous() is Square.B1.",
                   li.previous(),
                   is(Square.B1));
        assertThat("iter.previous() is Square.A1.",
                   li.previous(),
                   is(Square.A1));
    }

    @Test
    public final void testListIterator_1() {
        final ListIterator<Square> li = new SquareList(3L).listIterator(1);
        assertThat("iter.hasPrevious() is true.",
                   li.hasPrevious(),
                   is(true));
        assertThat("iter.next() is Square.B1.",
                   li.next(),
                   is(Square.B1));
    }

    @Test
    public final void testLastIndexOf() {
        assertThat("new SquareList(3L).lastIndexOf(Square.A1) is 0.",
                   new SquareList(3L).lastIndexOf(Square.A1),
                   is(0));
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testRemove_IntParameter() {
        new SquareList(1L).remove(0);
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testRemove_ObjectParameter() {
        new SquareList(1L).remove(Square.A1);
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testRemoveAll() {
        final List<Square> coll = new ArrayList<Square>();
        coll.add(Square.A1);
        coll.add(Square.B1);
        new SquareList(1L).removeAll(coll);
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testRetainAll() {
        final List<Square> coll = new ArrayList<Square>();
        coll.add(Square.A1);
        coll.add(Square.B1);
        new SquareList(1L).retainAll(coll);
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testSet() {
        new SquareList(1L).set(0, Square.H8);
    }

    @Test
    public final void testSize() {
        assertThat("new SquareList(3L).size() is 2.",
                   new SquareList(3L).size(),
                   is(2));
    }

    @Test
    public final void testToArray_A() {
        assertThat("(Square) new SquareList(1L).toArray()[0] is Square.A1",
                   (Square) new SquareList(1L).toArray()[0],
                   is(Square.A1));
        assertThat("(Square) new SquareList(7L).toArray()[2] is Square.C1",
                   (Square) new SquareList(7L).toArray()[2],
                   is(Square.C1));
    }

    @Test(expected = UnsupportedOperationException.class)
    public final void testToArray_B() {
        new SquareList(1L).toArray(new Square[1]);
    }


}
