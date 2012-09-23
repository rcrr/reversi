/*
 *  SquareList.java
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
import java.util.ListIterator;
import java.util.Iterator;
import java.util.Collection;
import java.util.NoSuchElementException;

/**
 * A {@code List} implementation.
 */
public final class SquareList implements List<Square> {

    /** Square array. */
    private static final Square[] SQUARE_VALUES = Square.values();

    private static Square[] toArray(final long squares) {
        final Square[] result = new Square[Long.bitCount(squares)];
        int i = 0;
        for (long transientSquares = squares; transientSquares != 0; transientSquares &= transientSquares - 1) {
            final int index = BitWorks.bitscanMS1B(BitWorks.lowestBitSet(transientSquares));
            final Square square = SQUARE_VALUES[index];
            result[i] = square;
            i++;
        }
        return result;
    }

    /** squares field. */
    private final long squares;

    /** squareArray field. */
    private final Square[] squareArray;

    /**
     * Class constructor.
     *
     * @param squares board square's as a bitboard
     */
    public SquareList(final long squares) {
        this.squares = squares;
        this.squareArray = toArray(squares);
    }

    /** Private constructor. Do not use. */
    private SquareList() {
        throw new UnsupportedOperationException("No argument constructor cannot be used.");
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public boolean add(final Square element) {
        throw new UnsupportedOperationException("SquareList is immutable.");
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public void add(final int index, final Square element) {
        throw new UnsupportedOperationException("SquareList is immutable.");
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public boolean addAll(final Collection<? extends Square> c) {
        throw new UnsupportedOperationException("SquareList is immutable.");
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public boolean addAll(final int index, final Collection<? extends Square> c) {
        throw new UnsupportedOperationException("SquareList is immutable.");
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public void clear() {
        throw new UnsupportedOperationException("SquareList is immutable.");
    }

    /**
     * {@inheritDoc}
     */
    public boolean contains(final Object o) {
        if (o == null) { throw new NullPointerException(); }
        if (!(o instanceof Square)) { throw new ClassCastException(); }
        final Square sq = (Square) o;
        final long bitsquare = 1L << sq.ordinal();
        return 0L != (squares & bitsquare);
    }

    /**
     * {@inheritDoc}
     */
    public boolean containsAll(final Collection<?> c) {
        for (final Object o : c) {
            if (!contains(o)) { return false; }
        }
        return true;
    }

    /**
     * {@inheritDoc}
     */
    public Square get(final int index) {
        return squareArray[index];
    }

    /**
     * {@inheritDoc}
     */
    public int indexOf(final Object o) {
        if (o == null) { throw new NullPointerException(); }
        if (!(o instanceof Square)) { throw new ClassCastException(); }
        for (int i = 0; i < squareArray.length; i++) {
            final Square sq = (Square) o;
            if (sq == squareArray[i]) { return i; }
        }
        return -1;
    }

    /**
     * {@inheritDoc}
     */
    public boolean isEmpty() {
        return (0L == squares) ? true : false;
    }

    /**
     * {@inheritDoc}
     */
    public Iterator<Square> iterator() {
        return new Iter();
    }

    /**
     * Iterator implementation for the SquareList.
     */
    private class Iter implements Iterator<Square> {

        /** Index of the next square element to retorn. */
        int cursor = 0;

        public boolean hasNext() {
            return cursor != SquareList.this.squareArray.length;
        }

        public Square next() {
            if (cursor >= SquareList.this.squareArray.length) {
                throw new NoSuchElementException();
            }
            final Square result = SquareList.this.squareArray[cursor];
            cursor += 1;
            return result;
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    /**
     * {@inheritDoc}
     */
    public int lastIndexOf(final Object o) {
        return indexOf(o);
    }

    /**
     * {@inheritDoc}
     */
    public ListIterator<Square> listIterator() {
        return new ListIter(0);
    }

    /**
     * {@inheritDoc}
     */
    public ListIterator<Square> listIterator(final int index) {
        if (index < 0 || index > SquareList.this.squareArray.length) {
            throw new IndexOutOfBoundsException("Parameter index=" + index);
        }
        return new ListIter(index);
    }

    /**
     * List iterator implementation for the SquareList.
     */
    private class ListIter extends Iter implements ListIterator<Square> {

        ListIter(final int index) {
            super();
            cursor = index;
        }

        public boolean hasPrevious() {
            return cursor != 0;
        }

        public int nextIndex() {
            return cursor;
        }

        public int previousIndex() {
            return cursor - 1;
        }

        public Square previous() {
            cursor -= 1;
            if (cursor < 0) {
                throw new NoSuchElementException();
            }
            final Square result = SquareList.this.squareArray[cursor];
            return result;
        }

        public void set(final Square sq) {
            throw new UnsupportedOperationException("SquareList is immutable.");
        }

        public void add(final Square sq) {
            throw new UnsupportedOperationException("SquareList is immutable.");
        }
    }


    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public Square remove(final int index) {
        throw new UnsupportedOperationException("SquareList is immutable.");
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public boolean remove(final Object o) {
        throw new UnsupportedOperationException("SquareList is immutable.");
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public boolean removeAll(final Collection<?> c) {
        throw new UnsupportedOperationException("SquareList is immutable.");
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public boolean retainAll(final Collection<?> c) {
        throw new UnsupportedOperationException("SquareList is immutable.");
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public Square set(final int index, final Square element) {
        throw new UnsupportedOperationException("SquareList is immutable.");
    }

    /**
     * {@inheritDoc}
     */
    public int size() {
        return SquareList.this.squareArray.length;
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    public List<Square> subList(final int fromIndex, final int toIndex) {
        throw new UnsupportedOperationException("Should be implemented.");
    }

    /**
     * {@inheritDoc}
     */
    public Object[] toArray() {
        return squareArray.clone();
    }

    /**
     * {@inheritDoc}
     *
     * @throws UnsupportedOperationException {@inheritDoc}
     */
    //@SuppressWarnings("unchecked")
    public <T> T[] toArray(final T[] a) {
        throw new UnsupportedOperationException("Should be implemented.");
        /*
        final int size = SquareList.this.squareArray.length;
        if (a.length < size) {
            return (T[]) Arrays.copyOf(elementData, size, a.getClass());
        }
        System.arraycopy(elementData, 0, a, 0, size);
        if (a.length > size)
            a[size] = null;
        return a;
        */
    }

}
