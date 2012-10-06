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
 * A {@code List} implementation dedicated for {@code Square} objects.
 * <p>
 * The implementation is based on {@code long} field that holds the squares
 * partecipating to the list. Squares are sorted from A1, B1, C1, ... , to H8.
 * No duplicates are possible.
 * <p>
 * The objects belonging to this class are immutable.
 */
public final class SquareList implements List<Square> {

    /** Square array. */
    private static final Square[] SQUARE_VALUES = Square.values();

    /**
     * Converts the {@code squares} parameter to an array od squares.
     *
     * @param squares the squares collected as a long value
     * @return        an array of squares
     */
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

    /** size field. */
    private final int size;

    /** squareArray field. */
    private final Square[] squareArray;

    /** squares field. */
    private final long squares;

    /**
     * Class constructor.
     *
     * @param squares board square's as a bitboard
     */
    public SquareList(final long squares) {
        this.squares = squares;
        this.squareArray = toArray(squares);
        this.size = SquareList.this.squareArray.length;
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

        /** Index of the next square element to return. */
        private int cursor = 0;

        /**
         * Returns {@code true} if the iteration has more elements.
         *
         * @return {@code true} if the iteration has more elements
         */
        public boolean hasNext() {
            return cursor != size();
        }

        /**
         * Returns the next element in the iteration.
         *
         * @return the next element in the iteration
         * @throws NoSuchElementException if the iteration has no more elements
         */
        public Square next() {
            if (cursor >= size()) {
                throw new NoSuchElementException();
            }
            final Square result = SquareList.this.squareArray[cursor];
            cursor += 1;
            return result;
        }

        /**
         * The operation is not supported by this iterator.
         *
         * @throws UnsupportedOperationException always, the remove operation is not supported by this iterator
         */
        public void remove() {
            throw new UnsupportedOperationException();
        }

        /**
         * Accessor method for the cursor field.
         *
         * @return the accessor field
         */
        final int cursor() { return cursor; }

        /**
         * Setter method for the cursor field.
         * Use it only in a subclass constructor!
         *
         * @param cursor the field new value
         */
        final void setCursor(final int cursor) { this.cursor = cursor; }
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
        if (index < 0 || index > size()) {
            throw new IndexOutOfBoundsException("Parameter index=" + index);
        }
        return new ListIter(index);
    }

    /**
     * List iterator implementation for the SquareList.
     */
    private class ListIter extends Iter implements ListIterator<Square> {

        /**
         * Class constructor.
         *
         * @param index index value where to set the cursor field
         */
        ListIter(final int index) {
            super();
            setCursor(index);
        }

        /** {@inheritDoc} */
        public boolean hasPrevious() {
            return cursor() != 0;
        }

        /** {@inheritDoc} */
        public int nextIndex() {
            return cursor();
        }

        /** {@inheritDoc} */
        public int previousIndex() {
            return cursor() - 1;
        }

        /** {@inheritDoc} */
        public Square previous() {
            setCursor(cursor() - 1);
            if (cursor() < 0) {
                throw new NoSuchElementException();
            }
            final Square result = SquareList.this.squareArray[cursor()];
            return result;
        }

        /**
         * The operation is not supported by this iterator.
         *
         * @param sq square value to set to the current cursor position
         * @throws UnsupportedOperationException always, the set operation is not supported by this iterator
         */
        public void set(final Square sq) {
            throw new UnsupportedOperationException("SquareList is immutable.");
        }

        /**
         * The operation is not supported by this iterator.
         *
         * @param sq square to add to the list
         * @throws UnsupportedOperationException always, the add operation is not supported by this iterator
         */
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
        return this.size;
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
    public <T> T[] toArray(final T[] a) {
    //@SuppressWarnings("unchecked")
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
