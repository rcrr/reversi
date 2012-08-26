/*
 *  BitBoard.java
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
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MUST  02111-1307, USA
 *  or visit the site <http://www.gnu.org/licenses/>.
 */

package rcrr.reversi.board;

/**
 * The class is a tool box of static methods that work on long and int values
 * manipulating the bits of such primitives.
 * <p>
 * See: <a href="https://chessprogramming.wikispaces.com/Bitscan" target="_blank">
 *      Chess programming techniques related to bitscan operations</a>.
 */
public final class BitWorks {

    /** Array for de Bruijn multiplication. */
    private static final int DEBRUIJN_64_INDEX[] = {
        63,  0, 58,  1, 59, 47, 53,  2,
        60, 39, 48, 27, 54, 33, 42,  3,
        61, 51, 37, 40, 49, 18, 28, 20,
        55, 30, 34, 11, 43, 14, 22,  4,
        62, 57, 46, 52, 38, 26, 32, 41,
        50, 36, 17, 19, 29, 10, 13, 21,
        56, 45, 25, 31, 35, 16,  9, 12,
        44, 24, 15,  8, 23,  7,  6,  5
    };
    
    /** 64 bit value for the de Bruijn's "magical" constant. */
    private static final long DEBRUIJN_64_MAGIC_CONSTANT = 0x07EDD5E59A4E28C2L;

    /** Right shift for the de Bruijn's algorithm. */
    private static final int DEBRUIJN_64_SHIFT_VALUE = 58;

    /** Log2 table. */
    static private int LOG2_ARRAY[] = new int[] {
        0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
        5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
        7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
    };

    /**
     * Returns the index of the least significant bit set in the {@code bitboard} parameter
     * via de Bruijn's perfect hashing.
     * <p>
     * Parameter {@code bitboard} must be different from {@code 0L}.
     * If no bit set is found, meaning that {@code bitboard} is equal to {@code 0L}, {@code 63} is
     * returned, that is clearly a wrong value.
     * <p>
     * See: <a href="https://chessprogramming.wikispaces.com/Bitscan#DeBruijnMultiplation" target="_blank">
     *      de Bruijn multiplication</a>
     *
     * @param bitboard long value that is scanned
     * @return         the index of the least significant bit set
     *
     * @see BitWorks#bitscanLS1B0(long)
     */
    public static int bitscanLS1B(final long bitboard) {
        /** mask isolates the least significant one bit (LS1B). */
        final long mask = bitboard & (-bitboard);
        return DEBRUIJN_64_INDEX[(int) ((mask * DEBRUIJN_64_MAGIC_CONSTANT) >>> DEBRUIJN_64_SHIFT_VALUE)];
    }

    /**
     * Returns the index of the least significant bit set in the {@code bitboard} parameter.
     * <p>
     * When parameter {@code bitboard} is {@code 0L} it returns {@code 64}.
     * <p>
     * It is here for documentation purposes. The performances of the linear access here proposed are poor,
     * the extimation of the iterations is 64/2 = 32 on avarage.
     * Here the algorithm:
     * <pre>
     *   long mask = 1L;
     *   for (int i = 0; i < 64; i++) {
     *       if ((bitboard & mask) != 0L) { return i; }
     *       mask <<= 1;
     *   }
     *   return 64;
     * <pre>
     *
     * @param bitboard long value that is scanned
     * @return         the index of the least significant bit set
     *
     * @see BitWorks#bitscanLS1B(long)
     */
    public static int bitscanLS1B0(final long bitboard) {
        long mask = 1L;
        for (int i = 0; i < 64; i++) {
            if ((bitboard & mask) != 0L) { return i; }
            mask <<= 1;
        }
        return 64;
    }

    /**
     * Returns the index of the most significant bit set in the {@code bitboard} parameter.
     * <p>
     * Parameter {@code bitboard} must be different from {@code 0L}.
     * If no bit set is found, meaning that {@code bitboard} is equal to {@code 0L}, {@code 0} is
     * returned, that is clearly a wrong value.
     * <p>
     * The proposed technique does three divide and conqueror steps, then makes a lookup in a table
     * hosting the log2 value for integers up to 255.
     * <p>
     * So far it is the preferred choice for the reversi implementation.
     *
     * @param bitboard long value that is scanned
     * @return         the index of the most significant bit set
     *
     * @see BitWorks#bitscanMS1B0(long)
     */
    public static int bitscanMS1B(final long bitboard) {
        long tmp = bitboard;
        int result = 0;
        if((tmp & 0xFFFFFFFF00000000L) != 0) { tmp >>>= 32; result  = 32; }
        if(tmp > 0x000000000000FFFFL)        { tmp >>>= 16; result |= 16; }
        if(tmp > 0x00000000000000FFL)        { tmp >>>=  8; result |=  8; }
        result |= LOG2_ARRAY[(int)tmp];
        return result;
    }

    /**
     * Returns the index of the most significant bit set in the {@code bitboard} parameter.
     * <p>
     * Parameter {@code bitboard} must be different from {@code 0L}.
     * If no bit set is found, meaning that {@code bitboard} is equal to {@code 0L}, {@code 0} is
     * returned, that is clearly a wrong value.
     * <p>
     * The inplementation follows the "divide and conqueror" approach that consumes a number
     * of operations proportional to the logarithm base 2 of 64 that is equal to 6.
     * Here the technique:
     * <pre>
     *   int low = 0;
     *   int high = 64;
     *   while (true) {
     *       final int bit = (low + high) >>> 1;
     *       final long window = bitboard >>> bit;
     *       if (low == bit) {
     *           return bit;
     *       } else if (window == 0L) {
     *           high = bit;
     *       } else {
     *           low = bit;
     *       }
     *   }
     * <pre>
     *
     * @param bitboard long value that is scanned
     * @return         the index of the most significant bit set
     *
     * @see BitWorks#bitscanMS1B(long)
     */
    public static int bitscanMS1B0(final long bitboard) {
        int low = 0;
        int high = 64;
        while (true) {
            final int bit = (low + high) >>> 1;
            final long window = bitboard >>> bit;
            if (low == bit) {
                return bit;
            } else if (window == 0L) {
                high = bit;
            } else {
                low = bit;
            }
        }

    }

    /**
     * Returns an int value having all the bit set in {@code bitsequence} turned to {@code 0}
     * except the most significant one.
     * <p>
     * When parameter {@code bitsequence} is equal to {@code 0} it returns {@code 0}.
     *
     * @param bitsequence the value analyzed
     * @return            an int value having set the bit most significative found in bitsequence
     */
    public static int highestBitSet(final int bitsequence) {
        if (bitsequence == 0) return 0;
        int result = 1;
        int tmp = bitsequence;
        if ((tmp & 0xFFFF0000) != 0) { tmp >>>= 16; result = 0x10000; }
        if (tmp > 0x000000FF)        { tmp >>>=  8; result <<= 8; }
        result <<= LOG2_ARRAY[tmp];
        return result;
    }

    /**
     * Returns a string representation of the sequence of bits.
     *
     * @param bitsequence the value passed to the procedure 
     * @return            the string representation of the bitsequence
     */
    public static String longToString(final long bitsequence) {
        final StringBuilder sb = new StringBuilder();
        final String toBinaryString = Long.toBinaryString(bitsequence);
        final int len = toBinaryString.length();
        final int missingZeroes = 64 - len;
        for (int i = 64; i > 0; i--) {
            if ((i != 64) && (i % 8 == 0)) { sb.append("."); }
            if ((missingZeroes - (64 - i)) > 0) {
                sb.append('0');
            } else {
                sb.append(toBinaryString.charAt(len - i));
            }
        }
        return sb.toString();
    }

    public static long lowestBitSet(final long bitboard) {
        return (bitboard & (bitboard - 1)) ^ bitboard;
    }

    public static int lowestBitSet(final int bitrow) {
        return (bitrow & (bitrow - 1)) ^ bitrow;
    }

    /** TESTS have to be written .... */
    public static long signedLeftShift(long x, byte signedAmount) {
        return signedAmount >= 0 ? x << signedAmount : x >>> -signedAmount;
    }

    /** TESTS have to be written .... */
    /** DOCUMENTATION ........
     * bitscan receives a long !!!
     * what happens if the int is not a byte ???
     **/
    public static int fillInBetween(final int x) {
        return ((1 << BitWorks.bitscanMS1B(x)) - 1) & ((~x & 0xFF) ^ (x - 1));
    }

    /** Class constructor. */
    private BitWorks() { }

}
