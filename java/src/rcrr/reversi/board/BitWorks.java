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

    /**
     * Returns the index of the least significant bit set in the bitboard via de Bruijn's perfect hashing.
     *
     * See https://chessprogramming.wikispaces.com/Bitscan#DeBruijnMultiplation.
     */
    public static int bitscanLS1B(final long bitboard) {
        /** mask isolates the least significant one bit (LS1B). */
        final long mask = bitboard & (-bitboard);
        return DEBRUIJN_64_INDEX[(int) ((mask * DEBRUIJN_64_MAGIC_CONSTANT) >>> DEBRUIJN_64_SHIFT_VALUE)];
    }
        
}
