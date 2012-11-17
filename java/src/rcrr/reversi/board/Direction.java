/*
 *  Direction.java
 *
 *  Copyright (c) 2010, 2012 Roberto Corradini. All rights reserved.
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

/**
 * The directions that are available in a regular board's square are
 * eight, Up, Down, Left, Right, and the four diagonal between them.
 * <pre>
 * {@code
 * .   NW    N     NE
 *       \___|____/
 *       |        |
 *     W-| Square |-E
 *       |________|
 *       /   |     \
 *     SW    S     SE
 * }
 * </pre>
 * Each regular {@link Square} has eight neighbor ones,
 * each identified by the proper direction. Boundary squares have fewer neighbors.
 * <p>
 * The {@code Direction enum} is represented by the respective cardinal point literal,
 * for instance the Left is associated with {@code W}.
 */
public enum Direction {
    /**
     * North-West direction.
     */
    NW(Axis.DD, Versus.NEGATIVE, -9) {
        @Override
        public Direction opposite() {
            return SE;
        };
    },
    /**
     * North direction.
     */
    N(Axis.VE, Versus.NEGATIVE, -8) {
        @Override
        public Direction opposite() {
            return S;
        };
    },
    /**
     * North-East direction.
     */
    NE(Axis.DU, Versus.NEGATIVE, -7) {
        @Override
        public Direction opposite() {
            return SW;
        };
    },
    /**
     * West direction.
     */
    W(Axis.HO, Versus.NEGATIVE, -1) {
        @Override
        public Direction opposite() {
            return E;
        };
    },
    /**
     * East direction.
     */
    E(Axis.HO, Versus.POSITIVE, +1) {
        @Override
        public Direction opposite() {
            return W;
        };
    },
    /**
     * South-West direction.
     */
    SW(Axis.DU, Versus.POSITIVE, +7) {
        @Override
        public Direction opposite() {
            return NE;
        };
    },
    /**
     * South direction.
     */
    S(Axis.VE, Versus.POSITIVE, +8) {
        @Override
        public Direction opposite() {
            return N;
        };
    },
    /**
     * South-East direction.
     */
    SE(Axis.DD, Versus.POSITIVE, +9) {
        @Override
        public Direction opposite() {
            return NW;
        };
    };

    /** The null direction. */
    public static final Direction NULL = null;

    /** A generic direction instance. */
    public static final Direction AN_INSTANCE = SW;

    /** The number of directions. */
    public static final int NUMBER_OF = values().length;

    /** A bitboard being all set with the exception of column A. */
    private static final long ALL_SQUARES_EXCEPT_COLUMN_A = 0xFEFEFEFEFEFEFEFEL;

    /** A bitboard being all set with the exception of column H. */
    private static final long ALL_SQUARES_EXCEPT_COLUMN_H = 0x7F7F7F7F7F7F7F7FL;

    /**
     * Nine squares configurations arranged in an array. Position 0 is all set.
     * Position 1 has columns B, C, D, E, F, G, H set, column A is empty.
     * Position 2 has columns C, D, E, F, G, H set, column A and B are empty.
     * Positions 3 to 7 follow the same pattern, up to position 8 that has all empies.
     */
    private static final long[] ALL_SQUARES_EXCEPT_LEFT_COLUMNS = {
        0xFFFFFFFFFFFFFFFFL, 0xFEFEFEFEFEFEFEFEL, 0xFCFCFCFCFCFCFCFCL,
        0xF8F8F8F8F8F8F8F8L, 0xF0F0F0F0F0F0F0F0L, 0xE0E0E0E0E0E0E0E0L,
        0xC0C0C0C0C0C0C0C0L, 0x8080808080808080L, 0x0000000000000000L
    };

    /**
     * Nine squares configurations arranged in an array. Position 0 is all set.
     * Position 1 has columns A, B, C, D, E, F, G set, column H is empty.
     * Position 2 has columns A, B, C, D, E, F, set, column G and H are empty.
     * Positions 3 to 7 follow the same pattern, up to position 8 that has all empies.
     */
    private static final long[] ALL_SQUARES_EXCEPT_RIGTH_COLUMNS = {
        0xFFFFFFFFFFFFFFFFL, 0x7F7F7F7F7F7F7F7FL, 0x3F3F3F3F3F3F3F3FL,
        0x1F1F1F1F1F1F1F1FL, 0x0F0F0F0F0F0F0F0FL, 0x0707070707070707L,
        0x0303030303030303L, 0x0101010101010101L, 0x0000000000000000L
    };

    /** Shifts 1 position. */
    private static final int SHIFT_1 = 1;

    /** Shifts 9 positions, 8 - 1. */
    private static final int SHIFT_7 = 7;

    /** Shifts 8 positions. */
    private static final int SHIFT_8 = 8;

    /** Shifts 9 positions, 8 + 1. */
    private static final int SHIFT_9 = 9;

    /** axis field. */
    private final Axis axis;

    /** versus field. */
    private final Versus versus;

    /** shift field. It is coupled with the ordered sequence defined by the Square enum. */
    private final int shift;

    /**
     * Returns an array of integers having the length of the number of directions.
     * Each position has the value given by the shift method for the corresponding direction.
     *
     * @return an array of shift values
     */
    public static int[] shifts() {
        final Direction[] arrayOfDirections = values();
        final int[] shifts = new int[NUMBER_OF];
        for (int i = 0; i < NUMBER_OF; i++) {
            shifts[i] = arrayOfDirections[i].shift();
        }
        return shifts;
    }

    /**
     * Enum constructor.
     *
     * @param axis   the axis that the direction belongs to
     * @param versus the versus that identifies the direction
     * @param shift  the number of position to shift when moving on the ordered sequence of squares
     */
    private Direction(final Axis axis, final Versus versus, final int shift) {
        this.axis = axis;
        this.versus = versus;
        this.shift = shift;
    }

    /**
     * Returns the axis associated with the direction.
     *
     * @return the axis of the direction
     *
     * @see Axis
     */
    public Axis axis() { return axis; }

    /**
     * Returns the opposite direction.
     *
     * @return the opposite direction
     */
    public abstract Direction opposite();

    /**
     * Returns the shift to apply when moving on the ordered sequence of squares.
     *
     * @return the shift associated with the direction
     *
     * @see Square
     */
    public int shift() { return shift; }

    /**
     * Returns a new long value by shifting the {@code squares} parameter by one position
     * on the board.
     *
     * @param squares the squares set on the bitboard
     * @return        the shifted squares
     */
    public long shiftBitboard(final long squares) {
        switch (this) {
        case NW: return (squares >>> SHIFT_9) & ALL_SQUARES_EXCEPT_COLUMN_H;
        case N:  return (squares >>> SHIFT_8);
        case NE: return (squares >>> SHIFT_7) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case W:  return (squares >>> SHIFT_1) & ALL_SQUARES_EXCEPT_COLUMN_H;
        case E:  return (squares <<  SHIFT_1) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case SW: return (squares <<  SHIFT_7) & ALL_SQUARES_EXCEPT_COLUMN_H;
        case S:  return (squares <<  SHIFT_8);
        case SE: return (squares <<  SHIFT_9) & ALL_SQUARES_EXCEPT_COLUMN_A;
        default: throw new IllegalArgumentException("Undefined value for direction. dir=" + this);
        }
    }

    /**
     * Returns a new long value by shifting the {@code squares} parameter on the board, by a number of positions
     * as given by the {@code amount} parameter.
     * <p>
     * Amount must be in the 0..8 range, meaning that 0 is equal to no shift, 1 is on position,
     * and 8 always return an empy squares.
     *
     * @param squares the squares set on the bitboard
     * @param amount  the amount to shift
     * @return        the shifted squares
     */
    public long shiftBitboard(final long squares, final int amount) {
        switch (this) {
        case NW: return (squares >>> (SHIFT_9 * amount)) & ALL_SQUARES_EXCEPT_RIGTH_COLUMNS[amount];
        case N:  return (squares >>> (SHIFT_8 * amount));
        case NE: return (squares >>> (SHIFT_7 * amount)) & ALL_SQUARES_EXCEPT_LEFT_COLUMNS[amount];
        case W:  return (squares >>> (SHIFT_1 * amount)) & ALL_SQUARES_EXCEPT_RIGTH_COLUMNS[amount];
        case E:  return (squares <<  (SHIFT_1 * amount)) & ALL_SQUARES_EXCEPT_LEFT_COLUMNS[amount];
        case SW: return (squares <<  (SHIFT_7 * amount)) & ALL_SQUARES_EXCEPT_RIGTH_COLUMNS[amount];
        case S:  return (squares <<  (SHIFT_8 * amount));
        case SE: return (squares <<  (SHIFT_9 * amount)) & ALL_SQUARES_EXCEPT_LEFT_COLUMNS[amount];
        default: throw new IllegalArgumentException("Undefined value for direction. dir=" + this);
        }
    }

    /**
     * Returns the versus defined by the direction on the axis associated with it.
     *
     * @return the versus of the direction
     *
     * @see Versus
     */
    public Versus versus() { return versus; }

}
