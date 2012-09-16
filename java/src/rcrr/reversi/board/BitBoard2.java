/*
 *  BitBoard2.java
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

import java.math.BigInteger;

import java.util.Arrays;
import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * A board concrete implementation in the bitboard family.
 * <p>
 * A {@code BitBoard2} object holds the information of the state of each board's square.
 * The board state is kept into a long array having a length equal two.
 * The first entry keeps the black squares, the second the whites.
 * <p>
 * The board applyes some variants to the {@code BitBoard1} implementation ....
 * <p>
 * {@code BitBoard2} is immutable.
 * <p>
 * @see Square
 */
public final class BitBoard2 extends BitBoard1 {

    private static final boolean LOG = true;
    private static int callsTolegalMoves = 0;
    private static int callsToMakeMove = 0;
    private static int callsToConstructor = 0;

    private static final Direction[] DIRECTION_VALUES = Direction.values();

    private static final int[] DIRECTION_SHIFTS = Direction.shifts(); 

    private static final Square[] SQUARE_VALUES = Square.values();

    private static final long ALL_SQUARES_EXCEPT_COLUMN_A = 0x7F7F7F7F7F7F7F7FL;
    private static final long ALL_SQUARES_EXCEPT_COLUMN_H = 0xFEFEFEFEFEFEFEFEL;

    /** Used for masking a byte when using integer values. */
    static final int BYTE_MASK_FOR_INT = 0xFF;

    /**
     * This array has sixtyfour entries. The index, having range 0-63, represent one of the squares
     * of the table. Each entry is a bitboard mask having set all the squares that are
     * reachable moving along the eigth directions, when starting from the square identified by
     * the index itself.
     * <p>
     * Values do not change.
     */
    private static final long[] BITBOARD_MASK_FOR_ALL_DIRECTIONS = new long[] {
        0x81412111090503FEL, 0x02824222120A07FDL, 0x0404844424150EFBL, 0x08080888492A1CF7L, 
        0x10101011925438EFL, 0x2020212224A870DFL, 0x404142444850E0BFL, 0x8182848890A0C07FL, 
        0x412111090503FE03L, 0x824222120A07FD07L, 0x04844424150EFB0EL, 0x080888492A1CF71CL, 
        0x101011925438EF38L, 0x20212224A870DF70L, 0x4142444850E0BFE0L, 0x82848890A0C07FC0L, 
        0x2111090503FE0305L, 0x4222120A07FD070AL, 0x844424150EFB0E15L, 0x0888492A1CF71C2AL, 
        0x1011925438EF3854L, 0x212224A870DF70A8L, 0x42444850E0BFE050L, 0x848890A0C07FC0A0L, 
        0x11090503FE030509L, 0x22120A07FD070A12L, 0x4424150EFB0E1524L, 0x88492A1CF71C2A49L, 
        0x11925438EF385492L, 0x2224A870DF70A824L, 0x444850E0BFE05048L, 0x8890A0C07FC0A090L, 
        0x090503FE03050911L, 0x120A07FD070A1222L, 0x24150EFB0E152444L, 0x492A1CF71C2A4988L, 
        0x925438EF38549211L, 0x24A870DF70A82422L, 0x4850E0BFE0504844L, 0x90A0C07FC0A09088L, 
        0x0503FE0305091121L, 0x0A07FD070A122242L, 0x150EFB0E15244484L, 0x2A1CF71C2A498808L, 
        0x5438EF3854921110L, 0xA870DF70A8242221L, 0x50E0BFE050484442L, 0xA0C07FC0A0908884L, 
        0x03FE030509112141L, 0x07FD070A12224282L, 0x0EFB0E1524448404L, 0x1CF71C2A49880808L, 
        0x38EF385492111010L, 0x70DF70A824222120L, 0xE0BFE05048444241L, 0xC07FC0A090888482L, 
        0xFE03050911214181L, 0xFD070A1222428202L, 0xFB0E152444840404L, 0xF71C2A4988080808L, 
        0xEF38549211101010L, 0xDF70A82422212020L, 0xBFE0504844424140L, 0x7FC0A09088848281L
    };

    public static String printLog() {
        String ret = "callsTolegalMoves=" + callsTolegalMoves + ", callsToMakeMove=" + callsToMakeMove + ", callsToConstructor=" + callsToConstructor;
        return ret;
    }

    /**
     * Base static factory for the class.
     * <p>
     * {@code squares} must be not null, and must have an entry for every board square.
     * Given that the map cannot have duplicate keys, its size must be equal to the number
     * of class instances defined by the {@code Square} enum.
     *
     * @param  squares the map of squares
     * @return         a new board having as state the given square map
     * @throws NullPointerException     if parameter {@code squares} is null
     * @throws IllegalArgumentException if the {@code squares} is not complete
     */
    static Board valueOf(final Map<Square, SquareState> squares) {
        BoardUtils.checkForConsistencyTheSquareMap(squares);
        return new BitBoard2(BoardUtils.mapToBitboard(squares));
    }

    /**
     * Static factory for the class.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set are not valid.
     * Precondition on the {@code bitboard} parameter are not enforced.
     *
     * @param  bitboard the bitboard field
     */
    static Board valueOf(final long[] bitboard) {
        return new BitBoard2(bitboard);
    }

    /**
     * Returns an int having the bits from position 8 to position 31 set to zero,
     * and the bits from position 0 to position 7, corresponding to Row One in the board,
     * copied from the Column A of the {@code bitboard} parameter.
     *
     * @param bitboard the bitboard representation for one color
     * @return         colum a copied to row one, all other position are set to zero         
     */
    static int trasformColumnAInRow0(long bitboard) {
        bitboard &= 0x0101010101010101L;
        bitboard |= bitboard >> 28;
        bitboard |= bitboard >> 14;
        bitboard |= bitboard >> 7;
        return (int)bitboard & BYTE_MASK_FOR_INT;
    }

    private static int trasformDiagonalA1H8InRow0(long bitboard) {
        bitboard &= 0x8040201008040201L;
        bitboard |= bitboard >> 32;
        bitboard |= bitboard >> 16;
        bitboard |= bitboard >> 8;
        return (int)bitboard & BYTE_MASK_FOR_INT;
    }

    private static int trasformDiagonalH1A8InRow0(long bitboard) {
        bitboard &= 0x0102040810204080L;
        bitboard |= bitboard >> 32;
        bitboard |= bitboard >> 16;
        bitboard |= bitboard >> 8;
        return (int)bitboard & BYTE_MASK_FOR_INT;
    }

    private static long reTrasformRow0BackToColumnA(int bitrow) {
        bitrow |= bitrow << 7;
        bitrow |= bitrow << 14;
        long z = (long)bitrow | ((long)bitrow << 28);
        return z & 0x0101010101010101L;
    }

    private static long reTrasformRow0BackToDiagonalA1H8(int bitrow) {
        bitrow |= bitrow << 8;
        long z = (long)bitrow | ((long)bitrow << 16);
        z |= z << 32;
        return z & 0x8040201008040201L;
    }

    private static long reTrasformRow0BackToDiagonalH1A8(int bitrow) {
        bitrow |= bitrow << 8;
        bitrow |= (bitrow & 0x1122) << 16;
        long z = (long)bitrow | ((long)bitrow << 32);
        return z & 0x0102040810204080L;
    }

    private static long neighbors(final long squares) {
        long neighbors = squares;
        neighbors |= (neighbors >>> 8);
        neighbors |= (neighbors >>> 1) & ALL_SQUARES_EXCEPT_COLUMN_A;
        neighbors |= (neighbors <<  1) & ALL_SQUARES_EXCEPT_COLUMN_H;
        neighbors |= (neighbors <<  8);
        return neighbors;
    }

    private static long neighbor(final long square, final Direction dir, final int amount) {
        long result = square;
        for (int i = 0; i < amount; i++) {
            result = neighbor(result, dir);
        }
        return result;
    }

    private static long neighbor(final long square, final Direction dir) {
        switch (dir) {
        case NW: return (square >>> 9) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case N:  return (square >>> 8);
        case NE: return (square >>> 7) & ALL_SQUARES_EXCEPT_COLUMN_H;
        case W:  return (square >>> 1) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case E:  return (square <<  1) & ALL_SQUARES_EXCEPT_COLUMN_H;
        case SW: return (square <<  7) & ALL_SQUARES_EXCEPT_COLUMN_A;
        case S:  return (square <<  8);
        case SE: return (square <<  9) & ALL_SQUARES_EXCEPT_COLUMN_H;
        default: throw new IllegalArgumentException("Undefined value for direction. dir=" + dir);
        }
    }

    /**
     * Class constructor.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set to one are not allowed.
     *
     * @param  bitboard the bitboard field
     */
    private BitBoard2(final long[] bitboard) {
        super(bitboard);
        if (LOG) callsToConstructor++;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Square> legalMoves(final Player player) {

        if (LOG) callsTolegalMoves++;

        //final List<Square> legalMoves = super.legalMoves(player); 

        if (player == null) { throw new NullPointerException("Parameter player must be not null."); }
        final List<Square> legalMoves = new ArrayList<Square>(); 
        // The loop modifies likelyMoves removing the less significative bit set on each iteration.
        for (long likelyMoves = likelyMoves(player); likelyMoves != 0; likelyMoves &= likelyMoves - 1) {
            final int iSquare = BitWorks.bitscanMS1B(BitWorks.lowestBitSet(likelyMoves));
            final Square square = SQUARE_VALUES[iSquare];
            if (isLegal(square, player)) {
                legalMoves.add(square);
            }
        }
        
        /*
        final long lm = legalMoves(player.ordinal());

        final List<Square> lmSquares = new ArrayList<Square>();

        long lmEroding = lm;
        while (lmEroding != 0L) { 
            final int movePosition = BitWorks.bitscanLS1B(lmEroding);
            final Square move = Square.values()[movePosition];
            lmSquares.add(move);
            lmEroding &= ~(1L << movePosition);
        }
        */

        /*
        if (!legalMoves.equals(lmSquares)) {
            System.out.println("legalMoves=" + legalMoves + ", lmSquares=" + lmSquares);
        }
        */

        return legalMoves;
    }

    /**
     * Returns a new updated board to reflect move by player. This static
     * factory executes a game move to the board and returns a new one,
     * reflecting the move. The original board is not modified.
     * <p>
     * A null value for player is not allowed, a {@code NullPointerException}
     * is thrown in such a case.
     * <p>
     * A null value for move is allowed, and moreover is the only valid value
     * acceptable by the method, when the player has not any legal move.
     * Otherwise a null move is forbidden, and a {@code NullPointerException}
     * is risen.
     * <p>
     * The method does check if the move is legal. It throws an
     * {@code IllegalArgumentException} in case it is not.
     *
     * @param  move   the board square where to put the disk
     * @param  player the disk color to put on the board
     * @return        a new {@code Board} reflecting the move made
     * @throws NullPointerException     if parameter {@code move}
     *                                  or {@code player} is null
     * @throws IllegalArgumentException if the {@code move}
     *                                  by {@code player} is illegal
     */
    public Board makeMove(final Square move, final Player player) {

        if (LOG) callsToMakeMove++;

        if (player == null) {
            throw new NullPointerException("Parameter player must be not null.");
        }
        if (move == null) {
            if (hasAnyLegalMove(player)) {
                throw new NullPointerException("Parameter move must be not null when a legal one is available.");
            } else {
                return this;
            }
        }
        if (!isLegal(move, player)) {
            throw new IllegalArgumentException("The move<"
                                               + move + "> by player<"
                                               + player + "> is illegal.");
        }

        return valueOf(makeMoveImpl(move, player));
    }

    private long likelyMoves(final Player player) {
        final int intPlayer = player.ordinal(); 
        final int intOpponent = intPlayer ^ WHITE;
        final long empties = ~(bitboard[BLACK] | bitboard[WHITE]);
        return neighbors(bitboard[intOpponent]) & empties;
    }

}
