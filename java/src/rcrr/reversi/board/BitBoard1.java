/*
 *  BitBoard1.java
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

import java.util.Map;
import java.util.List;
import java.util.ArrayList;

/**
 * A board concrete implementation in the bitboard family.
 * <p>
 * A {@code BitBoard1} object holds the information of the state of each board's square.
 * The board state is kept into a long array having a length equal two.
 * The first entry keeps the black squares, the second the whites.
 * <p>
 * The board uses the best machinery so far identified ....
 * <p>
 * {@code BitBoard1} is immutable.
 * <p>
 * @see Square
 */
public class BitBoard1 extends BitBoard {

    /** It turns on or off the class logging for performances. */
    private static final boolean LOG = true;

    /** Collects the number of call to legalMoves method. */
    private static int callsTolegalMoves = 0;

    /** Collects the number of call to makeMove method. */
    private static int callsToMakeMove = 0;

    /** Collects the number of call to the class constructor. */
    private static int callsToConstructor = 0;

    /** Collects the number of call to isLegal method. */
    private static int callsToIsLegal = 0;

    /** Collects the number of likely moves in the legalMoves method. */
    private static int numberOflikelyMoves = 0;

    /** Collects the number of legal moves in the legalMoves method. */
    private static int numberOflegalMoves = 0;

    /** Caches the direction enum values in a local array. */
    private static final Direction[] DIRECTION_VALUES = Direction.values();

    /** Caches the direction's shift values in a local array. */
    private static final int[] DIRECTION_SHIFTS = Direction.shifts();

    /** Caches the square enum values in a local array. */
    private static final Square[] SQUARE_VALUES = Square.values();

    /** A bitboard being all set with the exception of column A. */
    private static final long ALL_SQUARES_EXCEPT_COLUMN_A = 0xFEFEFEFEFEFEFEFEL;

    /** A bitboard being all set with the exception of column H. */
    private static final long ALL_SQUARES_EXCEPT_COLUMN_H = 0x7F7F7F7F7F7F7F7FL;

    /** A bitboard being set on column A. */
    private static final long COLUMN_A = 0x0101010101010101L;

    /** A bitboard being set on diagonal A1-H8. */
    private static final long DIAGONAL_A1_H8 = 0x8040201008040201L;

    /** A bitboard being set on diagonal H1-A8. */
    private static final long DIAGONAL_H1_A8 = 0x0102040810204080L;

    /** A bitboard being set on diagonal H1-A8. */
    private static final long SQUARES_B1_F1_A2_E2 = 0x1122;

    /** Macic number 3. */
    private static final int MAGIC_NUMBER_3 = 3;

    /** Macic number 7. */
    private static final int MAGIC_NUMBER_7 = 7;

    /** Macic number 8. */
    private static final int MAGIC_NUMBER_8 = 8;

    /** Macic number 14. */
    private static final int MAGIC_NUMBER_14 = 14;

    /** Macic number 16. */
    private static final int MAGIC_NUMBER_16 = 16;

    /** Macic number 28. */
    private static final int MAGIC_NUMBER_28 = 28;

    /** Macic number 32. */
    private static final int MAGIC_NUMBER_32 = 32;

    /** Macic number 256. */
    private static final int MAGIC_NUMBER_256 = 256;

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

    /**
     * This array is an implementation of the precomputed table that contains the effects of moving
     * a piece in any of the eigth squares in a row.
     * The size is so computed:
     *  - there are 256 arrangments of player discs,
     *  - and 256 arrangements of opponent pieces,
     *  - the potential moves are 8.
     * So the array size is 256 * 256 * 8 = 524,288 Bytes = 512kB.
     * Not all the entries are legal! The first set of eigth bits and the second one (opponent row)
     * must not set the same position.
     *
     * The index of the array is computed by this formula:
     * index = playerRow | (opponentRow << 8) | (movePosition << 16);
     */
    private static final byte[] BITROW_CHANGES_FOR_PLAYER_ARRAY = initializeBitrowChangesForPlayerArray();

    /**
     * Returns info for performance statistics.
     *
     * @return a string having class performance statistics
     */
    public static String printLog() {
        String ret = "callsTolegalMoves=" + callsTolegalMoves + ", callsToMakeMove=" + callsToMakeMove
            + ", callsToConstructor=" + callsToConstructor + ", numberOflikelyMoves=" + numberOflikelyMoves
            + ", numberOflegalMoves=" + numberOflegalMoves + ", callsToIsLegal=" + callsToIsLegal;
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
        return new BitBoard1(BoardUtils.mapToBitboard(squares));
    }

    /**
     * Static factory for the class.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set are not valid.
     * Precondition on the {@code bitboard} parameter are not enforced.
     *
     * @param  bitboard the bitboard field
     * @return         a new board having as state the given bitboard array
     */
    static Board valueOf(final long[] bitboard) {
        return new BitBoard1(bitboard);
    }

    /**
     * Returns an 8-bit row representation of the player pieces after applying the move.
     *
     * @param playerRow    8-bit bitboard corrosponding to player pieces
     * @param opponentRow  8-bit bitboard corrosponding to opponent pieces
     * @param movePosition square to move
     * @return             the new player's row index after making the move
     */
    private static int bitrowChangesForPlayer(final int playerRow, final int opponentRow, final int movePosition) {
        final int arrayIndex = playerRow | (opponentRow << MAGIC_NUMBER_8) | (movePosition << MAGIC_NUMBER_16);
        return (int) BITROW_CHANGES_FOR_PLAYER_ARRAY[arrayIndex] & BYTE_MASK_FOR_INT;
    }

    /**
     * Used to initialize the BITROW_CHANGES_FOR_PLAYER_ARRAY.
     *
     * @return a byte array having the row changes for the given index value
     */
    private static byte[] initializeBitrowChangesForPlayerArray() {

        final byte[] arrayResult = new byte[MAGIC_NUMBER_256 * MAGIC_NUMBER_256 * MAGIC_NUMBER_8];
        for (int playerRow = 0; playerRow < MAGIC_NUMBER_256; playerRow++) {
            for (int opponentRow = 0; opponentRow < MAGIC_NUMBER_256; opponentRow++) {
                final int filledInRow = playerRow | opponentRow;
                final int emptiesInRow = ~(filledInRow) & BYTE_MASK_FOR_INT;
                for (int movePosition = 0; movePosition < MAGIC_NUMBER_8; movePosition++) {
                    final int move = 1 << movePosition;
                    final int arrayResultIndex = playerRow
                        | (opponentRow << MAGIC_NUMBER_8)
                        | (movePosition << MAGIC_NUMBER_16);

                    int playerRowAfterMove;

                    /**
                     * It checks two conditions that cannot happen because are illegal.
                     * First player and opponent cannot have overlapping discs.
                     * Second the move cannot overlap existing discs.
                     * When either one of the two condition applys the result is set being equal
                     * to the player row index. Otherwise when black and white do not overlap,
                     * and the move is on an empy square it procede with the else block.
                     **/
                    if (((playerRow & opponentRow) != 0) || ((move & filledInRow) != 0)) {
                        playerRowAfterMove = playerRow;
                    } else {

                        /** The square of the move is added to the player configuration of the row after the move. */
                        playerRowAfterMove = playerRow | move;

                        /**
                         * The potential bracketing disc on the right is the first player disc found moving
                         * on the left starting from the square of the move.
                         */
                        final int potentialBracketingDiscOnTheLeft = BitWorks.highestBitSet(playerRow & (move - 1));

                        /**
                         * The left rank is the sequence of adiacent discs that start from the bracketing disc and end
                         * with the move disc. */
                        final int leftRank = BitWorks.fillInBetween(potentialBracketingDiscOnTheLeft | move);

                        /**
                         * If the rank contains empy squares, this is a fake flip, and it doesn't do anything.
                         * If the rank is full, it cannot be full of anything different than opponent discs, so
                         * it adds the discs to the after move player configuration.
                         */
                        if ((leftRank & emptiesInRow) == 0) {
                            playerRowAfterMove |= leftRank;
                        }

                        /** Here it does the same computed on the left also on the right. */
                        final int potentialBracketingDiscOnTheRight = BitWorks.lowestBitSet(playerRow & ~(move - 1));
                        final int rightRank = BitWorks.fillInBetween(potentialBracketingDiscOnTheRight | move);
                        if ((rightRank & emptiesInRow) == 0) {
                            playerRowAfterMove |= rightRank;
                        }

                        /**
                         * It checks that the after move configuration is different from
                         * the starting one for the player.
                         * This case can happen because it never checked that
                         * the bracketing piece was not adjacent to the move disc,
                         * on such a case, on both side, the move is illegal, and it is recorded setting
                         * the result configuation appropriately.
                         */
                        if (playerRowAfterMove == (playerRow | move)) {
                            playerRowAfterMove = playerRow;
                        }
                    }

                    /** Asignes the computed player row index to the proper array position. */
                    arrayResult[arrayResultIndex] = (byte) playerRowAfterMove;

                }
            }
        }

        return arrayResult;
    }

    /**
     * Returns all the cells surrounding the the squares given as parameter.
     * It gives back also the original squares.
     *
     * @param squares the set of given squares
     * @return        the union of given and surrounding squares
     */
    private static long neighbors(final long squares) {
        long neighbors = squares;
        neighbors |= (neighbors >>> MAGIC_NUMBER_8);
        neighbors |= (neighbors >>> 1) & ALL_SQUARES_EXCEPT_COLUMN_H;
        neighbors |= (neighbors <<  1) & ALL_SQUARES_EXCEPT_COLUMN_A;
        neighbors |= (neighbors <<  MAGIC_NUMBER_8);
        return neighbors;
    }

    /**
     * Returns an int having the bits from position 8 to position 31 set to zero,
     * and the bits from position 0 to position 7, corresponding to Row One in the board,
     * copied from the Column A of the {@code bitboard} parameter.
     * Bit value corresponding to square A1 is moved to A1, A2 to B1, ... , A8 to H1.
     *
     * @param bitboard the bitboard representation for one color
     * @return         colum a copied to row one, all other position are set to zero
     */
    private static int trasformColumnAInRow0(final long bitboard) {
        long tmp = bitboard;
        tmp &= COLUMN_A;
        tmp |= tmp >> MAGIC_NUMBER_28;
        tmp |= tmp >> MAGIC_NUMBER_14;
        tmp |= tmp >> MAGIC_NUMBER_7;
        return (int) tmp & BYTE_MASK_FOR_INT;
    }

    /**
     * Returns an int having the bits from position 8 to position 31 set to zero,
     * and the bits from position 0 to position 7, corresponding to Row One in the board,
     * copied from the Diagonal A1-H8 of the {@code bitboard} parameter.
     * Bit value corresponding to square A1 is moved to A1, B2 to B1, ... , H8 to H1.
     *
     * @param bitboard the bitboard representation for one color
     * @return         diagonal A1-H8 copied to row one, all other position are set to zero
     */
    private static int trasformDiagonalA1H8InRow0(final long bitboard) {
        long tmp = bitboard;
        tmp &= DIAGONAL_A1_H8;
        tmp |= tmp >> MAGIC_NUMBER_32;
        tmp |= tmp >> MAGIC_NUMBER_16;
        tmp |= tmp >> MAGIC_NUMBER_8;
        return (int) tmp & BYTE_MASK_FOR_INT;
    }

    /**
     * Returns an int having the bits from position 8 to position 31 set to zero,
     * and the bits from position 0 to position 7, corresponding to Row One in the board,
     * copied from the Diagonal H1-A8 of the {@code bitboard} parameter.
     * Bit value corresponding to square A8 is moved to A1, B7 to B1, ... , H1 to H1.
     *
     * @param bitboard the bitboard representation for one color
     * @return         diagonal H1-A8 copied to row one, all other position are set to zero
     */
    private static int trasformDiagonalH1A8InRow0(final long bitboard) {
        long tmp = bitboard;
        tmp &= DIAGONAL_H1_A8;
        tmp |= tmp >> MAGIC_NUMBER_32;
        tmp |= tmp >> MAGIC_NUMBER_16;
        tmp |= tmp >> MAGIC_NUMBER_8;
        return (int) tmp & BYTE_MASK_FOR_INT;
    }

    /**
     * Returns a long having the bits along the colomn A set to the corresponding ones
     * on the {@code bitrow} parameter.
     * Bits ranging from 8 to 31 in the {@code bitrow} parameter must be 0.
     * Bit value corresponding to square A1 is moved to A1, B1 to A2, ... , H1 to A8.
     *
     * @param bitrow bits 0 to 7 represents row one, bits forom position 8 to 31 must be 0
     * @return       a bitboard having column A set as the bitboard parameter,
     *               all other position are set to zero
     */
    private static long reTrasformRow0BackToColumnA(final int bitrow) {
        int tmp = bitrow;
        tmp |= tmp << MAGIC_NUMBER_7;
        tmp |= tmp << MAGIC_NUMBER_14;
        final long bitboard = (long) tmp | ((long) tmp << MAGIC_NUMBER_28);
        return bitboard & COLUMN_A;
    }

    /**
     * Returns a long having the bits along the diagonal A1-H8 set to the corresponding ones
     * on the {@code bitrow} parameter.
     * Bits ranging from 8 to 31 in the {@code bitrow} parameter must be 0.
     * Bit value corresponding to square A1 is moved to A1, B1 to B2, ... , H1 to H8.
     *
     * @param bitrow bits 0 to 7 represents row one, bits forom position 8 to 31 must be 0
     * @return       a bitboard having diagonal A1-H8 set as the bitboard parameter,
     *               all other position are set to zero
     */
    private static long reTrasformRow0BackToDiagonalA1H8(final int bitrow) {
        int tmp = bitrow;
        tmp |= tmp << MAGIC_NUMBER_8;
        long bitboard = (long) tmp | ((long) tmp << MAGIC_NUMBER_16);
        bitboard |= bitboard << MAGIC_NUMBER_32;
        return bitboard & DIAGONAL_A1_H8;
    }

    /**
     * Returns a long having the bits along the diagonal H1-A8 set to the corresponding ones
     * on the {@code bitrow} parameter.
     * Bits ranging from 8 to 31 in the {@code bitrow} parameter must be 0.
     * Bit value corresponding to square A1 is moved to A8, B1 to B7, ... , H1 to H1.
     *
     * @param bitrow bits 0 to 7 represents row one, bits forom position 8 to 31 must be 0
     * @return       a bitboard having diagonal H1-A8 set as the bitboard parameter,
     *               all other position are set to zero
     */
    private static long reTrasformRow0BackToDiagonalH1A8(final int bitrow) {
        int tmp = bitrow;
        tmp |= tmp << MAGIC_NUMBER_8;
        tmp |= (tmp & SQUARES_B1_F1_A2_E2) << MAGIC_NUMBER_16;
        final long bitboard = (long) tmp | ((long) tmp << MAGIC_NUMBER_32);
        return bitboard & DIAGONAL_H1_A8;
    }

    /**
     * Class constructor.
     * <p>
     * {@code bitboard} must be not null, and must have a size equal to
     * two. Overlapping bit set to one are not allowed.
     *
     * @param  bitboard the bitboard field
     */
    BitBoard1(final long[] bitboard) {
        super(bitboard);
        if (LOG) { callsToConstructor++; }
    }

    /**
     * Returns the boolean value telling if the move, done by the
     * specified player, is legal.
     * <p>
     * Parameter {@code move} must be not {@code null}.
     * Parameter {@code player} must be not {@code null}.
     *
     * A few more optimizations are possible:
     * Precompute the shiftDistance value, remake signedhift using a shift.
     * Eliminate the check for diagonals shorter than 3 pieces.
     * Verify that the square is empty. !!!! Eureka! -15%
     *
     *
     * @param move   the square where to put the new disk
     * @param player the player moving
     * @return       true if the move is legal, otherwise false
     * @throws NullPointerException if parameter {@code move} or {@code player} is null
     */
    public boolean isLegal(final Square move, final Player player) {

        if (LOG) { callsToIsLegal++; }

        isLegalInvariantsAreSatisfied(move, player);

        return isLegal(1L << move.ordinal(), player.ordinal());
    }

    /**
     * column and row MUST be computed from move.
     * See BitWorks MS1B without LOG2 lookup for row and consider a flipA1H8 new method.
     */
    boolean isLegal(final long move, final int player) {

        if ((move & (bitboard()[BLACK] | bitboard()[WHITE])) != 0) {
            return false;
        }

        final long playerBitboard = bitboard()[player];
        final long opponentBitboard = bitboard()[opponent(player)];

        /** MUST BE IMPROVED! */
        /*
        final int iMove = BitWorks.bitscanLS1B(move);
        final int row    = iMove / 8;
        final int column = iMove % 8;
        final int[] xy = BitWorks.xy(move);

        boolean error = false;
        if (xy[0] != column) { error = true; }
        if (xy[1] != row) { error = true; }
        if (error) {
            System.out.println("move=" + move + ", iMove=" + iMove);
            System.out.println("xy[0]=" + xy[0] + ", column=" + column);
            System.out.println("xy[1]=" + xy[1] + ", row=" + row);
        }
        */

        final int[] xy = BitWorks.xy(move);
        final int column = xy[0];
        final int row    = xy[1];

        int playerBitrow;
        int opponentBitrow;
        int shiftDistance;

        /** Check for flipping on row. */
        playerBitrow = (int) (playerBitboard >>> (MAGIC_NUMBER_8 * row)) & BYTE_MASK_FOR_INT;
        opponentBitrow = (int) (opponentBitboard >>> (MAGIC_NUMBER_8 * row)) & BYTE_MASK_FOR_INT;
        if (bitrowChangesForPlayer(playerBitrow, opponentBitrow, column) != playerBitrow) {
            return true;
        }

        /** Check for flipping on column. */
        playerBitrow = trasformColumnAInRow0(playerBitboard >>> column);
        opponentBitrow = trasformColumnAInRow0(opponentBitboard >>> column);
        if (bitrowChangesForPlayer(playerBitrow, opponentBitrow, row) != playerBitrow) {
            return true;
        }

        /** Check for flipping on diagonal having direction A1-H8. */
        shiftDistance = (column - row) << MAGIC_NUMBER_3;
        playerBitrow = trasformDiagonalA1H8InRow0(BitWorks.signedLeftShift(playerBitboard, shiftDistance));
        opponentBitrow = trasformDiagonalA1H8InRow0(BitWorks.signedLeftShift(opponentBitboard, shiftDistance));
        if (bitrowChangesForPlayer(playerBitrow, opponentBitrow, column) != playerBitrow) {
            return true;
        }

        /** Check for flipping on diagonal having direction H1-A8. */
        shiftDistance = (MAGIC_NUMBER_7 - column - row) << MAGIC_NUMBER_3;
        playerBitrow = trasformDiagonalH1A8InRow0(BitWorks.signedLeftShift(playerBitboard, shiftDistance));
        opponentBitrow = trasformDiagonalH1A8InRow0(BitWorks.signedLeftShift(opponentBitboard, shiftDistance));
        if (bitrowChangesForPlayer(playerBitrow, opponentBitrow, column) != playerBitrow) {
            return true;
        }

        /** If no capture on the four directions happens, return false. */
        return false;        
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<Square> legalMoves(final Player player) {

        if (LOG) { callsTolegalMoves++; }

        if (player == null) { throw new NullPointerException("Parameter player must be not null."); }

        return legalMoves(player.ordinal());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Board makeMove(final Square move, final Player player) {

        if (LOG) { callsToMakeMove++; }

        makeMoveInvariantsAreSatisfied(move, player);

        return valueOf(makeMoveImpl(move, player));
    }

    private List<Square> legalMoves(final int player) {
        long legalMoves = 0L;
        for (long likelyMoves = likelyMoves(player);
             likelyMoves != 0L;
             likelyMoves = BitWorks.unsetLowestBit(likelyMoves)) {
            final long move = BitWorks.lowestBitSet(likelyMoves);
            if (isLegal(move, player)) { legalMoves |= move; }
        }
        return new SquareList(legalMoves);
    }

    /**
     * Returns a new bitboard long array to reflect move by player.
     * <p>
     * A null value for player is not allowed, but not checked.
     * A null value for move is not allowed, but not checked.
     * The method does not check if the move is legal.
     *
     * @param  move   the board square where to put the disk
     * @param  player the disk color to put on the board
     * @return        a new bitboard array reflecting the move made
     */
    long[] makeMoveImpl(final Square move, final Player player) {

        final int intMove = move.ordinal();
        final int intPlayer = player.ordinal();
        final int column = move.column().ordinal();
        final int row = move.row().ordinal();

        final long playerBitboard = bitboard()[intPlayer];
        final long opponentBitboard = bitboard()[opponent(intPlayer)];

        long finalPBoard;
        long finalOBoard;

        int playerBitrow;
        int opponentBitrow;
        int shiftDistance;

        final long unmodifiedMask = ~BITBOARD_MASK_FOR_ALL_DIRECTIONS[intMove];
        finalPBoard = playerBitboard & unmodifiedMask;
        finalOBoard = opponentBitboard & unmodifiedMask;

        /** Compute row changes. */
        playerBitrow = (int) (playerBitboard >>> (MAGIC_NUMBER_8 * row)) & BYTE_MASK_FOR_INT;
        opponentBitrow = (int) (opponentBitboard >>> (MAGIC_NUMBER_8 * row)) & BYTE_MASK_FOR_INT;
        playerBitrow = bitrowChangesForPlayer(playerBitrow, opponentBitrow, column);
        opponentBitrow &= ~playerBitrow;
        finalPBoard |= ((long) playerBitrow << (MAGIC_NUMBER_8 * row));
        finalOBoard |= ((long) opponentBitrow << (MAGIC_NUMBER_8 * row));

        /** Compute column changes. */
        playerBitrow = trasformColumnAInRow0(playerBitboard >>> column);
        opponentBitrow = trasformColumnAInRow0(opponentBitboard >>> column);
        playerBitrow = bitrowChangesForPlayer(playerBitrow, opponentBitrow, row);
        opponentBitrow &= ~playerBitrow;
        finalPBoard |= reTrasformRow0BackToColumnA(playerBitrow) << column;
        finalOBoard |= reTrasformRow0BackToColumnA(opponentBitrow) << column;

        /** Compute changes on diagonal having direction A1-H8. */
        shiftDistance = (column - row) << MAGIC_NUMBER_3;
        playerBitrow = trasformDiagonalA1H8InRow0(BitWorks.signedLeftShift(playerBitboard, shiftDistance));
        opponentBitrow = trasformDiagonalA1H8InRow0(BitWorks.signedLeftShift(opponentBitboard, shiftDistance));
        playerBitrow = bitrowChangesForPlayer(playerBitrow, opponentBitrow, column);
        opponentBitrow &= ~playerBitrow;
        finalPBoard |= BitWorks.signedLeftShift(reTrasformRow0BackToDiagonalA1H8(playerBitrow), -shiftDistance);
        finalOBoard |= BitWorks.signedLeftShift(reTrasformRow0BackToDiagonalA1H8(opponentBitrow), -shiftDistance);

        /** Compute changes on diagonal having direction H1-A8. */
        shiftDistance = (MAGIC_NUMBER_7 - column - row) << MAGIC_NUMBER_3;
        playerBitrow = trasformDiagonalH1A8InRow0(BitWorks.signedLeftShift(playerBitboard, shiftDistance));
        opponentBitrow = trasformDiagonalH1A8InRow0(BitWorks.signedLeftShift(opponentBitboard, shiftDistance));
        playerBitrow = bitrowChangesForPlayer(playerBitrow, opponentBitrow, column);
        opponentBitrow &= ~playerBitrow;
        finalPBoard |= BitWorks.signedLeftShift(reTrasformRow0BackToDiagonalH1A8(playerBitrow), -shiftDistance);
        finalOBoard |= BitWorks.signedLeftShift(reTrasformRow0BackToDiagonalH1A8(opponentBitrow), -shiftDistance);

        final long[] newbitboard = new long[2];
        newbitboard[intPlayer] = finalPBoard;
        newbitboard[opponent(intPlayer)] = finalOBoard;

        return newbitboard;
    }

    /**
     * See the overloaded method having the player parameter as an int.
     *
     * @param player the player that has to move
     * @return       the set of likely moves
     */
    private long likelyMoves(final Player player) {
        return likelyMoves(player.ordinal());
    }

    /**
     * Returns a set of likely moves. It is guaranted that the legal moves set
     * is fully contained by the likely move set.
     * On avarage likely moves are 1.7 times the number of legal moves.
     *
     * @param player the player that has to move
     * @return       the set of likely moves
     */
    private long likelyMoves(final int player) {
        return neighbors(bitboard()[opponent(player)]) & empties();
    }

}
