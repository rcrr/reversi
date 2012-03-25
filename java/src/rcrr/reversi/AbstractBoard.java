/*
 *  AbstractBoard.java
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

import java.util.List;

public abstract class AbstractBoard implements Board {
    public abstract int countDifference(Player player);
    public abstract SquareState get(Square square);
    public abstract List<Square> legalMoves(Player player);
    public abstract Board makeMove(Square move, Player player);
    public abstract boolean hasAnyLegalMove(Player player);
    public abstract boolean isLegal(Square move, Player player);
    public abstract boolean hasAnyPlayerAnyLegalMove();
    public abstract int countPieces(SquareState color);

    /**
     * Returns a formatted string showing a 2d graphical represention of the board.
     *
     * @return a string being a 2d representation of the board
     */
    public String printBoard() {
        final StringBuilder sb = new StringBuilder();
        sb.append("    a b c d e f g h ");
        for (final Row r : Row.values()) {
            sb.append("\n ").append(r.label()).append("  ");
            for (final Column c : Column.values()) {
                String p = get(Square.getInstance(r, c)).symbol();
                sb.append(p).append(" ");
            }
        }
        sb.append("\n");
        return sb.toString();
    }

    /**
     * Returns a formatted string showing a 2d graphical composed view
     * of the board and the disk count.
     * <p>
     * The method joins the printBoard() and the printCount() output,
     * setting the second on the right of the first board's row.
     *
     * @return a string being a 2d representation of the board with the disk count
     */
    public String printBoardWithCount() {
        final StringBuilder sbBoardWithCount = new StringBuilder();
        final String sBoard = printBoard();
        final String sCount = printCount();
        final String[] lines = sBoard.split("\n");
        for (int i = 0; i < lines.length; i++) {
            String line = lines[i];
            sbBoardWithCount.append(line);
            if (i == 0) { sbBoardWithCount.append(sCount); }
            sbBoardWithCount.append("\n");
        }
        return (sbBoardWithCount.toString());
    }

    /**
     * Returns a formatted string, giving the two player disk count and their difference.
     *
     * @return a string showing the two player's count
     */
    public String printCount() {
        final int cb = countPieces(SquareState.BLACK);
        final int cw = countPieces(SquareState.WHITE);
        final int cd = cb - cw;
        return "[@=" + cb + " 0=" + cw + " (" + cd + ")]";
    }

}
