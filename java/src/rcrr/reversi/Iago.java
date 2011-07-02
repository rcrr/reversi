/*
 *  Iago.java
 *
 *  Copyright (c) 2011 Roberto Corradini. All rights reserved.
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

/**
 * Iago is an advanced strategy, that implements the features described in the PAIP book 18.12.
 * See for reference the paper:
 * <i>"Paul S. Rosenbloom. A World-Championship-Level Othello Program. Artif. Intell., 1982: 279~320"</i>
 * <p>
 * <i>"Kay-Fu Lee, S. Mahajan. The development of a world class Othello program. Artif. Intell., 1990: 21~36"</i>
 * <p>
 * The strategy mixes the concepts of mobility and edge stability ...
 */
public class Iago implements EvalFunction {

    public static final class Mobility {
        private final int current;
        private final int potential;
        public Mobility(final int current, final int potential) {
            this.current = current;
            this.potential = potential;
        }
        public int current() { return this.current; }
        public int potential() { return this.potential; }
    }

    /**
     * Class constructor.
     */
    public Iago() { }

    /**
     *
     * @param position the game position to evaluate
     * @return the position value
     * @throws NullPointerException if parameter {@code position} is null
     */
    public final int eval(final GamePosition position) {
        int result = 0;
        return result;
    }

    /**
     * Current mobility is the number of legal moves.
     * Potential mobility is the number of blank squares adjacent to an opponent that are not legal moves.
     * Returns current and potential mobility for the player.
     * <p>
     * See PAIP 18.12 pages 637-638.
     *
     * @param position the game position to evaluate
     * @return the position mobility evaluation
     * @throws NullPointerException if parameter {@code position} is null
     */
    public final Mobility mobility(final GamePosition position) {
        if (position == null) { throw new NullPointerException("Parameter position cannot be null."); }

        int current = 0;
        int potential = 0;

        Board board = position.board();
        Player player = position.player();
        Player opp = player.opponent();

        for (Square square : Square.values()) {
            if (board.get(square) == SquareState.EMPTY) {
                if (board.isLegal(square, player)) {
                    current++;
                } else if (isPotentialMove(board, square, opp)) {
                    potential ++;
                }
            }
        }
        return new Mobility(current, potential);
    }

    private boolean isPotentialMove(final Board board,
                                    final Square square,
                                    final Player opp) {
        for (Square neighbor : square.neighbors().values()) {
            if (board.get(neighbor) == opp.color()) {
                return true;
            }
        }
        return false;
    }

}
