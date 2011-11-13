/*
 *  IagoStrategy.java
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
 * IagoStrategy is an advanced strategy, that implements the features described in the PAIP book 18.12.
 * <p>
 * See also for reference the two papers:
 * <ul>
 *   <li><i>"Paul S. Rosenbloom. A World-Championship-Level Othello Program. Artif. Intell., 1982: 279~320"</i></li>
 *   <li><i>"Kay-Fu Lee, S. Mahajan. The development of a world class Othello program.
 *           Artif. Intell., 1990: 21~36"</i></li>
 * </ul>
 * <p>
 * The strategy adopts the {@code Iago} evaluation function that mixes the concepts of mobility and edge stability, and
 * searches using the the {@code AlphaBeta3} variant of the alpha-beta algorithm.
 *
 * @see AlphaBeta3
 * @see Iago
 */
public final class IagoStrategy implements Strategy {

    /** Default value used for the search depth. */
    public static final int DEFAULT_DEPTH = 6;

    /** An instance of the Iago evaluation function. */
    public static final EvalFunction IAGO_EVAL_FUNCTION = new Iago();

    /** The iago field. **/
    private final Strategy iago;

    /**
     * Class constructor.
     */
    public IagoStrategy() {
        this(DEFAULT_DEPTH);
    }

    /**
     * Class constructor.
     * <p>
     * {@code depth} must be greater than 0.
     *
     * @param depth the depth of search
     * @throws IllegalArgumentException when the depth parameter is less than 1.
     */
    public IagoStrategy(final int depth) {
        if (depth < 1) {
            throw new IllegalArgumentException("Parameter depth must be greather than 0.");
        }
        this.iago = AlphaBeta3.getInstance().searcher(depth, IAGO_EVAL_FUNCTION);
    }

    /**
     * The strategy's move method.
     * <p>
     * Parameter {@code gameSnapshot} must be not {@code null}.
     *
     * @param gameSnapshot the game snapshot
     * @return             the strategy's move
     * @throws NullPointerException when the gameSnapshot parameter is null
     */
    public Move move(final GameSnapshot gameSnapshot) {
        if (gameSnapshot == null) {
            throw new NullPointerException("Parameter gameSnapshot cannot be null.");
        }
        return iago.move(gameSnapshot);
    }

}
