/*
 *  RandomStrategyTest.java
 *
 *  Copyright (c) 2010 Roberto Corradini. All rights reserved.
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

import org.joda.time.Duration;
import org.joda.time.Period;

import java.util.Map;
import java.util.HashMap;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

import org.junit.*;
import static org.junit.Assert.*;

import static rcrr.reversi.Square.*;

public class RandomStrategyTest {

    private static final int TIMES = 2000;
    private static final double DEVIATION = 0.001;

    private static final List<Square> INITIAL_LEGAL_MOVES = Arrays.asList(E6, C4, F5, D3);
    private static final int N = INITIAL_LEGAL_MOVES.size();
    private static final double PROBABILITY = 1./N;

    /** Strategy fixtures. */
    private Strategy randomStrategy = new RandomStrategy();
 
    private List<Square> randomSquareSample(int length) {
	List<Square> result = new ArrayList<Square>(length);
	for (int i=0; i<length; i++) {
	    Square sq = randomStrategy.move(GameSnapshotFixtures.INITIAL).square();
	    assertTrue(INITIAL_LEGAL_MOVES.contains(sq));
	    result.add(sq);
	}
	return result;
    }

    private int count(Square sample, List<Square> distribution) {
	int count = 0;
	for (Square sq : distribution) {
	    if (sample == sq) count++;
	}
	return count;
    }

    /**
     * Binomial Coefficient: see http://en.wikipedia.org/wiki/Binomial_distribution
     */
    @Test
    public void testMove() {
	List<Square> distribution = randomSquareSample(TIMES);
	Map<Square, Double> result = new HashMap<Square, Double>(N);
	for (Square sq : INITIAL_LEGAL_MOVES) {
	    result.put(sq, Math.pow(count(sq, distribution)/Float.valueOf(TIMES)-PROBABILITY, 2));
	}
	for (Square sq : INITIAL_LEGAL_MOVES) {
	    assertTrue(result.get(sq) < DEVIATION);
	}
    }
    

}