/*
 *  PerfCheck.java
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

import org.junit.Test;
import static org.junit.Assert.assertTrue;

public class PerfCheck {

    private static final int NUMBER_OF_GAMES = 1000;

    @Test
    public final void checkPerfPlay() {

        final Game.Builder builder = new Game.Builder()
            .withSequence(GameSequenceFixtures.INITIAL)
            .withActors(new ActorsPair.Builder()
                        .withActor(Player.BLACK,
                                   new Actor.Builder()
                                   .withStrategy(Minimax.getInstance().searcher(6, new CountDifference()))
                                   .build())
                        .withActor(Player.WHITE,
                                   new Actor.Builder()
                                   .withStrategy(Minimax.maximizer(new CountDifference()))
                                   .build())
                        .build());

        for (int i = 0; i < NUMBER_OF_GAMES; i++) {
            builder.build().play();
        }

        assertTrue("No exception must be risen.", true);

    }

}