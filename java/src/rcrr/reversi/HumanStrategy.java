/*
 *  HumanStrategy.java
 *
 *  Copyright (c) 2010, 2011 Roberto Corradini. All rights reserved.
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
import java.util.ArrayList;

import java.io.OutputStream;
import java.io.InputStream;
import java.io.InputStreamReader;

import java.io.BufferedReader;

import java.io.IOException;

/**
 * An interacting strategy.
 */
public class HumanStrategy implements Strategy {

    /** The buffered writer field. */
    private final OutputStream out;

    /** The reader field. */
    private final BufferedReader reader;

    /**
     * No parameter class constructor.
     * <p>
     * The input is defaulted to {@code System.in},
     * the output is defaulted to {@code System.out}.
     */
    public HumanStrategy() {
        this(System.in, System.out);
    }

    /**
     * Class constructor.
     *
     * @param in  the human input stream reader
     * @param out the human output stream writer
     */
    public HumanStrategy(final InputStream in, final OutputStream out) {
        this.out = out;
        this.reader = new BufferedReader(new InputStreamReader(in));
    }

    /**
     * The move method's implementation.
     * <p>
     * Parameter {@code gameSnapshot} cannot be {@code null}.
     *
     * @param gameSnapshot the game snapshot
     * @return             the selected move
     * @throws NullPointerException if parameter {@code gameSnapshot} is null
     * @throws RuntimeException     if an {@code IOException} is catched during
     *                              input/output operations from and to the
     *                              reader and writer human strategy fields.
     */
    public final Move move(final GameSnapshot gameSnapshot) {
        if (gameSnapshot == null) { throw new NullPointerException("Parameter gameSnapshot cannot be null."); }
        Move move = null;
        while (move == null) {
            List<String> moves = new ArrayList<String>();
            for (Square mv : gameSnapshot.board().legalMoves(gameSnapshot.player())) {
                moves.add(mv.label());
            }
            try {
                out.write((gameSnapshot.player() + " has to move. Available moves are: "
                           + moves + ". Enter a move:\n").getBytes());
            } catch (IOException ioe) {
                throw new RuntimeException(ioe);
            }
            String inputLine = null;
            try {
                inputLine = reader.readLine();
            } catch (IOException ioe) {
                throw new RuntimeException(ioe);
            }
            if (inputLine != null) {
                if (inputLine.equalsIgnoreCase("resign")) {
                    move = Move.valueOf(Move.Action.RESIGN);
                } else if (inputLine.equalsIgnoreCase("pass")) {
                    move = Move.valueOf(Move.Action.PASS);
                } else {
                    try {
                        move = Move.valueOf(Square.getInstance(inputLine));
                    } catch (IllegalArgumentException iae) {
                        try {
                            out.write((inputLine + " is not a move. Retry: ").getBytes());
                        } catch (IOException ioe) {
                            throw new RuntimeException(ioe);
                        }
                    }
                }
            }
        }
        return move;
    }

}
