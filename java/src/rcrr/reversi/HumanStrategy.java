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

import java.io.Reader;
import java.io.BufferedReader;
import java.io.InputStreamReader;

import java.io.Writer;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;

import java.io.IOException;

/**
 * An interacting strategy.
 */
public class HumanStrategy implements Strategy {

    /** The buffered reader field. */
    private final BufferedReader bufferedReader;

    /** The buffered writer field. */
    private final BufferedWriter bufferedWriter;

    /**
     * No parameter class constructor.
     * <p>
     * The input is defaulted to {@code System.in},
     * the output is defaulted to {@code System.out}.
     */
    public HumanStrategy() {
        this(new InputStreamReader(System.in),
             new OutputStreamWriter(System.out));
    }

    /**
     * Class constructor.
     *
     * @param reader the human input stream reader
     * @param writer the human output stream writer
     */
    public HumanStrategy(final Reader reader, final Writer writer) {
        this.bufferedReader = new BufferedReader(reader);
        this.bufferedWriter = new BufferedWriter(writer);
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
        Square move = null;
        while (move == null) {
            List<String> moves = new ArrayList<String>();
            for (Square mv : gameSnapshot.board().legalMoves(gameSnapshot.player())) {
                moves.add(mv.label());
            }
            try {
                bufferedWriter.write(gameSnapshot.player().toString() + " to move " + moves + ": ");
                bufferedWriter.flush();
            } catch (IOException ioe) {
                throw new RuntimeException("Error in writing to the output writer.");
            }
            String inputLine = null;
            try {
                inputLine = bufferedReader.readLine();
            } catch (IOException ioe) {
                throw new RuntimeException("Error in writing to the output writer.");
            }
            if (inputLine != null) {
                try {
                    move = Square.getInstance(inputLine);
                } catch (IllegalArgumentException iae) {
                    try {
                        bufferedWriter.write(inputLine + " is not a move. Retry:");
                        bufferedWriter.flush();
                    } catch (IOException ioe) {
                        throw new RuntimeException("Error in writing to the output writer.");
                    }
                }
            }
        }
        return Move.valueOf(move);
    }

}
