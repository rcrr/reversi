/*
 *  ReversiTest.java
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
import java.util.Map;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Set;
import java.util.HashSet;

import org.junit.Test;

import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import static org.hamcrest.CoreMatchers.is;

import java.io.File;
import java.io.PrintStream;
import java.io.OutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.FileNotFoundException;

import org.joda.time.Duration;
import org.joda.time.Period;

/**
 * Test Suite for {@code Reversi} class.
 */
public class ReversiTest {

    /** The standard game duration is thirty minutes. */
    private static final Duration STANDARD_GAME_DURATION = Period.minutes(30).toStandardDuration();

    /** Print to the console. */
    private static final PrintStream PRINT = System.out;

    /** A null print stram. */
    private static final PrintStream NO_PRINT = null;

    /** Ten times. */
    private static final int TEN_TIMES = 10;

    /** A thousand times. */
    private static final int A_THOUSAND_TIMES = 1000;

    /** The number of nanosecond contained by a millisecond. */
    private static final long NANOSECONDS_PER_MILLISECOND = 1000000;

    /** Class constructor. */
    public ReversiTest() { }

    /**
     * Tests that the game played as described by <i>PAIP 18.4 (pg 614)</i> is reproduced
     * accurately running the {@code reversi(Strategy, Strategy, PrintStream, Duration)} method.
     * <p>
     * The method is run assigning the following values to invocation parameters:
     * <ul>
     *   <li>{@code blStrategy} is {@code Minimax.getInstance().searcher(3, new CountDifference())}</li>
     *   <li>{@code whStrategy} is {@code Minimax.maximizer(new CountDifference())}</li>
     *   <li>{@code ps} is {@code null}</li>
     *   <li>{@code gameDuration} is 30 minutes</li>
     * </ul>
     * <p>
     * The method must return a value of +53. The game ends [@=53 O=0 (+53)].
     *
     * @see Reversi#reversi(Strategy, Strategy, PrintStream, Duration)
     */
    @Test
    public final void testPAIP_18_4_0() {
        assertEquals(+53, Reversi.reversi(Minimax.getInstance().searcher(3, new CountDifference()),
                                          Minimax.maximizer(new CountDifference()),
                                          NO_PRINT,
                                          STANDARD_GAME_DURATION));
    }

    /**
     * Tests that the game played as described by <i>PAIP 18.6 (pg 617)</i> is reproduced
     * accurately running the {@code reversi(Strategy, Strategy, PrintStream, Duration)} method.
     * <p>
     * The method is run assigning the following values to invocation parameters:
     * <ul>
     *   <li>{@code blStrategy} is {@code AlphaBeta.getInstance().searcher(4, new CountDifference())}</li>
     *   <li>{@code whStrategy} is {@code AlphaBeta.getInstance().searcher(4, new WeightedSquares())}</li>
     *   <li>{@code ps} is {@code null}</li>
     *   <li>{@code gameDuration} is 30 minutes</li>
     * </ul>
     * <p>
     * The method must return a value of -16. The game ends [@=24 O=40 (-16)].
     * <p>
     * The test than verifies that running the two strategies using the minimax algorithm instead
     * of the alpha-beta search generates the same result.
     *
     * @see Reversi#reversi(Strategy, Strategy, PrintStream, Duration)
     */
    @Test
    public final void testPAIP_18_6_0() {
        assertEquals(-16, Reversi.reversi(AlphaBeta.getInstance().searcher(4, new CountDifference()),
                                          AlphaBeta.getInstance().searcher(4, new WeightedSquares()),
                                          NO_PRINT,
                                          STANDARD_GAME_DURATION));

        /** Verifies that the minimax algorithm plays the same game. */
        assertEquals(-16, Reversi.reversi(Minimax.getInstance().searcher(4, new CountDifference()),
                                          Minimax.getInstance().searcher(4, new WeightedSquares()),
                                          NO_PRINT,
                                          STANDARD_GAME_DURATION));
    }

    /**
     * Tests that the game played as described by <i>PAIP 18.6 (pg 619)</i> is reproduced
     * accurately running the {@code reversi(Strategy, Strategy, PrintStream, Duration)} method.
     * <p>
     * The method is run assigning the following values to invocation parameters:
     * <ul>
     *   <li>{@code blStrategy} is {@code AlphaBeta.getInstance().searcher(6, new CountDifference())}</li>
     *   <li>{@code whStrategy} is {@code AlphaBeta.getInstance().searcher(4, new WeightedSquares())}</li>
     *   <li>{@code ps} is {@code null}</li>
     *   <li>{@code gameDuration} is 30 minutes</li>
     * </ul>
     * <p>
     * PAIP pg. 620 ends the game [@=24 O=40 (-16)].
     * The test is consistent with the PAIP game till the last position before the end.
     * The difference has not fully traced, given the time needed to manually check
     * a 6 ply search. Anyhow the CL version obtained porting on SBCL the original PAIP
     * source code play the exact same game, ending with a -30 score.
     * <p>
     * The method must return a value of -30.
     * <p>
     * The test than should verify that running the two strategies using the minimax algorithm instead
     * of the alpha-beta search generates the same result. It takes around one munute, and it is skipped.
     *
     * @see Reversi#reversi(Strategy, Strategy, PrintStream, Duration)
     */
    @Test
    public final void testPAIP_18_6_1() {
        /**
         * PAIP pg. 620 ends the game [@=24 O=40 (-16)].
         * The test is consistent with the PAIP game till the last position before the end.
         * The difference has not fully traced, given the time needed to manually check
         * a 6 ply search. Anyhow the CL version obtained porting on SBCL the original PAIP
         * source code play the exact same game, ending with a -30 score.
         */
        assertEquals(-30, Reversi.reversi(AlphaBeta.getInstance().searcher(6, new CountDifference()),
                                          AlphaBeta.getInstance().searcher(4, new WeightedSquares()),
                                          NO_PRINT,
                                          STANDARD_GAME_DURATION));

        /** Verifing that the minimax algorithm plays the same game takes far too long. */
        /*
        assertEquals(-30, Reversi.reversi(Minimax.getInstance().searcher(6, new CountDifference()),
                                          Minimax.getInstance().searcher(4, new WeightedSquares()),
                                          NO_PRINT,
                                          STANDARD_GAME_DURATION));
        */
    }

    /**
     * Tests that the game played as described by <i>PAIP 18.6 (pg 621)</i> is reproduced
     * accurately by substituiting the WeightedSquares strategy with the modified one.
     * <p>
     * The {@code WeightedSquares} strategy running a four ply search selects F6,
     * the {@code ModifiedWeightedSquares} instead selects C1.
     * <pre>
     * {@code
     * BLACK moves to b1
     *     a b c d e f g h [@=20 0=1 (19)]
     *  1  O @ . . . . . .
     *  2  . @ . . . @ @ .
     *  3  @ @ @ @ @ @ . .
     *  4  . @ . @ @ . . .
     *  5  @ @ @ @ @ @ . .
     *  6  . @ . . . . . .
     *  7  . . . . . . . .
     *  8  . . . . . . . . [@=29:59, O=29:59]
     *  Next to play: WHITE, legal moves: [c1, f6]
     * }
     * </pre>
     *
     * @see WeightedSquares
     * @see ModifiedWeightedSquares
     */
    @Test
    public final void testPAIP_18_6_2() {

        final GameSnapshot paip1862 = new GameSnapshot.Builder()
            .withPosition(new GamePosition.Builder()
                          .withBoard(new Board.Builder()
                                     .withSquaresLiteral(2, 1, 0, 0, 0, 0, 0, 0,
                                                         0, 1, 0, 0, 0, 1, 1, 0,
                                                         1, 1, 1, 1, 1, 1, 0, 0,
                                                         0, 1, 0, 1, 1, 0, 0, 0,
                                                         1, 1, 1, 1, 1, 1, 0, 0,
                                                         0, 1, 0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0, 0, 0,
                                                         0, 0, 0, 0, 0, 0, 0, 0)
                                     .build())
                          .withPlayer(Player.WHITE)
                          .build())
            .withClock(ClockFixtures.ONE_MINUTE_LEFT_TO_BOTH_PLAYERS)
            .withRegister(MoveRegisterFixtures.EMPTY)
            .build();

        assertThat("AlphaBeta.getInstance().searcher(4, new WeightedSquares())"
                   + ".move(paip1862) must return F6.",
                   AlphaBeta.getInstance().searcher(4, new WeightedSquares())
                   .move(paip1862),
                   is(Move.valueOf(Square.F6)));

        assertThat("AlphaBeta.getInstance().searcher(4, new ModifiedWeightedSquares())"
                   + ".move(paip1862) must return C1.",
                   AlphaBeta.getInstance().searcher(4, new ModifiedWeightedSquares())
                   .move(paip1862),
                   is(Move.valueOf(Square.C1)));

    }

    /**
     * Tests the {@code reversi(Strategy, Strategy, PrintStream, Duration)} method.
     * <p>
     * The test runs a number of games (actualy one thousand) opposing two random strategies.
     * <p>
     * Results are then written to a file named <i>ReversiTest.txt</i> that
     * is saved under a properly configured directory as prepared by the build process, or
     * in the system tmp dir when the first is not found.
     * <p>
     * Results should be simalar to:
     * <ul>
     *   <li>{@code mean = -0.9}</li>
     *   <li>{@code variance = 330}</li>
     *   <li>{@code black wins = 45%}</li>
     *   <li>{@code white wins = 50%}</li>
     *   <li>{@code draws = 5%}</li>
     * </ul>
     * <p>
     *
     * @see Reversi#reversi(Strategy, Strategy, PrintStream, Duration)
     */
    @Test
    public final void testReversi_runsASeriesOfRandomVsRandom() {

        /** Runs the game series. */
        Map<String, Object> results
            = runSeriesOfGames(new RandomStrategy(),
                               new RandomStrategy(),
                               NO_PRINT,
                               STANDARD_GAME_DURATION,
                               A_THOUSAND_TIMES);

        /** Output of the test. */
        write((String) results.get("output"),
              "ReversiTest",
              "Method=testReversi_runsASeriesOfRandomVsRandom");

        assertTrue("The test must run without exceptions.", true);

    }

    /**
     * Tests the {@code reversi(Strategy, Strategy, PrintStream, Duration)} method.
     * <p>
     * Runs ten games between the Random and the 2PlyCountDifference strategies.
     * <p>
     * Results are then written to a file named <i>ReversiTest.txt</i> that
     * is saved under a properly configured directory as prepared by the build process, or
     * in the system tmp dir when the first is not found.
     *
     * @see Reversi#reversi(Strategy, Strategy, PrintStream, Duration)
     */
    @Test
    public final void testReversi_runsASeriesOfRandomVs2PlyCountDifference() {

        /** Runs the game series. */
        Map<String, Object> results
            = runSeriesOfGames(new RandomStrategy(),
                               AlphaBeta.getInstance().searcher(2, new CountDifference()),
                               NO_PRINT,
                               STANDARD_GAME_DURATION,
                               TEN_TIMES);

        /** Output of the test. */
        write((String) results.get("output"),
              "ReversiTest",
              "Method=testReversi_runsASeriesOfRandomVs2PlyCountDifference");

        assertTrue("The test must run without exceptions.", true);

    }

    /**
     * Tests the {@code reversi(Strategy, Strategy, PrintStream, Duration)} method.
     * <p>
     * Runs ten games between the 2PlyCountDifference and the Random strategies.
     * <p>
     * Results are then written to a file named <i>ReversiTest.txt</i> that
     * is saved under a properly configured directory as prepared by the build process, or
     * in the system tmp dir when the first is not found.
     *
     * @see Reversi#reversi(Strategy, Strategy, PrintStream, Duration)
     */
    @Test
    public final void testReversi_runsASeriesOf2PlyCountDifferenceVsRandom() {

        /** Runs the game series. */
        Map<String, Object> results
            = runSeriesOfGames(AlphaBeta.getInstance().searcher(2, new CountDifference()),
                               new RandomStrategy(),
                               NO_PRINT,
                               STANDARD_GAME_DURATION,
                               TEN_TIMES);

        /** Output of the test. */
        write((String) results.get("output"),
              "ReversiTest",
              "Method=testReversi_runsASeriesOf2PlyCountDifferenceVsRandom");

        assertTrue("The test must run without exceptions.", true);

    }

    /**
     * Tests the {@code reversi(Strategy, Strategy, PrintStream, Duration)} method.
     * <p>
     * Runs ten games between the Random and the 2PlyWeightedSquares strategies.
     * <p>
     * Results are then written to a file named <i>ReversiTest.txt</i> that
     * is saved under a properly configured directory as prepared by the build process, or
     * in the system tmp dir when the first is not found.
     *
     * @see Reversi#reversi(Strategy, Strategy, PrintStream, Duration)
     */
    @Test
    public final void testReversi_runsASeriesOfRandomVs2PlyWeightedSquares() {

        /** Runs the game series. */
        Map<String, Object> results
            = runSeriesOfGames(new RandomStrategy(),
                               AlphaBeta.getInstance().searcher(2, new WeightedSquares()),
                               NO_PRINT,
                               STANDARD_GAME_DURATION,
                               TEN_TIMES);

        /** Output of the test. */
        write((String) results.get("output"),
              "ReversiTest",
              "Method=testReversi_runsASeriesOfRandomVs2PlyWeightedSquares");

        assertTrue("The test must run without exceptions.", true);

    }

    /**
     * Tests the {@code reversi(Strategy, Strategy, PrintStream, Duration)} method.
     * <p>
     * Runs ten games between the 2PlyWeightedSquares and the Random strategies.
     * <p>
     * Results are then written to a file named <i>ReversiTest.txt</i> that
     * is saved under a properly configured directory as prepared by the build process, or
     * in the system tmp dir when the first is not found.
     *
     * @see Reversi#reversi(Strategy, Strategy, PrintStream, Duration)
     */
    @Test
    public final void testReversi_runsASeriesOf2PlyWeightedSquaresVsRandom() {

        /** Runs the game series. */
        Map<String, Object> results
            = runSeriesOfGames(AlphaBeta.getInstance().searcher(2, new WeightedSquares()),
                               new RandomStrategy(),
                               NO_PRINT,
                               STANDARD_GAME_DURATION,
                               TEN_TIMES);

        /** Output of the test. */
        write((String) results.get("output"),
              "ReversiTest",
              "Method=testReversi_runsASeriesOf2PlyWeightedSquaresVsRandom");

        assertTrue("The test must run without exceptions.", true);

    }

    /**
     * Tests the {@code reversiSeries(Actor, Actor, int, Duration)} method.
     *
     * @see Reversi#reversiSeries(Actor, Actor, int, Duration)
     */
    @Test
    public final void testReversiSeries() {

        Map<String, Object> results = Reversi.reversiSeries(new Actor.Builder()
                                                            .withName("Modified weighted squares, a-b two ply search.")
                                                            .withStrategy(AlphaBeta.getInstance()
                                                                          .searcher(2, new ModifiedWeightedSquares()))
                                                            .build(),
                                                            new Actor.Builder()
                                                            .withName("Weighted squares, a-b two ply search.")
                                                            .withStrategy(AlphaBeta.getInstance()
                                                                          .searcher(2, new WeightedSquares()))
                                                            .build(),
                                                            5,
                                                            STANDARD_GAME_DURATION);

        /** Output of the test. */
        write((String) results.get("scores").toString() + "\n",
              "ReversiTest",
              "Method=testReversiSeries");

        assertTrue("The test must run without exceptions.", true);

    }

    /**
     * Tests the {@code reversiSeries(Actor, Actor, int, Duration)} method.
     * Uses the Iago evaluation function.
     *
     * @see Reversi#reversiSeries(Actor, Actor, int, Duration)
     */
    @Test
    public final void testReversiSeries_withIagoEvaluationFunction() {

        Map<String, Object> results = Reversi.reversiSeries(new Actor.Builder()
                                                            .withName("Modified weighted squares, a-b two ply search.")
                                                            .withStrategy(AlphaBeta.getInstance()
                                                                          .searcher(2, new ModifiedWeightedSquares()))
                                                            .build(),
                                                            new Actor.Builder()
                                                            .withName("Iago, a-b two ply search.")
                                                            .withStrategy(AlphaBeta.getInstance()
                                                                          .searcher(2, new Iago()))
                                                            .build(),
                                                            5,
                                                            STANDARD_GAME_DURATION);

        /** Output of the test. */
        write((String) results.get("scores").toString() + "\n",
              "ReversiTest",
              "Method=testReversiSeries_withIagoEvaluationFunction");

        assertTrue("The test must run without exceptions.", true);

    }

    /**
     * Tests the {@code roundRobin(Set, int, Duration)} method.
     *
     * @see Reversi#roundRobin(Set, int, Duration)
     */
    @Test
    public final void testRoundRobin_usesMaximizer() {

        Set<Actor> actors = new HashSet<Actor>();
        actors.add(Actor.valueOf("Maximize Count Difference", AbstractDecisionRule.maximizer(new CountDifference())));
        actors.add(Actor.valueOf("Maximize Mobility", AbstractDecisionRule.maximizer(new Mobility())));
        actors.add(Actor.valueOf("Maximize Weighted Squares", AbstractDecisionRule.maximizer(new WeightedSquares())));
        actors.add(Actor.valueOf("Maximize Mod-Weighted Squares", AbstractDecisionRule.maximizer(new ModifiedWeightedSquares())));
        actors.add(Actor.valueOf("Random Strategy", new RandomStrategy()));

        Map<String, Object> results = Reversi.roundRobin(actors, 5, STANDARD_GAME_DURATION);

        String report = Reversi.postProcessRoundRobinResults(results);

        /** Output of the test. */
        write(report,
              "ReversiTest",
              "Method=testRoundRobin_usesMaximizer");

        assertTrue("The test must run without exceptions.", true);

    }

    /**
     * Tests the {@code roundRobin(Set, int, Duration)} method.
     *
     * @see Reversi#roundRobin(Set, int, Duration)
     */
    @Test
    public final void testRoundRobin_usesAlphaBeta() {

        Set<Actor> actors = new HashSet<Actor>();
        actors.add(Actor.valueOf("Alpha-Beta 2 ply Count Difference", AlphaBeta.getInstance().searcher(2, new CountDifference())));
        actors.add(Actor.valueOf("Alpha-Beta 2 ply Mobility", AlphaBeta.getInstance().searcher(2, new Mobility())));
        actors.add(Actor.valueOf("Alpha-Beta 2 ply Weighted Squares", AlphaBeta.getInstance().searcher(2, new WeightedSquares())));
        actors.add(Actor.valueOf("Alpha-Beta 2 ply Mod-Weighted Squares", AlphaBeta.getInstance().searcher(2, new ModifiedWeightedSquares())));
        actors.add(Actor.valueOf("Alpha-Beta 2 ply Iago", AlphaBeta.getInstance().searcher(2, new Iago())));
        actors.add(Actor.valueOf("Random Strategy", new RandomStrategy()));

        Map<String, Object> results = Reversi.roundRobin(actors, 10, STANDARD_GAME_DURATION);

        String report = Reversi.postProcessRoundRobinResults(results);

        /** Output of the test. */
        write(report,
              "ReversiTest",
              "Method=testRoundRobin_usesAlphaBeta");

        assertTrue("The test must run without exceptions.", true);

    }

    /**
     * Runs a series of games, than returns a map collecting the results.
     *
     * @param blStrategy    the black strategy passed to the reversi method
     * @param whStrategy    the white strategy passed to the reversi method
     * @param ps            the print stream passed to the reversi method
     * @param gameDuration  the game duration passed to the reversi method
     * @param numberOfGames the number of games played
     * @return              a map hosting the results
     */
    private Map<String, Object> runSeriesOfGames(final Strategy blStrategy,
                                                 final Strategy whStrategy,
                                                 final PrintStream ps,
                                                 final Duration gameDuration,
                                                 final int numberOfGames) {

        Map<String, Object> results = new HashMap<String, Object>();

        /** Statistical values. */
        double mean = 0.;
        double variance = 0.;
        int blackWins = 0;
        int whiteWins = 0;
        int draws = 0;

        final Calendar c = Calendar.getInstance();
        final Duration testDuration;
        final List<Integer> scores = new ArrayList<Integer>(numberOfGames);

        /** Running games. */
        long startTime = System.nanoTime();
        for (int i = 0; i < numberOfGames; i++) {
            int score = Reversi.reversi(blStrategy,
                                        whStrategy,
                                        ps,
                                        gameDuration);
            assertTrue(score <= +64 && score >= -64);
            scores.add(score);
        }
        long endTime = System.nanoTime() - startTime;
        testDuration = new Duration(endTime / NANOSECONDS_PER_MILLISECOND);
        results.put("scores", scores);
        results.put("testDuration", testDuration);

        /** Statistical calculations. */
        for (Integer value : scores) {
            if (value > 0) {
                blackWins++;
            } else if (value < 0) {
                whiteWins++;
            } else {
                draws++;
            }
            mean = mean + value;
        }
        mean = mean / (double) numberOfGames;
        for (Integer value : scores) {
            variance = variance + (value - mean) * (value - mean);
        }
        variance = variance / (double) numberOfGames;

        results.put("mean", mean);
        results.put("variance", variance);
        results.put("blackWins", blackWins);
        results.put("whiteWins", whiteWins);
        results.put("draws", draws);

        StringBuilder sb = new StringBuilder();
        sb.append(String.format("Date & time:\t\t%tB %te, %tY - %tl:%tM %tp%n", c, c, c, c, c, c));
        sb.append(String.format("Number of games:\t%d%n", scores.size()));
        sb.append(String.format("Test duration:\t\t%s%n", testDuration));

        sb.append(String.format("Mean of scores:\t\t%1.2f%n", mean));
        sb.append(String.format("Variance of scores:\t%1.2f%n", variance));

        sb.append(String.format("Black wins:\t\t%d\t[%4.2f percent]%n",
                                blackWins, 100.0 * (double) blackWins / (double) numberOfGames));
        sb.append(String.format("White wins:\t\t%d\t[%4.2f percent]%n",
                                whiteWins, 100.0 * (double) whiteWins / (double) numberOfGames));
        sb.append(String.format("Draws:\t\t\t%d\t[%4.2f percent]%n",
                       draws, 100.0 * (double) draws / (double) numberOfGames));

        results.put("output", sb.toString());

        return results;
    }

    /**
     * Writes the content to the specified filename.
     * <p>
     * The method should be refacored into a dedicated utility class.
     *
     * @param content  the content to be written to the file.
     * @param filename the nane of the file.
     * @param header   the header written before the content.
     */
    static void write(final String content, final String filename, final String header) {
        final PrintStream out = testOutputStream(filename);
        try {
            out.println(header);
            out.printf(content);
            out.println();
        } catch (Exception e) {
            throw new RuntimeException(e);
        } finally {
            out.close();
        }
    }

    /**
     * Provides a print stream given a filename.
     * The method try to open the file into a properly configured directory.
     * The directory is identified using a system property. If the directory
     * is not found a tmp dir is then used.
     * The filename is obtained chaining the filename parameter with a txt file suffix.
     *
     * @param filename the filename used for the print stream construction
     * @return         a new print stream
     */
    private static PrintStream testOutputStream(final String filename) {
        OutputStream out;
        File fOut = null;
        String sTestOutputFileDir = System.getProperty("test.output-files.dir");
        if (sTestOutputFileDir != null) {
            File testOutputFileDir = new File(sTestOutputFileDir);
            if (testOutputFileDir.exists() && testOutputFileDir.isDirectory()) {
                fOut = new File(testOutputFileDir, filename + ".txt");
            }
        }
        if (fOut == null || (fOut.exists() && !fOut.canWrite())) {
            try {
                fOut = File.createTempFile(filename, "txt");
            } catch (IOException ioe) {
                throw new RuntimeException(ioe);
            }
        }
        try {
            out = new FileOutputStream(fOut, true);
        } catch (FileNotFoundException fnfe) {
                throw new RuntimeException(fnfe);
        }
        return new PrintStream(out);
    }

}
