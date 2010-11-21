/*
 *  package-info.java
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

/**
The Reversi Program.
Main algorithm and structural classes.
<p>
To do:
<p>
<ul>
  <li>Moves returned from the Strategies must be rocordered. The Sequence structure has to host
      the moves between each transition. Ideally each adiacent pair should have a list of: [Move, Clock, Status] tuple.
      The "record" could be attached to GameSnapshot. null is a valid value, but is not compatible with a valid game sequence.
      The move method has to validate the returned move from the Strategy, record the move with a new clock,
      recur when the move is not legal.
      When the move is a STANDARD LEGAL one, the method updates the game sequence, and returns to play method.
      When the move is LEGAL, like PASS or RESIGN, but is not STANDARD (a real PUT_DISK move), the program logic
      ha to be designed.</li>
  <li>Game: develop a "state machine" and the appropriate transitions.
      Game objects are mutable. Fields are gameClock, gameHistory, gameState.
      Each gameState (an inner enum) has the appropriate transitions.
      State transitions trigger the clock changes. Transitions are based on client/server messages.
      The server is the game object, clients are the strategies.
      Use java.lang.Timer (or ScheduledThreadPoolExecutor) to schedule the gameClock refresh.</li>
  <li>Clock: tests are a bit ugly.</li>
  <li>Clock: parameters boundaries are not fully tested.</li>
  <li>Game, Clock: The end of time is not handled correctly, after adding the Timer thread for updating the clock,
      also the two strategies must have a dedicated thread.</li>
  <li>Game: strategies are a map defined into Game. A new Actor class is needed. Actors are the identity of players.
      An Actor has as a minimum an id, a name, a Strategy.
      A game must have an object (Opponents) having two actors assigned to black and to white.</li>
  <li>Game: write junit tests.</li>
  <li>Game: review javadocs.</li>
  <li>HumanStrategy: review input and output streams usage. Review the prompt management.</li>
  <li>MaximizeDifference: write tests and javadocs.</li>
  <li>Strategies:
      - Minimax: The final value should be calculated by the eval function.
      - Brainstorming on a strategy builder class, that is implemented into an AbstratctStrategy, and that
        recieve a configuration structure (XML or properties).</li>
</ul>

<p>
Java source files:
<p>
<ul>
  <li>AbstractDecisionRule: Javadocs missing.</li>
  <li>AlphaBeta: Javadocs missing. Tests missing.</li>
  <li>AlphabetaSearcherCountDifference: to be deleted.</li>
  <li>AlphabetaSearcherModifiedWeightedSquares: to be deleted.</li>
  <li>AlphabetaSearcherWeightedSquares: to be deleted.</li>
  <li>Board: ok. Javadocs complete. Tests complete.</li>
  <li>Clock: changes are pending. Javadocs complete. Tests complete.</li>
  <li>Column: ok. Javadocs complete. Tests complete. Notes: see Row.</li>
  <li>CountDifference: ok. Javadocs complete. Tests complete.</li>
  <li>DecisionRule: Javadocs missing.</li>
  <li>Direction: ok. Javadocs complete. Tests complete.
      Notes: (1) deltaRow and deltaColumn are int, could be better having an enum Shift UP, NEUTRAL, DOWN...?
             (2) shift(int delta) method in Row and Column would be transformed accordingly ....</li>
  <li>EvalFunction: ok. Javadocs complete.</li>
  <li>Game: class design is unclear. Tests are on hold.</li>
  <li>GameState: ok. Javadocs complete. Tests complete.</li>
  <li>HumanStrategy: Javadocs and junit test missing.</li>
  <li>MaximizeDifference: Javadocs and junit test missing.</li>
  <li>Minimax: changes pending.</li>
  <li>MinimaxSearcherCountDifference: to be deleted.</li>
  <li>ModifiedWeightedSquares: ok. Javadocs complete. Tests complete.</li>
  <li>package-info: ok</li>
  <li>Player: ok. Javadocs complete. Tests complete.</li>
  <li>RandomStrategy: Javadocs and junit test missing..</li>
  <li>Reversi: under review.</li>
  <li>Row: ok. Javadocs complete. Tests complete.
      Notes: (1) getInstance(int index) is redundant.
             (2) shift(int delta) is used just in the neighbor table calculation. It is a bit ugly.</li>
  <li>SearchNode: Javadocs missing. Tests missing.</li>
  <li>Square: ok. Javadocs complete. Tests complete.</li>
  <li>SquareState: ok. Javadocs complete. Tests complete.</li>
  <li>Strategy: ok. Javadocs complete.</li>
  <li>WeightedSquares: ok. Javadocs complete. Tests complete.</li>
</ul>

<p>
JUnit source files:
<p>
<ul>
  <li>BoardTest: tests complete.</li>
  <li>ClockTest: tests under construction.</li>
  <li>ColumnTest: tests complete.</li>
  <li>CountDifferenceTest: tests complete.</li>
  <li>DirectionTest: tests complete.</li>
  <li>GameStateTest: to be written.</li>
  <li>GameTest: tests complete.</li>
  <li>MinimaxTest: under review.</li>
  <li>ModifiedWeightedSquaresTest: tests complete.</li>
  <li>PlayerTest: tests complete.</li>
  <li>ReversiTest: to be written.</li>
  <li>RowTest: tests complete.</li>
  <li>SquareStateTest: tests complete.</li>
  <li>SquareTest: tests complete.</li>
  <li>WeightedSquaresTest: tests complete.</li>
</ul>

@author Roberto Corradini
@version 0.1
*/

package rcrr.reversi;
