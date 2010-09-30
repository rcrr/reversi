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
  <li>Javadoc has to be completed.</li>
  <li>Reversi class has no tests. Do it.</li>
  <li>Move has to be a full object having: "put disk", "pass", and "resign".</li>
  <li>Minimax: searcher should be a single method receiving a minimax object ....</li>
  <li>Board + Player should be enclosed into a compound GameNode Object.
      Node should become SearchNode.</li>
  <li>Minimax: The final value should be calculated by the eval function.</li>
  <li>Clock: Rewrite the class using the Joda-Time library.</li>
  <li>Clock: move the two long fields into an EnumMap having the Players as keys.</li>
  <li>Clock: parameters boundaries are not tested.</li>
  <li>Clock: create a GameClock class that accept start(), pause(), stop() and creates the Clocks to
      be distributed to clients. Clients are GameSates and so the Strategies.
      Use java.lang.Timer to schedule the GameClock refresh.
      GameClock has to be a mutable field of Game.
      May be just a class is enough. The Clock (immutable) could have two roles GameState's Clock and Game's Clock.</li>
  <li>Game, Clock: The end of time is not handled correctly.</li>
  <li>Game: Game should have two fields: the GameHistory (the sequence of game states),
      and the GameClock.</li>
  <li>CountDifference: ok</li>
  <li>Game: write junit tests.</li>
  <li>Game: review javadocs.</li>
  <li>GameOverException: delete it.</li>
  <li>GameState: review test completeness.</li>
  <li>HumanStrategy: review input and output streams usage. Review the prompt management.</li>
  <li>MaximizeDifference: write tests and javadocs.</li>
  <li>MinimaxSearcherCountDifference: write javadocs and tests.</li>
  <li>Reversi: Strategies should receive a configuration structure (XML or properties).</li>
  <li>Brainstorming on a strategy builder class.</li>
  <li>ModifiedWeightedSquares: javadocs and tests.</li>
  <li>RandomStrategy: javadocs and tests.</li>
  <li>WeightedSquares: javadocs and tests.</li>
  <li>....</li>
</ul>

<p>
Java source files:
<p>
<ul>
  <li>AlphabetaSearcherCountDifference</li>
  <li>AlphabetaSearcherModifiedWeightedSquares</li>
  <li>AlphabetaSearcherWeightedSquares</li>
  <li>Board: ok. Javadocs complete. Tests complete.</li>
  <li>Clock: changes are pending. Javadocs complete. Tests complete.</li>
  <li>Column: ok. Javadocs complete. Tests complete. Notes: see Row.</li>
  <li>CountDifference: ok. Javadocs complete. Tests complete.</li>
  <li>Direction: ok. Javadocs complete. Tests complete.
      Notes: (1) deltaRow and deltaColumn are int, could be better having an enum Shift UP, NEUTRAL, DOWN...?
             (2) shift(int delta) method in Row and Column would be transformed accordingly ....</li>
  <li>EvalFunction: ok. Javadocs complete.</li>
  <li>Game</li>
  <li>GameOverException: to be removed.</li>
  <li>GameState</li>
  <li>HumanStrategy</li>
  <li>MaximizeDifference</li>
  <li>Minimax: changes pending.</li>
  <li>MinimaxSearcherCountDifference</li>
  <li>ModifiedWeightedSquares: ok. Javadocs complete. Tests complete.</li>
  <li>package-info: ok</li>
  <li>Player: ok. Javadocs complete. Tests complete.</li>
  <li>RandomStrategy</li>
  <li>Reversi</li>
  <li>Row: ok. Javadocs complete. Tests complete.
      Notes: (1) getInstance(int index) is redundant.
             (2) shift(int delta) is used just in the neighbor table calculation. It is a bit ugly.</li>
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
  <li>GameStateTest</li>
  <li>GameTest</li>
  <li>MinimaxTest</li>
  <li>ModifiedWeightedSquaresTest: tests complete.</li>
  <li>PlayerTest: tests complete.</li>
  <li>ReversiTest</li>
  <li>RowTest: tests complete.</li>
  <li>SquareStateTest: tests complete.</li>
  <li>SquareTest: tests complete.</li>
  <li>WeightedSquaresTest: tests complete.</li>
</ul>

@author Roberto Corradini
@version 0.1
*/

package rcrr.reversi;
