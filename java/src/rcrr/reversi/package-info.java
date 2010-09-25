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
  <li>Write the Eval Function tests.</li>
  <li>Javadoc has to be completed.</li>
  <li>Reversi class has no tests. Do it.</li>
  <li>Board and Player should be grouped into a compound object.
      It is not GameState, because the Clock is too volatile.</li>
  <li>The end of time is not handled correctly.</li>
  <li>The game should start an "independent" Clock using a timer thread.
      The GameState Clock should be just a transcription value.</li>
  <li>Move has to be a full object having: "put disk", "pass", and "resign".</li>
  <li>Minimax searcher should be a single method receiving a minimax object ....</li>
  <li>Board + Player should be enclosed into a compound GameNode Object.
      Node should become SearchNode.</li>
  <li>Minimax: The final value should be calculated by the eval function.</li>
  <li>Minimax: maximizer method should be put into a StrategyUtils or StrategyCollection class.</li>
  <li>Board: complete the class description.</li>
  <li>Clock: move the two long fields into an EnumMap having the Players as keys.</li>
  <li>Clock: parameters boundaries are not tested.</li>
  <li>CountDifference: ok</li>
  <li>Direction: ok</li>
  <li>Game: write junit tests.</li>
  <li>Game: review javadocs.</li>
  <li>GameOverException: delete it.</li>
  <li>GameState: review test completenes.</li>
  <li>HumanStrategy: review input and output streams usage. Review the prompt management.</li>
  <li>MaximizeDifference: write tests and javadocs.</li>
  <li>MinimaxSearcherCountDifference: write javadocs and tests.</li>
  <li>Reversi: Strategies should receive a configuration structure (XML or properties).</li>
  <li>Brainstorming on a strategy builder class.</li>
  <li>ModifiedWeightedSquares: javadocs and tests.</li>
  <li>Player: ok</li>
  <li>RandomStrategy: javadocs and tests.</li>
  <li>Square: ok</li>
  <li>SquareState: ok</li>
  <li>Strategy: ok</li>
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
  <li>Board</li>
  <li>Clock</li>
  <li>Column</li>
  <li>CountDifference</li>
  <li>Direction</li>
  <li>EvalFunction</li>
  <li>Game</li>
  <li>GameOverException</li>
  <li>GameState</li>
  <li>HumanStrategy</li>
  <li>MaximizeDifference</li>
  <li>Minimax</li>
  <li>MinimaxSearcherCountDifference</li>
  <li>ModifiedWeightedSquares</li>
  <li>package-info</li>
  <li>Player</li>
  <li>RandomStrategy</li>
  <li>Reversi</li>
  <li>Row</li>
  <li>Square</li>
  <li>SquareState</li>
  <li>Strategy</li>
  <li>WeightedSquares</li>
</ul>

<p>
JUnit source files:
<p>
<ul>
  <li>BoardTest</li>
  <li>ClockTest</li>
  <li>ColumnTest</li>
  <li>CountDifferenceTest</li>
  <li>DirectionTest</li>
  <li>GameStateTest</li>
  <li>GameTest</li>
  <li>MinimaxTest</li>
  <li>PlayerTest</li>
  <li>ReversiTest</li>
  <li>RowTest</li>
  <li>SquareStateTest</li>
  <li>SquareTest</li>
</ul>

@author Roberto Corradini
@version 0.1
*/

package rcrr.reversi;
