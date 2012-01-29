/*
 *  package-info.java
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

/**
The Reversi Program.
Main algorithm and structural classes.
<p>
To do:
<p>
<ul>
  <li>Reversi.reversiSeries and Reversi.roundRobin should write results to a binary/ascii file
      during execution of each game. A post processor has to be developed.</li>
  <li>Some methods receive as parameters the board and the player. Replace them with a GamePosition.</li>
  <li>Verify that all the factories that receive a mutable object make the appropriate defensive copy.</li>
  <li>The actor that share the strategy by means of the strategy() method is not safe.
      The client can get a reference of the strategy object and modify it. It is better to
      add a move() method to the actor and keep the strategy more shielded.</li>
  <li>Veryfy that the Builder classes are really thread-safe. Be careful about accessor methods that publish a reference
      of a collection, or in general a mutable object reference. It must be copied.
      The Builder inner classes must have a unit test.</li>
  <li>Prepare some tests (ReversiTest suite) that repeat selected tests but print the game to a file.
      Some part of the Game class are used only when the PrintStream ps field is not null.</li>
  <li>Row: ok. Javadocs complete. Tests complete.
      Notes: (1) getInstance(int index) is redundant.
             (2) shift(int delta) is used just in the neighbor table calculation. It is a bit ugly.</li>
  <li>Direction: ok. Javadocs complete. Tests complete.
      Notes: (1) deltaRow and deltaColumn are int, could be better having an enum Shift UP, NEUTRAL, DOWN...?
             (2) shift(int delta) method in Row and Column would be transformed accordingly ....</li>
  <li>Prepare a "<i>Literate Paper</i>" that describes the software architecture of the java version.</li>
  <li>Replace Ant with Maven.</li>
  <li>Develop JUnit performance testing and reports.
      See: <a href="http://databene.org/contiperf.html" target="_blank">ContiPerf</a></li>
  <li>Introduce a unit test coverage kpi.
      See: <a href="http://cobertura.sourceforge.net/introduction.html" target="_blank">Coberdura</a></li>
      See: <a href="http://emma.sourceforge.net" target="_blank">EMMA</a></li>
  <li>A simple Java SWING front end is ready. A complete refactor is needed.
  <li>Which practice is best when it comes to write unit test for UI?
      After a brief search on google the best so far tool to investigate
      on is UISpec4J.
      See: <a href="http://www.uispec4j.org" target="_blank">UISpec4J</a>
      See: <a href="http://code.google.com/p/windowlicker/" target="_blank">WindowLicker</a></li>
  <li>Organize consistently the common lisp and clojure codebases.</li>
  <li>Publish the "<i>Reversi Web Site</i>" on GitHub.</li>
  <li>Develop a client-server architecture that separates carefully the game-server from
      the two players.
      The clock should run asynchronously. See ScheduledThreadPoolExecutor.
      Evaluate the option to use an XMPP protocol.
      A proposed library by the book <i>"Growing Object-Oriented Software, Guided by Tests"</i> is Openfire.
      See: <a href="http://xmpp.org/" target="_blank">XMPP Standards Foundation</a>
      See: <a href="http://www.igniterealtime.org/projects/openfire/index.jsp" target="_blank">Openfire</a></li>
  <li>Develop a concurrent search algorithm.</li>
</ul>

<p>

@author Roberto Corradini
@version 0.1
*/

package rcrr.reversi;
