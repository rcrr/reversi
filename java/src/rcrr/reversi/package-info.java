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
  <li>Complete the Minimax unit test.</li>
  <li>Write the Eval Functions test.</li>
  <li>Javadoc has to be completed.</li>
  <li>Reversi class has no tests. Do it.</li>
  <li>Board and Player shoul be grouped into a compound object.
      It is not GameState, because the Clock is too volatile.</li>
  <li>The end of time is not handled correctly.</li>
  <li>The game should start an "indipendent" Clock using a timer thread.
      The GameState Clock should be just a transcription value.</li>
</ul>

@author Roberto Corradini
@version 0.1
*/

package rcrr.reversi;
