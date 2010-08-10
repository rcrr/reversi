/*
    Copyright (c) 2010 Roberto Corradini

    This file is part of the reversi program
    http://github.com/rcrr/reversi

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the
    Free Software Foundation; either version 3, or (at your option) any
    later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
    or visit the site <http://www.gnu.org/licenses/>.
*/

package rcrr.reversi.ui;

import java.io.*;

class TestMain {

    public static void main(String args[]) {

	String line = "";
       
	System.out.println("Enter a command (type 'quit' to exit): ");

	InputStreamReader isr = new InputStreamReader(System.in);
	BufferedReader in = new BufferedReader(isr);

	TestView tv = TestView.initDisplay();
    
	while (!(line.equals("quit"))){
	    try {
		line = in.readLine();
            } catch (Exception e) {
		System.out.println("Error in reading the input line, exiting.");
		System.exit(1);
	    }

	    if (!(line.equals("quit"))){
		System.out.println("You typed: " + line);
		tv.execCommand(line);
	    } else {
		System.exit(0);
	    }
	}
    }
}