
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