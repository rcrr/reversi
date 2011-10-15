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

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

import java.net.URL;

class TestView {

    private static final Color BACKGROUND_COLOR = new Color(0, 0, 0);
    private static final Color LABEL_TEXT_COLOR = new Color(220, 220, 220);
    private static final Color BASE_COLOR = new Color(32, 142, 32);
    private static final Color BUTTON_COLOR = new Color(30, 30, 30);
    private static final Color BUTTON_BG_COLOR = new Color(30, 30, 30);

    private static final int FRAME_W = 300;
    private static final int FRAME_H = 142;
    private static final int SQUARE_SIDE = 70;
    private static final int SURROUNDING_GAP = 10;

    private static final int BUTTON_W = 80;
    private static final int BUTTON_H = 24;

    private static final Font BUTTON_FONT = new Font("Courier New", Font.ITALIC, 12);

    boolean initialized = false;
    String squareColor = "";

    JFrame frame;
    JLayeredPane lp;
    JPanel bg;
    JPanel square;
    JLabel disk;
    JPanel commands;

    JButton empty;
    JButton black;
    JButton white;

    TestView() {}

    private synchronized void init() throws Exception {

	if (initialized) throw new Exception("Object instance already initialized");

	squareColor = "empty";

	int w = FRAME_W;
	int h = FRAME_H;
	Dimension baseDim = new Dimension (w, h);

	frame = new JFrame("Testing the Reversi Board");
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

	lp = new JLayeredPane();
	frame.getContentPane().add(lp);
	lp.setPreferredSize(baseDim);
	lp.setOpaque(false);


	bg = new JPanel();
	lp.add(bg, new Integer(0));
	bg.setLayout(new BorderLayout());
        bg.setBounds(0, 0, w, h);
	bg.setBackground(BACKGROUND_COLOR);
	bg.setPreferredSize(baseDim);
	bg.setOpaque(true);


	square = new JPanel();
	lp.add(square, new Integer(10));
	square.setLayout(new BorderLayout());
        square.setBounds((w - SQUARE_SIDE) / 2, SURROUNDING_GAP, SQUARE_SIDE, SQUARE_SIDE);
	square.setBackground(BASE_COLOR);
	square.setOpaque(true);

	disk = new JLabel();
	disk.setHorizontalAlignment(JLabel.CENTER);
	disk.setVerticalAlignment(JLabel.CENTER);
	square.add(disk);


	int commx = SURROUNDING_GAP;
	int commy = SQUARE_SIDE + 2 * SURROUNDING_GAP;
	int commw = w - 2 * SURROUNDING_GAP;
	int commh = h - (SQUARE_SIDE + 3 * SURROUNDING_GAP);
	commands = new JPanel();
	lp.add(commands, new Integer(10));
	commands.setLayout(null);
        commands.setBounds(commx, commy, commw, commh);
	commands.setBackground(BUTTON_BG_COLOR);
	commands.setOpaque(true);
	Border b1 = BorderFactory.createLineBorder(LABEL_TEXT_COLOR, 1);
	Border b2 = BorderFactory.createLineBorder(BACKGROUND_COLOR, 3);
	Border commandsBorder = BorderFactory.createCompoundBorder(b1, b2); 
	commands.setBorder(commandsBorder);

	Insets commandsInsets = commands.getInsets();
	int bfreespacew = commw - (commandsInsets.left + commandsInsets.right) - (3 * BUTTON_W);
	int bex = commandsInsets.left + (bfreespacew / 4);
	int bey = (commh - BUTTON_H) / 2;
	int bbx = (commw - BUTTON_W) / 2;
	int bby = (commh - BUTTON_H) / 2;
	int bwx = commandsInsets.left + 3 * (bfreespacew / 4) + 2 * BUTTON_W;
	int bwy = (commh - BUTTON_H) / 2;
	int bw = BUTTON_W;
	int bh = BUTTON_H;
	empty = new JButton("Empty");
	empty.setEnabled(false);
	commands.add(empty);
	empty.setBounds(bex, bey, bw, bh);
	empty.setOpaque(true);
	empty.setFont(BUTTON_FONT);
	empty.setBackground(BACKGROUND_COLOR);
	empty.setForeground(LABEL_TEXT_COLOR);
	empty.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent ae) {
		    setSquareColor("empty");
		}
	    });
	black = new JButton("Black");
	black.setEnabled(true);
	commands.add(black);
	black.setBounds(bbx, bby, bw, bh);
	black.setOpaque(true);
	black.setFont(BUTTON_FONT);
	black.setBackground(BACKGROUND_COLOR);
	black.setForeground(LABEL_TEXT_COLOR);
	black.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent ae) {
		    setSquareColor("black");
		}
	    });
	white = new JButton("White");
	white.setEnabled(true);
	commands.add(white);
	white.setBounds(bwx, bwy, bw, bh);
	white.setOpaque(true);
	white.setFont(BUTTON_FONT);
	white.setBackground(BACKGROUND_COLOR);
	white.setForeground(LABEL_TEXT_COLOR);
	white.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent ae) {
		    setSquareColor("white");
		}
	    });

	/* Create the menu bar. */
	JMenuBar jmb = new JMenuBar();

	/* Create the file menu. */
	JMenu jmFile = new JMenu("File");
	JMenuItem jmiExit = new JMenuItem("Exit");
	jmFile.add(jmiExit);
	jmb.add(jmFile);

	/* Add the action listener to the exit command. */
	jmiExit.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent ae) {
		    System.exit(0);
		}
	    });

	/* Add the menu bar tothe main frame. */
	frame.setJMenuBar(jmb);

	frame.pack();
	frame.setVisible(true);

	initialized = true;

    }

    public synchronized String getSquareColor() {
	return this.squareColor;
    }

    public synchronized void setSquareColor(String sc) {
	if (this.squareColor != sc) {
	    Icon ico = disk.getIcon();
	    if ("empty".equals(sc)) {
		ico = null;
		empty.setEnabled(false);
		black.setEnabled(true);
		white.setEnabled(true);		
	    } else if ("black".equals(sc)) {
		ico = Constants.BLACK_DISK_ICON;
		empty.setEnabled(true);
		black.setEnabled(false);
		white.setEnabled(true);		
	    } else if ("white".equals(sc)) {
		ico = Constants.WHITE_DISK_ICON;
		empty.setEnabled(true);
		black.setEnabled(true);
		white.setEnabled(false);		
	    } else {
		System.out.println("Error: invalid square color sc: " + sc);
		sc = this.squareColor;
	    }
	    disk.setIcon(ico);
	    this.squareColor = sc;
	}
	return;
    }

    public void execCommand(final String command) {
	if (command != null) {
	    if (command.equals("empty") || command.equals("white") || command.equals("black")) {
		SwingUtilities.invokeLater(new Runnable() {public void run () {setSquareColor(command);}});
	    }
	}
    } 

    public static TestView initDisplay() {
	final TestView tv = new TestView();
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    try {
			tv.init();
		    } catch (Exception e) {
			System.out.println("Exiting on exception: " + e);
			System.exit(1);
		    }
		}
	    });
	return tv;
    }

    public static void main(String args[]) {
	TestView tv = TestView.initDisplay();    
    }

}