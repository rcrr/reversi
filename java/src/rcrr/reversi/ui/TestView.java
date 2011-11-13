/*
 *  TestView.java
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
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 *  or visit the site <http://www.gnu.org/licenses/>.
 */

package rcrr.reversi.ui;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

class TestView {

    private static final int FRAME_W = 300;
    private static final int FRAME_H = 142;
    private static final int SQUARE_SIDE = 70;
    private static final int SURROUNDING_GAP = 10;

    private static final int BUTTON_W = 80;
    private static final int BUTTON_H = 24;

    private String squareColor;

    private JFrame frame;
    private JLayeredPane lp;
    private JPanel bg;
    private JPanel square;
    private JLabel disc;
    private JPanel commands;

    private JButton empty;
    private JButton black;
    private JButton white;

    TestView() {

        squareColor = "empty";

        Dimension baseDim = new Dimension (FRAME_W, FRAME_H);

        frame = new JFrame("Testing the Reversi UI");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        lp = new JLayeredPane();
        frame.getContentPane().add(lp);
        lp.setPreferredSize(baseDim);
        lp.setOpaque(false);


        bg = new JPanel();
        lp.add(bg, new Integer(0));
        bg.setLayout(new BorderLayout());
        bg.setBounds(0, 0, FRAME_W, FRAME_H);
        bg.setBackground(Constants.BACKGROUND_COLOR);
        bg.setPreferredSize(baseDim);
        bg.setOpaque(true);


        square = new JPanel();
        lp.add(square, new Integer(10));
        square.setLayout(new BorderLayout());
        square.setBounds((FRAME_W - SQUARE_SIDE) / 2, SURROUNDING_GAP, SQUARE_SIDE, SQUARE_SIDE);
        square.setBackground(Constants.BASE_COLOR);
        square.setOpaque(true);

        disc = new JLabel();
        disc.setHorizontalAlignment(JLabel.CENTER);
        disc.setVerticalAlignment(JLabel.CENTER);
        square.add(disc);


        int commx = SURROUNDING_GAP;
        int commy = SQUARE_SIDE + 2 * SURROUNDING_GAP;
        int commw = FRAME_W - 2 * SURROUNDING_GAP;
        int commh = FRAME_H - (SQUARE_SIDE + 3 * SURROUNDING_GAP);
        commands = new JPanel();
        lp.add(commands, new Integer(10));
        commands.setLayout(null);
        commands.setBounds(commx, commy, commw, commh);
        commands.setBackground(Constants.BUTTON_BG_COLOR);
        commands.setOpaque(true);
        Border b1 = BorderFactory.createLineBorder(Constants.LABEL_TEXT_COLOR, 1);
        Border b2 = BorderFactory.createLineBorder(Constants.BACKGROUND_COLOR, 3);
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
        empty.setFont(Constants.BUTTON_FONT);
        empty.setBackground(Constants.BACKGROUND_COLOR);
        empty.setForeground(Constants.LABEL_TEXT_COLOR);
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
        black.setFont(Constants.BUTTON_FONT);
        black.setBackground(Constants.BACKGROUND_COLOR);
        black.setForeground(Constants.LABEL_TEXT_COLOR);
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
        white.setFont(Constants.BUTTON_FONT);
        white.setBackground(Constants.BACKGROUND_COLOR);
        white.setForeground(Constants.LABEL_TEXT_COLOR);
        white.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    setSquareColor("white");
                }
            });

        /* Create the menu bar. */
        JMenuBar jmb = new JMenuBar();

        /* Create the File menu, with the Exit commnad. */
        JMenu jmFile = new JMenu("File");
        JMenuItem jmiExit = new JMenuItem("Exit");
        jmFile.add(jmiExit);
        jmb.add(jmFile);

        /* Add the action listener to the Exit command. */
        jmiExit.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    System.exit(0);
                }
            });

        /* Add the menu bar tothe main frame. */
        frame.setJMenuBar(jmb);

        frame.pack();
        frame.setVisible(true);

    }

    public synchronized String getSquareColor() {
        return this.squareColor;
    }

    public synchronized void setSquareColor(String sc) {
        if (this.squareColor != sc) {
            Icon ico = disc.getIcon();
            if ("empty".equals(sc)) {
                ico = null;
                empty.setEnabled(false);
                black.setEnabled(true);
                white.setEnabled(true);         
            } else if ("black".equals(sc)) {
                ico = Constants.BLACK_DISC_ICON;
                empty.setEnabled(true);
                black.setEnabled(false);
                white.setEnabled(true);         
            } else if ("white".equals(sc)) {
                ico = Constants.WHITE_DISC_ICON;
                empty.setEnabled(true);
                black.setEnabled(true);
                white.setEnabled(false);                
            } else {
                System.out.println("Error: invalid square color sc: " + sc);
                sc = this.squareColor;
            }
            disc.setIcon(ico);
            this.squareColor = sc;
        }
        return;
    }

    private void execCommand(final String command) {
        if (command != null) {
            if (command.equals("empty") || command.equals("white") || command.equals("black")) {
                SwingUtilities.invokeLater(new Runnable() {
                        public void run () {
                            setSquareColor(command);
                        }
                    });
            }
        }
    } 

    public static void main(String args[]) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    new TestView();
                }
            });
    }

}
