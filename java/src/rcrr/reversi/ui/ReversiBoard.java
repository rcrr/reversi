 /*
    Copyright (c) 2010, 2011 Roberto Corradini

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

import java.util.EnumMap;
import java.util.Map;
import java.util.StringTokenizer;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.FlowLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagLayout;
import java.awt.GridBagConstraints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JTextField;
import javax.swing.JPanel;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JLayeredPane;
import javax.swing.SwingUtilities;
import javax.swing.BorderFactory;
import javax.swing.border.TitledBorder;

import java.io.BufferedReader;
import java.io.InputStreamReader;

import java.net.URL;
 
public class ReversiBoard {

    private Map<BoardSquareKey, Square> squares = new EnumMap<BoardSquareKey, Square>(BoardSquareKey.class);
    private JFrame mainFrame;
    private JLayeredPane boardPane;
    private JPanel gridPanel;
    private JPanel dotsPanel;
    private JPanel labelsPanel;
    private JPanel labelsCornerPanel;
    private JPanel rowLabelsPanel;
    private JPanel colLabelsPanel;
    private JPanel commandPanel;

    /** The command text field. */
    private JTextField ctf;

    private boolean initialized = false;

    public ReversiBoard() { }

    private synchronized void init() {

	if (initialized) throw new RuntimeException("ReversiBoard instance already initialized.");

	mainFrame = new JFrame("Reversi Board");
	mainFrame.getContentPane().setLayout(new GridBagLayout());
	GridBagConstraints c = new GridBagConstraints();
	c.fill = GridBagConstraints.HORIZONTAL;
	mainFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	
	final int gsd = 8 * Constants.SQUARE_SIZE;
	final int lsd = gsd + (Constants.LABELS_HEIGHT + (2 * Constants.LABELS_GAP) + Constants.SQUARES_GAP);
        final Dimension gridSize = new Dimension(gsd, gsd);
        final Dimension labelsSize = new Dimension(lsd, lsd);
	final int gs = Constants.LABELS_HEIGHT + Constants.LABELS_GAP + Constants.SQUARES_GAP;
	
        // Use a Layered Pane for this application.
	boardPane = new JLayeredPane();
	c.gridx = 0;
	c.gridy = 0;
        mainFrame.getContentPane().add(boardPane, c);
        boardPane.setPreferredSize(labelsSize);

	// Add the labels panel to the Layered Pane.
	labelsPanel = new JPanel();
	boardPane.add(labelsPanel, new Integer(10));
        labelsPanel.setLayout(null);
        labelsPanel.setPreferredSize(labelsSize);
        labelsPanel.setBounds(0, 0, labelsSize.width, labelsSize.height);
	labelsPanel.setBackground(Constants.BACKGROUND_COLOR);
	labelsPanel.setOpaque(true);
	
	// Add the North-West corner to the Labels Panel.
	labelsCornerPanel = new JPanel();
	labelsPanel.add(labelsCornerPanel);
	labelsCornerPanel.setLayout(new GridLayout(1, 1, 0, 0));
	labelsCornerPanel.setBackground(Color.white);
        labelsCornerPanel.setBounds(Constants.LABELS_GAP, Constants.LABELS_GAP, Constants.LABELS_HEIGHT, Constants.LABELS_HEIGHT);
	JPanel jplc = new JPanel(new BorderLayout());
	labelsCornerPanel.add(jplc);
	jplc.setBackground(Constants.BASE_COLOR);

	// Add the Row-List to the Label Panel.
	rowLabelsPanel = new JPanel();
	labelsPanel.add(rowLabelsPanel);
	rowLabelsPanel.setLayout(new GridLayout(8, 1, 0, Constants.SQUARES_GAP));
        rowLabelsPanel.setBounds(Constants.LABELS_GAP, gs, Constants.LABELS_HEIGHT, gsd);
	rowLabelsPanel.setBackground(Constants.BACKGROUND_COLOR);
	for (BoardRowKey brk : BoardRowKey.values()) {
	    JPanel jp = new JPanel(new BorderLayout());
	    jp.setBackground(Constants.BASE_COLOR);
            rowLabelsPanel.add(jp);
	    JLabel jl = new JLabel(brk.toString().substring(1, 2), JLabel.CENTER);
	    jl.setFont(Constants.LABELS_FONT);
	    jl.setForeground(Constants.LABEL_TEXT_COLOR);
	    jp.add(jl);
	}

	// Add the Column-List to the Label Panel.
	colLabelsPanel = new JPanel();
	labelsPanel.add(colLabelsPanel);
	colLabelsPanel.setLayout(new GridLayout(1, 8, Constants.SQUARES_GAP, 0));
        colLabelsPanel.setBounds(gs, Constants.LABELS_GAP, gsd, Constants.LABELS_HEIGHT);
	colLabelsPanel.setBackground(Constants.BACKGROUND_COLOR);
	for (BoardColKey bck : BoardColKey.values()) {
	    JPanel jp = new JPanel(new BorderLayout());
	    jp.setBackground(Constants.BASE_COLOR);
            colLabelsPanel.add(jp);
	    JLabel jl = new JLabel(bck.toString().substring(1, 2), JLabel.CENTER);
	    jl.setFont(Constants.LABELS_FONT);
	    jl.setForeground(Constants.LABEL_TEXT_COLOR);
	    jp.add(jl);
	}

        // Add the grid panel to the Layered Pane. 
        gridPanel = new JPanel();
        boardPane.add(gridPanel, new Integer(20));
        gridPanel.setLayout(new GridLayout(8, 8, Constants.SQUARES_GAP, Constants.SQUARES_GAP));
        gridPanel.setPreferredSize(gridSize);
        gridPanel.setBounds(gs, gs, gridSize.width, gridSize.height);
	gridPanel.setBackground(Constants.BACKGROUND_COLOR);

	// Ad the dots panel to the Layered Pane.
	dotsPanel = new JPanel();
        boardPane.add(dotsPanel, new Integer(30));
        dotsPanel.setLayout(null);
        dotsPanel.setPreferredSize(gridSize);
        dotsPanel.setBounds(gs, gs, gridSize.width, gridSize.height);
	dotsPanel.setBackground(Constants.BACKGROUND_COLOR);
	dotsPanel.setOpaque(false);
	setDot(2, 2);
	setDot(6, 2);
	setDot(6, 6);
	setDot(2, 6);

	for (BoardSquareKey bsk : BoardSquareKey.values()) {
	    Square square = new Square(bsk);
            gridPanel.add(square.getJp());
	    squares.put(bsk, square);
	}

	// Add the data entry JTextField
	TitledBorder commandPanelTitle = BorderFactory.createTitledBorder("Command");
	commandPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
	commandPanel.setBorder(commandPanelTitle);
	commandPanel.setBackground(Constants.BACKGROUND_COLOR);
	c.gridx = 0;
	c.gridy = 1;
	mainFrame.getContentPane().add(commandPanel, c);
	ctf = new JTextField(50);
	ctf.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent event) {
		    String command = ctf.getText();
		    if (!(command.equals("quit"))){
			System.out.println("You typed: " + command);
			execCommand(command);
		    } else {
			System.exit(0);
		    }
		}
	    });
	commandPanel.add(ctf);

	mainFrame.pack();
	mainFrame.setResizable(true);
	mainFrame.setLocationRelativeTo(null);
	mainFrame.setVisible(true);

	this.initialized = true;

    }

    public synchronized boolean isInitialized() {
	return this.initialized;
    }

    public static ReversiBoard initDisplay() {
	final ReversiBoard rb = new ReversiBoard();
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    rb.init();
		}
	    });
	return rb;
    }
    
    public void setSquareColor(final BoardSquareKey sk, final SquareColor c) {
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    Square sq = squares.get(sk);
		    sq.setSc(c);
		}
	    });
    }

    public void drawInitialBoard() {
	for (BoardSquareKey bsk : BoardSquareKey.values()) {
	    setSquareColor(bsk, SquareColor.EMPTY);
	}
	setSquareColor(BoardSquareKey.D4, SquareColor.WHITE);
	setSquareColor(BoardSquareKey.E5, SquareColor.WHITE);
	setSquareColor(BoardSquareKey.D5, SquareColor.BLACK);
	setSquareColor(BoardSquareKey.E4, SquareColor.BLACK);
    }

    private void setDot(int x, int y) {
	JLabel dot = new JLabel(Constants.GRID_DOT_ICON);
        dotsPanel.add(dot);
	int xDotCenter = (Constants.SQUARE_SIZE * x) - Constants.DOT_SIZE / 2;
	int yDotCenter = (Constants.SQUARE_SIZE * y) - Constants.DOT_SIZE / 2;
	dot.setBounds(xDotCenter, yDotCenter, Constants.DOT_SIZE, Constants.DOT_SIZE);    
    }

    public static void main(String[] args) {
	ReversiBoard rb = ReversiBoard.initDisplay();
    }

    private void execCommand(String command) {
	StringTokenizer st = new StringTokenizer(command);
	int words = st.countTokens();
	BoardSquareKey bsk = null;
	SquareColor sc = null;
	if (words == 2) {
	    String w0 = st.nextToken();
	    String w1 = st.nextToken();
	    try {
		bsk = BoardSquareKey.valueOf(w0);
	    } catch (IllegalArgumentException iae) {
		System.out.println("Wrong value " + w0 + ". It is not a valid BoardSquareKey.");
	    }
	    try {
		sc = SquareColor.valueOf(w1);
	    } catch (IllegalArgumentException iae) {
		System.out.println("Wrong value " + w1 + ". It is not a valid SquareColor.");
	    }
	    if (bsk != null && sc != null) {
		System.out.println("Setting board square " + bsk + " to color " + sc + ".");
		setSquareColor(bsk, sc);
	    }
	} else {
	    System.out.println("Not a command: " + command);
	}
    }

}