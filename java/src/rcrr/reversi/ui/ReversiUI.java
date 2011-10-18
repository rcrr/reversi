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
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JTextArea;
import javax.swing.JScrollPane;

import java.io.PrintStream;
import java.io.OutputStream;
import java.io.IOException;

import java.net.URL;

import org.joda.time.Period;
import org.joda.time.Duration;

import rcrr.reversi.Board;
import rcrr.reversi.Square;
import rcrr.reversi.Column;
import rcrr.reversi.Row;
import rcrr.reversi.SquareState;
import rcrr.reversi.Game;
import rcrr.reversi.Actor;
 
public class ReversiUI {

    private static final class TextAreaOutputStream extends OutputStream {

	private final JTextArea textArea;
	private final StringBuilder sb = new StringBuilder();

	public TextAreaOutputStream(final JTextArea textArea) {
	    this.textArea = textArea;
	}

	@Override
	public void flush(){ }
    
	@Override
	public void close(){ }

	@Override
	public void write(int b) throws IOException {		
	    sb.append((char)b);
	}
    }


    private static final int DEFAULT_GAME_DURATION_IN_MINUTES = 30;

    private static final String PROMPT = "c> ";
    private static final String ANSWER = "-> ";
    private static final String NEW_LINE = "\n";

    private Map<Square, SquarePanel> squares = new EnumMap<Square, SquarePanel>(Square.class);
    private JFrame mainFrame;
    private JLayeredPane boardPane;
    private JPanel gridPanel;
    private JPanel dotsPanel;
    private JPanel labelsPanel;
    private JPanel labelsCornerPanel;
    private JPanel rowLabelsPanel;
    private JPanel colLabelsPanel;
    private JPanel commandPanel;
    private JPanel consolePanel;

    private JTextArea textArea;

    /** The command text field. */
    private JTextField ctf;

    private Game game;

    private PrintStream consolePrintStream;

    public ReversiUI() {
	game = null;

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
	for (Row row : Row.values()) {
	    JPanel jp = new JPanel(new BorderLayout());
	    jp.setBackground(Constants.BASE_COLOR);
            rowLabelsPanel.add(jp);
	    JLabel jl = new JLabel(row.toString().substring(1, 2), JLabel.CENTER);
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
	for (Column column : Column.values()) {
	    JPanel jp = new JPanel(new BorderLayout());
	    jp.setBackground(Constants.BASE_COLOR);
            colLabelsPanel.add(jp);
	    JLabel jl = new JLabel(column.toString(), JLabel.CENTER);
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

	for (Square square : Square.values()) {
	    SquarePanel squarePanel = new SquarePanel(square);
            gridPanel.add(squarePanel);
	    squares.put(square, squarePanel);
	}

	// Add the command data entry JTextField
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
		    execCommand(ctf.getText());
		}
	    });
	commandPanel.add(ctf);

	TitledBorder consolePanelTitle = BorderFactory.createTitledBorder("Console");
	consolePanel = new JPanel(new GridBagLayout());
	consolePanel.setBorder(consolePanelTitle);
	consolePanel.setBackground(Constants.BACKGROUND_COLOR);
	c.gridx = 1;
	c.gridy = 0;
	c.gridheight = 2;
	c.fill = GridBagConstraints.BOTH;
	mainFrame.getContentPane().add(consolePanel, c);
        textArea = new JTextArea(30, 30);
        textArea.setEditable(false);
	textArea.setBackground(Color.GRAY);
        JScrollPane scrollPane = new JScrollPane(textArea);
        //Add Components to this panel.
        GridBagConstraints c2 = new GridBagConstraints();
        c2.fill = GridBagConstraints.BOTH;
        c2.weightx = 1.0;
        c2.weighty = 1.0;
        consolePanel.add(scrollPane, c2);

	consolePrintStream = new PrintStream(new TextAreaOutputStream(textArea));	

	/* Create the menu bar. */
	JMenuBar jmb = new JMenuBar();

	/* Create the File menu, with the Exit commnad. */
	JMenu jmFile = new JMenu("File");
	jmb.add(jmFile);

	/* Add the New game commnad to the File menu. */
	JMenuItem jmiNewGame = new JMenuItem("New game");
	jmFile.add(jmiNewGame);

	/* Add the action listener to the New game command. */
	jmiNewGame.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent ae) {

		    game = Game.initialGame(new Actor.Builder().build(),
					    new Actor.Builder().build(),
					    Period.minutes(DEFAULT_GAME_DURATION_IN_MINUTES).toStandardDuration(),
					    consolePrintStream);
		    drawGame(game);
		}
	    });

	/* Add the Exit commnad to the File menu. */
	JMenuItem jmiExit = new JMenuItem("Exit");
	jmFile.add(jmiExit);

	/* Add the action listener to the Exit command. */
	jmiExit.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent ae) {
		    System.exit(0);
		}
	    });

	/* Add the menu bar tothe main frame. */
	mainFrame.setJMenuBar(jmb);

	mainFrame.pack();
	mainFrame.setResizable(true);
	mainFrame.setLocationRelativeTo(null);
	mainFrame.setVisible(true);

    }
    
    private void setSquareState(final Square square, final SquareState state) {
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    SquarePanel sp = squares.get(square);
		    sp.setSquareState(state);
		}
	    });
    }

    private void drawGame(final Game game) {
	drawBoard(game.board());
    }

    private void drawBoard(final Board board) {
	for (Square square : Square.values()) {
	    setSquareState(square, board.get(square));
	}
    }

    private void setDot(int x, int y) {
	JLabel dot = new JLabel(Constants.GRID_DOT_ICON);
        dotsPanel.add(dot);
	int xDotCenter = (Constants.SQUARE_SIZE * x) - Constants.DOT_SIZE / 2;
	int yDotCenter = (Constants.SQUARE_SIZE * y) - Constants.DOT_SIZE / 2;
	dot.setBounds(xDotCenter, yDotCenter, Constants.DOT_SIZE, Constants.DOT_SIZE);    
    }

    public static void main(String[] args) {
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    new ReversiUI();
		}
	    });
    }

    private void appendToConsole(String line) {
	textArea.append(line + NEW_LINE);
    }

    private void execCommand(String command) {

	appendToConsole(PROMPT + command);

	StringTokenizer st = new StringTokenizer(command);
	int words = st.countTokens();
	Square square = null;
	SquareState state = null;
	if (words == 2) {
	    String w0 = st.nextToken();
	    String w1 = st.nextToken();
	    try {
		square = Square.valueOf(w0);
	    } catch (IllegalArgumentException iae) {
		appendToConsole(ANSWER + "Wrong value " + w0 + ". It is not a valid Square.");
	    }
	    try {
		state = SquareState.valueOf(w1);
	    } catch (IllegalArgumentException iae) {
		appendToConsole(ANSWER + "Wrong value " + w1 + ". It is not a valid SquareState.");
	    }
	    if (square != null && state != null) {
		appendToConsole(ANSWER + "Setting board square " + square + " to state " + state + ".");
		setSquareState(square, state);
	    }
	} else {
	    appendToConsole(ANSWER + "Not a command: " + command);
	}
    }

}