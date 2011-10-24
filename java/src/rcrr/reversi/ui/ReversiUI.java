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
import java.awt.Font;
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
import javax.swing.SwingWorker;
import javax.swing.JSeparator;

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
import rcrr.reversi.Move;
import rcrr.reversi.GameSnapshot;
import rcrr.reversi.Strategy;
import rcrr.reversi.IagoStrategy;
import rcrr.reversi.HumanStrategy;
import rcrr.reversi.Player;

public class ReversiUI {

    private enum State {
	INITIALIZING,
	GAME_PAUSED,
	BLACK_MOVING,
	WHITE_MOVING,
	GAME_OVER
    }

    private static final class TextAreaOutputStream extends OutputStream {
	private final JTextArea textArea;
	public TextAreaOutputStream(final JTextArea textArea) {
	    this.textArea = textArea;
	}
	@Override
	public void flush() { }
	@Override
	public void close() { }
	@Override
	public void write(final int b) throws IOException {		
	    SwingUtilities.invokeLater(new Runnable() {
		    public void run() {
			textArea.append(Character.toString((char)b));
			textArea.setCaretPosition(textArea.getText().length());
		    }
		});
	}
    }

    private static class ThreadEvent {
	private final Object lock = new Object();
	public void signal() {
	    synchronized (lock) {
		lock.notify();
	    }
	}
	public void await() throws InterruptedException {
	    synchronized (lock) {
		lock.wait();
	    }
	}
    }

    private final class HumanStrategyUI implements Strategy {
	public HumanStrategyUI() { }
	public final Move move(final GameSnapshot gameSnapshot) {
	    activateCommand();
	    try {
		resultsReady.await();
	    } catch (InterruptedException ie) {
		throw new RuntimeException(ie);
	    }
	    return move;
	}
    }

    private final class GameCreator extends SwingWorker<Void, Void> {
	@Override public Void doInBackground() {
	    deactivateCommand();
	    clearConsole();
	    newGame();
	    return null;
	}
	@Override protected void done() {
	    drawGame(game);
	}
    }

    private final class MoveFinder extends SwingWorker<Void, Void> {
	@Override public Void doInBackground() {
	    setMover();
	    game.move();
	    setState(State.GAME_PAUSED);
	    return null;
	}
	@Override protected void done() {
	    drawGame(game);
	}
    }

    private void setMover() {
	Player player = game.player();
	if (player == Player.BLACK) {
	    setState(State.BLACK_MOVING);
	} else {
	    setState(State.WHITE_MOVING);
	}
    }

    private State state;

    private static final int DEFAULT_GAME_DURATION_IN_MINUTES = 30;

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
    private JPanel statePanel;
    private JPanel messagePanel;

    private JLabel stateLabel;
    private JLabel messageLabel;

    private JTextArea textArea;

    /** The command text field. */
    private JTextField commandTextField;

    private Game game;

    private Move move;

    private PrintStream consolePrintStream;
    private ThreadEvent resultsReady;

    public ReversiUI() {

	state = State.INITIALIZING;
	game = null;
	move = null;
	resultsReady = new ThreadEvent();

	mainFrame = new JFrame("Reversi");
	mainFrame.getContentPane().setLayout(new GridBagLayout());
	GridBagConstraints c = new GridBagConstraints();
	c.fill = GridBagConstraints.HORIZONTAL;
	mainFrame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

	JPanel boardPanel = new JPanel();
	boardPanel.setBackground(Constants.BACKGROUND_COLOR);
	TitledBorder boardPanelTitle = BorderFactory.createTitledBorder("Board");
	boardPanel.setBorder(boardPanelTitle);

	final int gsd = 8 * Constants.SQUARE_SIZE;
	final int lsd = gsd + (Constants.LABELS_HEIGHT + (2 * Constants.LABELS_GAP) + Constants.SQUARES_GAP);
        final Dimension gridSize = new Dimension(gsd, gsd);
        final Dimension labelsSize = new Dimension(lsd, lsd);
	final int gs = Constants.LABELS_HEIGHT + Constants.LABELS_GAP + Constants.SQUARES_GAP;
	
        // Use a Layered Pane for this application.
	boardPane = new JLayeredPane();
	c.gridx = 0;
	c.gridy = 0;
        boardPanel.add(boardPane);
        mainFrame.getContentPane().add(boardPanel, c);
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

	JPanel bottomPanel = new JPanel(new GridBagLayout());
	bottomPanel.setBackground(Constants.BACKGROUND_COLOR);
	GridBagConstraints cBottomPanel = new GridBagConstraints();
	c.gridx = 0;
	c.gridy = 1;
	mainFrame.getContentPane().add(bottomPanel, c);

	// Add the state JPanel
	TitledBorder statePanelTitle = BorderFactory.createTitledBorder("State");
	statePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 8));
	statePanel.setBorder(statePanelTitle);
	statePanel.setBackground(Constants.BACKGROUND_COLOR);
	cBottomPanel.fill = GridBagConstraints.VERTICAL;
	cBottomPanel.gridx = 0;
	cBottomPanel.gridy = 0;
	cBottomPanel.anchor = GridBagConstraints.PAGE_START;
	bottomPanel.add(statePanel, cBottomPanel);
	stateLabel = new JLabel(state.toString());
	Dimension d = stateLabel.getPreferredSize();
        stateLabel.setPreferredSize(new Dimension(d.width + 20, d.height));
        stateLabel.setFont(new Font("Monospaced", Font.PLAIN, 12)); 
	statePanel.add(stateLabel);

	// Add the command data entry JTextField
	TitledBorder commandPanelTitle = BorderFactory.createTitledBorder("Move");
	commandPanel = new JPanel();
	commandPanel.setBorder(commandPanelTitle);
	commandPanel.setBackground(Constants.BACKGROUND_COLOR);
	cBottomPanel.fill = GridBagConstraints.VERTICAL;
	cBottomPanel.gridx = 1;
	cBottomPanel.gridy = 0;
	bottomPanel.add(commandPanel, cBottomPanel);
	commandTextField = new JTextField(8);
        commandTextField.setFont(new Font("Monospaced", Font.PLAIN, 12)); 
	commandTextField.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent event) {
		    move(commandTextField.getText());
		}
	    });
	deactivateCommand();
	commandPanel.add(commandTextField);

	// Add the message JPanel
	TitledBorder messagePanelTitle = BorderFactory.createTitledBorder("Message");
	messagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT, 6, 8));
	messagePanel.setBorder(messagePanelTitle);
	messagePanel.setBackground(Constants.BACKGROUND_COLOR);
	cBottomPanel.fill = GridBagConstraints.BOTH;
	cBottomPanel.gridx = 2;
	cBottomPanel.gridy = 0;
	cBottomPanel.weightx = 1;
	bottomPanel.add(messagePanel, cBottomPanel);
	messageLabel = new JLabel("messages ....");
        messageLabel.setFont(new Font("Monospaced", Font.PLAIN, 12)); 
	messagePanel.add(messageLabel);

	TitledBorder consolePanelTitle = BorderFactory.createTitledBorder("Console");
	consolePanel = new JPanel(new GridBagLayout());
	consolePanel.setBorder(consolePanelTitle);
	consolePanel.setBackground(Constants.BACKGROUND_COLOR);
	c.gridx = 1;
	c.gridy = 0;
	c.gridheight = 2;
	c.fill = GridBagConstraints.BOTH;
	mainFrame.getContentPane().add(consolePanel, c);
        textArea = new JTextArea(40, 60);
        textArea.setEditable(false);
        textArea.setFont(new Font("Monospaced", Font.PLAIN,10));
        textArea.setBackground(Color.BLACK);
        textArea.setForeground(Color.LIGHT_GRAY);
        JScrollPane scrollPane = new JScrollPane(textArea);
	scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
	scrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS);
        //Add Components to this panel.
        GridBagConstraints c2 = new GridBagConstraints();
        c2.fill = GridBagConstraints.BOTH;
        c2.weightx = 1.0;
        c2.weighty = 1.0;
        consolePanel.add(scrollPane, c2);

	consolePrintStream = new PrintStream(new TextAreaOutputStream(textArea));	

	/* Create the menu bar. */
	JMenuBar jmb = new JMenuBar();

	/* Create the Game menu, with the Exit commnad. */
	JMenu jmGame = new JMenu("Game");
	jmb.add(jmGame);

	/* Add the New game commnad to the Game menu. */
	JMenuItem jmiNewGame = new JMenuItem("New game");
	jmGame.add(jmiNewGame);

	/* Add the action listener to the New game command. */
	jmiNewGame.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent ae) {
		    new GameCreator().execute();
		}
	    });

	/* Add the Play commnad to the Game menu. */
	JMenuItem jmiPlay = new JMenuItem("Play");
	jmGame.add(jmiPlay);

	/* Add the action listener to the Play command. */
	jmiPlay.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent ae) {
		    new MoveFinder().execute();
		}
	    });

	/* Add the Exit commnad to the Game menu. */
	JMenuItem jmiExit = new JMenuItem("Exit");
	jmGame.add(new JSeparator());
	jmGame.add(jmiExit);

	/* Add the action listener to the Exit command. */
	jmiExit.addActionListener(new ActionListener() {
		public void actionPerformed(ActionEvent ae) {
		    System.exit(0);
		}
	    });

	/* Add the menu bar tothe main frame. */
	mainFrame.setJMenuBar(jmb);

	mainFrame.pack();
	mainFrame.setResizable(false);
	mainFrame.setLocationRelativeTo(null);
	mainFrame.setVisible(true);

    }

    public static void main(String[] args) {
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    new ReversiUI();
		}
	    });
    }

    private void newGame() {
	game = Game.initialGame(new Actor.Builder()
				.withName("Iago")
				.withStrategy(new IagoStrategy())
				.build(),
				new Actor.Builder()
				.withName("Human")
				.withStrategy(new HumanStrategyUI())
				.build(),
				Period.minutes(DEFAULT_GAME_DURATION_IN_MINUTES).toStandardDuration(),
				consolePrintStream);
	setState(State.GAME_PAUSED);
    }

    private void setSquareState(final Square square, final SquareState state) {
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    SquarePanel sp = squares.get(square);
		    sp.setSquareState(state);
		}
	    });
    }

    private void setState(final State state) {
	this.state = state;
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    stateLabel.setText(state.toString());
		}
	    });
    }

    private State getState() {
	return this.state;
    }

    private void drawGame(final Game game) {
	drawBoard(game.board());
	consolePrintStream.print(game.lastGameSnapshot().printGameSnapshot());
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

    private void clearConsole() {
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    textArea.setText(null);
		}
	    });
    }

    private void activateCommand() {
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    commandTextField.setBackground(Color.WHITE);
		    commandTextField.setForeground(Color.BLACK);
		    commandTextField.setEditable(true);
		    commandTextField.setText(null);
		    commandTextField.requestFocusInWindow();
		}
	    });
    }

    private void deactivateCommand() {
	SwingUtilities.invokeLater(new Runnable() {
		public void run() {
		    commandTextField.setEditable(false);
		    commandTextField.setBackground(Color.BLACK);
		    commandTextField.setForeground(Color.LIGHT_GRAY);
		}
	    });
    }

    private void move(final String command) {
        Move newMove = null;
	if (command != null) {
	    if (command.equalsIgnoreCase("resign")) {
		newMove = Move.valueOf(Move.Action.RESIGN);
	    } else if (command.equalsIgnoreCase("pass")) {
		newMove = Move.valueOf(Move.Action.PASS);
	    } else {
		try {
		    newMove = Move.valueOf(Square.getInstance(command));
		} catch (IllegalArgumentException iae) {
		    consolePrintStream.println(command + " is not a move.");
		}
	    }
	}
	consolePrintStream.println("move=" + newMove);
	this.move = newMove;
	deactivateCommand();
	resultsReady.signal();
    }

}