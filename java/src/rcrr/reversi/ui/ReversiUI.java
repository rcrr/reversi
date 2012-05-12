/*
 *  ReversiUI.java
 *
 *  Copyright (c) 2010, 2011, 2012 Roberto Corradini. All rights reserved.
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

import java.util.EnumMap;
import java.util.Map;
import java.util.StringTokenizer;

import java.awt.Container;
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

import javax.swing.JComboBox;
import javax.swing.BoxLayout;
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
import javax.swing.JButton;
import javax.swing.Box;
import javax.swing.SpinnerNumberModel;
import javax.swing.JSpinner;
import javax.swing.event.ChangeListener;
import javax.swing.event.ChangeEvent;

import java.io.PrintStream;
import java.io.OutputStream;
import java.io.IOException;

import java.net.URL;

import org.joda.time.Period;
import org.joda.time.Duration;

import rcrr.reversi.board.Board;
import rcrr.reversi.board.Player;
import rcrr.reversi.board.Square;
import rcrr.reversi.board.Column;
import rcrr.reversi.board.Row;
import rcrr.reversi.board.SquareState;

import rcrr.reversi.Game;
import rcrr.reversi.Actor;
import rcrr.reversi.Move;
import rcrr.reversi.GameSnapshot;
import rcrr.reversi.Strategy;
import rcrr.reversi.IagoStrategy;
import rcrr.reversi.HumanStrategy;

import java.util.concurrent.ExecutionException;
import java.lang.reflect.InvocationTargetException;

public class ReversiUI {

    private static final class AboutFrame extends JFrame implements ActionListener {
        AboutFrame() {
            super("About Reversi");
            setLayout(new FlowLayout());
            add(new JLabel(Constants.LOGO_128X128_ICON));
            JLabel programAndVersion = new JLabel("Reversi 1.0.0");
            programAndVersion.setFont(new Font("Courier", Font.BOLD, 24));
            add(programAndVersion);
            JLabel info = new JLabel("http://github.com/rcrr/reversi");
            info.setFont(new Font("Courier", Font.PLAIN, 10));
            add(info);
            add(Box.createRigidArea(new Dimension(0, 30)));
            JLabel copyright = new JLabel("Copyright (c) 2010, 2011, 2012");
            copyright.setFont(new Font("Courier", Font.PLAIN, 10));
            add(copyright);
            JLabel copyright2 = new JLabel("Roberto Corradini, rob_corradini@yahoo.it");
            copyright2.setFont(new Font("Courier", Font.PLAIN, 10));
            add(copyright2);
            add(Box.createRigidArea(new Dimension(0, 30)));
            JButton closeButton = new JButton("Close");
            add(closeButton);
            closeButton.addActionListener(this);
            setSize(280, 320);
            setResizable(false);
            setLocationRelativeTo(null);
            setVisible(true);
        }
        public void actionPerformed(ActionEvent e) {
            SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        dispose();
                    }
                });
        }
    }

    private static final class PreferencesFrame extends JFrame implements ActionListener {

        private static final int MIN_SEARCH_DEPTH = 1;
        private static final int MAX_SEARCH_DEPTH = 8;
        private static final int SEARCH_DEPTH_STEP = 1;

        private static final int MIN_GAME_DURATION = 5;
        private static final int MAX_GAME_DURATION = 60;
        private static final int GAME_DURATION_STEP = 5;

        private final ReversiUI ui;
        private final JSpinner searchDepthSpinner;
        private final JComboBox humanPlayerColor;
        private final JSpinner gameDurationSpinner;

        PreferencesFrame(final ReversiUI ui) {
            super("Preferences");
            this.ui = ui;

            JPanel p = new JPanel(new GridLayout(3, 2, 10, 10));
            p.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));

            JLabel humanPlayerColorLabel = new JLabel("Human player color");
            p.add(humanPlayerColorLabel);
            Player[] colors = { Player.BLACK, Player.WHITE };
            humanPlayerColor = new JComboBox(colors);
            humanPlayerColor.setSelectedItem(ui.humanPlayer);
            p.add(humanPlayerColor);

            JLabel gameDurationLabel = new JLabel("Game duration in minutes");
            p.add(gameDurationLabel);
            SpinnerNumberModel gameDurationModel = new SpinnerNumberModel(ui.gameDuration,
                                                                          MIN_GAME_DURATION,
                                                                          MAX_GAME_DURATION,
                                                                          GAME_DURATION_STEP);
            gameDurationSpinner = new JSpinner(gameDurationModel);
            ((JSpinner.DefaultEditor) gameDurationSpinner.getEditor()).getTextField().setEditable(false);
            p.add(gameDurationSpinner);

            JLabel searchDepthLabel = new JLabel("AI search depth");
            p.add(searchDepthLabel);
            SpinnerNumberModel searchDepthModel = new SpinnerNumberModel(ui.searchDepth,
                                                                         MIN_SEARCH_DEPTH,
                                                                         MAX_SEARCH_DEPTH,
                                                                         SEARCH_DEPTH_STEP);
            searchDepthSpinner = new JSpinner(searchDepthModel);
            ((JSpinner.DefaultEditor) searchDepthSpinner.getEditor()).getTextField().setEditable(false);
            p.add(searchDepthSpinner);

            JButton discardButton = new JButton("Discard");
            discardButton.addActionListener(this);

            JButton applyButton = new JButton("Apply");
            applyButton.addActionListener(this);

            //Lay out the buttons from left to right.
            JPanel buttonPane = new JPanel();
            buttonPane.setLayout(new BoxLayout(buttonPane, BoxLayout.LINE_AXIS));
            buttonPane.setBorder(BorderFactory.createEmptyBorder(0, 10, 10, 10));
            buttonPane.add(Box.createHorizontalGlue());
            buttonPane.add(discardButton);
            buttonPane.add(Box.createRigidArea(new Dimension(10, 0)));
            buttonPane.add(applyButton);

            Container contentPane = getContentPane();
            contentPane.add(p, BorderLayout.CENTER);
            contentPane.add(buttonPane, BorderLayout.PAGE_END);

            setResizable(false);
            setLocationRelativeTo(null);
            pack();
            setVisible(true);
        }

        public void actionPerformed(final ActionEvent e) {
            SwingUtilities.invokeLater(new Runnable() {
                    public void run() {
                        if (e.getActionCommand().equals("Discard")) {
                            dispose();
                        } else if (e.getActionCommand().equals("Apply")) {
                            ui.searchDepth = (Integer) searchDepthSpinner.getValue();
                            ui.humanPlayer = (Player) humanPlayerColor.getSelectedItem();
                            ui.gameDuration = (Integer) gameDurationSpinner.getValue();
                            dispose();
                        } else {
                            throw new RuntimeException("Unknown action command.");
                        }
                    }
                });
        }

    }

    private enum State {

        NO_GAME("No game present"),
        INITIALIZING("Initializing") {
            @Override public void reach(final ReversiUI ui) {
                super.reach(ui);
                ui.deactivateMenuItem(ui.jmiNewGame);
            }
            @Override public void leave(final ReversiUI ui) {
                super.leave(ui);
                ui.activateMenuItem(ui.jmiNewGame);
                ui.activateMenuItem(ui.jmiTranscript);
                ui.setMessageLabel("Begin to play activating the Play menu.");
            }
        },
        GAME_PAUSED("Game paused") {
            @Override public void reach(final ReversiUI ui) {
                super.reach(ui);
                ui.activateMenuItem(ui.jmiPlay);
            }
        },
        PLAYER_MOVING("Moving") {
            @Override public String displayName(final ReversiUI ui) {
                return ui.game.player() + "'s turn.";
            }
            @Override public void reach(final ReversiUI ui) {
                super.reach(ui);
                ui.deactivateMenuItem(ui.jmiPlay);
                ui.activateMenuItem(ui.jmiPause);
            }
            @Override public void leave(final ReversiUI ui) {
                super.leave(ui);
                ui.deactivateMenuItem(ui.jmiPause);
                ui.activateMenuItem(ui.jmiPlay);
                int value = IagoStrategy.IAGO_EVAL_FUNCTION.eval(ui.game.position());
                ui.setMessageLabel("AI one ply board evaluation is: " + value + ".");
            }
        },
        GAME_OVER("Game over") {
            @Override public void reach(final ReversiUI ui) {
                super.reach(ui);
                ui.setMessageLabel(ui.gameOverMessage());
            }       
        };

        private boolean verbose = false;

        private String displayName;

        private State(final String displayName) {
            this.displayName = displayName;
        }

        public String displayName(final ReversiUI ui) { return this.displayName; }

        public void leave(final ReversiUI ui) {
            if (verbose) System.out.println("Leaving state " + this + "....");
        }
        public void reach(final ReversiUI ui) {
            if (verbose) System.out.println("Reaching state " + this + "....");
        }
        public static void transition(final State from, final State to) { } 
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
            setState(State.INITIALIZING);
            deactivateCommand();
            clearConsole();
            newGame();
            setState(State.GAME_PAUSED);
            return null;
        }
        @Override protected void done() {
            drawGame(game);
        }
    }

    private final class MoveFinder extends SwingWorker<Void, Void> {
        @Override public Void doInBackground() {
            setState(State.PLAYER_MOVING);
            Move move = game.move();
            drawGame(game);
            setState(State.GAME_PAUSED);
            return null;
        }
        @Override protected void done() {
        }
    }

    private final class GamePlayer extends SwingWorker<Void, Void> {
        @Override public Void doInBackground() {
            while (!pauseRequested && game.areThereAvailableMoves()) {
                MoveFinder mf = new MoveFinder();
                mf.execute();
                try {
                    mf.get();
                } catch (InterruptedException ie) {
                    throw new RuntimeException(ie);
                } catch (ExecutionException ee) {
                    throw new RuntimeException(ee);
                }
            }
            if (!game.areThereAvailableMoves()) {
                setState(State.GAME_OVER);
            }
            return null;
        }
    }

    private boolean pauseRequested;
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

    private JMenuItem jmiPlay;
    private JMenuItem jmiNewGame;
    private JMenuItem jmiPause;
    private JMenuItem jmiTranscript;

    /** The command text field. */
    private JTextField commandTextField;

    private Game game;
    private Move move;
    private int searchDepth;
    private int gameDuration;
    private Player humanPlayer;

    private PrintStream consolePrintStream;
    private ThreadEvent resultsReady;

    public ReversiUI() {

        pauseRequested = false;
        state = State.NO_GAME;
        game = null;
        move = null;
        gameDuration = DEFAULT_GAME_DURATION_IN_MINUTES; 
        searchDepth = IagoStrategy.DEFAULT_DEPTH;
        humanPlayer = Player.BLACK;
        resultsReady = new ThreadEvent();

        mainFrame = new JFrame("Reversi");
        mainFrame.getContentPane().setLayout(new GridBagLayout());
        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.HORIZONTAL;
        mainFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

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
        stateLabel = new JLabel(state.displayName(this));
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
        messageLabel = new JLabel("Welcome to the Reversi board, start a new game.");
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

        /* Add the menu bar tothe main frame. */
        mainFrame.setJMenuBar(jmb);

        /* Create the Game menu, with the Exit commnad. */
        JMenu jmGame = new JMenu("Game");
        jmb.add(jmGame);

        /* Add the New game commnad to the Game menu. */
        jmiNewGame = new JMenuItem("New game");
        jmGame.add(jmiNewGame);

        /* Add the action listener to the New game command. */
        jmiNewGame.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    new GameCreator().execute();
                }
            });

        /* Add the Play commnad to the Game menu. */
        jmiPlay = new JMenuItem("Play");
        jmiPlay.setEnabled(false);
        jmGame.add(jmiPlay);

        /* Add the action listener to the Play command. */
        jmiPlay.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    pauseRequested = false;
                    new GamePlayer().execute();
                }
            });

        /* Add the Pause commnad to the Game menu. */
        jmiPause = new JMenuItem("Pause");
        jmiPause.setToolTipText("Pause will be entered at the end of this turn.");
        jmiPause.setEnabled(false);
        jmGame.add(jmiPause);

        /* Add the action listener to the Pause command. */
        jmiPause.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    pauseRequested = true;
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

        /* Create the Settings menu, with the Preferences commnad. */
        JMenu jmSettings = new JMenu("Settings");
        jmb.add(jmSettings);

        /* Add the Prefernces commnad to the Settings menu. */
        JMenuItem jmiPreferences = new JMenuItem("Preferences");
        jmSettings.add(jmiPreferences);

        /* Add the action listener to the Preferences command. */
        jmiPreferences.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    new PreferencesFrame(ReversiUI.this);
                }
            });

        /* Create the Data menu, with the Transcript commnad. */
        JMenu jmData = new JMenu("Data");
        jmb.add(jmData);

        /* Add the Transcript commnad to the Data menu. */
        jmiTranscript = new JMenuItem("Transcript");
        jmiTranscript.setEnabled(false);
        jmData.add(jmiTranscript);

        /* Add the action listener to the Transcript command. */
        jmiTranscript.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    new TranscriptFrame(ReversiUI.this);
                }
            });

        /* Create the Help menu, with the About commnad. */
        JMenu jmHelp = new JMenu("Help");
        jmb.add(jmHelp);

        /* Add the About commnad to the Help menu. */
        JMenuItem jmiAbout = new JMenuItem("About");
        jmHelp.add(jmiAbout);

        /* Add the action listener to the About command. */
        jmiAbout.addActionListener(new ActionListener() {
                public void actionPerformed(ActionEvent ae) {
                    new AboutFrame();
                }
            });

        mainFrame.pack();
        mainFrame.setResizable(false);
        mainFrame.setLocationRelativeTo(null);
        mainFrame.setVisible(true);

    }

    Game game() {
        return this.game;
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    new ReversiUI();
                }
            });
    }

    private void newGame() {

        this.pauseRequested = false;
        this.move = null;
        this.resultsReady = new ThreadEvent();

        Actor black, white;

        final Actor ai = new Actor.Builder()
            .withName("Iago")
            .withStrategy(new IagoStrategy(searchDepth))
            .build();

        final Actor human = new Actor.Builder()
            .withName("Human")
            .withStrategy(new HumanStrategyUI())
            .build();

        if (humanPlayer == Player.BLACK) {
            black = human;
            white = ai;
        } else {
            black = ai;
            white = human;
        };

        game = Game.initialGame(black,
                                white,
                                Period.minutes(gameDuration).toStandardDuration(),
                                consolePrintStream);
    }

    private void setSquareState(final Square square, final SquareState state) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    SquarePanel sp = squares.get(square);
                    sp.setSquareState(state);
                }
            });
    }

    private void setMessageLabel(final String message) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    messageLabel.setText(message);
                }
            });
    }

    private void setState(final State newState) {
        final State to = newState;
        final State from = state;
        from.leave(this);
        to.reach(this);
        State.transition(from, to);
        ReversiUI.this.state = newState;
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    stateLabel.setText(ReversiUI.this.state.displayName(ReversiUI.this));
                }
            });
    }

    private State getState() {
        return this.state;
    }

    private void drawGame(final Game game) {
        invokeInDispatchThreadIfNeeded(new Runnable() {
                public void run() {
                    drawBoard(game.board());
                    consolePrintStream.print(game.lastGameSnapshot().printGameSnapshot());
                }
            }, WAIT);
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

    private void activateMenuItem(final JMenuItem item) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    item.setEnabled(true);
                }
            });
    }

    private void deactivateMenuItem(final JMenuItem item) {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    item.setEnabled(false);
                }
            });
    }

    private void activateCommand() {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    commandTextField.setBackground(Color.WHITE);
                    commandTextField.setForeground(Color.BLACK);
                    commandTextField.setEditable(true);
                    commandTextField.setEnabled(true);
                    commandTextField.setText(null);
                    commandTextField.requestFocusInWindow();
                }
            });
    }

    private void deactivateCommand() {
        SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    commandTextField.setEnabled(false);
                    commandTextField.setEditable(false);
                    commandTextField.setBackground(Color.BLACK);
                    commandTextField.setForeground(Color.LIGHT_GRAY);
                }
            });
    }

    private String gameOverMessage() {
        final int discCount = game.countDiscDifference();
        if (discCount == 0) return "Game is a draw.";
        if (discCount > 0) {
            return "Black wins by a disc difference of " + discCount + " pieces.";
        } else {
            return "White wins by a disc difference of " + (- discCount) + " pieces.";
        }
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
        if (newMove != null && game.validateMove(newMove)) {
            this.move = newMove;
            deactivateCommand();
            resultsReady.signal();
        } else {
            consolePrintStream.println(command + " is not a legal move.");
        }
    }

    private final static boolean WAIT = true;
    private final static boolean NO_WAIT = false;

    public static void invokeInDispatchThreadIfNeeded(final Runnable runnable, final boolean wait) {
        if (SwingUtilities.isEventDispatchThread()) {
            runnable.run();
        } else {
            if (wait) {
                try {
                    SwingUtilities.invokeAndWait(runnable);
                } catch (InterruptedException ie) {
                    throw new RuntimeException(ie);
                } catch (InvocationTargetException ite) {
                    throw new RuntimeException(ite);
                }
            } else {
                SwingUtilities.invokeLater(runnable);
            }
        }
    }

}
